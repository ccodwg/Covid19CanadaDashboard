# server
server <- function(input, output, session) {
  
  # pop-up alert upon app start
  if (!(identical(readLines("text/alert_title.html"), character(0)) &
        identical(readLines("text/alert_title.html"), character(0)))) {
    shinyalert(
      title = readLines("text/alert_title.html"),
      text = readLines("text/alert_text.html"),
      type = "",
      html = TRUE,
      size = "s",
      closeOnClickOutside = TRUE,
      closeOnEsc = TRUE
    ) 
  }
  
  # check if dataset has been updated on GitHub and if so, download it and redeploy the app
  observe({
    
    ## run only if app is running on the server, not locally
    if (!is_local) {
      
      ### run every 5 minutes
      invalidateLater(1000 * 60 * 5, session)
      
      ### report run
      cat(paste("Checking for data update:", Sys.time()), fill = TRUE)
      
      ### read update_time.txt from GitHub repo
      new_time <- readLines("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/update_time.txt")
      
      ### check if update_time.txt is different from current version
      if (identical(new_time, update_time)) {
        cat("No data update required.", fill = TRUE)
      } else {
        cat("Updating data...", fill = TRUE)
        source("data_updater.R") # update data
        cat("Redeploying dashboard...", fill = TRUE)
        source("dashboard_redeploy.R") # redeploy dashboard
      } 
      
    }
    
  })
  
  # render sidebar
  output$sidebar_controls <- renderUI({
    if (input$tab %in% c(
      "tab_trends",
      "tab_maps",
      "tab_cases",
      "tab_mortality",
      "tab_recovered",
      "tab_testing",
      "tab_vaccines",
      "tab_travel"
    )) {
      list(
        selectInput(
          "prov",
          "Province",
          choices = c(
            "All provinces" = "Canada",
            "Alberta" = "Alberta",
            "British Columbia" = "BC",
            "Manitoba" = "Manitoba",
            "New Brunswick" = "New Brunswick",
            "Newfoundland and Labrador" = "NL",
            "Northwest Territories" = "NWT",
            "Nova Scotia" = "Nova Scotia",
            "Nunavut" = "Nunavut",
            "Ontario" = "Ontario",
            "Prince Edward Island" = "PEI",
            "Quebec" = "Quebec",
            "Saskatchewan" = "Saskatchewan",
            "Yukon" = "Yukon"
          )
        ),
        dateRangeInput(
          "date_range",
          "Date range",
          start = date_min,
          end = date_max,
          min = date_min,
          max = date_max,
          format = "yyyy/mm/dd"
        )
      )
    }
  })
  
  # return plot instructing the user no cases match their filter settings
  plot_no_data <- function(completely_blank = FALSE) {
    if (completely_blank) {
      ## select message
      m <- ""
    } else {
      m <-
        "No data match your filter settings.\nPlease try again with different settings."
    }
    
    ## produce blank plot w/ or w/out message
    plot_ly() %>%
      add_text(x = 0, y = 0, text = m, textfont = list(size = 25), textposition = "center") %>%
      layout(
        xaxis = axis_hide,
        yaxis = axis_hide,
        legend = plotly_legend
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = plotly_buttons
      )
  }
  
  # reactive data
  
  ## function: get and filter data
  get_data <- function(dataset, var_date, filter_province = ifelse(isTruthy(input[["prov"]]), input$prov, "Canada"), filter_date_lower = ifelse(isTruthy(input[["date_range"]]), input$date_range[1], date_min), filter_date_upper = ifelse(isTruthy(input[["date_range"]]), input$date_range[2], date_max), filter_province_ignore = FALSE, filter_date_ignore = FALSE) {
    get(dataset) %>%
      ### filter by province (if applicable)
      {if (!filter_province_ignore & filter_province != "Canada") filter(., province == filter_province) else .} %>%
      ### filter by date range (if applicable)
      {if (!filter_date_ignore) filter(., !!sym(var_date) >= filter_date_lower & !!sym(var_date) <= filter_date_upper) else .}
  }
  
  ## function: define comparison variable name
  get_var_comp <- function(var_val, comparison_window, comparison_scale) {
    if (comparison_scale == "per-capita") {
      if (comparison_window > 1) {
        paste(var_val, "per_100k", "avg", comparison_window, "day", sep = "_")
      } else {
        paste(var_val, "per_100k", sep = "_")
      }
    } else {
      if (comparison_window > 1) {
        paste(var_val, "avg", comparison_window, "day", sep = "_")
      } else {
        var_val
      }
    }
  }
  
  ## function: get and filter data for provincial comparisons
  get_data_comp <- function(dataset, var_date, var_val, comparison_window, comparison_scale) {
    
    ### ensure comparison_window is an integer
    comparison_window <- as.integer(comparison_window)
    
    ### comparison variable name
    var_comp <- get_var_comp(var_val, comparison_window, comparison_scale)
    
    ### calculate comparisons
    get_data(dataset, var_date, filter_province_ignore = TRUE, filter_date_ignore = TRUE) %>%
      filter(province != "Repatriated") %>%
      select(province_short, pop, !!sym(var_val)) %>%
      rename(!!sym(var_comp) := !!sym(var_val)) %>%
      group_by(province_short) %>%
      ### select only data from the window (e.g., most recent 7 days)
      slice_tail(n = comparison_window) %>%
      ### convert to per-capita (if applicable)
      {if (comparison_scale == "per-capita") mutate(., !!sym(var_comp) := !!sym(var_comp) / pop * 100000) else .} %>%
      summarize(pop = max(pop), !!sym(var_comp) := mean(!!sym(var_comp)), .groups = "drop")
  }
  
  ## cases
  data_cases <- reactive({
    get_data("cases", "date_report")
  })
  
  ## mortality
  data_mortality <- reactive({
    get_data("mortality", "date_death_report")
  })
  
  ## cases time series
  data_ts_cases <- reactive({
    get_data("ts_cases", "date_report")
  })
  
  ## mortality time series
  data_ts_mortality <- reactive({
    get_data("ts_mortality", "date_death_report")
  })
  
  ## recovered time series
  data_ts_recovered <- reactive({
    get_data("ts_recovered", "date_recovered")
  })
  
  ## testing time series
  data_ts_testing <- reactive({
    get_data("ts_testing", "date_testing")
  })
  
  ## vaccine administration time series
  data_ts_vaccine_administration <- reactive({
    get_data("ts_vaccine_administration", "date_vaccine_administered")
  })
  
  ## vaccine distribution time series
  data_ts_vaccine_distribution <- reactive({
    get_data("ts_vaccine_distribution", "date_vaccine_distributed")
  })
  
  ## vaccine distribution time series
  data_ts_vaccine_completion <- reactive({
    get_data("ts_vaccine_completion", "date_vaccine_completed")
  })
  
  ## case time series (health regions)
  data_ts_cases_hr <- reactive({
    get_data("ts_cases_hr", "date_report")
  })
  
  ## mortality time series (health regions)
  data_ts_mortality_hr <- reactive({
    get_data("ts_mortality_hr", "date_death_report")
  })
  
  ## case time series (filter by date only for pie chart)
  data_ts_cases_pie <- reactive({
    get_data("ts_cases", "date_report", filter_province_ignore = TRUE)
  })
  
  ## mortality time series (filter by date only for pie chart)
  data_ts_mortality_pie <- reactive({
    get_data("ts_mortality", "date_death_report", filter_province_ignore = TRUE)
  })
  
  ## comparisons data cases
  data_comp_cases <- reactive({
    get_data_comp("ts_cases", "date_report", "cases", comparison_window = input$window_comp_cases, comparison_scale = input$scale_comp_cases)
  })
  
  ## comparisons data mortality
  data_comp_mortality <- reactive({
    get_data_comp("ts_mortality", "date_death_report", "deaths", comparison_window = input$window_comp_mortality, comparison_scale = input$scale_comp_mortality)
  })
  
  ## comparisons data testing
  data_comp_testing <- reactive({
    get_data_comp("ts_testing", "date_testing", "testing", comparison_window = input$window_comp_testing, comparison_scale = input$scale_comp_testing)
  })
  
  # render info tables
  
  ## info: testing
  output$table_info_testing <- renderDT({
    info_testing %>%
      datatable(
        class = "stripe compact hover",
        rownames = FALSE,
        escape = FALSE,
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          compact = TRUE,
          autoWidth = TRUE
        )
      )
  })
  
  # summary numbers for overview tab
  
  ## function: summary numbers for overview tab
  value_box_summary <- function(table_overview, var_cum, var_update, lab_title, colour_box, update_type = c("new", "change"), val_cum_format = c("raw", "million"), font_size_update = "40%") {
    
    ### check update type (new or change +/-)
    match.arg(update_type, c("new", "change", several.ok = FALSE))
    
    ### check cumulative value format (raw or millions)
    match.arg(val_cum_format, c("raw", "million"), several.ok = FALSE)
    
    ### calculate values
    val_cum <- table_overview %>%
      filter(Province == "Canada") %>%
      pull(var_cum) %>%
      {
        if (val_cum_format == "raw") {
          formatC(., format = "f", digits = 0, big.mark = ",")
        } else if (val_cum_format == "million") {
          paste0(formatC(. / 1000000, digits = 2, format = "f", big.mark = ","), "m")
        }
      }
    val_update <- table_overview %>% filter(Province == "Canada") %>% pull(var_update)
    
    ### value box
    HTML(paste0(
      tags$p(val_cum, style = "font-size: 95%;"),
      "<div style='font-size:",
      font_size_update,
      "'>(",
      {if (update_type == "new") {
        paste0(
          format(val_update, big.mark = ","),
          " new)</div>"
        )
      } else {
        paste0(
          "Change: ",
          if (val_update >= 0) "+" else "",
          format(val_update, big.mark = ","),
          ")</div>"
        )}})) %>%
      valueBox(lab_title,
               color = colour_box)
  }
  
  ## cases
  output$value_box_summary_cases <- renderValueBox({
    value_box_summary(table_overview, "Cumulative cases", "Cases (new)", "Reported cases", "orange", update_type = "new", val_cum_format = "raw")
  })
  
  ## active cases
  output$value_box_summary_active <- renderValueBox({
    value_box_summary(table_overview, "Active cases", "Active cases (change)", "Active cases", "purple", update_type = "change", val_cum_format = "raw")
  })
  
  ## recovered
  output$value_box_summary_recovered <- renderValueBox({
    value_box_summary(table_overview, "Cumulative recovered", "Recovered (new)", "Total recovered", "light-blue", update_type = "new", val_cum_format = "raw")
  })
  
  ## mortality
  output$value_box_summary_mortality <- renderValueBox({
    value_box_summary(table_overview, "Cumulative deaths", "Deaths (new)", "Total deaths", "navy", update_type = "new", val_cum_format = "raw")
  })
  
  ## vaccine doses administered
  output$value_box_summary_doses_administered <- renderValueBox({
    value_box_summary(table_overview, "Cumulative vaccine doses administered", "Vaccine doses administered (new)", "Vaccine doses administered", "fuchsia", update_type = "new", val_cum_format = "million")
  })
  
  ## people fully vaccinated
  output$value_box_summary_fully_vaccinated <- renderValueBox({
    value_box_summary(table_overview, "Cumulative people fully vaccinated", "People fully vaccinated (new)", "People fully vaccinated", "aqua", update_type = "new", val_cum_format = "raw")
  })
  
  ## hospitalized
  output$value_box_summary_hosp <- renderValueBox({
    value_box_summary(table_overview, "Hospitalized", "Hospitalized (change)", "Hospitalized", "maroon", update_type = "change", val_cum_format = "raw")
  })
  
  ## testing
  output$value_box_summary_testing <- renderValueBox({
    value_box_summary(table_overview, "Cumulative testing", "Testing (new)", "Total testing", "olive", update_type = "new", val_cum_format = "million")
  })
  
  # title for province case map for overview tab
  output$title_choropleth_overview_cases <- renderText({
    
    ### don't run without inputs defined
    req(input$window_choropleth_overview_cases)
    
    ### render text
    paste("Reported cases by province/territory in the last", input$window_choropleth_overview_cases, "days")
  })
  
  # province case map for overview tab
  output$plot_choropleth_overview_cases <- renderPlotly({
    
    ### don't run without inputs defined
    req(input$window_choropleth_overview_cases)
    
    ### join provincial case numbers for relevant window to provincial map
    dat <- geo_prov_simple %>%
      left_join(
        data_ts_cases() %>%
          select(province_short, cases) %>%
          group_by(province_short) %>%
          slice_tail(n = input$window_choropleth_overview_cases) %>%
          summarize(cases = sum(cases), .groups = "drop"),
        by = "province_short"
      )
    
    ### even out colour scale by rooting case numbers
    dat <- dat %>%
      mutate(cases_colour = cases^(1/3))
    
    ### define value labels
    labs <- data.frame(
      province_short = dat[["province_short"]],
      x = as.numeric(st_coordinates(suppressWarnings((st_centroid(dat))))[, 1]),
      y = as.numeric(st_coordinates(suppressWarnings((st_centroid(dat))))[, 2]),
      lab_cases = format(dat[["cases"]], big.mark = ",", trim = TRUE),
      ### arrows for NS, NB, PE to avoid overlap
      show_arrow = ifelse(dat[["province_short"]] %in% c("NS", "NB", "PE"), TRUE, FALSE),
      arrow_x = as.numeric(st_coordinates(suppressWarnings((st_centroid(dat))))[, 1]),
      arrow_y = as.numeric(st_coordinates(suppressWarnings((st_centroid(dat))))[, 2]),
      stringsAsFactors = FALSE
    )
    
    ### manually nudge some label positions (x = label, ax = arrowhead tail)
    labs[labs$province_short == "AB", "y"] <- labs[labs$province_short == "AB", "y"] + 0.5
    labs[labs$province_short == "MB", "y"] <- labs[labs$province_short == "MB", "y"] + 0.5
    labs[labs$province_short == "ON", "y"] <- labs[labs$province_short == "ON", "y"] + 0.5
    labs[labs$province_short == "BC", "x"] <- labs[labs$province_short == "BC", "x"] + 0.5
    labs[labs$province_short == "BC", "y"] <- labs[labs$province_short == "BC", "y"] - 2.5
    labs[labs$province_short == "SK", "y"] <- labs[labs$province_short == "SK", "y"] - 2.5
    labs[labs$province_short == "NL", "x"] <- labs[labs$province_short == "NL", "x"] + 4
    labs[labs$province_short == "NL", "y"] <- labs[labs$province_short == "NL", "y"]
    labs[labs$province_short == "NU", "x"] <- labs[labs$province_short == "NU", "x"] - 7
    labs[labs$province_short == "NU", "y"] <- labs[labs$province_short == "NU", "y"] - 6
    labs[labs$province_short == "NT", "y"] <- labs[labs$province_short == "NT", "y"] - 2
    labs[labs$province_short == "YT", "y"] <- labs[labs$province_short == "YT", "y"] - 0.5
    labs[labs$province_short == "PE", "arrow_y"] <- labs[labs$province_short == "PE", "y"] + 3
    labs[labs$province_short == "NB", "arrow_x"] <- labs[labs$province_short == "NB", "x"] - 5
    labs[labs$province_short == "NS", "arrow_x"] <- labs[labs$province_short == "NS", "x"] + 7
    labs[labs$province_short == "NS", "arrow_y"] <- labs[labs$province_short == "NS", "y"] - 1
    
    ### plot data
    plot_ly() %>%
      add_sf(
        data = dat,
        type = "scatter",
        split = ~ province,
        color = ~ cases_colour,
        colors = "Reds",
        stroke = I("#000000"),
        span = I(1.5),
        hoverinfo = "none"
      ) %>%
      add_annotations(
        data = labs,
        x = ~ x,
        y = ~ y,
        text = ~ lab_cases,
        hoverinfo = "text",
        hovertext = paste0(labs$province_short, ": ", labs$lab_cases),
        font = list(color = "black", size = 14),
        bgcolor = "white",
        bordercolor = "black",
        showarrow = ~ show_arrow,
        ax = ~ arrow_x,
        ay = ~ arrow_y,
        axref = "x",
        ayref = "y"
      ) %>%
      layout(
        xaxis = axis_hide,
        yaxis = axis_hide,
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE,
             ### hide all plotly buttons, since they do nothing here
             displayModeBar = FALSE) %>%
      hide_colorbar()
  })
  
  # render province case map for overview tab with dynamic width
  output$ui_plot_choropleth_overview_cases <- renderUI({
    
    ### don't run without inputs defined
    req(input$screen_width, input$window_choropleth_overview_cases)
    
    ### render plot
    fluidRow(
      column(
        h4(textOutput("title_choropleth_overview_cases")),
        plotlyOutput("plot_choropleth_overview_cases",
                     ### max width of plot = 600px
                     ### scale so plot doesn't overflow screen at small widths
                     width = ifelse(input$screen_width * (7/8) > 600, 600, input$screen_width * (7/8))),
        width = 12,
        align = "center"
      )
    )
  })
  
  # render province case map slider
  output$ui_window_choropleth_overview_cases <- renderUI({
    
    ### don't run without inputs defined
    req(input$screen_width)
    
    ### render UI
    fluidRow(
      column(
        sliderInput(
          "window_choropleth_overview_cases",
          "Show how many days?",
          min = 7,
          max = data_ts_cases() %>% pull(date_report) %>% unique %>% length,
          step = 1,
          value = 14
        ),
        width = 12,
        align = "center"
      )
    )
  })
  
  # title for province vaccine map for overview tab
  output$title_choropleth_overview_vaccine_administration <- renderText({
    
    ### don't run without inputs defined
    req(input$window_choropleth_overview_vaccine_administration)
    
    ### render text
    paste("Vaccine doses administered by province/territory in the last", input$window_choropleth_overview_vaccine_administration, "days")
  })
  
  # province vaccine administration map for overview tab
  output$plot_choropleth_overview_vaccine_administration <- renderPlotly({
    
    ### don't run without inputs defined
    req(input$window_choropleth_overview_vaccine_administration)
    
    ### join provincial vaccine numbers for relevant window to provincial map
    dat <- geo_prov_simple %>%
      left_join(
        data_ts_vaccine_administration() %>%
          select(province_short, avaccine) %>%
          group_by(province_short) %>%
          slice_tail(n = input$window_choropleth_overview_vaccine_administration) %>%
          summarize(avaccine = sum(avaccine), .groups = "drop"),
        by = "province_short"
      )
    
    ### even out colour scale by rooting vaccine numbers
    dat <- dat %>%
      mutate(cases_colour = avaccine^(1/3))
    
    ### define value labels
    labs <- data.frame(
      province_short = dat[["province_short"]],
      x = as.numeric(st_coordinates(suppressWarnings((st_centroid(dat))))[, 1]),
      y = as.numeric(st_coordinates(suppressWarnings((st_centroid(dat))))[, 2]),
      lab_cases = format(dat[["avaccine"]], big.mark = ",", trim = TRUE),
      ### arrows for NS, NB, PE to avoid overlap
      show_arrow = ifelse(dat[["province_short"]] %in% c("NS", "NB", "PE"), TRUE, FALSE),
      arrow_x = as.numeric(st_coordinates(suppressWarnings((st_centroid(dat))))[, 1]),
      arrow_y = as.numeric(st_coordinates(suppressWarnings((st_centroid(dat))))[, 2]),
      stringsAsFactors = FALSE
    )
    
    ### manually nudge some label positions (x = label, ax = arrowhead tail)
    labs[labs$province_short == "AB", "y"] <- labs[labs$province_short == "AB", "y"] + 0.5
    labs[labs$province_short == "MB", "y"] <- labs[labs$province_short == "MB", "y"] + 0.5
    labs[labs$province_short == "ON", "y"] <- labs[labs$province_short == "ON", "y"] + 0.5
    labs[labs$province_short == "BC", "x"] <- labs[labs$province_short == "BC", "x"] + 0.5
    labs[labs$province_short == "BC", "y"] <- labs[labs$province_short == "BC", "y"] - 2.5
    labs[labs$province_short == "SK", "y"] <- labs[labs$province_short == "SK", "y"] - 2.5
    labs[labs$province_short == "NL", "x"] <- labs[labs$province_short == "NL", "x"] + 4
    labs[labs$province_short == "NL", "y"] <- labs[labs$province_short == "NL", "y"]
    labs[labs$province_short == "NU", "x"] <- labs[labs$province_short == "NU", "x"] - 7
    labs[labs$province_short == "NU", "y"] <- labs[labs$province_short == "NU", "y"] - 6
    labs[labs$province_short == "NT", "y"] <- labs[labs$province_short == "NT", "y"] - 2
    labs[labs$province_short == "YT", "y"] <- labs[labs$province_short == "YT", "y"] - 0.5
    labs[labs$province_short == "PE", "arrow_y"] <- labs[labs$province_short == "PE", "y"] + 3
    labs[labs$province_short == "NB", "arrow_x"] <- labs[labs$province_short == "NB", "x"] - 5
    labs[labs$province_short == "NS", "arrow_x"] <- labs[labs$province_short == "NS", "x"] + 7
    labs[labs$province_short == "NS", "arrow_y"] <- labs[labs$province_short == "NS", "y"] - 1
    
    ### plot data
    plot_ly() %>%
      add_sf(
        data = dat,
        type = "scatter",
        split = ~ province,
        color = ~ cases_colour,
        colors = "Reds",
        stroke = I("#000000"),
        span = I(1.5),
        hoverinfo = "none"
      ) %>%
      add_annotations(
        data = labs,
        x = ~ x,
        y = ~ y,
        text = ~ lab_cases,
        hoverinfo = "text",
        hovertext = paste0(labs$province_short, ": ", labs$lab_cases),
        font = list(color = "black", size = 14),
        bgcolor = "white",
        bordercolor = "black",
        showarrow = ~ show_arrow,
        ax = ~ arrow_x,
        ay = ~ arrow_y,
        axref = "x",
        ayref = "y"
      ) %>%
      layout(
        xaxis = axis_hide,
        yaxis = axis_hide,
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE,
             ### hide all plotly buttons, since they do nothing here
             displayModeBar = FALSE) %>%
      hide_colorbar()
  })
  
  # render province vaccine map for overview tab with dynamic width
  output$ui_plot_choropleth_overview_vaccine_administration <- renderUI({
    
    ### don't run without inputs defined
    req(input$screen_width, input$window_choropleth_overview_vaccine_administration)
    
    ### render plot
    fluidRow(
      column(
        h4(textOutput("title_choropleth_overview_vaccine_administration")),
        plotlyOutput("plot_choropleth_overview_vaccine_administration",
                     ### max width of plot = 600px
                     ### scale so plot doesn't overflow screen at small widths
                     width = ifelse(input$screen_width * (7/8) > 600, 600, input$screen_width * (7/8))),
        width = 12,
        align = "center"
      )
    )
  })
  
  # render province vaccine map slider
  output$ui_window_choropleth_overview_vaccine_administration <- renderUI({
    
    ### don't run without inputs defined
    req(input$screen_width)
    
    ### render UI
    fluidRow(
      column(
        sliderInput(
          "window_choropleth_overview_vaccine_administration",
          "Show how many days?",
          min = 7,
          max = data_ts_vaccine_administration() %>% pull(date_vaccine_administered) %>% unique %>% length,
          step = 1,
          value = data_ts_vaccine_administration() %>% pull(date_vaccine_administered) %>% unique %>% length
        ),
        width = 12,
        align = "center"
      )
    )
  })
  
  # province summary table for overview tab
  output$table_prov_overview <- renderDT({
    
    table_overview %>%
      ### show NA instead of 0 for "people fully vaccinated" (info not yet available)
      mutate(
        `Cumulative people fully vaccinated` = ifelse(`Cumulative people fully vaccinated` == 0, NA, `Cumulative people fully vaccinated`),
        `People fully vaccinated (new)` = ifelse(`Cumulative people fully vaccinated` == 0, NA, `People fully vaccinated (new)`)
      ) %>%
      ### merge short names
      left_join(map_prov %>% select(province, province_short),
                by = c("Province" = "province")) %>%
      mutate(Province = coalesce(province_short, Province)) %>%
      select(-province_short) %>%
      DT::datatable(class = "stripe compact hover", rownames = FALSE, extensions = "FixedColumns",
                    options = list(
                      paging = FALSE,
                      scrollX = TRUE,
                      compact = TRUE,
                      autoWidth = TRUE,
                      ### centre all but the first column
                      columnDefs = list(list(className = 'dt-center', targets = 1:(ncol(.) - 1))),
                      ### freeze province column
                      fixedColumns = list(leftColumns = 1)
                    )) %>%
      formatRound(columns = c("Cumulative cases", "Cases (new)", "Active cases", "Active cases (change)", "Cumulative vaccine doses administered", "Vaccine doses administered (new)", "Cumulative people fully vaccinated", "People fully vaccinated (new)", "Hospitalized", "Hospitalized (change)", "Cumulative deaths", "Deaths (new)", "Cumulative recovered", "Recovered (new)", "Cumulative testing", "Testing (new)"), digits = 0) %>%
      formatRound(columns = c("Cases (new) per 100k", "Cumulative cases per 100k", "Active cases per 100k", "Hospitalized per 100k", "Cumulative deaths per 100k"), digits = 1) %>%
      formatRound(columns = c("Cumulative testing per 100k"), digits = 0)
  })
  
  # flattening plots
  
  ## flattening interpretation
  output$text_flattening <- renderText({
    if (input$scale_flattening == "linear") {
      "<center><b>Interpretation</b><br>
  Graphs display trends for daily cases and deaths over time on a linear scale.
  <br><br>
  - An <i>upward slope</i> means the number of cases/deaths reported each day is <i>still growing</i>.
  <br>
  - A <i>flat line</i> means the number of cases/deaths reported each day is <i>staying the same</i>. 
  <br>
  - A <i>downward slope</i> means the number of cases/deaths reported each day is <i>falling</i>.
  <br><br>
  Exponential growth on a linear scale looks like a line curving in the upward direction.</center>"
    } else if (input$scale_flattening == "logarithmic") {
      "<center><b>Interpretation</b><br>
  Graphs display trends for daily cases and deaths over time on a logarithmic scale.
  <br><br>
  - An <i>upward slope</i> means the number of cases/deaths reported each day is <i>still growing</i>.
  <br>
  - A <i>flat line</i> means the number of cases/deaths reported each day is <i>staying the same</i>. 
  <br>
  - A <i>downward slope</i> means the number of cases/deaths reported each day is <i>falling</i>.
  <br><br>
  Exponential growth on a logarithmic scale looks like a straight line pointing in the upward direction.</center>"
    }
  })
  
  ## function: flattening plot title
  title_flattening <- function(lab_title, val_scale = input$scale_flattening) {
    
    ### don't run without inputs defined
    req(input$scale_flattening)
    
    ### render title
    if (val_scale == "linear") {
      paste0(lab_title, " (7-day rolling average)")
    } else if (val_scale == "logarithmic") {
      paste0(lab_title, " (7-day rolling average, logarithmic scale)")
    }
  }
  
  ## function: flattening plot
  plot_flattening <- function(fun_data, var_date, var_val, var_val_cum, min_val_cum, filter_val_min, lab_y, val_scale = input$scale_flattening) {
    
    ### don't run without inputs defined
    req(input$scale_flattening, input$min_flattening_cases, input$min_flattening_mortality)
    
    ### get data
    dat <- fun_data
    
    ### process data
    dat <- dat %>%
      filter(province != "Repatriated") %>%
      ### calculate first day with filter_val_min cumulative cases or deaths
      group_by(province_short) %>%
      inner_join(
        dat %>%
          group_by(province_short) %>%
          filter(!!sym(var_val_cum) >= min_val_cum) %>%
          slice_head(n = 1) %>%
          rename(date_start = !!sym(var_date)) %>%
          select(province_short, date_start),
        by = "province_short"
      ) %>%
      filter(!!sym(var_date) >= date_start) %>%
      mutate(
        days_since_start = !!sym(var_date) - date_start,
        roll_avg = rollapply(!!sym(var_val), 7, mean, align = "right", partial = TRUE),
        ### minimum value should be 1 (for log plot)
        roll_avg = ifelse(roll_avg < 1, 1, roll_avg),
        roll_avg_lab = ifelse(roll_avg == 1, "≤1", formatC(roll_avg, digits = 1, format = "f", big.mark = ","))
      )
    
    ### generate x-axis label from inputs
    lab_x <- paste("Days since", min_val_cum, sub("_", " ", var_val_cum))
    
    ### plot data
    dat %>%
      plot_ly(
        x = ~ days_since_start,
        y = ~ roll_avg,
        color = ~ province_short,
        colors = palette_province_short,
        hoverinfo = "text",
        hovertext = paste0(
          "Province: ", dat[["province_short"]], "\n",
          "Date: ", dat[[var_date]], "\n",
          "Avg. daily ", var_val, ": ", dat[["roll_avg_lab"]])
      ) %>%
      add_lines() %>%
      layout(
        xaxis = list(title = lab_x, fixedrange = TRUE),
        yaxis = {if (val_scale == "logarithmic") list(type = "log", title = lab_y, fixedrange = TRUE) else list(title = lab_y, fixedrange = TRUE)},
        legend = plotly_legend
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
  }
  
  ## cases
  output$title_flattening_cases <- renderText({title_flattening("Daily reported cases by province")})
  output$plot_flattening_cases <- renderPlotly({plot_flattening(data_ts_cases(), "date_report", "cases", "cumulative_cases", input$min_flattening_cases, 30, "Daily reported cases")})
  
  ## mortality
  output$title_flattening_mortality <- renderText({title_flattening("Daily reported deaths by province")})
  output$plot_flattening_mortality <- renderPlotly({plot_flattening(data_ts_mortality(), "date_death_report", "deaths", "cumulative_deaths", input$min_flattening_mortality, 10, "Daily reported deaths")})
  
  # comparisons
  
  ## function: comparisons plot title
  title_comp <- function(label_val, comparison_window, comparison_scale, date_max) {
    if (comparison_scale == "per-capita") {
      if (comparison_window > 1) {
        paste0("Average daily ", label_val, " per 100,000 (last ", comparison_window, " days)")
      } else {
        paste0(capitalize(label_val), " per 100,000 (", date_max, ")")
      }
    } else {
      if (comparison_window > 1) {
        paste0("Average daily ", label_val, " (last ", comparison_window, " days)")
      } else {
        paste0(capitalize(label_val), " (", date_max, ")")
      }
    }
  }
  
  ## function: comparisons plot
  plot_comp <- function(fun_data, var_val, comparison_window, comparison_scale, lab_y) {
    
    ### get comparison variable
    var_comp <- get_var_comp(var_val, comparison_window, comparison_scale)
    
    ### get data
    dat <- fun_data %>%
      ### create labels for values
      mutate(lab_comp = formatC(!!(sym(var_comp)), digits = 2, format = "f", big.mark = ","))
    
    ### add per-capita to y-axis label (if applicable)
    if (comparison_scale == "per-capita") {
      lab_y <- paste(lab_y, "per 100,000")
    }
    
    ### plot data
    dat %>%
      plot_ly() %>%
      add_trace(
        x = ~ province_short,
        y = as.formula(paste("~", var_comp)),
        type = "bar",
        color = ~ province_short,
        colors = palette_province_short,
        hoverinfo = "text",
        hovertext = paste0(
          dat[["province_short"]], ": ", dat[["lab_comp"]]
        )
      ) %>% 
      layout(
        xaxis = list(title = "Province", fixedrange = TRUE),
        yaxis = list(title = lab_y, fixedrange = TRUE),
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
    
  }
  
  # ## function: comparisons data download
  download_comp <- function(fun_data, file) {
    write.csv(fun_data, file, row.names = FALSE)
  }
  
  ## cases
  output$title_comp_cases <- renderText({
    title_comp("reported cases", input$window_comp_cases, input$scale_comp_cases, date_max)
  })
  output$plot_comp_cases <- renderPlotly({
    plot_comp(data_comp_cases(), "cases", input$window_comp_cases, input$scale_comp_cases, "Daily reported cases")
  })
  output$download_comp_cases <- downloadHandler(
    filename = "comp_prov_cases.csv",
    content = function(file) {
      download_comp(data_comp_cases(), file)
    }
  )
  
  ## mortality
  output$title_comp_mortality <- renderText({
    title_comp("reported deaths", input$window_comp_mortality, input$scale_comp_mortality, date_max)
  })
  output$plot_comp_mortality <- renderPlotly({
    plot_comp(data_comp_mortality(), "deaths", input$window_comp_mortality, input$scale_comp_mortality, "Daily reported mortality")
  })
  output$download_comp_mortality <- downloadHandler(
    filename = "comp_prov_mortality.csv",
    content = function(file) {
      download_comp(data_comp_mortality(), file)
    }
  )
  
  ## testing
  output$title_comp_testing <- renderText({
    title_comp("testing", input$window_comp_testing, input$scale_comp_testing, date_max)
  })
  output$plot_comp_testing <- renderPlotly({
    plot_comp(data_comp_testing(), "testing", input$window_comp_testing, input$scale_comp_testing, "Daily testing")
  })
  output$download_comp_testing <- downloadHandler(
    filename = "comp_prov_testing.csv",
    content = function(file) {
      download_comp(data_comp_testing(), file)
    }
  )
  
  # maps tab
  
  ## load health region map
  load_geo_hr <- reactive({
    st_read("geo/esri_health_region_sk_old/RegionalHealthBoundaries.shp", quiet = TRUE) %>%
      ### join health region names used in the dataset by HR_UID
      ### first convert HR_UID in map to integer so types match
      mutate(HR_UID = as.integer(HR_UID)) %>%
      inner_join(
        map_hr,
        by = "HR_UID"
      )
  })
  
  ## health region map title
  output$title_choropleth_hr <- renderText({
    
    ### don't run without inputs defined
    req(input$metric_choropleth_hr, input$scale_choropleth_hr)
    
    ### render title
    if (input$metric_choropleth_hr == "cases") {
      if (input$scale_choropleth_hr == "absolute") {
        "Map of reported cases in Canada"
      } else if (input$scale_choropleth_hr == "per-capita") {
        "Map of reported cases in Canada (per 100,000)"
      }} else if (input$metric_choropleth_hr == "mortality") {
        if (input$scale_choropleth_hr == "absolute") {
          "Map of reported deaths in Canada"
        } else if (input$scale_choropleth_hr == "per-capita") {
          "Map of reported deaths in Canada (per 100,000)"
        }}
    
  })
  
  ## data: health region map
  data_choropleth_hr <- reactive({
    
    ### don't run without inputs defined
    req(input$metric_choropleth_hr, input$scale_choropleth_hr)
    
    ### get data
    if (input$metric_choropleth_hr == "cases") {
      dat <- data_ts_cases_hr() %>%
        left_join(
          map_hr,
          by = c("province", "health_region")
        )
      var_val <- "cases"
    } else if (input$metric_choropleth_hr == "mortality") {
      dat <- data_ts_mortality_hr() %>%
        left_join(
          map_hr,
          by = c("province", "health_region")
        )
      var_val <- "deaths"
    }
    
    ### process data
    dat %>%
      select(province, health_region, !!sym(var_val), pop) %>%
      group_by(province, health_region) %>%
      summarize(count := sum(!!sym(var_val)), pop = max(pop), .groups = "drop")
  })
  
  ## health region map
  output$choropleth_hr <- renderLeaflet({
    
    ### don't run without inputs defined
    req(input$metric_choropleth_hr, input$scale_choropleth_hr)
    
    ### load map
    geo_hr <- load_geo_hr()
    
    ### get data
    dat <- data_choropleth_hr() %>%
      ### exclude data where province == "Repatriated" or health_region == "Not Reported"
      filter(province != "Repatriated" & health_region != "Not Reported") %>%
      ### convert to per-capita (if applicable)
      {if (input$scale_choropleth_hr == "per-capita") mutate(., count = count / pop * 100000) else .}
    
    ### join data to map
    geo_hr <- geo_hr %>%
      left_join(
        dat,
        by = c("province", "health_region", "pop")
      )
    
    ### generate colour gradient (up to 5 bins)
    n_quantile <- 5
    probs <- seq(0, 1, length.out = n_quantile + 1)
    bins <- quantile(dat$count, probs, na.rm = TRUE, names = FALSE)
    ### ensure bins are integers for absolute scale
    if (input$scale_choropleth_hr == "absolute") {
      bins <- round(bins)
    }
    while (length(unique(bins)) != length(bins)) {
      n_quantile <- n_quantile - 1
      probs <- seq(0, 1, length.out = n_quantile + 1)
      bins <- quantile(dat$count, probs, na.rm = TRUE, names = FALSE)
      ### ensure bins are integers for absolute scale
      if (input$scale_choropleth_hr == "absolute") {
        bins <- round(bins)
      }
    }
    
    ## define bin colours
    if (input$metric_choropleth_hr == "cases") {
      gradient <- colorBin(brewer.pal(n = length(bins), "Blues"), bins = bins)
    } else if (input$metric_choropleth_hr == "mortality") {
      gradient <- colorBin(brewer.pal(n = length(bins), "Reds"), bins = bins)
    }
    
    ### render map
    leaflet(geo_hr) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE, minZoom = 2, maxZoom = 8)) %>%
      addPolygons(
        stroke = TRUE,
        color = "black",
        opacity = 1,
        weight = 0.5,
        fillOpacity = 0.5,
        smoothFactor = 0.5,
        fillColor = ~gradient(count),
        popup =
          paste0(
            "<p>",
            "Province: ",
            geo_hr$province,
            "<br>",
            "Health Region: ",
            geo_hr$health_region,
            "<br>",
            "Population: ",
            formatC(geo_hr$pop, format = "f", big.mark = ",", digits = 0),
            "<br>",
            if (input$metric_choropleth_hr == "cases") {
              if (input$scale_choropleth_hr == "absolute") {
                "Reported cases: "
              } else if (input$scale_choropleth_hr == "per-capita") {
                "Reported cases (per 100,000): "
              }} else if (input$metric_choropleth_hr == "mortality") {
                if (input$scale_choropleth_hr == "absolute") {
                  "Reported deaths: "
                } else if (input$scale_choropleth_hr == "per-capita") {
                  "Reported deaths (per 100,000): "
                }},
            ### format numbers
            if (input$scale_choropleth_hr == "absolute") {
              ifelse(is.na(geo_hr$count), "0", formatC(geo_hr$count, format = "f", big.mark = ",", digits = 0))
            } else if (input$scale_choropleth_hr == "per-capita") {
              ifelse(is.na(geo_hr$count), "0", formatC(geo_hr$count, format = "f", big.mark = ",", digits = 2))
            },
            "</p>"
          )) %>%
      addLegend("bottomright", pal = gradient, values = ~count,
                title = {
                  if (input$metric_choropleth_hr == "cases") {
                    if (input$scale_choropleth_hr == "absolute") {
                      "Reported cases"
                    } else if (input$scale_choropleth_hr == "per-capita") {
                      "Reported cases</br>(per 100,000)"
                    }} else if (input$metric_choropleth_hr == "mortality") {
                      if (input$scale_choropleth_hr == "absolute") {
                        "Reported deaths"
                      } else if (input$scale_choropleth_hr == "per-capita") {
                        "Reported deaths</br>(per 100,000)"
                      }}
                },
                labFormat = labelFormat(prefix = "", suffix = "", between = " – ",
                                        digits = 2, big.mark = ",", transform = identity),
                na.label = "None reported",
                opacity = 0.5)
  })
  
  ## health region map text
  output$text_choropleth_hr <- renderText({
    
    ### don't run without inputs defined
    req(input$metric_choropleth_hr, input$scale_choropleth_hr)
    
    ### get data and only keep data where province == "Repatriated" or health_region == "Not Reported"
    dat <- data_choropleth_hr() %>%
      filter(province == "Repatriated" | health_region == "Not Reported")
    
    ### render text
    if (nrow(dat) == 0) {
      if (input$metric_choropleth_hr== "cases") {
        HTML("No cases were excluded from the map due to missing health regions.")
      } else if (input$metric_choropleth_hr == "mortality") {
        "No deaths were excluded from the map due to missing health regions."
      }
    } else {
      HTML(
        text = paste0(
          "We excluded ",
          if (input$metric_choropleth_hr == "cases") {
            "cases"
          } else if (input$metric_choropleth_hr == "mortality") {
            "deaths"
          },
          " from the map due to missing health regions:",
          "<ul>",
          paste("<li>", dat$province, ": ", dat$count, sep = "", collapse = ""),
          "</ul>")
      )
    }
  })
  
  # daily/cumulative numbers plots
  
  ## function: daily/cumulative numbers plot title
  title_daily_cumulative <- function(fun_data, var_date, var_val, lab_title, exclude_repatriated = FALSE, by_province = FALSE, filter_province = input$prov) {
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get data
    dat <- fun_data %>%
      ### remove repatriated cases from count (if applicable)
      {if (exclude_repatriated) filter(., province != "Repatriated") else .}
    
    ### calculate n
    n <- sum(dat[, var_val])
    
    ### render title
    if (n == 0) {
      paste0(lab_title, " in ", filter_province)
    } else {
      if (filter_province == "Canada" & by_province) {
        paste0(lab_title, " in ", filter_province, " by province (n = ", format(n, big.mark = ","), ")")
      } else {
        paste0(lab_title, " in ", filter_province, " (n = ", format(n, big.mark = ","), ")")
      }
    }
  }
  
  ## function: daily numbers plot
  plot_daily <- function(fun_data, var_date, var_val, lab_x, lab_y) {
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get data
    dat <- fun_data
    
    ### if no matching data, return blank plot
    if (sum(dat[, var_val]) == 0) {
      return(plot_no_data())
    }
    
    ### transform data
    dat <- dat %>%
      ### aggregate values so that one date = one row to ensure correct plotting of bars
      select(!!sym(var_date), !!sym(var_val)) %>%
      group_by(!!sym(var_date)) %>%
      summarize(!!sym(var_val) := sum(!!sym(var_val)), .groups = "drop") %>%
      ### create labels for values
      mutate(lab_val = formatC(!!(sym(var_val)), big.mark = ","))
    
    ### plot data
    dat %>%
      plot_ly(x = as.formula(paste("~", var_date)),
              y = as.formula(paste("~", var_val)),
              hoverinfo = "text",
              hovertext = paste0(
                "Province: ", input$prov, "\n",
                "Date: ", dat[[var_date]], "\n",
                case_when(
                  var_val == "avaccine" ~ paste0("Vaccine doses administered: ", dat[["lab_val"]]),
                  var_val == "dvaccine" ~ paste0("Vaccine doses distributed: ", dat[["lab_val"]]),
                  TRUE ~ paste0(capitalize(var_val), ": ", dat[["lab_val"]])
                )
              )
      ) %>%
      add_bars() %>%
      layout(
        xaxis = list(title = lab_x, fixedrange = TRUE),
        yaxis = list(title = lab_y, fixedrange = TRUE),
        legend = plotly_legend
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
  }
  
  ## function: cumulative numbers plot
  plot_cumulative <- function(fun_data, var_date, var_val, lab_x, lab_y) {
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get data
    dat <- fun_data
    
    ### if no matching data, return blank plot
    if (sum(dat[, var_val]) == 0) {
      return(plot_no_data())
    }
    
    ### transform data
    dat <- dat %>%
      filter(province != "Repatriated") %>%
      ### create labels for values
      mutate(lab_val = formatC(!!(sym(var_val)), big.mark = ","))
    
    ### plot data
    dat %>%
      plot_ly(
        x = as.formula(paste("~", var_date)),
        y = as.formula(paste("~", var_val)),
        color = ~ province_short,
        colors = palette_province_short,
        hoverinfo = "text",
        hovertext = paste0(
          "Province: ", dat[["province_short"]], "\n",
          "Date: ", dat[[var_date]], "\n",
          case_when(
            var_val == "cumulative_avaccine" ~ paste0("Cumulative vaccine doses administered: ", dat[["lab_val"]]),
            var_val == "cumulative_dvaccine" ~ paste0("Cumulative vaccine doses distributed: ", dat[["lab_val"]]),
            TRUE ~ paste0(capitalize(sub("_", " ", var_val)), ": ", dat[["lab_val"]])
          )
        )
      ) %>%
      add_lines() %>%
      layout(
        xaxis = list(title = lab_x, fixedrange = TRUE),
        yaxis = list(title = lab_y, fixedrange = TRUE),
        legend = plotly_legend
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
  }
  
  ## daily numbers: cases
  output$title_daily_cases <- renderText({title_daily_cumulative(data_ts_cases(), "date_report", "cases", "Daily reported cases")})
  output$plot_daily_cases <- renderPlotly({
    plot_daily(data_ts_cases(), "date_report", "cases", "Report date", "Daily reported cases")
  })
  
  ## daily numbers: mortality
  output$title_daily_mortality <- renderText({title_daily_cumulative(data_ts_mortality(), "date_death_report", "deaths", "Daily reported deaths")})
  output$plot_daily_mortality <- renderPlotly({
    plot_daily(data_ts_mortality(), "date_death_report", "deaths", "Report date", "Daily reported deaths")
  })
  
  ## daily numbers: recovered
  output$title_daily_recovered <- renderText({title_daily_cumulative(data_ts_recovered(), "date_report", "recovered", "Daily recovered")})
  output$plot_daily_recovered <- renderPlotly({
    plot_daily(data_ts_recovered(), "date_recovered", "recovered", "Date", "Daily recovered")
  })
  
  ## daily numbers: testing
  output$title_daily_testing <- renderText({title_daily_cumulative(data_ts_testing(), "date_testing", "testing", "Daily testing")})
  output$plot_daily_testing <- renderPlotly({
    plot_daily(data_ts_testing(), "date_testing", "testing", "Date", "Daily testing")
  })
  
  ## daily numbers: vaccine administration
  output$title_daily_vaccine_administration <- renderText({title_daily_cumulative(data_ts_vaccine_administration(), "date_vaccine_administered", "avaccine", "Daily vaccine doses administered")})
  output$plot_daily_vaccine_administration <- renderPlotly({
    plot_daily(data_ts_vaccine_administration(), "date_vaccine_administered", "avaccine", "Date", "Daily vaccine doses administered")
  })
  
  ## cumulative numbers: cases
  output$title_cumulative_cases <- renderText({title_daily_cumulative(data_ts_cases(), "date_report", "cases", "Cumulative reported cases", exclude_repatriated = TRUE, by_province = TRUE)})
  output$plot_cumulative_cases <- renderPlotly({
    plot_cumulative(data_ts_cases(), "date_report", "cumulative_cases", "Report date", "Cumulative reported cases")
  })
  
  ## cumulative numbers: mortality
  output$title_cumulative_mortality <- renderText({title_daily_cumulative(data_ts_mortality(), "date_death_report", "deaths", "Cumulative reported deaths", exclude_repatriated = TRUE, by_province = TRUE)})
  output$plot_cumulative_mortality <- renderPlotly({
    plot_cumulative(data_ts_mortality(), "date_death_report", "cumulative_deaths", "Date", "Cumulative reported deaths")
  })
  
  ## cumulative numbers: recovered
  output$title_cumulative_recovered <- renderText({title_daily_cumulative(data_ts_recovered(), "date_recovered", "recovered", "Cumulative recovered", exclude_repatriated = TRUE, by_province = TRUE)})
  output$plot_cumulative_recovered <- renderPlotly({
    plot_cumulative(data_ts_recovered(), "date_recovered", "cumulative_recovered", "Date", "Cumulative recovered")
  })
  
  ## cumulative numbers: testing
  output$title_cumulative_testing <- renderText({title_daily_cumulative(data_ts_testing(), "date_testing", "testing", "Cumulative testing", exclude_repatriated = TRUE, by_province = TRUE)})
  output$plot_cumulative_testing <- renderPlotly({
    plot_cumulative(data_ts_testing(), "date_testing", "cumulative_testing", "Date", "Cumulative testing")
  })
  
  ## cumulative numbers: vaccine administration
  output$title_cumulative_vaccine_administration <- renderText({title_daily_cumulative(data_ts_vaccine_administration(), "date_vaccine_administered", "avaccine", "Cumulative vaccine doses administered", exclude_repatriated = TRUE, by_province = TRUE)})
  output$plot_cumulative_vaccine_administration <- renderPlotly({
    plot_cumulative(data_ts_vaccine_administration(), "date_vaccine_administered", "cumulative_avaccine", "Date", "Cumulative vaccine doses administered")
  })
  
  ## cumulative numbers: vaccine distribution
  output$title_cumulative_vaccine_distribution <- renderText({title_daily_cumulative(data_ts_vaccine_distribution(), "date_vaccine_distributed", "dvaccine", "Cumulative vaccine doses distributed", exclude_repatriated = TRUE, by_province = TRUE)})
  output$plot_cumulative_vaccine_distribution <- renderPlotly({
    plot_cumulative(data_ts_vaccine_distribution(), "date_vaccine_distributed", "cumulative_dvaccine", "Date", "Cumulative vaccine doses distributed")
  })
  
  # pie charts
  
  ## function: pie chart plot title
  title_pie <- function(fun_data, var_val, lab_title) {
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get data
    n <- fun_data %>%
      filter(province != "Repatriated") %>%
      group_by(province_short) %>%
      ungroup %>%
      summarize(!!sym(var_val) := sum(!!sym(var_val)), .groups = "drop") %>%
      pull
    
    ### render title
    if (n == 0) {
      paste0("Pie chart of ", lab_title, " in Canada")
    } else {
      paste0("Pie chart of ", lab_title, " in Canada", " (n = ", format(n, big.mark = ","), ")")
    }
  }
  
  ## function: pie chart plot
  plot_pie <- function(fun_data, var_val) {
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get data
    dat <- fun_data
    
    ### if no matching data, return blank plot
    if (nrow(dat) == 0) {
      return(plot_no_data())
    }
    
    ### transform data and join colours
    dat <- dat %>%
      filter(province != "Repatriated") %>%
      group_by(province_short) %>%
      summarize(!!sym(var_val) := sum(!!sym(var_val)), .groups = "drop") %>%
      filter(!!sym(var_val) > 0) %>%
      left_join(
        data.frame(
          province_short = names(palette_province_short),
          colour_code = palette_province_short,
          stringsAsFactors = FALSE
        ),
        by = "province_short"
      ) %>%
      ### create labels
      mutate(
        lab_val = formatC(!!sym(var_val), big.mark = ","),
        lab_percent = paste0(formatC(!!sym(var_val) / sum(.[, var_val]) * 100, format = "f", digit = 2), "%")
      )
    
    ### plot data
    dat %>%
      plot_ly(
        labels = ~ province_short,
        values = as.formula(paste("~", var_val)),
        sort = TRUE,
        type = "pie",
        direction = "counterclockwise",
        textposition = "inside",
        marker = list(colors = ~ colour_code),
        textinfo = "text",
        text = ~ lab_percent,
        hoverinfo = "text",
        hovertext = ~ paste0(
          "Province: ", dat$province_short, "\n",
          capitalize(var_val), ": ", dat$lab_val, "\n",
          "Percent of total: ", dat$lab_percent
        )
      ) %>%
      layout(
        xaxis = axis_hide,
        yaxis = axis_hide,
        legend = plotly_legend
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
  }
  
  ## cases
  output$title_pie_cases <- renderText({
    title_pie(data_ts_cases_pie(), "cases", "reported cases")
  })
  output$plot_pie_cases <- renderPlotly({
    plot_pie(data_ts_cases_pie(), "cases")
  })
  
  ## mortality
  output$title_pie_mortality <- renderText({
    title_pie(data_ts_mortality_pie(), "deaths", "reported deaths")
  })
  output$plot_pie_mortality <- renderPlotly({
    plot_pie(data_ts_mortality_pie(), "deaths")
  })
  
  # demographics plots
  
  ## function: demographics plot title
  title_demographics <- function(fun_data, lab_title, filter_province = input$prov) {
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get data
    dat <- fun_data %>%
      ### filter out data with no demographic info
      filter(!(age == "NR" & sex == "Not Reported"))
    
    ### calculate n
    n <- nrow(dat)
    
    ### render title
    if (n == 0) {
      paste0("Demographics of ", lab_title, " in ", filter_province)
    } else {
      paste0("Demographics of ", lab_title, " in ", filter_province, " (n = ", format(n, big.mark = ","), " with demographic data)")
    }
  }
  
  ## function: demographics plot
  plot_demographics <- function(fun_data, lab_y) {
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get data
    dat <- fun_data
    
    ### if no matching data, return blank plot
    if (nrow(dat) == 0) {
      return(plot_no_data())
    }
    
    ### transform data
    dat <- dat %>%
      select(age, sex) %>%
      group_by(age, sex) %>%
      summarize(count = n(), .groups = "drop")
    
    ### note if the data has no sex information
    ### if so, show not reported sex by default (is off by default otherwise)
    no_demo_info <- ifelse(identical(
      sum(dat %>% pull(count)),
      sum(dat %>% filter(sex == "Not Reported") %>% pull(count))
    ), TRUE, FALSE)
    
    ### prepare data for plotting
    dat <- dat %>%
      pivot_wider(names_from = sex,
                  values_from = count,
                  values_fill = 0)
    
    ### plot data
    dat %>%
      plot_ly() %>%
      {
        if ("Male" %in% names(dat))
          add_trace(
            .,
            x = ~ age,
            y = ~ Male,
            type = "bar",
            name = "Male",
            hoverinfo = "text",
            hovertext = paste0(
              "Sex: Male\n",
              "Age group: ", ifelse(as.character(dat[["age"]]) == "NR", "Not reported", as.character(dat[["age"]])), "\n",
              "Count: ", formatC(dat[["Male"]], big.mark = ",")
            )
          )
        else .
      } %>%
      {
        if ("Female" %in% names(dat))
          add_trace(.,
                    x = ~ age,
                    y = ~ Female,
                    type = "bar",
                    name = "Female",
                    hoverinfo = "text",
                    hovertext = paste0(
                      "Sex: Female\n",
                      "Age group: ", ifelse(as.character(dat[["age"]]) == "NR", "Not reported", as.character(dat[["age"]])), "\n",
                      "Count: ", formatC(dat[["Female"]], big.mark = ",")
                    )
          )
        else .
      }  %>%
      {
        if ("Not Reported" %in% names(dat))
          add_trace(
            .,
            x = ~ age,
            y = ~ `Not Reported`,
            type = "bar",
            name = "Not reported",
            hoverinfo = "text",
            hovertext = paste0(
              "Sex: Not reported\n",
              "Age group: ", ifelse(as.character(dat[["age"]]) == "NR", "Not reported", as.character(dat[["age"]])), "\n",
              "Count: ", formatC(dat[["Not Reported"]], big.mark = ",")
            ),
            marker = list(color = "rgb(128, 128, 128)"),
            visible = ifelse(no_demo_info, TRUE, "legendonly")
          )
        else .
      } %>% 
      layout(
        xaxis = list(title = "Age group", fixedrange = TRUE),
        yaxis = list(title = lab_y, fixedrange = TRUE),
        legend = plotly_legend
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
    
  }
  
  ## cases
  output$title_demographics_cases <- renderText({
    title_demographics(data_cases(), "reported cases")
  })
  output$plot_demographics_cases <- renderPlotly({
    plot_demographics(data_cases(), "Reported cases")
  })
  
  ## mortality
  output$title_demographics_mortality <- renderText({
    title_demographics(data_mortality(), "reported deaths")
  })
  output$plot_demographics_mortality <- renderPlotly({
    plot_demographics(data_mortality(), "Reported deaths")
  })
  
  # vaccine tab-specific plots
  
  ## vaccine gap data
  data_vaccine_gap <- reactive({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### merge data
    dat <- full_join(
      data_ts_vaccine_administration() %>%
        rename(date_vaccine = date_vaccine_administered) %>%
        select(province, date_vaccine, cumulative_avaccine),
      data_ts_vaccine_distribution() %>%
        rename(date_vaccine = date_vaccine_distributed) %>%
        select(province, date_vaccine, cumulative_dvaccine),
      by = c("province", "date_vaccine")
    ) %>%
      replace_na(list(cumulative_avaccine = 0)) # 2020-12-13 is NA for avaccine
    
    ### collapse observations into one row per date
    dat %>%
      select(date_vaccine, cumulative_dvaccine, cumulative_avaccine) %>%
      group_by(date_vaccine) %>%
      summarize(across(everything(), sum), .groups = "drop")
    
  })
  
  ## vaccine gap title
  output$title_vaccine_gap <- renderText({
    
    ### don't run without inputs defined
    req(input$prov)
    
    if (input$prov != "all") {
      paste("Vaccine gap in", input$prov)
    } else {
      "Vaccine gap in Canada"
    }
    
  })
  
  ## vaccine gap plot
  output$plot_vaccine_gap <- renderPlotly({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get merged vaccine data
    dat <- data_vaccine_gap()
    
    ### plot
    dat %>%
      plot_ly(
        x = ~date_vaccine, 
        y = ~cumulative_avaccine, 
        name = "Administered", 
        type = "scatter",
        mode = "none",
        fill = "tozeroy",
        fillcolor = "rgba(0, 0, 255, 0.3)"
      ) %>%
      add_trace(y = ~cumulative_dvaccine, 
                name = "Distributed",
                fill = "tonexty",
                fillcolor = "rgba(0, 0, 0, 0.7)"
      ) %>%
      layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Vaccine doses", fixedrange = TRUE),
        legend = plotly_legend,
        hovermode = "x unified"
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
    
  })
  
  ## percent fully vaccinated data
  data_fully_vaccinated <- reactive({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### merge data
    dat <- full_join(
      data_ts_vaccine_completion() %>%
        rename(date_vaccine = date_vaccine_completed) %>%
        select(province, date_vaccine, cumulative_cvaccine),
      data_ts_vaccine_administration() %>%
        rename(date_vaccine = date_vaccine_administered) %>%
        select(province, date_vaccine, cumulative_avaccine),
      by = c("province", "date_vaccine")
    ) %>%
      replace_na(list(cumulative_avaccine = 0,
                      cumulative_cvaccine = 0)) # 2020-12-13 is NA for avaccine
    
    ### collapse observations into one row per date
    dat %>%
      ### merge short names
      left_join(map_prov %>% select(province, province_short),
                by = c("province")) %>% 
      select(date_vaccine, cumulative_avaccine, cumulative_cvaccine, province_short) %>%
      group_by(date_vaccine, province_short) %>%
      summarize(across(everything(), sum), .groups = "drop") %>% 
      mutate(
        full_vaxx = 100 * cumulative_cvaccine / cumulative_avaccine,
        lab_percent = paste0(formatC(full_vaxx, format = "f", digit = 2), "%")
      ) %>% 
      ungroup() %>% 
      group_by(province_short) %>% 
      filter(date_vaccine == max(date_vaccine))
    
  })
  
  ## percent fully vaccinated title
  output$title_fully_vaccinated <- renderText({
    
    ### don't run without inputs defined
    req(input$prov)
    
    if (input$prov != "all") {
      paste("Percent fully vaccinated in", input$prov)
    } else {
      "Percent fully vaccinated in Canada"
    }
    
  })
  
  ## percent fully vaccinated plot
  output$plot_fully_vaccinated <- renderPlotly({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get merged vaccine data
    dat <- data_fully_vaccinated()
    
    ### plot data
    dat %>%
      plot_ly() %>%
      add_trace(
        x = ~ province_short,
        y = ~ full_vaxx,
        type = "bar",
        color = ~ province_short,
        colors = palette_province_short,
        hoverinfo = "text",
        hovertext = ~ paste0(
          "Province: ", dat$province_short, "\n",
          "Fully vaccinated", ": ", dat$cumulative_cvaccine, "\n",
          "Percent of total: ", dat$lab_percent
        )
      ) %>% 
      layout(
        xaxis = list(title = "Province", fixedrange = TRUE),
        yaxis = list(title = "Percent", fixedrange = TRUE),
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
    
  })
  
  ## doses administered by province (absolute/per-capita) data
  
  data_avaccine_per_capita <- reactive({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range, input$scale_comp_avacc)
    
    
    ### convert to per-capita (if applicable)
    if (input$scale_comp_avacc == "per-capita") {
      data_ts_vaccine_administration() %>% 
        # left_join(map_prov %>% select(province, pop), by = c("province")) %>% 
        group_by(province) %>% 
        ### convert to per-capita (if applicable)
        filter(date_vaccine_administered == max(date_vaccine_administered)) %>% 
        ungroup() %>% 
        dplyr::mutate(avaccine_per_capita = cumulative_avaccine / pop * 100000) %>%
        ### create labels for values
        dplyr::mutate(lab_avaccine_per_capita = formatC(avaccine_per_capita, digits = 2, format = "f", big.mark = ",")) 
      ### merge short names
      # left_join(map_prov %>% select(province, province_short), by = c("province"))
      
    } else {
      data_ts_vaccine_administration() %>% 
        group_by(province) %>% 
        ### convert to per-capita (if applicable)
        filter(date_vaccine_administered == max(date_vaccine_administered)) %>% 
        summarize(pop = max(pop), 
                  avaccine_per_capita := mean(cumulative_avaccine), .groups = "drop") %>% 
        ### create labels for values
        dplyr::mutate(lab_avaccine_per_capita = formatC(avaccine_per_capita, digits = 2, format = "f", big.mark = ",")) %>% 
        ### merge short names
        left_join(map_prov %>% select(province, province_short), by = c("province"))
    }
    
  })
  
  ## doses administered by province (absolute/per-capita) title
  
  output$title_avaccine_per_capita <- renderText({
    
    ### don't run without inputs defined
    req(input$prov, input$scale_comp_avacc)
    
    if (input$prov != "all") {
      if (input$scale_comp_avacc == "per-capita") {
        paste("Doses administered per 100,000 in", input$prov)
      } else {
        paste("Total doses administered in", input$prov)
      }
    } else {
      if (input$scale_comp_avacc == "per-capita") {
        paste0("Doses administered per 100,000 in Canada")
      } else {
        paste0("Total doses administered in Canada")
      }
    }
    
  })
  
  ## doses administered by province (absolute/per-capita) plot
  
  output$plot_avaccine_per_capita <- renderPlotly({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range, input$scale_comp_avacc)
    
    ### get merged vaccine data
    dat <- data_avaccine_per_capita()
    
    ### add per-capita to y-axis label (if applicable)
    # input$scale_comp_avacc == "per-capita"
    if (input$scale_comp_avacc == "per-capita") {
      lab_y <- "Vaccine doses administered per 100,000"
    } else {
      lab_y <- "Total vaccine doses administered"
    }
    
    ### plot data
    dat %>%
      plot_ly() %>%
      add_trace(
        x = ~ province_short,
        y = ~ avaccine_per_capita,
        type = "bar",
        color = ~ province_short,
        colors = palette_province_short,
        hoverinfo = "text",
        hovertext = paste0(
          dat$province_short, ": ", dat$lab_avaccine_per_capita
        )
      ) %>% 
      layout(
        xaxis = list(title = "Province", fixedrange = TRUE),
        yaxis = list(title = lab_y, fixedrange = TRUE),
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
    
  })
  
  ## doses distributed by province (absolute/per-capita) data
  
  data_dvaccine_per_capita <- reactive({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range, input$scale_comp_dvacc)
    
    
    ### convert to per-capita (if applicable)
    if (input$scale_comp_dvacc == "per-capita") {
      data_ts_vaccine_distribution() %>% 
        # left_join(map_prov %>% select(province, pop), by = c("province")) %>% 
        group_by(province) %>% 
        ### convert to per-capita (if applicable)
        filter(date_vaccine_distributed == max(date_vaccine_distributed)) %>% 
        ungroup() %>% 
        dplyr::mutate(dvaccine_per_capita = cumulative_dvaccine / pop * 100000) %>%
        ### create labels for values
        dplyr::mutate(lab_dvaccine_per_capita = formatC(dvaccine_per_capita, digits = 2, format = "f", big.mark = ","))
      
    } else {
      data_ts_vaccine_distribution() %>% 
        group_by(province) %>% 
        ### convert to per-capita (if applicable)
        filter(date_vaccine_distributed == max(date_vaccine_distributed)) %>%
        summarize(pop = max(pop), 
                  dvaccine_per_capita := mean(cumulative_dvaccine), .groups = "drop") %>% 
        ### create labels for values
        dplyr::mutate(lab_dvaccine_per_capita = formatC(dvaccine_per_capita, digits = 2, format = "f", big.mark = ",")) %>% 
        ### merge short names
        left_join(map_prov %>% select(province, province_short), by = c("province"))
    }
    
  })
  
  ## doses distributed by province (absolute/per-capita) title
  
  output$title_dvaccine_per_capita <- renderText({
    
    ### don't run without inputs defined
    req(input$prov, input$scale_comp_dvacc)
    
    if (input$prov != "all") {
      if (input$scale_comp_dvacc == "per-capita") {
        paste("Doses distributed per 100,000 in", input$prov)
      } else {
        paste("Total doses distributed in", input$prov)
      }
    } else {
      if (input$scale_comp_dvacc == "per-capita") {
        paste0("Doses distributed per 100,000 in Canada")
      } else {
        paste0("Total doses distributed in Canada")
      }
    }
    
  })
  
  ## doses distributed by province (absolute/per-capita) plot
  
  output$plot_dvaccine_per_capita <- renderPlotly({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range, input$scale_comp_dvacc)
    
    ### get merged vaccine data
    dat <- data_dvaccine_per_capita()
    
    ### add per-capita to y-axis label (if applicable)
    if (input$scale_comp_dvacc == "per-capita") {
      lab_y <- "Vaccine doses distributed per 100,000"
    } else {
      lab_y <- "Total vaccine doses distributed"
    }
    
    ### plot data
    dat %>%
      plot_ly() %>%
      add_trace(
        x = ~ province_short,
        y = ~ dvaccine_per_capita,
        type = "bar",
        color = ~ province_short,
        colors = palette_province_short,
        hoverinfo = "text",
        hovertext = paste0(
          dat$province_short, ": ", dat$lab_dvaccine_per_capita
        )
      ) %>% 
      layout(
        xaxis = list(title = "Province", fixedrange = TRUE),
        yaxis = list(title = lab_y, fixedrange = TRUE),
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = plotly_buttons)
    
  })
  
  # summary table for time to percent vaccinated by province
  
  output$table_prov_time_to_pct_vaccination <- renderDT({
    
    req(input$prov, input$date_range, input$pct_vaccination)
    
    ## create dataframe by province
    table_time_to_pct_vaccination <- data_ts_vaccine_administration() %>% 
      select(date_vaccine_administered, avaccine, cumulative_avaccine, province) %>%
      dplyr::group_by(date_vaccine_administered, province) %>% 
      dplyr::summarize(
        avaccine = sum(avaccine),
        cumulative_avaccine = sum(cumulative_avaccine),
        .groups = "drop") %>%
      ungroup() %>% 
      mutate(province = as.factor(province)) %>% 
      group_by(province) %>% 
      dplyr::mutate(
        # current_speed = mean(avaccine)
        roll_avg = rollapply(avaccine, 7, mean, align = "right", partial = TRUE),
        ### minimum value should be 1 (for log plot)
        roll_avg = ifelse(roll_avg < 1, 1, roll_avg),
        roll_avg_lab = ifelse(roll_avg == 1, "≤1", formatC(roll_avg, digits = 1, format = "f", big.mark = ","))
      ) %>% 
      arrange(date_vaccine_administered) %>% 
      dplyr::mutate(
        current_cum = last(cumulative_avaccine)
      ) %>% 
      filter(date_vaccine_administered == max(date_vaccine_administered)) %>% 
      ungroup() %>% 
      left_join(map_prov, by = "province") %>% 
      dplyr::mutate(
        total_needed = (input$pct_vaccination/100) * 2 * pop,
        date_to_vaxx = max(date_vaccine_administered) + round((total_needed - current_cum) / roll_avg, 0),
        days_to_vaxx = date_to_vaxx - date_vaccine_administered,
        weeks_to_vaxx = as.numeric(days_to_vaxx) / 7,
        mths_to_vaxx = as.numeric(days_to_vaxx) / 12
      ) %>% 
      mutate_if(is.numeric, round, 1) %>% 
      select(province, pop, current_cum, roll_avg_lab, total_needed, date_to_vaxx) %>% 
      dplyr::rename(
        "Province" = province,
        "Population" = pop,
        "Total Doses Administered" = current_cum,
        "7-day Avg. Rate of Vaccination" = roll_avg_lab,
        "Remaining Vaccinations Needed" = total_needed,
        "Expected Date to Reach % Vaccination" = date_to_vaxx)
    
    table_time_to_pct_vaccination %>%
      DT::datatable(class = "stripe compact hover", rownames = FALSE, extensions = "FixedColumns",
                    options = list(
                      paging = FALSE,
                      scrollX = TRUE,
                      compact = TRUE,
                      autoWidth = TRUE,
                      ### centre all but the first column
                      columnDefs = list(list(className = 'dt-center', targets = 1:(ncol(.) - 1))),
                      ### freeze province column
                      fixedColumns = list(leftColumns = 1:(ncol(.) - 1))
                    ))
    
  })
  
  # travel tab
  
  ## load travel map
  load_geo_travel <- reactive({
    
    ### load map
    st_read("geo/natural_earth/ne_countries_simple.geojson", quiet = TRUE) %>%
      ### keep only NAME_LONG column
      select(NAME_LONG) %>%
      ### rename some countries
      mutate(
        NAME_LONG = recode(NAME_LONG, `Republic of Korea` = "South Korea", `Russian Federation` = "Russia")
      )
  })
  
  ## data: number of international travel cases
  data_cases_travel_n <- reactive({
    data_cases() %>%
      filter(travel_yn == 1 & travel_history_country != "Canada") %>%
      nrow
  })
  
  ## data: travel
  data_cases_travel <- reactive({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get travel cases data
    dat <- data_cases() %>%
      select(date_report,
             province,
             travel_yn,
             travel_history_country) %>%
      ### keep only those with known travel history
      filter(travel_yn == 1) %>%
      ### cases identified as repatriated should be in repatriated category even if they came from a cruise
      mutate(
        travel_history_country = ifelse(province == "Repatriated", "Repatriated", travel_history_country)
      ) %>%
      ### separate out multiple countries visited into one row each
      mutate(visited_multiple_countries = ifelse(grepl(", ", travel_history_country), 1, 0)) %>%
      separate_rows(travel_history_country, sep = ", ") %>%
      ### rename a few countries to match the map
      mutate(
        travel_history_country = case_when(
          travel_history_country == "Canary Islands" ~ "Spain",
          travel_history_country == "U.S. Virgin Islands" ~ "United States Virgin Islands",
          TRUE ~ .$travel_history_country))
    
    ### remove non-specific entries (but keep count)
    n_before <- nrow(dat)
    dat <- dat %>%
      filter(travel_history_country != "Not Reported")
    n_not_reported <- n_before - nrow(dat); n_before <- nrow(dat)
    dat <- dat %>%
      filter(travel_history_country != "Canada")
    n_canada <- n_before - nrow(dat); n_before <- nrow(dat)
    dat <- dat %>%
      filter(travel_history_country != "Repatriated") # must be above cruise (some repatriated come from cruises, but this takes priority)
    n_repatriated <- n_before - nrow(dat); n_before <- nrow(dat)
    dat <- dat %>%
      filter(!grepl("cruise", travel_history_country, ignore.case = TRUE))
    n_cruise <- n_before - nrow(dat); n_before <- nrow(dat)
    dat <- dat %>%
      filter(travel_history_country != "Europe")
    n_europe <- n_before - nrow(dat); n_before <- nrow(dat)
    dat <- dat %>%
      filter(travel_history_country != "Middle East")
    n_me <- n_before - nrow(dat); n_before <- nrow(dat)
    dat <- dat %>%
      filter(travel_history_country != "Overseas")
    n_over <- n_before - nrow(dat); n_before <- nrow(dat)
    dat <- dat %>%
      filter(travel_history_country != "Asia")
    n_asia <- n_before - nrow(dat); n_before <- nrow(dat)
    dat <- dat %>%
      filter(travel_history_country != "Caribbean")
    n_caribbean <- n_before - nrow(dat)
    
    ### make data and counts available
    n_all <- c(
      "n_not_reported",
      "n_canada",
      "n_repatriated",
      "n_cruise",
      "n_europe",
      "n_me",
      "n_over",
      "n_asia",
      "n_caribbean"
    )
    list(
      dat,
      setNames(
        mget(n_all),
        n_all
      )
    )
  })
  
  ## travel map title
  output$title_choropleth_travel <- renderText({
    
    ### count travel cases (avoid double-counting with multiple countries)
    ### counting like this in case a case ends up both included and excluded categories (e.g., travel to United States, Europe)
    n <- data_cases_travel_n()
    
    if (n == 0) {
      "Cases associated with international travel"
    } else {
      paste0("Cases associated with international travel (n = ", format(n, big.mark = ","), ")")
    }
  })
  
  ## travel map
  output$plot_choropleth_travel <- renderLeaflet({
    
    ### don't run without inputs defined
    req(input$prov, input$date_range)
    
    ### get travel data
    dat <- data_cases_travel()[[1]]
    
    ### return blank map if there are no cases
    if (nrow(dat) == 0) {
      return(leaflet() %>%
               addProviderTiles(providers$Stamen.TonerLite,
                                options = providerTileOptions(noWrap = TRUE, minZoom = 1, maxZoom = 6)))
    }
    
    ### process travel data
    dat <- dat %>%
      group_by(travel_history_country, visited_multiple_countries) %>%
      summarize(case_count = n(), .groups = "drop") %>%
      pivot_wider(names_from = visited_multiple_countries, values_from = case_count, values_fill = list(case_count = 0)) %>%
      mutate(`0` = if ("0" %in% names(.)) {`0`} else {0},
             `1` = if ("1" %in% names(.)) {`1`} else {0}
      ) %>% # ensure that these columns exist, otherwise there will be an error
      rename(case_count_single = `0`,
             case_count_multiple = `1`) %>%
      mutate(case_count_total = case_count_single + case_count_multiple)
    
    ### load map
    geo_travel <- load_geo_travel()
    
    ### join travel data to map and plot (keeping only countries with matches)
    geo_travel <- geo_travel %>%
      inner_join(
        dat,
        by = c("NAME_LONG" = "travel_history_country")
      )
    
    ### report countries that won't be merged
    unmerged_countries <- dat %>%
      filter(!travel_history_country %in% geo_travel$NAME_LONG) %>%
      pull(travel_history_country) %>%
      unique
    if (!identical(unmerged_countries, character(0))) {
      warning("Countries could not be merged to travel map: ",
              paste(unmerged_countries, collapse = ", "))
    }
    
    ### generate colour gradient
    gradient <- colorNumeric(
      palette = c("#71c7ec", "#189ad3", "#107dac", "#005073"),
      domain = geo_travel$case_count_total)
    
    ### display map
    leaflet(geo_travel) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE, minZoom = 1, maxZoom = 6)) %>%
      addPolygons(
        stroke = FALSE,
        fillOpacity = 0.5,
        smoothFactor = 0.5,
        color = ~gradient(geo_travel$case_count_total),
        popup = paste0(
          "<p>",
          geo_travel$NAME_LONG,
          "<br>",
          "Total travel cases: ",
          format(geo_travel$case_count_total, big.mark = ","),
          "<br>",
          "Visited only this country: ",
          format(geo_travel$case_count_single, big.mark = ","),
          "<br>",
          "Visited this country among others: ",
          format(geo_travel$case_count_multiple, big.mark = ","),
          "</p>"
        )) %>%
      addLegend("bottomright", pal = gradient, values = ~geo_travel$case_count_total,
                title = "Total cases",
                opacity = 0.5)
    
  })
  
  ## travel map text
  output$text_choropleth_travel <- renderText({
    
    ### load exclusion data
    dat <- data_cases_travel()[[2]]
    
    ### what is printed depends on types of cases excluded
    if (sum(unlist(dat)) == 0) {
      ""
    } else {
      
      ### function for printing exclusions
      print_travel_exclusion <- function(dat, name, value) {
        if (dat[value] > 0) {paste0(name, " (", format(dat[value], big.mark = ","), ")")} else {}
      }
      
      ### report excluded cases
      HTML(
        paste0("We excluded cases from the map above if their travel history was given as",
               gsub("\\)(?=[a-zA-Z]+)", "), ", perl = TRUE,
                    paste0(
                      ": ",
                      print_travel_exclusion(dat, "not reported", "n_not_reported"),
                      print_travel_exclusion(dat, "repatriated", "n_repatriated"),
                      print_travel_exclusion(dat, "cruise", "n_cruise"),
                      print_travel_exclusion(dat, "Asia", "n_asia"),
                      print_travel_exclusion(dat, "Europe", "n_europe"),
                      print_travel_exclusion(dat, "Middle East", "n_me"),
                      print_travel_exclusion(dat, "Caribbean", "n_caribbean"),
                      print_travel_exclusion(dat, "overseas", "n_over"),
                      ". ")),
               "Only individual cases definitively linked to international travel as their route of exposure were included in this dataset (n = ",
               format(data_cases_travel_n(), big.mark = ","), ").")
      )
    }
  })
  
}
