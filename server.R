server <- function(input, output, session) {
  
  # JavaScript function to format number with a thousands separator
  format_thousands <- apexcharter::JS(
    "function(value) {if (typeof(value) == 'number') {return value.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, \",\");} else {return value;}}")
  
  # render Apex chart of PT timeseries for "value" or "value_daily"
  ts_chart_pt <- function(
    dat, val = c("value", "value_daily"), xlab, ylab, remove_negative_values = FALSE) {
    match.arg(val, c("value", "value_daily"), several.ok = FALSE)
    if (remove_negative_values) {dat[!is.na(dat[val]) & dat[val] < 0, val] <- NA}
    apexcharter::renderApexchart(
      apexcharter::apex(dat, type = "line", mapping = apexcharter::aes(
        x = date, y = !!rlang::sym(val), colour = region)) %>%
        apexcharter::ax_labs(x = xlab, y = ylab) %>%
        apexcharter::ax_chart(animations = list(enabled = FALSE)) %>%
        apexcharter::ax_yaxis(labels = list(formatter = format_thousands)) %>%
        apexcharter::ax_tooltip(y = list(formatter = format_thousands)) %>%
        apexcharter::ax_colors_manual(palette_pt)
    )
  }
  
  # render case time series chart
  output$pt_cases_ts <- ts_chart_pt(
    dat = cases_pt,
    val = "value_daily",
    xlab = "Date",
    ylab = "Daily confirmed cases",
    remove_negative_values = TRUE)
  
  # check if dataset has been updated on GitHub and if so, download it and redeploy the app
  shiny::observe({
    ## run only if app is running on the server, not locally
    if (!is_local) {
      ### run every 5 minutes
      shiny::invalidateLater(1000 * 60 * 5, session)
      ### report run
      cat(paste("Checking for data update:", Sys.time()), fill = TRUE)
      ### read update_time.txt from GitHub repo
      new_time <- readLines("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/update_time.txt")
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
  
  # when app stops, update file time of app.R
  # trick to update cache: https://stackoverflow.com/a/55476883
  shiny::onStop(function() {
    Sys.setFileTime("app.R", lubridate::now())
  })
  
}
