server <- function(input, output, session) {
  
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
  
  ##### ----- WASTEWATER PLOT
  
  output$pt_wastewater_ts  <- apexcharter::renderApexfacet(wastewater_pt %>%
                                     apexcharter::apex(mapping = apexcharter::aes(date,value,colour = region),type = "area", synchronize = "value") %>%
                                     apexcharter::ax_facet_wrap(apexcharter::vars(area), ncol = 1, scales = "free_y") %>% ## don't facet by province but instead areas
                                     apexcharter::ax_chart(animations = list(enabled = FALSE)) %>%
                                     apexcharter::ax_legend(showForSingleSeries = TRUE) %>%
                                     apexcharter::ax_stroke(lineCap = "round", curve = "smooth") %>%
                                     apexcharter::ax_fill(gradient = list(enabled = TRUE, opacityFrom = 0.9, opacityTo = 0.3)) %>%
                                     apexcharter::ax_xaxis(tooltip = list(enabled = FALSE), title = list(text = ""),
                                                           crosshairs = list(
                                                             opacity = 0.75,
                                                             width = 2,
                                                             fill = list(color = "#1C4579"),
                                                             stroke = list(width = 0))) %>%
                                     apexcharter::ax_yaxis(labels = list(formatter = apexcharter::JS("function(val) {return val.toFixed(1);}")),
                                                           tooltip = list(enabled = FALSE), title = list(text = "Weekly Average of Raw copies per / ml")) %>%
                                     apexcharter::ax_tooltip(y = list(formatter = apexcharter::JS("function(val) {return val.toFixed(1);}")), followCursor = FALSE) %>%
                                     apexcharter::ax_colors_manual(list("AB" = "#5C4742","BC" = "#8D5B4C","MB" = "#948375","SK" = "#5A2A27",
                                                                        "ON" = "#5C4742","QC" = "#948375",
                                                                        "NB" = "#5C4742","NL" = "#8D5B4C","NS" = "#948375","PE" = "#5A2A27"))
   )
  
  output$hr_wastewater_ts  <- apexcharter::renderApexfacet(wastewater_hr %>%
                                                             apexcharter::apex(mapping = apexcharter::aes(date,value,colour = region),type = "area", synchronize = "value") %>%
                                                             apexcharter::ax_facet_wrap(apexcharter::vars(area), ncol = 1, scales = "free_y") %>% ## don't facet by province but instead areas
                                                             apexcharter::ax_chart(animations = list(enabled = FALSE)) %>%
                                                             apexcharter::ax_legend(showForSingleSeries = TRUE) %>%
                                                             apexcharter::ax_stroke(lineCap = "round", curve = "smooth") %>%
                                                             apexcharter::ax_fill(gradient = list(enabled = TRUE, opacityFrom = 0.9, opacityTo = 0.3)) %>%
                                                             apexcharter::ax_xaxis(tooltip = list(enabled = FALSE), title = list(text = ""),
                                                                                   crosshairs = list(
                                                                                     opacity = 0.75,
                                                                                     width = 2,
                                                                                     fill = list(color = "#1C4579"),
                                                                                     stroke = list(width = 0))) %>%
                                                             apexcharter::ax_yaxis(labels = list(formatter = apexcharter::JS("function(val) {return val.toFixed(1);}")),
                                                                                   tooltip = list(enabled = FALSE), title = list(text = "Weekly Average of Raw copies per / ml")) %>%
                                                             apexcharter::ax_tooltip(y = list(formatter = apexcharter::JS("function(val) {return val.toFixed(1);}")), followCursor = FALSE) %>%
                                                             apexcharter::ax_theme(palette = "palette9")
  )
  
  output$pt_hospitalizations_ts <- apexcharter::renderApexfacet(hospitalization_data %>%
                                                                       apexcharter::apex(mapping = apexcharter::aes(date,value,colour = region),type = "area", synchronize = "value") %>%
                                                                       apexcharter::ax_facet_wrap(apexcharter::vars(area), ncol = 1, scales = "free_y") %>%
                                                                       apexcharter::ax_chart(animations = list(enabled = FALSE)) %>%
                                                                       apexcharter::ax_legend(showForSingleSeries = TRUE) %>%
                                                                       apexcharter::ax_stroke(lineCap = "round", curve = "smooth") %>%
                                                                       apexcharter::ax_fill(gradient = list(enabled = TRUE, opacityFrom = 0.8, opacityTo = 0.2)) %>%
                                                                       apexcharter::ax_xaxis(tooltip = list(enabled = FALSE), title = list(text = ""),
                                                                                        crosshairs = list(
                                                                                          opacity = 0.75,
                                                                                          width = 2,
                                                                                          fill = list(color = "#1C4579"),
                                                                                          stroke = list(width = 0))) %>%
                                                                       apexcharter::ax_yaxis(labels = list(formatter = apexcharter::JS("function(val) {return val.toFixed(1);}")),
                                                                                             tooltip = list(enabled = FALSE), title = list(text = "Weekly Total Hospitalizations")) %>%
                                                                       apexcharter::ax_tooltip(y = list(formatter = apexcharter::JS("function(val) {return val.toFixed(0);}"))) %>%
                                                                       apexcharter::ax_colors_manual(list("ON" = "#4A62C4","QC" = "#70B3D1",
                                                                                                          "AB" = "#4A62C4","BC" = "#0393D2","MB" = "#70B3D1","SK" = "#0327Bf",
                                                                                                          "NB" = "#4A62C4","NL" = "#0393D2","NS" = "#70B3D1","PE" = "#0327Bf",
                                                                                                          "NT" = "#4A62C4", "NU" = "#70B3D1", "YK" = "#0327Bf"))
  )
  
  output$deaths_pt_ts <- apexcharter::renderApexfacet(mortality_data %>%
                                                     apexcharter::apex(mapping = apexcharter::aes(date, value_daily, colour = region), type = "area", synchronize = "value") %>%
                                                     apexcharter::ax_facet_wrap(apexcharter::vars(area), ncol = 1, scales = "free_y") %>%
                                                     apexcharter::ax_chart(animations = list(enabled = FALSE)) %>%
                                                     apexcharter::ax_legend(showForSingleSeries = TRUE) %>%
                                                     apexcharter::ax_stroke(lineCap = "round", curve = "smooth") %>%
                                                     apexcharter::ax_fill(gradient = list(enabled = TRUE, opacityFrom = 0.8, opacityTo = 0.2)) %>%
                                                     apexcharter::ax_xaxis(tooltip = list(enabled = FALSE), title = list(text = ""),
                                                                             crosshairs = list(
                                                                               opacity = 0.75,
                                                                               width = 2,
                                                                               fill = list(color = "#1C4579"),
                                                                               stroke = list(width = 0))) %>%
                                                     apexcharter::ax_yaxis(labels = list(formatter = apexcharter::JS("function(val) {return val.toFixed(1);}")),
                                                                           tooltip = list(enabled = FALSE), title = list(text = "Weekly Total Deaths")) %>%
                                                     apexcharter::ax_tooltip(y = list(formatter = apexcharter::JS("function(val) {return val.toFixed(0);}"))) %>%
                                                       apexcharter::ax_colors_manual(list("ON" = "#5D704D","QC" = "#8CA38B",
                                                                                          "AB" = "#5D704D","BC" = "#4E8852","MB" = "#8CA38B","SK" = "#20431D",
                                                                                          "NB" = "#5D704D","NL" = "#4E8852","NS" = "#8CA38B","PE" = "#20431D",
                                                                                          "NT" = "#5D704D", "NU" = "#8CA38B", "YK" = "#20431D"))
  )

  # when app stops, update file time of app.R
  # trick to update cache: https://stackoverflow.com/a/55476883
  shiny::onStop(function() {
    Sys.setFileTime("app.R", lubridate::now())
  })
  
  
}
