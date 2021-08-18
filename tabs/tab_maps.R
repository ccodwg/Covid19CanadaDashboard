tab_maps <- tabItem(tabName = "tab_maps",
                    fluidPage(
                               p(HTML(
                                 paste0("<center><b><h1 style='font-size:27px'>Map of COVID-19 in Canada</h1></b></center>
                                        <br>The map provides data on both COVID-19 cases and deaths specific to local health regions, 
                                        and are available as either raw numbers (absolute scale) or adjusted for population (per-capita scale).
                                        These data can be filtered using the date range selector in the side menu. The bins are based on quintiles.</p><br>"))),
                               
                      fluidRow(column(
                        width = 4,
                        offset = 4,
                        align = "center",
                        radioButtons(
                          "metric_choropleth_hr",
                          NULL,
                          choices = c("Cases" = "cases",
                                      "Deaths" = "mortality"),
                          inline = TRUE,
                          selected = "cases"
                        )
                      )),
                      fluidRow(column(
                        width = 4,
                        offset = 4,
                        align = "center",
                        radioButtons(
                          "scale_choropleth_hr",
                          NULL,
                          choices = c("Absolute" = "absolute",
                                      "Per-capita" = "per-capita"),
                          inline = TRUE,
                          selected = "absolute"
                        )
                      )),
                      fluidRow(box(
                        title = textOutput("title_choropleth_hr"),
                        width = 12,
                        leafletOutput("choropleth_hr", height = 700)
                      )),
                      includeHTML("text/sk_map.html"),
                      uiOutput("text_choropleth_hr")
                    ))