tab_maps <- tabItem(tabName = "tab_maps",
                    fluidPage(
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
                      p("Bins are based on quintiles."),
                      uiOutput("text_choropleth_hr")
                    ))