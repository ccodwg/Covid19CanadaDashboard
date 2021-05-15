tab_testing <- tabItem(tabName = "tab_testing",
                       fluidPage(tabsetPanel(
                         type = "tabs",
                         tabPanel("Daily testing",
                                  fluidRow(box(
                                    title = textOutput("title_daily_testing"),
                                    width = 12,
                                    plotlyOutput("plot_daily_testing")
                                  ))
                                  ),
                         tabPanel("Cumulative testing",
                                  fluidRow(box(
                                    title = textOutput("title_cumulative_testing"),
                                    width = 10,
                                    plotlyOutput("plot_cumulative_testing")
                                  ),
                                  box(
                                    title = "More options",
                                    width = 2,
                                    radioButtons(
                                      "plot_type_testing",
                                      "Plot type",
                                      choices = c("Bar" = "bar-graph", "Line" = "time-series"),
                                      selected = "time-series"
                                    ),
                                    radioButtons(
                                      "scale_testing",
                                      "Absolute/per-capita",
                                      choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                      selected = "absolute"
                                    )
                                  ))),
                         p("Provinces occasionally report negative cumulative testing numbers. We aim to reconcile these inconsistencies in future data updates, if possible."),
                         fluidRow(DTOutput("table_info_testing"))
                       )))