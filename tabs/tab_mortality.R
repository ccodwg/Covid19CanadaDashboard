tab_mortality <- tabItem(tabName = "tab_mortality",
                         fluidPage(tabsetPanel(
                           type = "tabs",
                           tabPanel("Daily reported deaths",
                                    fluidRow(box(
                                      title = textOutput("title_daily_mortality"),
                                      width = 12,
                                      plotlyOutput("plot_daily_mortality")
                                    ))),
                           tabPanel("Cumulative reported deaths",
                                    fluidRow(box(
                                      title = textOutput("title_cumulative_mortality"),
                                      width = 10,
                                      plotlyOutput("plot_cumulative_mortality")
                                    ),
                                    box(
                                      title = "More options",
                                      width = 2,
                                      radioButtons(
                                        "plot_type_deaths",
                                        "Plot type",
                                        choices = c("Bar" = "bar-graph", "Line" = "time-series"),
                                        selected = "time-series"
                                      ),
                                      radioButtons(
                                        "scale_deaths",
                                        "Absolute/per-capita",
                                        choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                        selected = "absolute"
                                      )
                                    ))),
                           tabPanel("Pie chart",
                                    fluidRow(box(
                                      title = textOutput("title_pie_mortality"),
                                      width = 12,
                                      plotlyOutput("plot_pie_mortality")
                                    )))
                         )))