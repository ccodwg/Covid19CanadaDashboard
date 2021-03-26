tab_recovered <- tabItem(tabName = "tab_recovered",
                         fluidPage(tabsetPanel(
                           type = "tabs",
                           tabPanel("Daily recovered",
                                    fluidRow(box(
                                      title = textOutput("title_daily_recovered"),
                                      width = 12,
                                      plotlyOutput("plot_daily_recovered")
                                    )),
                                    HTML("On July 17, Quebec <a href='https://opencovid.ca/work/data-faq/#6' target='_blank'> revised their definition of recovered</a>, causing a massive spike on that day. We are reviewing the feasibility of retroactively editing this time series.")
                                    ),
                           tabPanel("Cumulative recovered",
                                    fluidRow(box(
                                      title = textOutput("title_cumulative_recovered"),
                                      width = 10,
                                      plotlyOutput("plot_cumulative_recovered")
                                    ),
                                    box(
                                      title = "More options",
                                      width = 2,
                                      radioButtons(
                                        "plot_type_recovered",
                                        "Plot type",
                                        choices = c("Bar" = "bar-graph", "Line" = "time-series"),
                                        selected = "bar-graph"
                                      ),
                                      radioButtons(
                                        "scale_recovered",
                                        "Absolute/per-capita",
                                        choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                        selected = "per-capita"
                                      )
                                    )),
                                    HTML("On July 17, Quebec <a href='https://opencovid.ca/work/data-faq/#6' target='_blank'> revised their definition of recovered</a>, causing a massive spike on that day. We are reviewing the feasibility of retroactively editing this time series.")
                           )
                         )))