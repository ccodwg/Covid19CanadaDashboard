tab_vaccines <- tabItem(tabName = "tab_vaccines",
                        fluidPage(
                          includeHTML("text/vaccine_text.html"),
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Daily vaccine doses administered",
                                     fluidRow(box(
                                       title = textOutput("title_daily_vaccine_administration"),
                                       width = 12,
                                       plotlyOutput("plot_daily_vaccine_administration")
                                     ))
                            ),
                            tabPanel("Cumulative vaccine doses administered",
                                     fluidRow(box(
                                       title = textOutput("title_cumulative_vaccine_administration"),
                                       width = 12,
                                       plotlyOutput("plot_cumulative_vaccine_administration")
                                     ))
                            )
                            ),
                          p("At present, vaccine distribution data are updates less frequently than vaccine administration data. These numbers should be considered an underestimate of the number of doses distributed, and in some cases the number of doses administered may appear to exceed the number of doses distributed."),
                          fluidRow(box(
                            title = textOutput("title_cumulative_vaccine_distribution"),
                            width = 12,
                            plotlyOutput("plot_cumulative_vaccine_distribution")
                          )
                        )
                        )
)