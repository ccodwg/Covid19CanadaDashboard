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
                          HTML("Note that it is possible for doses administered to temporarily exceed doses distributed for two reasons: 1) Doses distributed is updated less frequently than doses administered and 2) <a href='https://www.saskatchewan.ca/government/news-and-media/2021/february/02/covid19-update-for-february-2-35575-vaccines-delivered-223-new-cases-266-recoveries-eight-deaths' target='_blank'>extra doses can sometimes be obtained beyond the labelled amount</a>."),
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Vaccine gap",
                                     fluidRow(box(
                                       title = textOutput("title_vaccine_gap"),
                                       width = 12,
                                       plotlyOutput("plot_vaccine_gap")
                                     )
                                     )
                            ),
                            tabPanel("Cumulative vaccine doses distributed",
                              fluidRow(box(
                                title = textOutput("title_cumulative_vaccine_distribution"),
                                width = 12,
                                plotlyOutput("plot_cumulative_vaccine_distribution")
                              )
                              )
                            )
                        )
                        )
)