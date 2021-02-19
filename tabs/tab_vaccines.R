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
                            ),
                            tabPanel("Cumulative vaccine doses administered per capita",
                                     fluidRow(
                                       box(
                                         title = textOutput("title_avaccine_per_capita"),
                                         width = 9,
                                         plotlyOutput("plot_avaccine_per_capita")
                                       ),
                                       box(
                                         title = "More options",
                                         width = 3,
                                         radioButtons(
                                           "scale_comp_avacc",
                                           "Absolute/per-capita",
                                           choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                           selected = "per-capita"
                                         )
                                       )
                                     ),
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
                            tabPanel("Percent fully vaccinated",
                                     fluidRow(box(
                                       title = textOutput("title_fully_vaccinated"),
                                       width = 12,
                                       plotlyOutput("plot_fully_vaccinated")
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
                            ),
                            tabPanel("Cumulative vaccine doses distributed per capita",
                                     fluidRow(
                                       box(
                                         title = textOutput("title_dvaccine_per_capita"),
                                         width = 9,
                                         plotlyOutput("plot_dvaccine_per_capita")
                                       ),
                                       box(
                                         title = "More options",
                                         width = 3,
                                         radioButtons(
                                           "scale_comp_dvacc",
                                           "Absolute/per-capita",
                                           choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                           selected = "per-capita"
                                         )
                                       )
                                     ),
                            )
                          ),
                          tabsetPanel(
                            type = "tabs",
                            tabPanel(
                              "Time to Percent Vaccination",
                              fluidRow(
                                DTOutput("table_prov_time_to_pct_vaccination")
                                       ),
                              fluidRow(
                                column(
                                  sliderInput(
                                    "pct_vaccination",
                                    "Specify desired level of percent vaccination:",
                                    min = 30,
                                    max = 100,
                                    step = 1,
                                    value = 70
                                  ),
                                  width = 12,
                                  align = "center"
                                )
                              )
                            ) 
                          )
                        )
)