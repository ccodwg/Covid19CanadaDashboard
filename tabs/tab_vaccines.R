tab_vaccines <- tabItem(tabName = "tab_vaccines",
                        fluidPage(
                          p(HTML(
                            paste0("<center><b><h1 style='font-size:27px'>Vaccines Administered/ Distributed</h1></b></center>
                                        <br></p>"))),
                          includeHTML("text/vaccine_text.html"),
                          HTML("<br>"),
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
                                     fluidRow(
                                       box(
                                         title = textOutput("title_avaccine_per_capita"),
                                         width = 10,
                                         plotlyOutput("plot_avaccine_per_capita")
                                       ),
                                       box(
                                         title = "More options",
                                         width = 2,
                                         radioButtons(
                                           "plot_type_avacc",
                                           "Plot type",
                                           choices = c("Bar" = "bar-graph", "Line" = "time-series"),
                                           selected = "time-series"
                                         ),
                                         radioButtons(
                                           "scale_comp_avacc",
                                           "Absolute/per-capita",
                                           choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                           selected = "absolute"
                                         )
                                       )
                                     ),
                            ),
                            tabPanel("Cumulative vaccine doses distributed",
                                     fluidRow(
                                       box(
                                         title = textOutput("title_dvaccine_per_capita"),
                                         width = 10,
                                         plotlyOutput("plot_dvaccine_per_capita")
                                       ),
                                       box(
                                         title = "More options",
                                         width = 2,
                                         radioButtons(
                                           "plot_type_dvacc",
                                           "Plot type",
                                           choices = c("Bar" = "bar-graph", "Line" = "time-series"),
                                           selected = "time-series"
                                         ),
                                         radioButtons(
                                           "scale_comp_dvacc",
                                           "Absolute/per-capita",
                                           choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                           selected = "absolute"
                                         )
                                       )
                                     ),
                            )
                          ),
                          HTML("<br>"), # blank line
                          p(HTML(
                            paste0("<center><b><h1 style='font-size:27px'>Vaccine Gap (Administered versus Distributed)</h1></b></center></p><br>
                                  <b>By default, the vaccine gap plot shows smoothed values of the vaccine distribution time series to account for 
                                  reporting delays in the distribution numbes. This can be disabled by clicking the \"Smoothed distributed\" button.</b><br><br>")))),
                        fluidRow(box(
                          title = textOutput("title_vaccine_gap"),
                          width = 12,
                          plotlyOutput("plot_vaccine_gap")
                        )
                        ),
                        HTML("<br>"),
                        p(HTML(
                          paste0("<center><b><h1 style='font-size:27px'>Percent Vaccinated</h1></b></center>"))),
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Percent at least one dose",
                                     fluidRow(box(
                                       title = textOutput("title_at_least_one_dose"),
                                       width = 12,
                                       plotlyOutput("plot_at_least_one_dose")
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
                            )
                          ),
                          HTML("Note that it is possible for doses administered to temporarily exceed doses distributed for two reasons: 1) Doses distributed is updated less frequently than doses administered and 2) <a href='https://www.saskatchewan.ca/government/news-and-media/2021/february/02/covid19-update-for-february-2-35575-vaccines-delivered-223-new-cases-266-recoveries-eight-deaths' target='_blank'>extra doses can sometimes be obtained beyond the labelled amount</a>.<br>"),
                          HTML("<br>"), # blank line
                          p(HTML(
                            paste0("<center><b><h1 style='font-size:27px'>Time to Percent Fully Vaccinated</h1></b></center></p>"))),
                              HTML("<br>"), # a little whitespace
                              fluidRow(
                                column(
                                  sliderInput(
                                    "pct_vaccination",
                                    "Specify desired level of % vaccination:",
                                    min = 30,
                                    max = 100,
                                    step = 1,
                                    value = 70
                                  ),
                                  width = 12,
                                  align = "center"
                                )
                              ),
                              fluidRow(
                                div(style="overflow-x:auto", DTOutput("table_prov_time_to_pct_vaccination")) # scrollX=TRUE is buggy with when table is updated with slider
                                )
                        )