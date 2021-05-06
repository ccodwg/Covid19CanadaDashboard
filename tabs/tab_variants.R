tab_variants <- tabItem(tabName = "tab_variants",
                     fluidPage(
                       includeHTML("text/variant_text.html"),
                       fluidRow(
                         valueBoxOutput("value_box_b117_cases", width = 4),
                         valueBoxOutput("value_box_b1351_cases", width = 4),
                         valueBoxOutput("value_box_p1_cases", width = 4)
                       )
                     ),HTML("<center><h1 style='font-size:27px'>Trend of Reported VOC+ Cases</h1></center>
                            <p>Regular COVID-19 tests are unable to identify the presence of VOCs. Specimens which have tested positive for COVID-19 often need to be sent for additional testing to determine the strain of the virus.
                            As such, it can be difficult to make direct comparisons of VOCs vs non-VOCs, as further testing results in delays between the initial reporting of COVID-19 positivity and VOC positivity.</p>
                            <p>VOC reporting has been inconsistent across Canada, and also inconsistent across time. There are no VOC data updates on weekends, resulting in zero reported cases on these days. Additionally, due to the
                            delay between reported COVID-19 positivity and reported VOCs, the number of VOCs can exceed the number of cases reported. We suggest using the 7-day rolling averages of daily report cases and VOC cases, 
                            as this allows for the overall trend of VOCs to be visualized.</p>"),
                              fluidRow(box(
                                title = textOutput("title_daily_voc"),
                                width = 12,
                                plotlyOutput("plot_daily_voc")
                              )),
                     HTML("<center><h1 style='font-size:27px'>Daily Reported VOCs</h1></center>"),
                       tabsetPanel(
                       type = "tabs",
                       tabPanel("Total Reported VOCs (by Type)",
                                fluidRow(box(
                                  title = textOutput("title_daily_variant_type"),
                                  width = 12,
                                  plotlyOutput("plot_daily_variant_type")
                                ))),
                       tabPanel("Reported B117 Cases",
                                fluidRow(box(
                                  title = textOutput("title_daily_b117_variants"),
                                  width = 12,
                                  plotlyOutput("plot_daily_b117_variants")
                                ))),
                       tabPanel("Reported B1351 Cases",
                                fluidRow(box(
                                  title = textOutput("title_daily_b1351_variants"),
                                  width = 12,
                                  plotlyOutput("plot_daily_b1351_variants")
                                ))),
                       tabPanel("Reported P1 Cases",
                                fluidRow(box(
                                  title = textOutput("title_daily_p1_variants"),
                                  width = 12,
                                  plotlyOutput("plot_daily_p1_variants")
                                )))
                     ), 
                     HTML("<center><h1 style='font-size:27px'>Cumulative Reported VOCs by Province</h1></center>"),
                     tabsetPanel(
                     tabPanel("Cumulative Reported VOCs",
                              fluidRow(box(
                                title = textOutput("title_cumulative_variants"),
                                width = 12,
                                plotlyOutput("plot_cumulative_variants")
                              ))),
                     tabPanel("Reported B117 Cases",
                              fluidRow(box(
                                title = textOutput("title_cumulative_b117_variants"),
                                width = 12,
                                plotlyOutput("plot_cumulative_b117_variants")
                              ))),
                     tabPanel("Reported B1351 Cases",
                              fluidRow(box(
                                title = textOutput("title_cumulative_b1351_variants"),
                                width = 12,
                                plotlyOutput("plot_cumulative_b1351_variants")
                              ))),
                     tabPanel("Reported P1 Cases",
                              fluidRow(box(
                                title = textOutput("title_cumulative_p1_variants"),
                                width = 12,
                                plotlyOutput("plot_cumulative_p1_variants")
                              )))
                     ))

