tab_variants <- tabItem(tabName = "tab_variants",
                     fluidPage(
                       includeHTML("text/variant_text.html"),
                       fluidRow(
                         valueBoxOutput("value_box_b117_cases", width = 4),
                         valueBoxOutput("value_box_b1351_cases", width = 4),
                         valueBoxOutput("value_box_p1_cases", width = 4)
                       )
                     ),HTML("<center><h1 style='font-size:27px'>Daily Reported VOCs</h1></center>
                          <p>The time series for VOC data begins on February 4, 2021, although VOC were first detected in Canada in late 2020.
                          Hence, cases collected before 04-02-2021 are summed and added to the total number of variant cases, but are missing from the daily plots.
                          Furthermore, the data for daily case numbers for VOCs is only collected on weekdays, resulting in zero cases of VOCs being reported on weekends. </p>"),
                       tabsetPanel(
                       type = "tabs",
                       tabPanel("Daily Variant Cases (by Variant Type)",
                                fluidRow(box(
                                  title = textOutput("title_daily_variant_type"),
                                  width = 12,
                                  plotlyOutput("plot_daily_variant_type")
                                ))),
                       tabPanel("Daily Reported B117 Variants",
                                fluidRow(box(
                                  title = textOutput("title_daily_b117_variants"),
                                  width = 12,
                                  plotlyOutput("plot_daily_b117_variants")
                                ))),
                       tabPanel("Daily Reported B1351 Variants",
                                fluidRow(box(
                                  title = textOutput("title_daily_b1351_variants"),
                                  width = 12,
                                  plotlyOutput("plot_daily_b1351_variants")
                                ))),
                       tabPanel("Daily Reported P1 Variants",
                                fluidRow(box(
                                  title = textOutput("title_daily_p1_variants"),
                                  width = 12,
                                  plotlyOutput("plot_daily_p1_variants")
                                )))
                     ), 
                     HTML("<center><h1 style='font-size:27px'>Cumulative Reported VOCs by Province</h1></center>
                          <p>The time series for Variants of Concern (VOC) data begins on February 4, 2021, although VOC were first detected in Canada in late 2020.
                          Additionally, daily case numbers for VOCs are only collected on weekdays. As such, cumulative VOC cases remain constant on weekends.</p>"),
                     tabsetPanel(
                     tabPanel("Cumulative Reported Total Variants",
                              fluidRow(box(
                                title = textOutput("title_cumulative_variants"),
                                width = 12,
                                plotlyOutput("plot_cumulative_variants")
                              ))),
                     tabPanel("Cumulative Reported B117 Variants",
                              fluidRow(box(
                                title = textOutput("title_cumulative_b117_variants"),
                                width = 12,
                                plotlyOutput("plot_cumulative_b117_variants")
                              ))),
                     tabPanel("Cumulative Reported B1351 Variants",
                              fluidRow(box(
                                title = textOutput("title_cumulative_b1351_variants"),
                                width = 12,
                                plotlyOutput("plot_cumulative_b1351_variants")
                              ))),
                     tabPanel("Cumulative Reported P1 Variants",
                              fluidRow(box(
                                title = textOutput("title_cumulative_p1_variants"),
                                width = 12,
                                plotlyOutput("plot_cumulative_p1_variants")
                              )))
                     ))

