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
                                    width = 12,
                                    plotlyOutput("plot_cumulative_testing"),
                                  ))
                                  ),
                         p("Provinces occasionally report negative cumulative testing numbers. We aim to reconcile these inconsistencies in future data updates, if possible."),
                         fluidRow(DTOutput("table_info_testing"))
                       )))