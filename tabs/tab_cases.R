tab_cases <- tabItem(tabName = "tab_cases",
                     fluidPage(tabsetPanel(
                       type = "tabs",
                       tabPanel("Daily reported cases",
                                fluidRow(box(
                                  title = textOutput("title_daily_cases"),
                                  width = 12,
                                  plotlyOutput("plot_daily_cases")
                                ))),
                       tabPanel("Cumulative reported cases",
                                fluidRow(box(
                                  title = textOutput("title_cumulative_cases"),
                                  width = 12,
                                  plotlyOutput("plot_cumulative_cases")
                                ))),
                       tabPanel("Pie chart",
                                fluidRow(box(
                                  title = textOutput("title_pie_cases"),
                                  width = 12,
                                  plotlyOutput("plot_pie_cases")
                                ))),
                       tabPanel("Demographics",
                                fluidRow(box(
                                  title = textOutput("title_demographics_cases"),
                                  width = 12,
                                  plotlyOutput("plot_demographics_cases")
                                )),
                                p("Note that cases with unreported sex are hidden by default."))
                     )))