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
                                  width = 10,
                                  plotlyOutput("plot_cumulative_cases")
                                ),
                                box(
                                  title = "More options",
                                  width = 2,
                                  radioButtons(
                                    "plot_type_cases",
                                    "Plot type",
                                    choices = c("Bar" = "bar-graph", "Line" = "time-series"),
                                    selected = "time-series"
                                  ),
                                  radioButtons(
                                    "scale_cases",
                                    "Absolute/per-capita",
                                    choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                    selected = "absolute"
                                  )
                                ))),
                       tabPanel("Pie chart",
                                fluidRow(box(
                                  title = textOutput("title_pie_cases"),
                                  width = 12,
                                  plotlyOutput("plot_pie_cases")
                                )))
                     )))