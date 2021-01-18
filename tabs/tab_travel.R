tab_travel <- tabItem(tabName = "tab_travel",
                      fluidPage(
                        p("Please note that this map represents only known locations of travel for a narrow slice of early travel-related cases where detailed travel history was provided."),
                        fluidRow(
                        box(
                          title = textOutput("title_choropleth_travel"),
                          width = 12,
                          leafletOutput("plot_choropleth_travel",
                                        height = 700)
                        )
                      ),
                      htmlOutput("text_choropleth_travel")))