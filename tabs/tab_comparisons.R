tab_comparisons <- tabItem(tabName = "tab_comparisons",
                           fluidPage(
                             p(HTML("<center><b><h1 style='font-size:27px'>Provincial Comparisons</h1></b></center><br>The <b>Comparisons</b> tab is intended to provide recent average <b>daily</b> values for cases, deaths, and testing by province. These values are available as either raw numbers (absolute scale) or adjusted for population (per-capita scale). For example, the 7-day average for daily per-capita cases indicates the average number of new cases reported <b>each day</b> in a province over the past 7 days.</p>")),
                             fluidRow(
                               HTML("<br>"), # blank line
                               p(HTML(
                                 paste0("<center><b><h1 style='font-size:27px'>COVID-19 Cases</h1></b></center>"))),
                               box(
                                 title = textOutput("title_comp_cases"),
                                 width = 9,
                                 plotlyOutput("plot_comp_cases")
                               ),
                               box(
                                 title = "More options",
                                 width = 3,
                                 radioButtons(
                                   "scale_comp_cases",
                                   "Absolute/per-capita",
                                   choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                   selected = "per-capita"
                                 ),
                                 radioButtons(
                                   "window_comp_cases",
                                   "Window",
                                   choices = c("1 day" = 1, "7 days" = 7, "14 days" = 14),
                                   selected = 7
                                 ),
                                 downloadButton("download_comp_cases", "Download")
                               )
                             ),
                           fluidRow(
                             HTML("<br>"), # blank line
                             p(HTML(
                               paste0("<center><b><h1 style='font-size:27px'>COVID-19 Mortality</h1></b></center>"))),
                             
                             box(
                               title = textOutput("title_comp_mortality"),
                               width = 9,
                               plotlyOutput("plot_comp_mortality")
                             ),
                             box(
                               title = "More options",
                               width = 3,
                               radioButtons(
                                 "scale_comp_mortality",
                                 "Absolute/per-capita",
                                 choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                 selected = "per-capita"
                               ),
                               radioButtons(
                                 "window_comp_mortality",
                                 "Window",
                                 choices = c("1 day" = 1, "7 days" = 7, "14 days" = 14),
                                 selected = 7
                               ),
                               downloadButton("download_comp_mortality", "Download")
                             )
                           ),
                           fluidRow(
                             HTML("<br>"), # blank line
                             p(HTML(
                               paste0("<center><b><h1 style='font-size:27px'>COVID-19 Testing</h1></b></center>"))),
                             
                             box(
                               title = textOutput("title_comp_testing"),
                               width = 9,
                               plotlyOutput("plot_comp_testing")
                             ),
                             box(
                               title = "More options",
                               width = 3,
                               radioButtons(
                                 "scale_comp_testing",
                                 "Absolute/per-capita",
                                 choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                 selected = "per-capita"
                               ),
                               radioButtons(
                                 "window_comp_testing",
                                 "Window",
                                 choices = c("1 day" = 1, "7 days" = 7, "14 days" = 14),
                                 selected = 7
                               ),
                               downloadButton("download_comp_testing", "Download")
                             )
                           )
))
                           