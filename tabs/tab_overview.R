tab_overview <- tabItem(tabName = "tab_overview",
                        fluidPage(
                              fluidRow(
                                column(
                                  width = 8,
                                  offset = 2,
                                  valueBoxOutput("value_box_summary_cases", width = 3),
                                  valueBoxOutput("value_box_summary_active", width = 3),
                                  valueBoxOutput("value_box_summary_recovered", width = 3),
                                  valueBoxOutput("value_box_summary_mortality", width = 3),
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 8,
                                  offset = 2,
                                  valueBoxOutput("value_box_summary_doses_administered", width = 3),
                                  valueBoxOutput("value_box_summary_fully_vaccinated", width = 3),
                                  valueBoxOutput("value_box_summary_hosp", width = 3),
                                  valueBoxOutput("value_box_summary_testing", width = 3)
                                )
                              ),
                              p(HTML("<br><br>")),
                              p(HTML(
                                paste0("<b><h1 style='font-size:18px'>Last updated: ", update_time, "</h1></b>"))),
                              fluidRow(box(HTML(gsub("\n", "<br>", data_notes)), title = "Daily update notes (click + to expand)", width = 12, collapsible = TRUE, collapsed = TRUE)),
                              p(
                                "Reported cases are cumulative and include both confirmed and presumptive positive cases. Repatriated cases are included in the total but not shown in the map below."
                              ),
                              HTML("<br>"),
                              p(HTML(
                                paste0("<center><b><h1 style='font-size:27px'>Summary of COVID-19 in Canada</h1></b></center>"))),
                              tags$head(
                                tags$style(".nav-tabs {font-size: 16px; font-weight: bold; font-color: black}")),
                              tabsetPanel(
                                tabPanel(
                                  "EpiView",
                                  tabsetPanel(
                                    type = "tabs",
                                    tabPanel(
                                      "Cases",
                                      uiOutput("ui_plot_choropleth_overview_cases"),
                                      uiOutput("ui_window_choropleth_overview_cases"),
                                    ),
                                    tabPanel(
                                      "Mortality",
                                      uiOutput("ui_plot_choropleth_overview_deaths"),
                                      uiOutput("ui_window_choropleth_overview_deaths"),
                                    ),
                                    tabPanel(
                                      "Recovered",
                                      uiOutput("ui_plot_choropleth_overview_recovered"),
                                      uiOutput("ui_window_choropleth_overview_recovered"),
                                    )
                                  )
                                ),
                                tabPanel(
                                  "VaxView",
                                  tabsetPanel(
                                    type = "tabs",
                                    tabPanel(
                                      "% 2 doses",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_full_pct"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_full_pct"),
                                    ),
                                    # tabPanel(
                                    #   "% At least one dose",
                                    #   uiOutput("ui_plot_choropleth_overview_vaccine_at_least_one_dose"),
                                    #   uiOutput("ui_window_choropleth_overview_vaccine_at_least_one_dose"),
                                    # ),
                                    tabPanel(
                                      "Administered",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_administration"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_administration"),
                                    ),
                                    tabPanel(
                                      "Distributed",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_distribution"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_distribution"),
                                    ),
                                    tabPanel(
                                      "% Administered of total distributed",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_admin_pct"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_admin_pct"),
                                    )
                                  ))),
                              HTML("<br>"), # blank line
                              p(HTML(
                                paste0("<center><b><h1 style='font-size:27px'>Daily COVID-19 Trends</h1></b></center>"))),
                                tabsetPanel(
                                  type = "tabs",
                                  tabPanel(
                                    "Cases",
                                    fluidRow(box(
                                      title = textOutput("title_flattening_cases"),
                                      plotlyOutput("plot_flattening_cases"),
                                      width = 12))
                                  ),
                                  tabPanel(
                                    "Mortality",
                                    fluidRow(box(
                                      title = textOutput("title_flattening_mortality"),
                                      plotlyOutput("plot_flattening_mortality"),
                                      width = 12))
                                  ),
                                  tabPanel(
                                    "Daily doses administered",
                                    fluidRow(box(
                                      title = textOutput("title_flattening_avaccine"),
                                      plotlyOutput("plot_flattening_avaccine"),
                                      width = 12))
                                )
                                )
                              ),
                              box(
                                title = NULL,
                                width = 12,
                                align = "center",
                                radioButtons(
                                  "scale_flattening",
                                  "Scale",
                                  choices = c("Linear" = "linear", "Logarithmic" = "logarithmic"),
                                  selected = "linear",
                                  inline = TRUE
                                ),
                                radioButtons(
                                  "scale_trends",
                                  "Absolute/per-capita",
                                  choices = c("Absolute" = "absolute", "Per-capita" = "per-capita"),
                                  selected = "absolute"
                                )),
                              HTML("<center>These charts may be viewed on a <b>linear scale</b> (the default) or a <b>logarithmic scale</b>. The choice of scale changes the interpretation of the plot. Select the scale using the button above. Additionally, the time range of data displayed may be changed using the range slider below the plot. By default, the entire range of data is shown.</center>"),
                              tags$p(""), # blank line
                              htmlOutput({"text_flattening"}),
                              tags$p(""), # blank line
                              HTML(paste0("<br><center><b><h1 style='font-size:27px'>Metrics by Province/Territory</h1></b></center>")),
                        fluidRow(DTOutput("table_prov_overview")),
                        "RP = repatriated travellers."
                        )
