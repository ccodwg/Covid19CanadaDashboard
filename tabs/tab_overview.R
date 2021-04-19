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
                                  bsTooltip("value_box_summary_fully_vaccinated",
                                            paste0(
                                              "The following provinces have not yet reported any people being fully vaccinated: ",
                                              paste(table_overview %>% filter(`Cumulative people fully vaccinated` == 0 & !Province %in% c("Canada", "Repatriated")) %>% pull(Province) %>% sort, collapse = ", ")
                                            ),
                                            "bottom",
                                            "hover"),
                                  valueBoxOutput("value_box_summary_hosp", width = 3),
                                  valueBoxOutput("value_box_summary_testing", width = 3)
                                )
                              ),
                              column(includeHTML("text/vaccine_text.html"), width = 12, align = "center"),
                              p(HTML("<br><br>")),
                              p(HTML(
                                paste0("<b>Last updated: ", update_time, "</b>"))),
                              fluidRow(box(HTML(gsub("\n", "<br>", news)), title = "Daily update notes (click + to expand)", width = 12, collapsible = TRUE, collapsed = TRUE)),
                              p(
                                "Reported cases are cumulative and include both confirmed and presumptive positive cases. Repatriated cases are included in the total but not shown in the map below."
                              ),
                              HTML("<b>Check out VaxView, our vaccine tracker, below:</b><br><br>"),
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
                                      "Deaths",
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
                                      "Vaccine Administration",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_administration"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_administration"),
                                    ),
                                    tabPanel(
                                      "Vaccine Distribution",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_distribution"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_distribution"),
                                    ),
                                    tabPanel(
                                      "% Vaccines Administered per Total Distribution",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_admin_pct"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_admin_pct"),
                                    ),
                                    tabPanel(
                                      "% Partially Vaccinated",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_partial_pct"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_partial_pct"),
                                    ),
                                    tabPanel(
                                      "% Fully Vaccinated",
                                      uiOutput("ui_plot_choropleth_overview_vaccine_full_pct"),
                                      uiOutput("ui_window_choropleth_overview_vaccine_full_pct"),
                                    )
                                  ))),
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
                                )),
                              HTML("<center>These charts may be viewed on a <b>linear scale</b> (the default) or a <b>logarithmic scale</b>. The choice of scale changes the interpretation of the plot. Select the scale using the button above. Additionally, the time range of data displayed may be changed using the range slider below the plot. By default, the entire range of data is shown.</center>"),
                              tags$p(""), # blank line
                              htmlOutput({"text_flattening"}),
                              tags$p(""), # blank line
                        fluidRow(DTOutput("table_prov_overview")),
                        "RP = repatriated travellers."
                        )
