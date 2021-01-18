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
                                  width = 6,
                                  offset = 3,
                                  valueBoxOutput("value_box_summary_doses_administered", width = 4),
                                  valueBoxOutput("value_box_summary_hosp", width = 4),
                                  valueBoxOutput("value_box_summary_testing", width = 4)
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
                                  uiOutput("ui_plot_choropleth_overview_cases"),
                                  uiOutput("ui_window_choropleth_overview_cases"),
                                ),
                                tabPanel(
                                  "VaxView",
                                  uiOutput("ui_plot_choropleth_overview_vaccine_administration"),
                                  uiOutput("ui_window_choropleth_overview_vaccine_administration"),
                                )
                              ),
                              HTML("How is each province <b>flattening the curve</b>? See below for our interactive plots of the average number of cases and deaths reported each day by province. These visualizations are inspired by charts by John Burn-Murdoch (<a href='https://twitter.com/jburnmurdoch' target='_blank'>@jburnmurdoch</a>) for <a href='https://www.ft.com/' target='_blank'> FT</a>.
                           <br><br>
                           These charts may be viewed on a <b>linear scale</b> (by default) or a <b>logarithmic scale</b>. On a linear scale, the distance between <i>absolute values</i> is constant (e.g., the distance between 10 and 20 is the same as the distance between 110 and 120). On a logarithmic scale, the distance between <i>orders of magnitude</i> is constant (e.g., on a log 10 scale, the distance between 10 and 100 is the same as the distance between 100 and 1000). The choice of scale changes the intrerpretation of the plot. Select the scale using the button below."),
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
                              htmlOutput({"text_flattening"}),
                              tabsetPanel(
                                type = "tabs",
                                tabPanel(
                                  "Cases",
                                  fluidRow(box(
                                    title = textOutput("title_flattening_cases"),
                                    plotlyOutput("plot_flattening_cases"),
                                    width = 12)),
                                  fluidRow(
                                    column(
                                      sliderInput(
                                        "min_flattening_cases",
                                        "Begin plot at how many cumulative cases?",
                                        min = 30,
                                        max = 1000,
                                        step = 100,
                                        value = NULL
                                      ),
                                      width = 12,
                                      align = "center"
                                      )
                                    )
                                  ),
                                tabPanel(
                                  "Mortality",
                                  fluidRow(box(
                                    title = textOutput("title_flattening_mortality"),
                                    plotlyOutput("plot_flattening_mortality"),
                                    width = 12)),
                                  fluidRow(
                                    column(
                                      sliderInput(
                                        "min_flattening_mortality",
                                        "Begin plot at how many cumulative deaths?",
                                        min = 10,
                                        max = 100,
                                        step = 10,
                                        value = 10
                                      ),
                                      width = 12,
                                      align = "center"
                                    )
                                  )
                                )
                              ),
                        fluidRow(DTOutput("table_prov_overview")))
                        )