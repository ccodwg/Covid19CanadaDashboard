# load analytics (if available)
analytics <- if (file.exists("google-analytics.html")) {
  shiny::tags$head(shiny::includeHTML("google-analytics.html"))
} else {}

# ui
ui <- function(req) {
  shinydashboardPlus::dashboardPage(
    md = TRUE,
    skin = "blue",
    header = shinydashboardPlus::dashboardHeader(
      title = "COVID-19 in Canada"
    ),
    sidebar = shinydashboardPlus::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "Navigation", startExpanded = TRUE, icon = shiny::icon("map", lib = "font-awesome"),
          shinydashboard::menuSubItem(text = "Top of page", href = "#", newtab = FALSE)
        )
      )
    ),
    body = shinydashboard::dashboardBody(
      metathis::meta() %>%
        metathis::meta_social(
          title = "COVID-19 in Canada",
          description = "An interactive visualization of the ongoing epidemic of COVID-19 in Canada",
          url = "https://art-bd.shinyapps.io/covid19canada/",
          image = "https://source.unsplash.com/VhjsGKMefkk/640x426",
          image_alt = "COVID-19"),
      analytics, # included if available
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            width = 8, offset = 2,
            shiny::tags$h2("Confirmed COVID-19 cases by province/territory"),
            apexcharter::apexchartOutput("pt_cases_ts")
          )
        )
      )
    ),
    controlbar = NULL, # no right sidebar
    footer = shinydashboardPlus::dashboardFooter(),
    title = "COVID-19 in Canada")
}