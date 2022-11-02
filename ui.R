# load analytics (if available)
analytics <- if (file.exists("google-analytics.html")) {
  shiny::tags$head(shiny::includeHTML("google-analytics.html"))
} else {}

# ui

ui <- function(req) {
  
shiny::tagList(
  shinydashboard::dashboardHeader(
    # modify size of header and magin whitespace
    tags$li(class = "dropdown",
            tags$style(".navbar {margin-top: -50px;}"),
            tags$style(".navbar {max-height:80px !important}"),
            tags$style(".navbar {min-height:80px !important}")
    )
  ),
  metathis::meta() %>%
    metathis::meta_social(
             title = "COVID-19 in Canada",
             description = "An interactive visualization of the ongoing epidemic of COVID-19 in Canada",
             url = "https://art-bd.shinyapps.io/covid19canada/",
             image = "https://source.unsplash.com/VhjsGKMefkk/640x426",
             image_alt = "COVID-19"),
         analytics, # included if available
  shiny::navbarPage(theme = bslib::bs_theme(bootswatch = "flatly", primary = "#1C4579",success = "#73B6DE"), # flatly bootswatch customization of colours
                    title = div(tags$a(img(src="ccodwg_logo_bold.png", height=65), href= "https://opencovid.ca")),
                    collapsible = TRUE,
                    windowTitle = "COVID-19 in Canada", # browser tab title
                    
                    ### WASTEWATER TAB
                    shiny::tabPanel(
                      title = "Wastewater", icon = shiny::icon("vial-virus", lib = "font-awesome"),
                      shiny::column(
                        width = 8, offset = 2,
                        shiny::fluidRow(
                          shiny::tags$h3("COVID-19 Wastewater Signal in Canada by Province", align = "center"),
                          shiny::tags$body("", align = "center"),
                          apexcharter::apexfacetOutput("pt_wastewater_ts")
                          ),
                        shiny::fluidRow(
                          shiny::tags$h3("COVID-19 Wastewater Signal in Canada by Region", align = "center"),
                          apexcharter::apexfacetOutput("hr_wastewater_ts")
                        )
                        )
                      ),
                    
                    ### HOSPITALIZATIONS TAB
                    shiny::tabPanel(
                      title = "Hospitalizations", icon = shiny::icon("circle-h", lib = "font-awesome"),
                      shiny::column(
                        width = 8, offset = 2,
                        shiny::fluidRow(
                          shiny::tags$h3("COVID-19 Hospitalizations in Canada", align = "center"),
                          apexcharter::apexfacetOutput("pt_hospitalizations_ts")
                          )
                        )
                      ),
                    
                    ### MORTALITY TAB
                    shiny::tabPanel(
                      title = "Mortality", icon = shiny::icon("heart-pulse", lib = "font-awesome"),
                      shiny::column(
                        width = 8, offset = 2,
                        shiny::fluidRow(
                          shiny::tags$h3("COVID-19 Mortality in Canada", align = "center"),
                          apexcharter::apexfacetOutput("deaths_pt_ts")
                        )
                      )
                    )
             ), # navbar page bracket
  tags$footer("© 2022 CCODWG (COVID-19 Canada Open Data Working Group)", align = "center", style = "background-color: #C2DFF1")
) #taglist bracket

}
