# load tabs
invisible(sapply(list.files("tabs", full.names = TRUE), source))

# JS for getting screen width
js_get_width <-
  '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("screen_width", jsWidth);
});
'

# ui
ui <- function(req) {
  shinydashboardPlus::dashboardPage(
    title = "COVID-19 in Canada",
    header = dashboardHeader(title = "COVID-19 in Canada"),
    sidebar = dashboardSidebar(
      useShinyjs(),
      sidebarMenu(
        id = "tab",
        uiOutput("sidebar_controls"),
        menuItem("Overview",
                 tabName = "tab_overview",
                 icon = icon("table")),
        # menuItem(
        #   "Trends",
        #   tabName = "tab_trends",
        #   icon = icon("chart-line")
        # ),
        menuItem(
          "Comparisons",
          tabName = "tab_comparisons",
          icon = icon("chart-bar")
        ),
        menuItem(
          "Maps",
          tabName = "tab_maps",
          icon = icon("map-marked-alt")
        ),
        menuItem("Vaccines",
                 tabName = "tab_vaccines",
                 icon = icon("syringe")
        ),
        menuItem(
          "Cases",
          tabName = "tab_cases",
          icon = icon("calendar-plus")
        ),
        menuItem(
          "Mortality",
          tabName = "tab_mortality",
          icon = icon("heartbeat")
        ),
        menuItem(
          "Recovered",
          tabName = "tab_recovered",
          icon = icon("file-medical")
        ),
        menuItem("Testing",
                 tabName = "tab_testing",
                 icon = icon("vial")
        ),
        menuItem(
          "About the data",
          tabName = "tab_about",
          icon = icon("question-circle")
        ),
        useShinyalert(),
        tags$footer(
          HTML(
            paste0(
              "<b>Please visit our <a href='https://opencovid.ca/work/data-faq/' target='_blank'> frequently<br/>asked questions</a> page</br>about our dataset for answers</br>to questions like this one:<br/></b>",
              sapply(strwrap(faq, 30, simplify = FALSE), paste, collapse = "<br/>"),
              "<b><br/><br/><a href='https://github.com/ishaberry/Covid19Canada' target='_blank'> Download our dataset</a><br/><br/>Last updated:<br/>",
              update_time,
              "</b><br/><br/>All data used to produce this<br/>dashboard are exclusively<br/>collected from publicly available<br/>sources including government<br/>reports and news media.<br/>"
            )
          ),
          align = "left",
          style = "
              position:absolute;
              width:100%;
              height:0px;
              color: white;
              padding: 10px;
              background-color: transparent;
              word-wrap: break-word;
              z-index: 1000;"
        )
      )
    ),
    body = dashboardBody(
      meta() %>%
        meta_social(
          title = "COVID-19 in Canada",
          description = "An interactive visualization of the ongoing epidemic of COVID-19 in Canada",
          url = "https://art-bd.shinyapps.io/covid19canada/",
          image = "https://source.unsplash.com/VhjsGKMefkk/640x426",
          image_alt = "COVID-19"
        ),
      ### google-analytics.html is not needed to run app locally
      analytics,
      tags$script(js_get_width),
      tags$head(tags$style(
        HTML(
          '.myClass {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 5px;
        overflow: hidden;
        color: white;
      }
    '
        )
      )),
      tags$script(
        HTML(
          '
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Menu </span>\');
      })
     '
        )
      ),
      tabItems(
        tab_overview,
        # tab_trends,
        tab_comparisons,
        tab_maps,
        tab_vaccines,
        tab_cases,
        tab_mortality,
        tab_recovered,
        tab_testing,
        tab_about
      )
    ),
    footer = dashboardFooter(
      left = HTML(
        "Created by <a href='https://twitter.com/JPSoucy' target='_blank'> Jean-Paul R. Soucy</a> and <a href='https://twitter.com/ishaberry2' target='_blank'> Isha Berry</a>, Dalla Lana School of Public Health, University of Toronto. Developed by Jean-Paul R. Soucy, Isha Berry, <a href='https://twitter.com/benkcwong' target='_blank'> Ben Wong</a> and <a href='https://github.com/kbelisar' target='_blank'> Kyla Belisario</a>, with contributions from <a href='https://www.linkedin.com/in/wenyu-huang-11b211124/' target='_blank'> Wenyu Huang</a>, <a href='https://www.linkedin.com/in/angelinazhu' target='_blank'> Angelina Zhu</a>, <a href='https://github.com/mattwarkentin' target='_blank'> Matthew T. Warkentin</a> and <a href='https://mountainmath.ca/' target='_blank'> Jens von Bergmann</a>. Data curated by the <a href='https://opencovid.ca' target='_blank'> COVID-19 Canada Open Data Working Group</a> (see \"About the data\" for sources)."
      )
    )
  )
}