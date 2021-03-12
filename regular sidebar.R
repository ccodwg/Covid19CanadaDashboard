
# render sidebar
output$sidebar_controls <- renderUI({
  if (input$tab %in% c(
    "tab_trends",
    "tab_maps",
    "tab_cases",
    "tab_mortality",
    "tab_recovered",
    "tab_testing",
    "tab_vaccines",
    "tab_travel"
  )) {
    list(
      selectInput(
        "prov",
        "Province",
        choices = c(
          "All provinces" = "Canada",
          "Alberta" = "Alberta",
          "British Columbia" = "BC",
          "Manitoba" = "Manitoba",
          "New Brunswick" = "New Brunswick",
          "Newfoundland and Labrador" = "NL",
          "Northwest Territories" = "NWT",
          "Nova Scotia" = "Nova Scotia",
          "Nunavut" = "Nunavut",
          "Ontario" = "Ontario",
          "Prince Edward Island" = "PEI",
          "Quebec" = "Quebec",
          "Saskatchewan" = "Saskatchewan",
          "Yukon" = "Yukon"
        )
      ),
      selectInput("hr","Health Region",
                  choices = list("Not Selected" = "None",
                                 "Alberta" = c("Calgary","Central","Edmonton","North","South"),
                                 "British Columbia" = c("Fraser","Interior","Island","Northern","Vancouver Coastal"),
                                 "Manitoba" = c("Interlake-Eastern","Northern","Prairie Mountain","Southern Health","Winnipeg"),
                                 "New Brunswick" = c("Zone 1 (Moncton area)","Zone 2 (Saint John area)","Zone 3 (Fredericton area)",
                                                     "Zone 4 (Edmundston area)","Zone 5 (Campbellton area)","Zone 6 (Bathurst area)",
                                                     "Zone 7 (Miramichi area)"),
                                 "Newfoundland" = c("Central","Eastern","Labrador-Grenfell","Western"),
                                 "Nova Scotia" = c("Zone 1 - Western","Zone 2 - Northern","Zone 3 - Eastern","Zone 4 - Central"),
                                 "Nunavut" = c("Nunavut"),
                                 "North West Territories" = c("North West Territories"),
                                 "Ontario" = c("Algoma","Brant","Chatham-Kent","Durham","Eastern","Grey Bruce","Haldimand-Norfolk",
                                               "Haliburton Kawartha Pineridge","Halton","Hamilton","Hastings Prince Edward","Huron Perth",
                                               "Kingston Frontenac Lennox & Addington","Lambton","Leeds Grenville and Lanark","Middlesex-London",
                                               "Niagara","North Bay Parry Sound","Northwestern","Ottawa","Peel","Peterborough","Porcupine",
                                               "Renfrew","Simcoe Muskoka","Southwestern","Sudbury","Thunder Bay","Timiskaming","Toronto",
                                               "Waterloo","Wellington Dufferin Guelph","Windsor-Essex","York"),
                                 "Prince Edward Island" = c("PEI"),
                                 "Quebec" = c("Abitibi-Témiscamingue","Bas-Saint-Laurent","Capitale-Nationale","Chaudière-Appalaches",
                                              "Côte-Nord","Estrie","Gaspésie-Îles-de-la-Madeleine","Lanaudière","Laurentides","Laval",
                                              "Mauricie","Montérégie","Montréal","Nord-du-Québec","Nunavik","Outaouais","Saguenay","Terres-Cries-de-la-Baie-James"),
                                 "Saskatchewan" = c("Central","Far North","North","Regina","Saskatoon","South"),
                                 "Yukon" = c("Yukon")
                  ),selectize = FALSE
      ),
      dateRangeInput(
        "date_range",
        "Date range",
        start = date_min,
        end = date_max,
        min = date_min,
        max = date_max,
        format = "yyyy/mm/dd"
      )
    )
  }
})