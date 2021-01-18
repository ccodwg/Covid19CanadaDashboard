tab_about <- tabItem(tabName = "tab_about",
                     fluidPage(
                       title = "About the data",
                       width = 12,
                       includeHTML("text/about.html")
                     ))