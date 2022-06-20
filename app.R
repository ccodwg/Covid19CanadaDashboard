# load pipe
library(magrittr)

# source components
source("global.R")
source("ui.R")
source("server.R")

# run app
shiny::shinyApp(ui, server)
