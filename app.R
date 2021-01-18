# define global
source("global.R")

# define UI
source("ui.R")

# define server
source("server.R")

# run app
shinyApp(ui, server)