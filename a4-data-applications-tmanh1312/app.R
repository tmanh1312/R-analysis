library("shiny")
library("rsconnect")
library("maps")
source("ui.r")
source("server.r")

shinyApp(ui = ui, server = server)