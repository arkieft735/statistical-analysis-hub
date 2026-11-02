library(shiny)

# User Interface
ui <- fluidPage(
  
  titlePanel("Statistical Methods Dashboard"),
  
  h2("Welcome!"),
  
  p("An interactive dashboard for exploring statistical methods.")
  
)

# Server
server <- function(input, output, session) {
  
}

# Run Application
shinyApp(ui = ui, server = server)