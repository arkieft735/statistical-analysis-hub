library(shiny)

# -----------------------------
# USER INTERFACE
# -----------------------------
#Creates the user interface
ui <- fluidPage(
  
  titlePanel("Statistical Analysis Hub"),
  #divides the page into 2 sections
  sidebarLayout(
    
    #left navigation sidebar
    sidebarPanel(
      
      h3("Navigation"),
      
      radioButtons(
        inputId = "page",
        label = "Select a Section:",
        choices = c(
          "Home",
          "Statistical Tests",
          "Confidence Intervals",
          "Method Comparisons"
        ),
        selected = "Home"
      )
      
    ),
    
    # Main Dashboard Content
    mainPanel(
      
      h2(textOutput("page_title")),
      
      p("Welcome to the Statistical Analysis Hub."),
      
      textOutput("page_description")
      
    )
    
  )
)

# -----------------------------
# SERVER
# -----------------------------

server <- function(input, output, session) {
  
  output$page_title <- renderText({
    input$page
  })
  
  output$page_description <- renderText({
    
    switch(
      input$page,
      
      "Home" = "Explore statistical methods and interactive tools.",
      
      "Statistical Tests" = "Perform and explore statistical hypothesis tests.",
      
      "Confidence Intervals" = "Calculate and compare confidence intervals.",
      
      "Method Comparisons" = "Compare the performance of different statistical methods."
    )
    
  })
  
}

# -----------------------------
# RUN APPLICATION
# -----------------------------

shinyApp(ui = ui, server = server)