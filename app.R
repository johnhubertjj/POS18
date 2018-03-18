#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

POS_info <- fread("data/Festival_Summary_Sheet_2.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Social Media Handles POS18 Cardiff"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        uiOutput("theme"
        ),
        uiOutput("day_number"),
        
        uiOutput("select_speakers"),
        uiOutput("pre_requisites")
      )
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("text_1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  My_data <- reactive({
    
    ## Create arguments to shiny app
    theme.output <- unique(POS_info$Theme)
    night.output <- unique(POS_info$Night)
    
# alter the data.table here to append a date to each numerical date measurement...
    
   output$theme <- renderUI({
    checkboxGroupInput("theme", label = "Theme",
                       choices = theme.output)
  })
  
   output$night <- renderUI({
     checkboxGroupInput("night", label = "Date",
                        choices = theme.output)
   })
  
  output$geneset <- renderUI({
    checkboxGroupInput("geneset", label = "Geneset PRS to include:",
                       choices = Gene.sets.input)
  })
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
})
}
# Run the application 
shinyApp(ui = ui, server = server)

