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
library(plyr)
library(dplyr)

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
        uiOutput("night"),
        
        uiOutput("select_speakers")
        #uiOutput("pre_requisites")
      ),
      
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
                        choices = night.output)
   })
  

  
output$select_speakers<- renderUI({
  
  if (is.null(input$theme)) {
    return(NULL)
  }    
  if (is.null(input$night)) {
    return(NULL)
  }    
  
  POS_data_limited <- POS_info %>%
    filter(Theme %in% input$theme,
           Night %in% input$night,
           !is.na(Speakers)
    )
  speakers_to_use <- unique(POS_data_limited$Speakers)
  checkboxGroupInput("select_speakers", label = "Speakers",
                     choices = speakers_to_use,selected = speakers_to_use)
  
})

})
  
   output$text_1 <- renderText({
     My_data()
     
     if (is.null(input$theme)) {
       return(NULL)
     }    
     if (is.null(input$night)) {
       return(NULL)
     }   
     if (is.null(input$select_speakers)) {
       return(NULL)
     }   
     
     POS_data_final <- POS_info %>%
       filter(Theme %in% input$theme,
              Night %in% input$night,
              Speakers %in% input$select_speakers
       )
     text_output_speaker <- unique(unlist(POS_data_final$Twitter_handle_speaker))
     text_output_speaker
     
})
}
# Run the application 
shinyApp(ui = ui, server = server)

