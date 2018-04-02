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
library(rclipboard)

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
        checkboxGroupInput("Include_options", 
                           label = "Include twitter handles for?", 
                           choices = c("Pub" = "Twitter_handle_pub",
                                       "pint" = "#pint",
                                       "Sponsors" = "Twitter_handle_funder") ),
        uiOutput("select_speakers")
        

        
        #uiOutput("pre_requisites")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        rclipboardSetup(),
        
        textAreaInput("text_a", label = "Edit Message here:",width = "500px",resize = "both",placeholder = "Write message: (eg Pint of Science is Great!)"),
        hr(),
        
        textAreaInput("text_2",label = "Full message will appear here:",width = "500px",height = "100px", resize = "both",
                      placeholder = "Twitter handles will appear here at the end of your message depending on the options selected to the left (eg: Pint of Science is Great! @virustinkerer)"),
        
        # UI ouputs for the copy-to-clipboard buttons
        uiOutput("clip"),
        
        # A text input for testing the clipboard content.
        textInput("paste", "Paste here:"),
        textOutput("test1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
 
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
  
  observe({
    
     My_data()
     
     
     text_output_speaker <- POS_info %>%
       filter(Theme %in% input$theme,
              Night %in% input$night,
              Speakers %in% input$select_speakers
       )%>%
       pull(Twitter_handle_speaker)
     
     message = NULL
     
# Currently outputs tons of notifications everytime you type something in, change to reactive
     if(any(is.na(text_output_speaker))){
       message <- POS_info %>%
         filter(Theme %in% input$theme,
                Night %in% input$night,
                Speakers %in% input$select_speakers,
                is.na(Twitter_handle_speaker)
         )%>%
         pull(Speakers)
       
       message <- unique(message)
       
       if (length(message) == 1){
       message <- paste(message,"has no twitter handle",collapse = ",")
       }else{
       
       message <- paste(message, collapse = ", ", sep = " ")
       message <- paste(message,"have no twitter handles", sep = " ")
       }
     }
     
     output$test1 <- renderText({
       message
     })
     
     All_handles <- "remove_me"

      test <- which(input$Include_options == "Twitter_handle_pub")
      
      if (length(test) != 0){ 
         pub_twitter <- POS_info %>%
           filter(Theme %in% input$theme,
                  Night %in% input$night,
                  Speakers %in% input$select_speakers
           )%>%
           pull(Twitter_handle_pub)
         
         All_handles <- c(All_handles, pub_twitter)
      }
      
      test <- which(input$Include_options == "Twitter_handle_funder")
      
      if (length(test) != 0){ 
         funder_twitter <- POS_info %>%
           filter(Theme %in% input$theme,
                  Night %in% input$night,
                  Speakers %in% input$select_speakers
           )%>%
           pull(Twitter_handle_funder)
         
         funder_twitter <- unique(funder_twitter)
         
         All_handles <- c(All_handles, funder_twitter)
      }
         test <- which(input$Include_options == "#pint")
         
      if (length(test) != 0){ 
         All_handles <- c(All_handles, "#pint")
      }

     
     if(length(All_handles) > 1){
       All_handles <- All_handles[-1]
       text_output_speaker <- c(text_output_speaker,All_handles)
     }
     
     text_output_speaker_1 <- unique(text_output_speaker[!is.na(text_output_speaker)])
     text_output_speaker_2 <- c(input$text_a,text_output_speaker_1)
     
     
     updateTextInput(session,"text_2",value = paste(text_output_speaker_2, collapse = " "))
     
     
})
   
  
   
   # Add clipboard buttons
   output$clip <- renderUI({
     rclipButton("clipbtn", "Copy to Clipboard", input$text_2, icon("clipboard"))
   })
   
   # Workaround for execution within RStudio
   observeEvent(input$clipbtn, clipr::write_clip(input$text_2))
  }


# Run the application 
shinyApp(ui = ui, server = server)

