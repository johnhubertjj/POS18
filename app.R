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
POS_info$Date <- as.factor(POS_info$Date)

Media_Wales <- c("@BBCRadioWales", "@ITVWales", "@WalesOnline", "@capitalswanews", "@HeartWales", "@heartwalesnews", "@southwalesargus")
Media_Cardiff <- c("@WoiC", "@WeAreCardiff", "@cardiffonline", "@VisitCardiff", "@MadeInCardiffTV", "@cardiffcouncil", "@cardiffian_news", "@itsoncardiff")

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
                                       "pint18" = "#pint18",
                                       "Sponsors" = "Twitter_handle_funder",
                                       "Cardiff" = "Cardiff_handles",
                                       "Media-Wales" = "Media_Wales",
                                       "Media-Cardiff" = "Media_Cardiff")
                           ),
        uiOutput("select_speakers")
        

        
        #uiOutput("pre_requisites")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        rclipboardSetup(),
        
        textAreaInput("text_a", label = "Edit Message here:",width = "500px", resize = "both",placeholder = "Write message: (eg Pint of Science is Great!)"),
        hr(),
        
        textAreaInput("text_2",label = "Full message will appear here:",width = "500px",height = "100px", resize = "both",
                      placeholder = "Twitter handles will appear here at the end of your message depending on the options selected to the left (eg: Pint of Science is Great! @virustinkerer)"),
        fluidRow(column(8,
               htmlOutput("length_text_left")
        )),
        # UI ouputs for the copy-to-clipboard buttons
        uiOutput("clip"),
        
        textOutput("test1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
 
  My_data <- reactive({
    
    ## Create arguments to shiny app
    theme.output <- unique(POS_info$Theme)
    night.output <- levels(unique(POS_info$Date))
    
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
           Date %in% input$night,
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
              Date %in% input$night,
              Speakers %in% input$select_speakers
       )%>%
       pull(Twitter_handle_speaker)
     
     message = NULL
     
# Currently outputs tons of notifications everytime you type something in, change to reactive
     if(any(is.na(text_output_speaker))){
       message <- POS_info %>%
         filter(Theme %in% input$theme,
                Date %in% input$night,
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
                  Date %in% input$night,
                  Speakers %in% input$select_speakers
           )%>%
           pull(Twitter_handle_pub)
         
         All_handles <- c(All_handles, pub_twitter)
      }
      
      test <- which(input$Include_options == "Twitter_handle_funder")
      
      if (length(test) != 0){ 
         funder_twitter <- POS_info %>%
           filter(Theme %in% input$theme,
                  Date %in% input$night
           )%>%
           pull(Twitter_handle_funder)
         
         funder_twitter <- unique(funder_twitter)
         
         All_handles <- c(All_handles, funder_twitter)
      }

      
      test <- which(input$Include_options == "Media_Cardiff")

      if (length(test) != 0 ){
        All_handles <- c(All_handles, Media_Cardiff)
      }
      
      test <- which(input$Include_options == "Media_Wales")
      
      if (length(test) != 0 ){
        All_handles <- c(All_handles, Media_Wales)
      }
      
        test <- which(input$Include_options == "Cardiff_handles")
        
      if (length(test) != 0 ){
        All_handles <- c(All_handles, "@engagewithCU", "@CUPublicEvents")
      }
        
         test <- which(input$Include_options == "#pint18")
         
      if (length(test) != 0){ 
         All_handles <- c(All_handles, "@pintofscience", "#pint18")
      }
         
     
     if(length(All_handles) > 1){
       All_handles <- All_handles[-1]
       text_output_speaker <- c(text_output_speaker,All_handles)
     }
     
     text_output_speaker_1 <- unique(text_output_speaker[!is.na(text_output_speaker)])
     text_output_speaker_2 <- c(input$text_a,text_output_speaker_1)
     
     
     updateTextInput(session,"text_2",value = paste(text_output_speaker_2, collapse = " "))
     
     #number_of_characters <- paste(text_output_speaker_2, collapse = " ")
     #number_of_characters <- nchar(input$text_2)
     #output$length_text_left <- renderText(280 - number_of_characters)
})
  
  observeEvent(input$text_2,
               {  
                number_of_characters <- nchar(input$text_2)
                characters_left <- 280 - number_of_characters
                
                 if (characters_left > 0){
  output$length_text_left <- renderText({paste("<font color=\"#66ff33\"><b>", characters_left, "</b></font>") }) 
  
                 }else{
                   output$length_text_left <- renderText({paste("<font color=\"#ff3300\"><b>", characters_left, "</b></font>") }) 
                 }
  })
   
   # Add clipboard buttons
   output$clip <- renderUI({
     rclipButton("clipbtn", "Copy to Clipboard", input$text_2, icon("clipboard"))
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

