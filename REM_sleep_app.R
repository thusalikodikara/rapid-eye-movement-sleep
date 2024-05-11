
# Rapid Eye Movement Sleep


# reading the data set
sleep_data <- read.csv("Sleep_Efficiency.csv")

# loading necessary packages
library(shiny)
library(dplyr)


# user interface

ui <- fluidPage(
  
  span(titlePanel("RAPID EYE MOVEMENT SLEEP"), 
       style = "font-family: 'Bookman old style'; font-size : 22pt"),
  
  tags$style(HTML("
    body {
      background-color: lightgray;
      font-family: 'Arial', sans-serif;
      font-size: 16px;
    }
  ")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("The Sleep Efficiency dataset on Kaggle offers insights into sleep 
      patterns and related factors. It includes unique identifiers like age, gender, 
      and sleep metrics like duration, efficiency, and different sleep stages. 
      This dataset is valuable for studying health statistics related to health.", 
               style = "font-size: 14px;"),
      
      img(src = "sleep_image.jpg", height = 150, width = 220, 
          style = "display: block; margin-left: auto; margin-right: auto;"),
      
      selectInput("gender", "Gender : ",
                  choices = c("Male", "Female")),
      
      sliderInput("age",
                  label = "Age : ",
                  min = 0, max = 80, value = c(20, 40)),
      
      sliderInput("duration",
                  label = "Duration (in hours) : ",
                  min = 2, max = 12, value = c(6, 9))
    ),
    
    mainPanel(
      
      p("Quality sleep is key to our performance and well-being. 
        It supports cognitive function, emotional resilience, and physical health. 
        By prioritizing healthy sleep habits, we boost efficiency and productivity.", 
        style = "font-family: 'Bahnschrift light'; font-size : 12pt"),
      
      p("REM sleep percentage is the portion of time spent in REM", 
        strong("(Rapid Eye Movement)")," sleep compared to total sleep time. 
        For adults, around 20-25% of sleep is typically REM sleep. Optimal 
        percentages vary by age and individual factors, influencing overall sleep quality.",
        style = "font-family: 'Bahnschrift light'; font-size : 12pt"),
      
      div(
        
        p(strong("The combination you have selected is,")),
        
        textOutput("selected_gender"),
        
        textOutput("age_min_max"),
        
        textOutput("duration_min_max"),
        
        style = "font-family: 'Agency FB'; font-size:16pt; color:#142993"),
      
      br(),
      
      plotOutput("histogram")
      
    )
  )
)


# server function

server <- function(input, output) {
  
  output$selected_gender <- renderText({
    paste("Gender : ", input$gender)
  })  
  
  output$age_min_max <- renderText({
    paste("You have chosen the age range from", input$age[1], "to", input$age[2])
  })
  
  output$duration_min_max <- renderText({
    paste("You have chosen the sleep duration from", input$duration[1], "to",  input$duration[2])
  })
  
  filtered_data <- reactive({
    filter(sleep_data,
           Gender == input$gender,
           Age >= input$age[1] & Age <= input$age[2],
           Sleep.duration >= input$duration[1] & Sleep.duration <= input$duration[2])
  })
  
  output$histogram <- renderPlot({
    hist(filtered_data()$REM.sleep.percentage, 
         xlab = "REM Sleep Percentage", 
         main = "HISTOGRAM OF REM SLEEP PERCENTAGE",
         col = "#7BB5EC",
         border = "black")
  })
  
}


# running the app

shinyApp(ui = ui, server = server)
