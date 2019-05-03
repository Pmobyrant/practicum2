## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Heart Disease Prediction"),
  dashboardSidebar(
    selectInput("select1", label = h3("Chest Pain Type"), 
                choices = list("Typical Angina" = 0, "Atypical Angina" = 1, "Non-Aginal Pain" = 2, "Asymptomatic" = 3), 
                selected = 0),
    sliderInput("slider1", label = h3("Age in Years"), min = 0, 
                max = 130, value = 50),
    selectInput("select2", label = h3("Sex"), 
                choices = list("Male" = 1, "Female" = 0), 
                selected = 1),
    numericInput("num1", label = h3("Maximum Heart Rate"), value = 150),
    numericInput("num2", label = h3("Blood Pressure (in mm Hg)"), value = 132)
    ),
  dashboardBody(
    fluidRow(
      #dataTableOutput("test")
      infoBoxOutput("progressBox2",width=8)
      )
    )
  )

server <- function(input, output) {
  library(e1071)
  df = read.csv('C://Users//pobryant/Documents/heart/heart.csv')
  df = df[,c(1,2,3,4,8,14)]
  names(df) = c('age.1','sex.1','cp.1','trestbps.1','thalach.1','target')
  df$age.1 = as.numeric(df$age.1)
  df$sex.1 = as.numeric(df$sex.1)
  df$cp.1 = as.numeric(df$cp.1)
  df$trestbps.1 = as.numeric(df$trestbps.1)
  df$thalach.1 = as.numeric(df$thalach.1)
  df$target = as.numeric(df$target)
  M <- naiveBayes(target ~ ., data = df)
  column.names <- c('age','sex','cp','trestbps','thalach')
  row.names <- c("ROW1")
  matrix.names <- NULL
  
  create_sample <- reactive({as.data.frame(array(c(as.numeric(input$slider1),as.numeric(input$select1),as.numeric(input$select2),as.numeric(input$num1),as.numeric(input$num2)),dim=c(1,5,1), dimnames = list(row.names,column.names,
                                                                                                                                                                                                              matrix.names)))})
  
  #output$test = renderDataTable({create_sample()})
  
  create_output <- reactive({predict(M,create_sample(),type="raw")})
  
   output$progressBox2 <- renderInfoBox({
     infoBox(
       "", paste0("Likelihood of Heart Disease: ",round(create_output()[2]*100), "%"), icon = icon("heartbeat"),
       color = (if(create_output()[2]*100 < 11){'green'} else{'red'}), fill = TRUE
     )
   })
}

shinyApp(ui, server)