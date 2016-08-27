# ui.R
library(shiny)

shinyUI(
  pageWithSidebar(
  headerPanel("Body Mass Index (BMI) Calculation and Evaluation"),
  sidebarPanel(
    checkboxGroupInput("gender", "Select Gender",
                       c("Female" = "F",
                         "Male" = "M")),      
    checkboxGroupInput("age", "Select Age",
                       c("20-29" = "20-29",
                         "30-39" = "30-39",
                         "40-49" = "40-49",
                         "50-59" = "50-59",
                         "60-69" = "60-69",
                         "70-79" = "70-79",
                         "80+" = "80+")),    
    numericInput('weight', 'Input Weight (pounds)', 0, min = 0, max = 440, step= 1),
    numericInput('height_in_inches', 'Input Height (inches)', 0, min = 0, max = 84, step= 1),
    submitButton('Submit')  
  ),
  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
      h3('You Entered'),
      h4('Gender'),
      verbatimTextOutput("ogender"),
      h4('Age'),
      verbatimTextOutput("oage"),
      h4('Weight (pounds)'),
      verbatimTextOutput("oweight"),
      h4('Height (inches)'),
      verbatimTextOutput("oheight_in_inches"),
      h3('Your Results'),
      h4('BMI'),
      verbatimTextOutput("oBMI"),
      h4('Category'),
      verbatimTextOutput("oCategory"),
      h4('Percentile Tier:  For your gender and age group'),
      verbatimTextOutput("oPct")
  )
 )
) 