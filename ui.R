#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
setwd('C:\\Users\\User\\Desktop\\Coursera\\Data Science Specialization\\Developing Data Products\\Course_Project\\Course_Project')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fitting a Regression Model to Predict Temperature"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      h3("Choose your predictors below!"),
      
      checkboxInput("ozone", label = "Ozone"), 
      checkboxInput("solar", label = "Solar.R"),
      checkboxInput("wind", label = "Wind"),
      checkboxInput("month", label = "Month"),
      
      
      h3("Add values to predict with here!"),
      
      numericInput("ozone_pred", "Ozone Value", value = 1, step = 1), 
      numericInput("solar_pred", "Solar.R Value", value = 1, step = 1), 
      numericInput("wind_pred", "Wind Value", value = 1, step = 1),
      sliderInput("month_pred", "Month Value", value = 1, min = 5, max = 9, step = 1)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       verbatimTextOutput("err_str"),
       verbatimTextOutput("pred_value")
    )
  )
))
