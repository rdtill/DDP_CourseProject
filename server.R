#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    data('airquality')
    my_data = airquality[complete.cases(airquality),]
    
    oz = ifelse(test = input$ozone, yes = 1, no = 0)
    sol = ifelse(test = input$solar, yes = 1, no = 0)
    wnd = ifelse(test = input$wind, yes = 1, no = 0)
    
    my_data$Ozone   = my_data$Ozone * oz
    my_data$Solar.R = my_data$Solar.R * sol
    my_data$Wind    = my_data$Wind * wnd
    
    if(sum(oz, sol, wnd) > 0 && !(input$month)){
      fit = lm(Temp ~ Ozone + Solar.R + Wind, data = my_data)
      avg_err = sqrt(sum(resid(fit)^2) / length(resid(fit)))
      plot(predict(fit), my_data$Temp, 
           pch = 16, xlab = 'Predicted Temp', ylab = 'Actual Temp', 
           main = 'Predicted vs Actual Values')
      abline(lm(my_data$Temp ~ predict(fit)))
    } else {
      if(input$month) {
        fit = lm(Temp ~ Ozone + Solar.R + Wind + factor(Month), data = my_data)
        avg_err = sqrt(sum(resid(fit)^2) / length(resid(fit)))
        plot(predict(fit), my_data$Temp, 
             pch = 16, xlab = 'Predicted Temp', ylab = 'Actual Temp', 
             main = 'Predicted vs Actual Values')
        abline(lm(my_data$Temp ~ predict(fit)))
      }
    }
    
  })
  
  output$err_str <- renderText({
    
    data('airquality')
    my_data = airquality[complete.cases(airquality),]
    
    oz = ifelse(test = input$ozone, yes = 1, no = 0)
    sol = ifelse(test = input$solar, yes = 1, no = 0)
    wnd = ifelse(test = input$wind, yes = 1, no = 0)
    
    my_data$Ozone   = my_data$Ozone * oz
    my_data$Solar.R = my_data$Solar.R * sol
    my_data$Wind    = my_data$Wind * wnd
    
    if(sum(oz, sol, wnd) > 0 && !(input$month)){
      fit = lm(Temp ~ Ozone + Solar.R + Wind, data = my_data)
      avg_err = round(sqrt(sum(resid(fit)^2) / length(resid(fit))), 2)
      err_str = paste("Average Error: ", as.character(avg_err), sep = "")
    } else {
      if(input$month) {
        fit = lm(Temp ~ Ozone + Solar.R + Wind + factor(Month), data = my_data)
        avg_err = round(sqrt(sum(resid(fit)^2) / length(resid(fit))), 2)
        err_str = paste("Average Error: ", as.character(avg_err), sep = "")
      } else{
        err_str = "Choose a predictor!"
      }
    }
    
  })
  
  output$pred_value <- renderText({
    
    data('airquality')
    my_data = airquality[complete.cases(airquality),]
    
    oz = ifelse(test = input$ozone, yes = 1, no = 0)
    sol = ifelse(test = input$solar, yes = 1, no = 0)
    wnd = ifelse(test = input$wind, yes = 1, no = 0)
    
    my_data$Ozone   = my_data$Ozone * oz
    my_data$Solar.R = my_data$Solar.R * sol
    my_data$Wind    = my_data$Wind * wnd
    
    if(sum(oz, sol, wnd) > 0 && !input$month){
      fit = lm(Temp ~ Ozone + Solar.R + Wind, data = my_data)
      
      vals = c(input$ozone_pred,
               input$solar_pred,
               input$wind_pred)
      
      pred_df = data.frame(Ozone   = vals[1],
                           Solar.R = vals[2],
                           Wind    = vals[3],
                           Temp    = 0,
                           Month   = 0,
                           Day     = 0)
      
      pred_val   = round(predict(object = fit, newdata = pred_df))
      pred_value = paste("Predicted Value: ", pred_val, sep = "")
    } else {
      if(input$month){
        fit = lm(Temp ~ Ozone + Solar.R + Wind + factor(Month), data = my_data)
        
        vals = c(input$ozone_pred,
                 input$solar_pred,
                 input$wind_pred, 
                 input$month_pred)
        
        pred_df = data.frame(Ozone   = vals[1],
                             Solar.R = vals[2],
                             Wind    = vals[3],
                             Temp    = 0,
                             Month   = vals[4],
                             Day     = 0)
        
        pred_val   = round(predict(object = fit, newdata = pred_df))
        pred_value = paste("Predicted Value: ", pred_val, sep = "")
      }
      else{
        pred_value = "Select a predictor to generate a prediction!"
      }
    }
    
  })
  
})
