#This is for the shiny app
library(shiny)
#This is for sifting data
library(dplyr)
#This is for interactive plotting of data
library(plotly)

#This is for a faster read in of data
library(data.table)


students <- fread("recent-grads.csv", stringsAsFactors = FALSE)

shinyServer(function(input,output){
  ##This ouputs the first graph: men vs women
  output$men_women <- renderPlot({
    
  })
})