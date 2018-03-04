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
    men_women <- students %>% 
      
      ###major_select is the input value for which major is being selected
      select(Major,Men,Women,Total) %>% 
      filter(Major == major_select)
    
    ##this counts all the men in the specified major
    just_men <- men_women %>% 
      select(Men) %>% 
      summarize(sum(Men))
    #This counts all the women in the specified major
    just_women <- men_women %>% 
      select(Women) %>% 
      summarize(sum(Women))
    
    #This plots the men vs women in a pie chart
    plot_ly(data = men_women, labels = c("Men", "Women"), values = ~c(Men,Women), type = 'pie')
    
    
    
    
  })
})