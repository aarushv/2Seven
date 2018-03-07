#This is for the shiny app
library(shiny)
#This is for sifting data
library(dplyr)
#This is for interactive plotting of data
library(plotly)

#This is for a faster read in of data
library(data.table)

students <- fread("./college-majors/recent-grads.csv", stringsAsFactors = FALSE)


#creates dataframe with just list of majors
#major_list <- read.csv("./college-majors/majors-list.csv")
#major_list <- unique(major_list$Major)
#major_list <- sort(major_list, decreasing = FALSE)

shinyServer(function(input,output){
  
  #output$major_list <- renderUI({
   # selectInput("major", "Choose major: ", major_list)
  #})
  ##This ouputs the first graph: men vs women
  output$men_women <- renderPlotly({
    men_women <- students %>% 
      ###major_select is the input value for which major is being selected
      select(Major,Men,Women,Total) %>% 
      filter(Major == input$major_select)
    
    #This plots the men vs women in a pie chart
    plot_ly(data = men_women, labels = c("Men", "Women"), values = c(men_women$Men,men_women$Women), type = 'pie') %>% 
      layout(title = 'Gender Ratio in Major')
  })
  ##This outputs the pie chart for employment statistics
  output$employed <- renderPlotly({
    
    ###major_select is the input value for which major is selected
    employed <- students %>% 
      select(Major,Employed,Unemployed,College_jobs,Non_college_jobs,Low_wage_jobs) %>% 
      filter(Major == input$major_select)
    
    plot_ly(data = employed, labels = c("Jobs that Require College Degree", "Jobs that Do Not Require a College Degree", "Jobs Close to or at Minimum Wage","Unemployed"), values = ~c(College_jobs,Non_college_jobs, Low_wage_jobs, Unemployed), type = 'pie', marker = list(colors = c("#66a3ff","#4d94ff","#3385ff","#ff3333"))) %>% 
      layout(title = 'Employment vs Unemployment')
  })
  ##This outputs the bar graph for pay 
  output$pay <- renderPlotly({
    
    ##major_select is the input value for which major is selected
    pay <- students %>% 
      select(Major,P25th,Median,P75th) %>% 
      filter(Major == input$major_select)
    
    plot_ly(data = pay, x = c("25th Percentile", "Median", "75th Percentile"), y = ~c(P25th, Median, P75th), type = 'bar') %>% 
      layout(title = 'Pay Rate', yaxis = list(title = "Pay Rate in Thousands of Dollars"))
  })
  
  ##This outputs the bar graph comparison of median pay for the selected major vs median pay for the selected department
  output$department_major <- renderPlotly({
    
    #This selected the major, median pay for major, and major category
    #Keep in mind major_select here is to be used for input
    pay2 <- students %>% 
      select(Major,Major_category,Median) %>% 
      filter(Major == input$major_select)
      
   #This selects the department and gets the average median pay for that department's majors
   department_pay <- students %>% 
     select(Major,Major_category,Median) %>% 
     filter(Major_category == pay2[[2]]) %>% 
     summarize(Median = mean(Median))
   
   #This sets the department's data as a data frame to join with the pay of the specific major
   Department_Median <- as.data.frame(c("Major" = pay2[[2]],"Major_category" = pay2[[2]],department_pay))
   
   #This creates a full data frame of the two pay rates
   pay_comparison <- full_join(pay2,Department_Median)
   
   #This plots it
   plot_ly(data = pay_comparison, x = ~Major, y = ~Median, type = 'bar') %>% 
     layout(title = 'Median Pay of Major vs Median Pay for Department')
  })
  
  output$decision <- renderPlotly({
    if (input$displayOption == "all") {
      return(deparment_major)
    }
  })
  
})