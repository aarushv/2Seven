#This is for the shiny app
library(shiny)
#This is for sifting data
library(dplyr)
#This is for interactive plotting of data
library(plotly)

#This is for a faster read in of data
library(data.table)

students <- fread("./college-majors/recent-grads.csv", stringsAsFactors = FALSE)
states <- fread("./salaries-by-region.csv", stringsAsFactors = FALSE)

shinyServer(function(input,output){
  
  output$graph <- renderPlotly({
    men_women <- students %>% 
      ###major_select is the input value for which major is being selected
      select(Major,Men,Women,Total) %>% 
      filter(Major == input$major_select)
    ###major_select is the input value for which major is selected
    employed <- students %>% 
      select(Major,Employed,Unemployed,College_jobs,Non_college_jobs,Low_wage_jobs) %>% 
      filter(Major == input$major_select)
    
    ##major_select is the input value for which major is selected
    pay <- students %>% 
      select(Major,P25th,Median,P75th) %>% 
      filter(Major == input$major_select)
    
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
    
    if(input$displayOption == "department"){
      plot_ly(data = pay_comparison, x = ~Major, y = ~Median, type = 'bar') %>% 
        layout(title = 'Median Pay of Major vs Median Pay for Department')
    } else if(input$displayOption == "men_women"){
      plot_ly(data = men_women, labels = c("Men", "Women"), values = c(men_women$Men,men_women$Women), type = 'pie') %>% 
        layout(title = 'Gender Ratio in Major')
    } else if(input$displayOption == "employed") {
      plot_ly(data = employed, labels = c("Jobs that Require College Degree", "Jobs that Do Not Require a College Degree", "Jobs Close to or at Minimum Wage","Unemployed"), values = ~c(College_jobs,Non_college_jobs, Low_wage_jobs, Unemployed), type = 'pie', marker = list(colors = c("#66a3ff","#4d94ff","#3385ff","#ff3333"))) %>% 
        layout(title = 'Employment vs Unemployment')
    } else if (input$displayOption == "pay"){
      plot_ly(data = pay, x = c("25th Percentile", "Median", "75th Percentile"), y = ~c(P25th, Median, P75th), type = 'bar') %>% 
        layout(title = 'Pay Rate', yaxis = list(title = "Pay Rate in Thousands of Dollars"))
    } else {
      plot_ly(data = pay_comparison, x = ~Major, y = ~Median, type = 'bar') %>% 
        layout(title = 'Median Pay of Major vs Median Pay for Department')
      plot_ly(data = men_women, labels = c("Men", "Women"), values = c(men_women$Men,men_women$Women), type = 'pie') %>% 
        layout(title = 'Gender Ratio in Major')
      plot_ly(data = employed, labels = c("Jobs that Require College Degree", "Jobs that Do Not Require a College Degree", "Jobs Close to or at Minimum Wage","Unemployed"), values = ~c(College_jobs,Non_college_jobs, Low_wage_jobs, Unemployed), type = 'pie', marker = list(colors = c("#66a3ff","#4d94ff","#3385ff","#ff3333"))) %>% 
        layout(title = 'Employment vs Unemployment')
      plot_ly(data = pay, x = c("25th Percentile", "Median", "75th Percentile"), y = ~c(P25th, Median, P75th), type = 'bar') %>% 
        layout(title = 'Pay Rate', yaxis = list(title = "Pay Rate in Thousands of Dollars"))

    }
  })
  
  ##Renders the gender distirbution bar chart, $inputmajors.selected is the major chosen by user.
  output$gender.comp <- renderPlotly({
    
    gender.majors1 <- filter(students, students$Major==input$Major1) %>%
      select(Men, Women, Major)
    gender.majors2 <- filter(students, students$Major==input$Major2) %>%
      select(Men, Women, Major)
    gender.majors3 <- filter(students, students$Major==input$Major3) %>%
      select(Men, Women, Major)
    
    gendercomp.selected <- rbind(gender.majors1, gender.majors2, gender.majors3)
    
    gendercomp.plot <- plot_ly(gendercomp.selected, x = ~Major, y = ~Women,
                               type = 'bar', name = 'Women' ) %>%
      add_trace(y = ~Men, name = 'Men') %>%
      layout(yaxis = list(title = 'Gender Distribution for Selected Majors'),
             barmode = 'group')
  })
  
  ##Renders plot comparing the median pay of selected majors
  output$pay.comp <- renderPlotly({
    
    pay.majors1 <- filter(students, students$Major==input$Major1) %>%
      select(Major, Median)
    pay.majors2 <- filter(students, students$Major==input$Major2) %>%
      select(Major, Median)
    pay.majors3 <- filter(students, students$Major==input$Major3) %>%
      select(Major, Median)
    
    paycomp.selected <- rbind(pay.majors1, pay.majors2, pay.majors3)
    
    paycomp.plot <- plot_ly(x = paycomp.selected$Major, name = 'Major', y = paycomp.selected$Median,
                            name = 'Median Earnings', type = 'bar', orietation = 'h')
  })
  
  output$emp.comp <- renderPlotly({
    
    emp.majors1 <- filter(students, students$Major==input$Major1) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)
    
    emp.majors2 <- filter(students, students$Major==input$Major2) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)
    
    emp.majors3 <- filter(students, students$Major==input$Major3) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)
    list2 <- rbind(emp.majors1, emp.majors2, emp.majors3)
    
    empcomp.plot <- plot_ly(list2, x = ~Major, y = ~College_jobs, type = 'bar', name = 'College Jobs') %>%
      add_trace(y = ~Non_college_jobs, name = 'Non-College Jobs') %>%
      add_trace(y = ~Low_wage_jobs, name = 'Low Wage Jobs') %>%
      add_trace(y = ~Unemployed, name = 'Unemployed') %>%
      layout(yaxis = list(title = 'Number'), barmode = 'stack')
  })
})