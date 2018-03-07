##setwd("~/Desktop/INFO_201_Assignments/2seven")
library(dplyr)
library(data.table)
library(plotly)

#creates dataframe with just list of majors
major_list <- students %>% 
  select(Major)

major_list <- list(major_list[[1]])

recent_grads <- fread("college-majors/recent-grads.csv", stringsAsFactors = F)


compare.server <- shinyServer(function(input, output) {
  
  ##Renders the gender distirbution bar chart, $inputmajors.selected is the major chosen by user.
  output$gender.comp <- renderPlotly({
    
    gender.majors1 <- filter(recent_grads, recent_grad$Major==input$Major1) %>%
      select(Men, Women, Major)
    gender.majors2 <- filter(recent_grads, recent_grad$Major==input$Major2) %>%
      select(Men, Women, Major)
    gender.majors1 <- filter(recent_grads, recent_grad$Major==input$Major3) %>%
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
    
    for(i in input$majors.selected) {
    pay.majors <- filter_(recent_grads, Major==i) %>%
      select(Major, Median)
    paycomp.selected <- rbind(pay.majors)
    }
    paycomp.plot <- plot_ly(x = paycomp.selected$Major, name = 'Major', y = paycomp.selected$Median,
                            name = 'Median Earnings', type = 'bar', orietation = 'h')
  })
  
  output$emp.comp <- renderPlotly({
    
    ##for(i in input$major.selected) {
      
    emp.majors1 <- filter(recent_grads, Major=="ACCOUNTING") %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)
    Total1 <- (emp.majors1$Unemployed + emp.majors1$College_jobs + emp.majors1$Non_college_jobs + emp.majors1$Low_wage_jobs)
    
    emp.majors2 <- filter(recent_grads, Major=="BOTANY") %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)
    
    emp.majors3 <- filter(recent_grads, Major=="ECONOMICS") %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)
    list2 <- rbind(emp.majors1, emp.majors2, emp.majors3)
   ## }
    
    empcomp.plot <- plot_ly(list2, x = ~Major, y = ~College_jobs, type = 'bar', name = 'College Jobs') %>%
      add_trace(y = ~Non_college_jobs, name = 'Non-College Jobs') %>%
      add_trace(y = ~Low_wage_jobs, name = 'Low Wage Jobs') %>%
      add_trace(y = ~Unemployed, name = 'Unemployed') %>%
      layout(yaxis = list(title = 'Number'), barmode = 'stack')
      
    
    
    
    Animals <- c("giraffes", "orangutans", "monkeys")
    SF_Zoo <- c(20, 14, 23)
    LA_Zoo <- c(12, 18, 29)
    data <- data.frame(Animals, SF_Zoo, LA_Zoo)
    
    p <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
      add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack')
    
  })
  
})


