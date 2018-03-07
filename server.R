library(dplyr)
library(data.table)
library(plotly)

recent_grads <- fread("college-majors/recent-grads.csv", stringsAsFactors = F)

compare.server <- shinyServer(function(input, output) {
  
  ##Renders the gender distirbution bar chart, $inputmajors.selected is the major chosen by user.
  output$gender.comp <- renderPlotly({
    
    gender.majors1 <- filter(recent_grads, recent_grads$Major==input$Major1) %>%
      select(Men, Women, Major)
    gender.majors2 <- filter(recent_grads, recent_grads$Major==input$Major2) %>%
      select(Men, Women, Major)
    gender.majors3 <- filter(recent_grads, recent_grads$Major==input$Major3) %>%
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

    pay.majors1 <- filter(recent_grads, recent_grads$Major==input$Major1) %>%
      select(Major, Median)
    pay.majors2 <- filter(recent_grads, recent_grads$Major==input$Major2) %>%
      select(Major, Median)
    pay.majors3 <- filter(recent_grads, recent_grads$Major==input$Major3) %>%
      select(Major, Median)
    
    paycomp.selected <- rbind(pay.majors1, pay.majors2, pay.majors3)

    paycomp.plot <- plot_ly(x = paycomp.selected$Major, name = 'Major', y = paycomp.selected$Median,
                            name = 'Median Earnings', type = 'bar', orietation = 'h')
  })
  
  output$emp.comp <- renderPlotly({
  
    emp.majors1 <- filter(recent_grads, recent_grads$Major==input$Major1) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)

    emp.majors2 <- filter(recent_grads, recent_grads$Major==input$Major2) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)
    
    emp.majors3 <- filter(recent_grads, recent_grads$Major==input$Major3) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs)
    list2 <- rbind(emp.majors1, emp.majors2, emp.majors3)
    
    empcomp.plot <- plot_ly(list2, x = ~Major, y = ~College_jobs, type = 'bar', name = 'College Jobs') %>%
      add_trace(y = ~Non_college_jobs, name = 'Non-College Jobs') %>%
      add_trace(y = ~Low_wage_jobs, name = 'Low Wage Jobs') %>%
      add_trace(y = ~Unemployed, name = 'Unemployed') %>%
      layout(yaxis = list(title = 'Number'), barmode = 'stack')
  })
})


