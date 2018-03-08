library(dplyr)
library(data.table)
library(plotly)

recent_grads <- fread("college-majors/recent-grads.csv", stringsAsFactors = F)

compare.server <- shinyServer(function(input, output) {
  ##$Major==input$Major1
  ##Renders the gender distirbution bar chart, $inputmajors.selected is the major chosen by user.
  output$gender.comp <- renderPlotly({
    recent_grads <- mutate(recent_grads, ShareMen = 1 - ShareWomen)
    
    gender.majors1 <- filter(recent_grads, recent_grads$Major==input$Major1) %>%
      select(ShareWomen, ShareMen, Major)
    gender.majors2 <- filter(recent_grads, recent_grads$Major==input$Major2) %>%
      select(ShareWomen, ShareMen, Major)
    gender.majors3 <- filter(recent_grads, recent_grads$Major==input$Major3) %>%
      select(ShareWomen, ShareMen, Major)
    
    gendercomp.selected <- rbind(gender.majors1, gender.majors2, gender.majors3)
    
    gendercomp.plot <- plot_ly(gendercomp.selected, x = ~Major, y = ~ShareWomen*100,
                               type = 'bar', name = 'Women' ) %>%
                               add_trace(y = ~ShareMen*100, name = 'Men') %>%
                               layout(title = 'Gender Distribution for Selected Majors',
                                      yaxis = list(title = 'Percentages'), barmode = 'group')
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

    paycomp.plot <- plot_ly(paycomp.selected, x = paycomp.selected$Major, name = 'Major', y = paycomp.selected$Median,
                            name = 'Median Earnings', type = 'bar') %>%
        layout(title = 'Earrning Comparison For Selected Majors', 
               yaxis = list(title = 'Earrnings in $'), 
               xaxis = list(title = 'Major'))
    
  })
  
  output$emp.comp <- renderPlotly({
    
    recent_grads <- mutate(recent_grads, TotalJobForce = Unemployed + Non_college_jobs + College_jobs + Low_wage_jobs)
    
    emp.majors1 <- filter(recent_grads, recent_grads$Major==input$Major1) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs, TotalJobForce)

    emp.majors2 <- filter(recent_grads, recent_grads$Major==input$Major2) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs, TotalJobForce)
    
    emp.majors3 <- filter(recent_grads, recent_grads$Major==input$Major3) %>%
      select(Major, Unemployed, College_jobs, Non_college_jobs, Low_wage_jobs, TotalJobForce)
    list2 <- rbind(emp.majors1, emp.majors2, emp.majors3)
    
    empcomp.plot <- plot_ly(list2, x = ~Major, y = ~College_jobs/TotalJobForce*100, type = 'bar', name = 'College Jobs') %>%
      add_trace(y = ~Non_college_jobs/TotalJobForce*100, name = 'Non-College Jobs') %>%
      add_trace(y = ~Low_wage_jobs/TotalJobForce*100, name = 'Low Wage Jobs') %>%
      add_trace(y = ~Unemployed/TotalJobForce*100, name = 'Unemployed') %>%
      layout(title = 'Employment Comparison For Selected Majors', 
             yaxis = list(title = 'Percentages'),
                          barmode = 'stack')
  })
})