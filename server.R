##setwd("~/Desktop/INFO_201_Assignments/2seven")
library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)

recent_grads <- fread("college-majors/recent-grads.csv", stringsAsFactors = F)


compare.gender <- shinyServer(function(input, output) {
  
  ##Renders the gender distirbution bar chart, $inputmajors.selected is the major chosen by user.
  output$gender.comp <- renderPlot({
    for(i in input$majors.selected) {
    gender.majors <- filter_(recent_grads, Major==i) %>%
      select(Men, Women, Major)
    gendercomp.selected <- rbind(gender.majors)
    }
    gendercomp.plot <- plot_ly(gendercomp.selected, x = ~Major, y = ~Women,
                               type = 'bar', name = 'Women' ) %>%
                               add_trace(y = ~Men, name = 'Men') %>%
                               layout(yaxis = list(title = 'Gender Distribution for Selected Majors'),
                               barmode = 'group')
  })
  
  ##Renders plot comparing the median pay of selected majors
  output$pay.comp <- renderPlot({
    
    for(i in input$majors.selected) {
    pay.majors <- filter_(recent_grads, Major==i) %>%
      select(Major, Median)
    paycomp.selected <- rbind(pay.majors)
    }
    paycomp.plot <- plot_ly(x = paycomp.selected$Major, name = 'Major', y = paycomp.selected$Median,
                            name = 'Median Earnings', type = 'bar')
  })
  
})


