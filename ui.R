library(shiny)
library(plotly)
shinyUI(
  navbarPage( 'Major Stats',
    tabPanel('Major Data',
             titlePanel('Data For The Major'),
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput('major_select',
                             label = "Choose Major: ",
                             choices = c("Computer Science",
                                         "Electrical Engineering",
                                         "Informatics")
                 ),
                 radioButtons(
                   'displayOption', 'Choose Display: ',
                   c("All" = "all",
                     "Gender" = "men_women",
                     "Pay" = "pay",
                     "Employment" = "employed",
                     "Department" = "deparment_major")
                 )
               ),
               mainPanel(
                 plotlyOutput('men_women')
               )
             )
    )
  )
  
)