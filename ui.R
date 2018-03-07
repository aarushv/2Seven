library(shiny)
library(plotly)
library(dplyr)

#Creates list of majors for inputting
major_list <- read.csv("./college-majors/majors-list.csv")
major_list <- unique(major_list$Major)
major_list <- sort(major_list, decreasing = FALSE)

shinyUI(
  navbarPage( 'Major Stats',
    tabPanel('Major Data',
             titlePanel('Data For The Major'),
             
             sidebarLayout(
               
               sidebarPanel(
                 #uiOutput("major_list"),
                 selectInput("major_select","",major_list),
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