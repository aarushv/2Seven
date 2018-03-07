library(shiny)
library(plotly)
library(rsconnect)
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
                   c("Gender" = "men_women",
                     "Pay" = "pay",
                     "Employment" = "employed",
                     "Department" = "department"
                     )
                 )
               ),
               mainPanel(
                 plotlyOutput("graph")
               )
             )
    ),
    tabPanel("Major Comparison Data",
             titlePanel("Compare Majors!"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("Major1", label = "Choose 1st Major to Compare: ", choices = major_list),
                 selectInput("Major2", label = "Choose 2nd Major to Compare: ", choices = major_list),
                 selectInput("Major3", label = "Choose 3rd Major to Compare: ", choices = major_list)
               ),
               mainPanel(
                 plotlyOutput("gender.comp"),
                 plotlyOutput("pay.comp"),
                 plotlyOutput("emp.comp")
               )
             )
    ), tabPanel("Regional Comparison Data",
                titlePanel("Compare Regions"),
                mainPanel(plotlyOutput("states")))
  )
)