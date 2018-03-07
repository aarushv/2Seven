
library(shiny)
library(plotly)
major_list <- read.csv("./college-majors/majors-list.csv")
major_list <- unique(major_list$Major)
major_list <- sort(major_list, decreasing = FALSE)


ui <- fluidPage(
  navbarPage("Major Comparison Stats",
  tabPanel("Major Comparison Data",
  titlePanel("Compare Majors!"),
                       
    sidebarLayout(
      sidebarPanel(
        selectInput("Major1", label = "Choose 1st Major to Compare: ", choices = major_list),
        selectInput("Major2", label = "Choose 2nd Major to Compare: ", choices = major_list),
        selectInput("Major3", label = "Choose 3rd Major to Compare: ", choices = major_list),
        radioButtons('displayOption', 'Choose Display: ', c("Gender" = "gender.comp", "Pay" = "pay.comp",
        "Employment" = "emp.comp"))
      ),
      mainPanel(
        plotlyOutput("gender.comp"),
        plotlyOutput("pay.comp"),
        plotlyOutput("emp.comp")
      )
    )
  )
  )
)