library(shiny)
library(plotly)
shinyUI(navbarPage('Major Helper',
                   
                   tabPanel('Major Stats',
                            titlePanel('Show Stats For My Major'),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput('major', label = 'Select a major please', list(
                                  
                                )
                                            )
                              )
                            ))
                   ))