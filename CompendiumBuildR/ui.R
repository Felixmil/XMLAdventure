library(shiny)
library(shinyjs)
library(shinythemes)
library(rclipboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("superhero"),
    rclipboardSetup(),
    useShinyjs(),
    # Application title
    titlePanel("CompendiumBuildR"),
    fluidRow(
        column(8,
               fluidRow(
                   column(6,
                          selectInput('category',
                                      'Select a category',
                                      choices = c('Item' = 'item','Monster'='monster',''),
                                      selected = '')),
                   column(6, 
                          div(id='form', 
                                 uiOutput('subCat'))
                   )),
               hr(),
               div(id='form', 
                   uiOutput('fields'))
        ),
        column(4,
               fluidRow(
                   column(4,actionButton('reset','Reset', icon = icon('refresh'))),
                   column(8, column(6,uiOutput('copyButtonElement')),align='center')),
               hr(),
               verbatimTextOutput('finalxml'),
               textOutput('properties')
        )
    )
)
)
