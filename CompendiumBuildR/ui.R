library(shiny)
library(shinyjs)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
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
               actionButton('reset','Reset', icon = icon('refresh')),
               hr(),
               verbatimTextOutput('finalxml'),
               fluidRow(
                   column(6,actionButton("copyButtonAll", "Copy All",icon = icon('copy'))),
                   column(6,uiOutput('copyButtonElement')),align='center'),
               textOutput('properties')
        )
    )
)
)
