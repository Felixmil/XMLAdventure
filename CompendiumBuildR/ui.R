library(shiny)
library(shinyjs)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("CompendiumBuildR"),
    fluidRow(
        column(7,
               fluidRow(
                   column(6,
                          selectInput('category',
                                      'Select a category',
                                      choices = c('Item' = 'item',''),
                                      selected = '')),
                   column(6, 
                          conditionalPanel(condition = "input.category == 'item'",
                                           selectInput('type', 
                                                       'Select a type',
                                                       choices=c('medium armor' = 'MA',
                                                                 'heavy armor' = 'HA',
                                                                 'shield' = 'S',
                                                                 'melee weapon' = 'R',
                                                                 'ranged weapon' = 'M',
                                                                 'ammunition' = 'A',
                                                                 'rod' = 'RD',
                                                                 'staff' = 'ST',
                                                                 'wand' = 'WD',
                                                                 'ring' = 'RG',
                                                                 'potion' = 'P',
                                                                 'scroll' = 'SC',
                                                                 'wondrous item' = 'W',
                                                                 'adventuring gear' = 'G',
                                                                 'money' = '$',
                                                                 ''),
                                                       selected = '')))),
               hr(),
               div(id='form', 
                   uiOutput('fields'))
        ),
        column(5,
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
