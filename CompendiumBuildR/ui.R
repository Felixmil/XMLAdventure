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
                                      'Select a category of element to add',
                                      choices = c('Item'='item','More comming...'=NA),
                                      selected= NA)),
                   column(6, 
                          conditionalPanel(condition = "input.category == 'item'",
                                           selectInput('type', 
                                                       'Select a type of Item',
                                                       choices=c('Melee Weapon'= 'M', 
                                                                 'Ranged Weapon' = 'R',
                                                                 'Armor' = 'A')
                                           )
                          )
                   )
               ),
               hr(),

               div(id='form', 
                   uiOutput('fields'))
        ),
        column(5,
               actionButton('reset','Reset', icon = icon('refresh')),
               hr(),
               verbatimTextOutput('finalxml'),
               actionButton("copyButton", "Copy!",icon = icon('copy'))
        )
    )
)
)
