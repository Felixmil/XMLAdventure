library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("CompendiumBuildR"),
    
    fluidRow(
        column(7,
               "Select",
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
                                                                 'Ranged Weapon' = 'R')
                                           )
                          )
                   )
               ),
               hr(),
               actionButton('del','delete'),
               div(id='form', uiOutput('fields'))
        ),
        column(5,
               "XML",
               verbatimTextOutput('finalxml'),
               actionButton("copyButton", "Copy!",icon = icon('copy'))
        )
    )
)
)
