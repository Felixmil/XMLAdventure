library(shiny)
library(shinyjs)
library(XML)
library(stringr)

layout <- "data/layout.xml"
# layout <- "CompendiumBuildR/data/layout.xml"


# skills

skills <- c('Acrobatics (Dex)',
            'Animal Handling (Wis)',
            'Arcana (Int)',
            'Athletics (Str)',
            'Deception (Cha)',
            'History (Int)',
            'Insight (Wis)',
            'Intimidation (Cha)',
            'Investigation (Int)',
            'Medicine (Wis)',
            'Nature (Int)',
            'Perception (Wis)',
            'Performance (Cha)',
            'Persuasion (Cha)',
            'Religion (Int)',
            'Sleight of Hand (Dex)',
            'Stealth (Dex)',
            'Survival (Wis)')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    observeEvent(input$reset,  {
        reset('form')
    })
    
    observeEvent(c(input$category,input$type), {
        click('reset')
        
    })
    
    observe({
        fields_r <-  reactive({
            if (input$category == 'NA' & input$type == 'NA') {
                HTML('Select a category and a type')
            } else if (input$category == 'item') {
                reset('form')
                list(
                    textInput('name',
                              'Name',
                              '',
                              width = '100%'),
                    fluidRow(
                        column(10,
                               textInput('detail',
                                         'Details',
                                         '',
                                         width = '100%')),
                        column(2,
                               numericInput('weight',
                                            'Weight',
                                            '')
                        )),
                    fluidRow(
                        column(4,radioButtons('magic',
                                              'Magic Item',
                                              choices = c('Yes'=1,'No'=''),
                                              selected= '')),
                        column(4, 
                               radioButtons('stealth',
                                            'Stealth Disadvantage',
                                            choices = c('Yes' ='YES', 'No' = ''),
                                            selected = '')),
                        column(4,
                               numericInput('strength',
                                            'Minimum Strength',
                                            value='')
                        )
                    ),
                    fluidRow(
                        column(6,
                               numericInput('ac',
                                            'Armor Class',value = '')),
                        column(6,
                               textInput('range',
                                         'Range',
                                         '')
                        )
                    ),
                    
                    fluidRow(
                        column(6,
                               textInput('dmg1',
                                         'Damage 1',
                                         placeholder = 'exemple: 2d6+2',
                                         ''),
                               ''),
                        column(6,
                               textInput('dmg2',
                                         'Damage 2', 
                                         '')),
                        ''),
                    fluidRow(
                        column(6,
                               selectInput('dmgType',
                                           'Damage Type',
                                           choices = c(
                                               'bludgeoning' = 'B',
                                               'piercing' = 'P',
                                               'slashing' = 'S',
                                               ''),
                                           ''
                               )
                        ),
                        column(6,
                               selectizeInput(inputId = 'property',
                                              label='Properties',
                                              choices= c('ammunition' = 'A',
                                                         'finesse' = 'F',
                                                         'heavy' = 'H',
                                                         'light' = 'L',
                                                         'loading' = 'LD',
                                                         'reach' = 'R',
                                                         'special' = 'S',
                                                         'thrown' = 'T',
                                                         'two-handed' = '2H',
                                                         'versatile' = "V"),
                                              multiple = TRUE))),
                    textAreaInput('text','
                                  Description',
                                  '')
                    
                )
            } else if (input$category == 'monster') { 
                reset('form')
                list(
                    textInput('name',
                              'Name',
                              '',
                              width = '100%'),
                    fluidRow(column(6,textInput('type',
                                                'Type',
                                                '')),
                             column(2,
                                    selectInput('cr',
                                                'CR',
                                                choices = c('','00','0','1/2','1/4','1/8', as.character(1:30)),
                                                selected = '')),
                             column(2,
                                    selectInput('size',
                                                'Size',
                                                choices = c('',
                                                            'tiny' = 'T',
                                                            'small' = 'S',
                                                            'medium' = 'M',
                                                            'large' = 'L',
                                                            'huge' = 'H',
                                                            'gargantuan' = 'G'),
                                                selected = '')),
                             column(2,textInput('speed',
                                       'Speed',
                                       value = NULL))),
                    fluidRow(column(3,numericInput('passive','Passive Percept.', value=NULL)
                                    ),
                             column(3,numericInput('init',
                                                   'Initiative bonus',value = '')),
                             column(3,textInput('ac',
                                                'AC (armor type)',
                                                '')),
                             column(3,textInput('hp',
                                                'HP (Dice formula)',
                                                ''))),
                    fluidRow(column(8,
                        wellPanel(
                            fluidRow(
                                column(3,tags$h4('Abilities')), column(6,tags$h4('Score')),column(3, tags$h4('Saving'))
                                ),
                            hr(),
                            fluidRow(column(3, tags$h5('STR')), column(6,numericInput('str',NULL, value=NULL)),column(3, numericInput('str_save',NULL, 0))),
                            fluidRow(column(3, tags$h5('DEX')), column(6,numericInput('dex',NULL, value=NULL)),column(3, numericInput('dex_save',NULL, 0))),
                            fluidRow(column(3, tags$h5('CON')), column(6,numericInput('con',NULL, value=NULL)),column(3, numericInput('con_save',NULL, 0))),
                            fluidRow(column(3, tags$h5('INT')), column(6,numericInput('int',NULL, value=NULL)),column(3, numericInput('int_save',NULL, 0))),
                            fluidRow(column(3, tags$h5('WIS')), column(6,numericInput('wis',NULL, value=NULL)),column(3, numericInput('wis_save',NULL, 0))),
                            fluidRow(column(3, tags$h5('CHA')), column(6,numericInput('cha',NULL, value=NULL)),column(3, numericInput('cha_save',NULL, 0)))
                        )
                    ),
                    column(4,
                           textInput('vulnerable','Dmg Vulnerabilities',value = ''),
                           textInput('resist','Dmg Resistances', value = ''),
                           textInput('immune','Dmg Immunities',value = ''),
                           textInput('conditionImmune','Conditionnal Immunities',value = ''),
                           textInput('senses','Senses',value = ''),
                           textInput('languages','Languages',value = ''))
                ))
            } else {
                reset('form')
                HTML('Not implemented (yet!)')
            }})
        fields_d <- debounce(fields_r, 500)
        output$fields <- renderUI(fields_d())
    }
    )
    
    
    
    
    # observe({
    #     insertUI(selector = "#form",multiple = T,
    #              ui = 
    #                  if (input$category == 'item' & input$type == 'M') {
    #                      list(
    #                          h4('General'),
    #                          textInput('name','Name:',''),
    #                           radioButtons('magic',
    #                                        'Magic Item:',
    #                                        choices = c('Yes'=1,'No'=0),0),
    #                           h4('Damages'),
    #                           textInput('dmg1',
    #                                     'Damage:',placeholder = '2d6+2'))
    #                  } else if (input$category == 'item' & input$type == 'R') {
    #                      list(h4('General'),
    #                           textInput('name','Name:'),
    #                           radioButtons('magic',
    #                                        'Magic Item:',
    #                                        choices = c('Yes'=1,'No'=0), 0),
    #                           h4('Damages'),
    #                           textInput('dmg1',
    #                                     'Damage:',placeholder = '2d6+2',value = ''),
    #                           textInput('range','Range:',value = '',placeholder = '20/60'))
    #                  }
    #              )
    # }
    # )
    
    new <- reactive({
        doc = xmlTreeParse(layout, useInternalNodes = T) 
        
        if(input$category=='NA') {
            doc
        } else {
            # PARSE STRING
            root = xmlRoot(doc)
            masterNode = newXMLNode(input$category, parent=root)
            newXMLNode('name',input$name, parent=masterNode)
            newXMLNode('type',input$type, parent=masterNode)
            newXMLNode('detail',input$detail, parent=masterNode)
            newXMLNode('magic',input$magic, parent=masterNode)
            newXMLNode('weight',input$weight, parent=masterNode)
            newXMLNode('text',input$text, parent=masterNode)
            newXMLNode('ac',input$ac, parent=masterNode)
            newXMLNode('strength',input$strength, parent=masterNode)
            newXMLNode('stealth',input$stealth, parent=masterNode)
            newXMLNode('dmg1',input$dmg1, parent=masterNode)
            newXMLNode('dmg2',input$dmg2, parent=masterNode)
            newXMLNode('dmgType',input$dmgType, parent=masterNode)
            newXMLNode('property',paste(input$property,collapse=','), parent=masterNode)
            newXMLNode('range', input$range, parent=masterNode)
            xmlParse(toString.XMLNode(xmlParse(toString.XMLNode(doc))) %>% 
                         str_remove_all(pattern = '<.*\\/>\\\n') %>%
                         str_remove_all(pattern = '<.*>NA</.*>'))
        }})
    
    
    output$finalxml <- renderPrint(new())
    
    
    observeEvent(input$copyButtonAll, {
        x <- toString.XMLNode(new())
        clipr::write_clip(x)
    })
    
    
    observeEvent(input$copyButtonElement, {
        x <- toString.XMLNode(xmlChildren(xmlRoot(new()))[[input$category]])
        clipr::write_clip(x)
    })
    
    observe({
        if(input$category != 'NA') {
            output$copyButtonElement <- renderUI(actionButton("copyButtonElement", 
                                                              paste("Copy",input$category),
                                                              icon = icon('copy')))
        } else {
            output$copyButtonElement <- renderUI(HTML(''))}
    })
    
    print('test')
    print('test')
    
})
