library(shiny)
library(shinyjs)
library(XML)
library(stringr)
library(shinyWidgets)

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

skills_simple <- str_remove(skills, ' \\(.*\\)')

shinyServer(function(input, output, session) {
    
    
    observeEvent(input$reset,  {
        reset('form')
        reset('fields')
        updateSliderTextInput(session, 'cr',selected = 'NA')
        updateSliderTextInput(session, 'size', selected = 'NA')
        rep(
            {removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[length(inserted)])
            )
            inserted <<- inserted[-length(inserted)]}, length(inserted))
    })
    
    
    observeEvent(c(input$category), {
        click('reset')
    })
    
    observe({
        subCat_r <- reactive({
            if (input$category == 'item') {
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
                                             selected = ''))
            } else {NULL}
        })
        subCat_d <- debounce(subCat_r, 500)
        output$subCat <- renderUI(subCat_d())
    })
    
    observe({
        fields_r <-  reactive({
            if (input$category == 'NA') {
                HTML('Select a category')
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
                
                skill_list_1 <- list()
                for (skill in skills_simple[1:round(length(skills_simple)/2)]) {
                    skill_i <- fluidRow(column(8, tags$h5(skill)),
                                        column(4,numericInput(skill,NULL, value=0)))
                    skill_list_1 <- append(skill_list_1,skill_i$children)
                }
                
                skill_list_2 <- list()
                for (skill in skills_simple[(round(length(skills_simple)/2)+1):length(skills_simple)]) {
                    skill_i <- fluidRow(column(8, tags$h5(skill)),
                                        column(4,numericInput(skill,NULL, value=0)))
                    skill_list_2 <- append(skill_list_2,skill_i$children)
                }
                
                
                list(
                    textInput('name',
                              'Name',value = '',
                              width = '100%'),
                    fluidRow(column(9,textInput('type',
                                                'Type',
                                                value = '',
                                                width = '100%')),
                             column(3,textInput('speed',
                                                'Speed',
                                                value = ''))),
                    
                    sliderTextInput('cr',
                                    'Challenge Rating',
                                    choices = c('NA',
                                                '00',
                                                '0',
                                                '1/2',
                                                '1/4',
                                                '1/8', 
                                                as.character(1:30)),
                                    selected = 'NA',
                                    grid = T, 
                                    hide_min_max = T,
                                    width = '100%'),
                    sliderTextInput('size',
                                    'Size',
                                    choices = c('NA' = 'NA',
                                                'tiny' = 'T',
                                                'small' = 'S',
                                                'medium' = 'M',
                                                'large' = 'L',
                                                'huge' = 'H',
                                                'gargantuan' = 'G'),
                                    selected = 'NA',
                                    grid = T,
                                    hide_min_max = T,
                                    width = '100%'),
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
                                            column(3,tags$h4('Abilities')), 
                                            column(6,tags$h4('Score')),
                                            column(3, tags$h4('Saving'))
                                        ),
                                        hr(),
                                        fluidRow(column(3, tags$h5('STR')), 
                                                 column(6,numericInput('str',NULL, value=NULL)),
                                                 column(3, numericInput('str_save',NULL, 0))),
                                        fluidRow(column(3, tags$h5('DEX')), 
                                                 column(6,numericInput('dex',NULL, value=NULL)),
                                                 column(3, numericInput('dex_save',NULL, 0))),
                                        fluidRow(column(3, tags$h5('CON')), 
                                                 column(6,numericInput('con',NULL, value=NULL)),
                                                 column(3, numericInput('con_save',NULL, 0))),
                                        fluidRow(column(3, tags$h5('INT')), 
                                                 column(6,numericInput('int',NULL, value=NULL)),
                                                 column(3, numericInput('int_save',NULL, 0))),
                                        fluidRow(column(3, tags$h5('WIS')), 
                                                 column(6,numericInput('wis',NULL, value=NULL)),
                                                 column(3, numericInput('wis_save',NULL, 0))),
                                        fluidRow(column(3, tags$h5('CHA')), 
                                                 column(6,numericInput('cha',NULL, value=NULL)),
                                                 column(3, numericInput('cha_save',NULL, 0)))
                                    )
                    ),
                    column(4,
                           textInput('vulnerable','Dmg Vulnerabilities',value = ''),
                           textInput('resist','Dmg Resistances', value = ''),
                           textInput('immune','Dmg Immunities',value = ''),
                           textInput('conditionImmune','Conditionnal Immunities',value = ''),
                           textInput('senses','Senses',value = ''),
                           textInput('languages','Languages',value = '')
                    )
                    ),
                    hr(),
                    fluidRow(tags$h4('Skills'),
                             column(6,skill_list_1),
                             column(6, skill_list_2)),
                    hr(),
                    fluidRow(
                        fluidRow(column(6,tags$h4('Traits & Actions')), column(3, actionButton('insertBtn', '+')),column(3, actionButton('removeBtn', '-'))),
                        fluidRow(column(2,tags$h4('Type')),column(2,tags$h4('Name')),column(2,tags$h4('Attack')), column(6,tags$h4('Description'))),
                        tags$div(id = 'traitsActions')
                    )
                    
                )
            } else {
                reset('form')
                HTML('Not implemented (yet!)')
            }})
        fields_d <- debounce(fields_r, 500)
        output$fields <- renderUI(fields_d())
    }
    )
    
    inserted <- c()
    
    observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        id <- paste0('text',btn)
        insertUI(
            selector = '#traitsActions',
            ## wrap element in a div with id for ease of removal
            ui = tags$div(
                fluidRow(
                    column(2,selectInput(paste0(id,'_type'),'', choices = c('trait', 'action','reaction','legendary'))),
                    column(2, textInput(paste0(id,'_name'),'')),
                    column(2, textInput(paste0(id,'_attack'),'')),
                    column(6, textAreaInput(paste0(id,'_desc'),label = ''))),
                id = id
            )
        )
        inserted <<- c(id, inserted)
    })
    
    observeEvent(input$removeBtn, {
        removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[length(inserted)])
        )
        inserted <<- inserted[-length(inserted)]
    })
    
    
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
            if (input$category == 'item') {
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
            } else if (input$category == 'monster') {
                
                newXMLNode('size', input$size, parent=masterNode)
                newXMLNode('type', input$type, parent=masterNode)
                newXMLNode('speed', input$speed, parent=masterNode)
                newXMLNode('ac', input$ac, parent=masterNode)
                newXMLNode('cr', input$cr, parent=masterNode)
                newXMLNode('str', input$str, parent=masterNode)
                newXMLNode('dex', input$dex, parent=masterNode)
                newXMLNode('con', input$con, parent=masterNode)
                newXMLNode('wis', input$wis, parent=masterNode)
                newXMLNode('int', input$int, parent=masterNode)
                newXMLNode('cha', input$cha, parent=masterNode)
                newXMLNode('passive', input$passive, parent=masterNode)
                newXMLNode('init', input$init, parent=masterNode)
                newXMLNode('hp', input$hp, parent=masterNode)
                newXMLNode('vulnerable', input$vulnerable, parent=masterNode)
                newXMLNode('resist', input$resist, parent=masterNode)
                newXMLNode('immune', input$immune, parent=masterNode)
                newXMLNode('conitionImmune', input$conditionImmune, parent=masterNode)
                newXMLNode('senses', input$senses, parent=masterNode)
                newXMLNode('languages', input$languages, parent=masterNode)
                newXMLNode('size', input$size, parent=masterNode)
                
                # Add saves 
                #TODO Better format of saving modifiers
                newXMLNode('save', paste0('Srength +',input$str_save), parent=masterNode)
                newXMLNode('save', paste0('Dexterity +',input$dex_save), parent=masterNode)
                newXMLNode('save', paste0('Constitution +',input$con_save), parent=masterNode) 
                newXMLNode('save', paste0('Wisdom +',input$wis_save), parent=masterNode)
                newXMLNode('save', paste0('Intelligence +',input$int_save), parent=masterNode)
                newXMLNode('save', paste0('Charism +',input$cha_save), parent=masterNode)
                
                
                
                # Add skills
                # TODO Add + symbol before modifier
                skills_v <- c()
                for (skill in skills_simple) {
                    skills_v <- append(skills_v, paste(skill, input[[skill]]))
                }
                skills_v <- paste(skills_v, collapse = ',')
                skills_v <- skills_v %>% str_remove_all(.,'(,)?[^,]* 0') %>% str_remove(.,'^,')
                newXMLNode('skill', skills_v, parent=masterNode)
                
                # Add Traits & Actions
                if (length(inserted > 0)) {
                    pleasreact <- input$insertBtn
                    pleasreact <- input$removeBtn
                    for (i in 1:length(inserted)) {
                        new_parent = newXMLNode(input[[paste0(inserted[i],'_type')]],'', parent = masterNode)
                        newXMLNode('name',input[[paste0(inserted[i],'_name')]], parent = new_parent)
                        newXMLNode('text',input[[paste0(inserted[i],'_desc')]], parent = new_parent)  
                        newXMLNode('attack',input[[paste0(inserted[i],'_attack')]], parent = new_parent)  
                        
                        
                    }
                    
                    
                }
            }
            
            
            
            # Clean output XML
            xmlParse(toString.XMLNode(xmlParse(toString.XMLNode(doc))) %>% 
                         str_remove_all(pattern = '<.*\\/>\\\n') %>%
                         str_remove_all(pattern = '<.*>NA</.*>') %>%
            str_remove_all(pattern = '<.*>.* \\+0</.*>') %>%
                str_remove_all(pattern= '[:blank:]{5,}'))
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
})
