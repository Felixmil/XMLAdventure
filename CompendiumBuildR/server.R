library(shiny)
library(shinyjs)
library(XML)
library(stringr)
library(shinyWidgets)
library(rclipboard)
library(tools)


# Abilities

abilities <- c('Force',
               'Dexterity',
               'Constitution',
               'Intelligence',
               'Wisdom',
               'Charisma')

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


# Modifiers

modifiers <- list()

modifiers$category <- c('Skills','Saving Throws','Ability Modifiers','Ability Scores','Bonus')
modifiers$Skills <- skills_simple
modifiers$SavingThrows <- c(paste(abilities,'Save'))
modifiers$AbilityModifiers <- c(paste(abilities,'Modifier'))
modifiers$AbilityScores <- c(paste(abilities,'Score'))
modifiers$Bonus <- c('Proficiency Bonus',
                       'Weapon Attacks',
                       'Weapon Damage',
                       'Melee Damage',
                       'Melee Attacks',
                       'Ranged Attacks',
                       'Ranged Damage',
                       'Spell Attack',
                       'Spell DC',
                       'Hit Points',
                       'Armor Class',
                       'Saving Throws',
                       'Initiative',
                       'Speed',
                       'Passive Wisdom')

modifiers$values <- c(c(-100:-1),'+ Proficiency Bonus',paste0('+',c(1:100)))

shinyServer(function(input, output, session) {
    
    # UI Resets
    observeEvent(input$reset,  {
        reset('form')
        reset('fields')
        updateSliderTextInput(session, 'cr',selected = 'NA')
        updateSliderTextInput(session, 'size', selected = 'NA')
        
        for (i in length(inserted):1) {
            removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[i])
            )
            
        }
        
        inserted <<- c()
        
        for (i in length(insertedMod):1) {
            removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedMod[i])
            )
        }
        insertedMod <<- c()
    })
    
    observeEvent(c(input$category), {
        click('reset')
    })
    
    
    # UI item type
    observe({
        subCat_r <- reactive({
            if (input$category == 'item') {
                selectInput('type', 
                            'Select a type',
                            choices=c('medium armor' = 'MA',
                                      'heavy armor' = 'HA',
                                      'shield' = 'S',
                                      'melee weapon' = 'M',
                                      'ranged weapon' = 'R',
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
                            selected = '')
            } else {NULL}
        })
        subCat_d <- debounce(subCat_r, 500)
        output$subCat <- renderUI(subCat_d())
    })
    
    # UI form
    observe({
        fields_r <-  reactive({
            if (input$category == '') {
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
                        column(4,
                               div('Magic Item', align='center'),
                               div(
                                   materialSwitch('magic',
                                                  '',
                                                  status = 'info'),
                                   style='margin-left:40%;margin-top:10px;')
                        ),
                        column(4,
                               div('Stealth Disadvantage', align='center'),
                               div(
                                   materialSwitch('stealth',
                                                  '',
                                                  status = 'info'),
                                   style='margin-left:40%;margin-top:10px;')
                        ),
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
                                  '',width = '195%'),
                    modifierUI
                    
                )
                
                
            } else if (input$category == 'monster') { 
                reset('form')
                
                skill_list_1 <- list()
                for (skill in skills_simple[1:round(length(skills_simple)/2)]) {
                    skill_i <- fluidRow(column(7, tags$h5(skill)),
                                        column(5,numericInput(skill,NULL, value=0)))
                    skill_list_1 <- append(skill_list_1,skill_i$children)
                }
                
                skill_list_2 <- list()
                for (skill in skills_simple[(round(length(skills_simple)/2)+1):length(skills_simple)]) {
                    skill_i <- fluidRow(column(7, tags$h5(skill)),
                                        column(5,numericInput(skill,NULL, value=0)))
                    skill_list_2 <- append(skill_list_2,skill_i$children)
                }
                
                
                list(wellPanel(
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
                    fluidRow(align='center',
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
                                             width = '75%'),
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
                                             width = '75%'))),
                    hr(),
                    wellPanel(
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
                                        wellPanel(style = 'border-color: #828088',
                                                  fluidRow(
                                                      column(3,tags$h4('Abilities')), 
                                                      column(6,tags$h4('Score')),
                                                      column(3, tags$h4('Saving'))
                                                  ),
                                                  hr(),
                                                  fluidRow(column(3, tags$h5('STR')), 
                                                           column(5,numericInput('str',NULL, value=10)),
                                                           column(4, numericInput('str_save',NULL, 0))),
                                                  fluidRow(column(3, tags$h5('DEX')), 
                                                           column(5,numericInput('dex',NULL, value=10)),
                                                           column(4, numericInput('dex_save',NULL, 0))),
                                                  fluidRow(column(3, tags$h5('CON')), 
                                                           column(5,numericInput('con',NULL, value=10)),
                                                           column(4, numericInput('con_save',NULL, 0))),
                                                  fluidRow(column(3, tags$h5('INT')), 
                                                           column(5,numericInput('int',NULL, value=10)),
                                                           column(4, numericInput('int_save',NULL, 0))),
                                                  fluidRow(column(3, tags$h5('WIS')), 
                                                           column(5,numericInput('wis',NULL, value=10)),
                                                           column(4, numericInput('wis_save',NULL, 0))),
                                                  fluidRow(column(3, tags$h5('CHA')), 
                                                           column(5,numericInput('cha',NULL, value=10)),
                                                           column(4, numericInput('cha_save',NULL, 0)))
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
                        )),
                    hr(),
                    
                    wellPanel(
                        fluidRow(column(12,tags$h4('Skills'))),
                        fluidRow(column(6,skill_list_1),
                                 column(6, skill_list_2),
                                 ' '
                        )
                    ),
                    hr(),
                    wellPanel(
                        fluidRow(column(4,tags$h4('Traits & Actions')), 
                                 column(8, actionButton('insertBtn', 
                                                        'Add Section', 
                                                        icon = icon('plus-circle'), 
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton('removeBtn', 
                                                     'Remove Last', 
                                                     icon = icon('minus-circle'),  
                                                     style="color: #000; background-color: #e47c7c; border-color: #c53d3d"), align='right')),
                        br(),
                        br(),
                        tags$div(id = 'traitsactions')
                    )
                    
                )
                
            } else if (input$category == 'spell' ){
                
                reset('form')
                
                list(wellPanel(
                    textInput('name',
                              'Name',
                              value = '',
                              width = '100%'),
                    fluidRow(
                        column(6,
                               selectInput('school',
                                           'School',
                                           choices = c(
                                               'abjuration' = 'A',
                                               'conjuration' = 'C',
                                               'divination' = 'D',
                                               'enchantment' = 'EN',
                                               'evocation' = 'EV',
                                               'illusion' = 'I',
                                               'necromancy' = 'N',
                                               'transmutation' = 'T',
                                               ''),
                                           selected = '')
                        ),
                        column(3, 
                               numericInput('level',
                                            label = 'Level',
                                            NULL, 
                                            min = 0,
                                            step = 1)
                        ),
                        column(3, 
                               div(materialSwitch(inputId = "ritual", 
                                                  label = "Ritual", 
                                                  status = "info"), 
                                   style='padding-top: 35px;'))
                    )),
                    hr(),
                    wellPanel(
                        fluidRow(
                            column(4,
                                   textInput('time','Casting Time', value = '')),
                            column(4,
                                   textInput('range','Range',value = '')),
                            column(4, textInput('duration','Duration',value=''))),
                        fluidRow(
                            column(6,
                                   textInput('roll','Roll','')),
                            column(6,
                                   textInput('classes','Classes','',placeholder = 'ABC, ABC, ...'))
                        ),
                        textInput('components', 'Components',value = '',width = '100%')
                    ),
                    hr(),
                    wellPanel(
                        fluidRow(column(4,
                                        tags$h4('Spell Descriptions')), 
                                 column(8, 
                                        actionButton('insertBtn', 
                                                     'Add description', 
                                                     icon = icon('plus-circle'), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton('removeBtn', 
                                                     'Remove Last', 
                                                     icon = icon('minus-circle'),  
                                                     style="color: #000; background-color: #e47c7c; border-color: #c53d3d"), 
                                        align='right')),
                        br(),
                        tags$div(id = 'descriptionblocks')
                    )
                )
            }
            else if (input$category == 'background') {
                
                reset('form')
                
                skill_list_1 <- list()
                for (skill in skills_simple[1:round(length(skills_simple)/2)]) {
                    skill_i <- fluidRow(column(9, skill),
                                        column(3,materialSwitch(str_remove_all(skill,' '),
                                                                NULL, 
                                                                value=FALSE, 
                                                                status = "info")))
                    skill_list_1 <- append(skill_list_1,skill_i$children)
                }
                
                skill_list_2 <- list()
                for (skill in skills_simple[(round(length(skills_simple)/2)+1):length(skills_simple)]) {
                    skill_i <- fluidRow(column(9, skill),
                                        column(3,materialSwitch(str_remove_all(skill,' '),
                                                                NULL, 
                                                                value=FALSE, 
                                                                status = "info")))
                    skill_list_2 <- append(skill_list_2,skill_i$children)
                }
                
                ability_list <- list()
                for (ability in abilities) {
                    ability_i <- fluidRow(column(7, ability),
                                          column(5, materialSwitch(ability, 
                                                                   NULL, 
                                                                   value = FALSE, 
                                                                   status = "info"))
                    )
                    ability_list <- append(ability_list, ability_i$children)
                }
                
                list(
                    textInput('name',
                              'Name',
                              value = '',
                              width = '100%'),
                    wellPanel(
                        tags$h4('Proficiencies'),
                        fluidRow(column(8,
                                        tags$h5("Skills", align='center'),
                                        hr(),
                                        fluidRow(
                                            column(6,
                                                   skill_list_1),
                                            column(6, '',
                                                   skill_list_2))),
                                 column(4, 
                                        tags$h5("Saving Throws", align='center'),
                                        hr(),
                                        fluidRow(renderUI(HTML('<br><br>'))),
                                        fluidRow(ability_list),
                                        fluidRow(renderUI(HTML('')))))
                        
                    ),
                    hr(),
                    wellPanel(
                        fluidRow(column(4,
                                        tags$h4('Traits')), 
                                 column(8, 
                                        actionButton('insertBtn', 
                                                     'Add Trait', 
                                                     icon = icon('plus-circle'), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton('removeBtn', 
                                                     'Remove Last', 
                                                     icon = icon('minus-circle'),  
                                                     style="color: #000; background-color: #e47c7c; border-color: #c53d3d"), 
                                        align='right')),
                        br(),
                        tags$div(id = 'backgroundtraitsblocks')),
                    br(),
                    renderUI(modifierUI)
                    
                )
                
            }
            else {
                reset('form')
                HTML('Not implemented (yet!)')
            }})
        fields_d <- debounce(fields_r, 500)
        output$fields <- renderUI(fields_d())
    }
    )
    
    # Add supplementary description blocks to XML
    
    inserted <<- c()
    
    observeEvent(input$insertBtn, {
        
        
        if (input$category == 'monster') {
            btn <- input$insertBtn
            id <- paste0('traitsactionsblock',btn)
            insertUI(
                selector = '#traitsactions',
                ## wrap element in a div with id for ease of removal
                ui = tags$div(
                    wellPanel(style = 'border-color: #828088',
                              fluidRow(
                                  column(12, 
                                         textInput(paste0(id,'_name'),'Name'))),
                              fluidRow(
                                  column(4, 
                                         selectInput(paste0(id,'_type'),
                                                     'Type', 
                                                     choices = c('trait', 
                                                                 'action',
                                                                 'reaction',
                                                                 'legendary')),
                                         textInput(paste0(id,'_attack'),'Attack')),
                                  column(8, 
                                         textAreaInput(paste0(id,'_desc'),
                                                       label = 'Description',
                                                       height = '200px'))),
                              id = id)
                )
            )
            inserted <<- c(id, inserted)
        }
        else if (input$category == 'spell'){
            btn <- input$insertBtn
            id <- paste0('descriptionblocks',btn)
            insertUI(
                selector = '#descriptionblocks',
                ## wrap element in a div with id for ease of removal
                ui = tags$div(
                    wellPanel(style = 'border-color: #828088',
                              textInput('notUsedInput','',''),
                              textAreaInput(paste0(id,'_text'),
                                            label = '',
                                            height = '200px'),
                              id = id)
                )
            )
            
            inserted <<- c(id, inserted)
        } else if (input$category == 'background'){
            btn <- input$insertBtn
            id <- paste0('addedblock',btn)
            insertUI(
                selector = '#backgroundtraitsblocks',
                ## wrap element in a div with id for ease of removal
                ui = tags$div(
                    wellPanel(style = 'border-color: #828088',
                              textInput(paste0(id,'_name'),
                                        'Name',
                                        ''),
                              textAreaInput(paste0(id,'_text'),
                                            label = '',
                                            height = '200px')),
                    id = id)
            )
            
            inserted <<- c(id, inserted)
        }
        
    })
    
    
    
    
    # Add Modifiers interface
    
    modifierUI <- list(
        wellPanel(
            fluidRow(
                column(4,
                       tags$h4('Modifiers')),
                column(8,
                       actionButton('insertModBtn', 
                                    'Add Section', 
                                    icon = icon('plus-circle'), 
                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       actionButton('removeModBtn', 
                                    'Remove Last', 
                                    icon = icon('minus-circle'),  
                                    style="color: #000; background-color: #e47c7c; border-color: #c53d3d"), align='right'),
                br(),
                br(),
                br(),
                tags$div(id = 'modifiers'))
            
            
        )
    )
    
    insertedMod <<- c()
    
    observeEvent(input$insertModBtn, {
        
        btn <- input$insertModBtn
        id <- paste0('addedMod',btn)
        insertUI(
            selector = '#modifiers',
            ui = tags$div(
                wellPanel(style = 'border-color: #828088',
                          fluidRow(
                              column(3, selectInput(paste0(id,'_category'),
                                                    'category',
                                                    choices = modifiers$category,
                                                    selected='Skills')),
                              # textInput('test','test',value= input[[paste0(id,'_category')]]),
                              column(4, uiOutput(paste0(id,'_targetui'))
                                     # selectInput(paste0(id,'_target'),
                                     #                'Target',
                                     #                choices = c(modifiers[['Skills']],''),
                                     #                selected=''))
                              ),
                              column(5, selectInput(paste0(id,'_value'),
                                                    'Value',
                                                    modifiers$values,
                                                    selected = '+ Proficiency Bonus'))
                          )),
                id = id)
        )
        
        insertedMod <<- c(id, insertedMod)
        
        
        output[[paste0(id,'_targetui')]] <- renderUI({
            selectInput(inputId = paste0(id,'_target'),
                        '',
                        choices= modifiers[[str_remove(input[[paste0(id,'_category')]],' ')]]
            )
        })
        
        
    }
    
    )
    
    
    
    # observeEvent(
    #     c(input$addedMod1_category,
    #       input$addedMod2_category,
    #       input$addedMod3_category,
    #       input$addedMod4_category,
    #       input$addedMod5_category,
    #       input$addedMod6_category,
    #       input$addedMod7_category,
    #       input$addedMod7_category,
    #       input$addedMod8_category,
    #       input$addedMod9_category,
    #       input$addedMod10_category)
    #     ,{
    #         # for (idtest in insertedMod) {
    #             # output[[paste0(idtest,'_targetui')]] <- renderUI({
    #             #     selectInput(inputId = paste0(idtest,'_target'),
    #             #                 '',
    #             #                 choices= modifiers[[str_remove(input[[paste0(idtest,'_category')]],' ')]]
    #             #     )
    #             # })
    #         #     
    #         # }
    #         
    # 
    # for (id in insertedMod) {
    #     if(input[[paste0(id,'_target')]] == ''){
    #         updateSelectInput(session,
    #                           inputId = paste0(id,'_target'),
    #                           choices= modifiers[[str_remove(input[[paste0(id,'_category')]],' ')]],
    #                           selected = ''
    #                               )
    #     }
    # }
    #     }
    # )
    
    
    
    observeEvent(input$removeBtn, {
        removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[1])
        )
        inserted <<- inserted[-1]
    })
    
    observeEvent(input$removeModBtn, {
        removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedMod[1])
        )
        insertedMod <<- insertedMod[-1]
    })
    
    # # Text box add and delete for Spell descriptions
    # 
    # inserted <<- c()
    # 
    # observeEvent(input$insertBtn, {
    #     btn <- input$insertBtn
    #     id <- paste0('desc',btn)
    
    # })
    
    
    
    
    new <- reactive({
        input$insertBtn
        input$removeBtn
        input$insertModBtn
        input$removeModBtn
        if(input$category !='') {
            # PARSE STRING
            masterNode = newXMLNode(input$category)
            newXMLNode('name',toTitleCase(input$name), parent=masterNode)
            newXMLNode('text',input$source, parent=masterNode)
            
            if (input$category == 'item') {
                
                newXMLNode('type',input$type, parent=masterNode)
                newXMLNode('detail',tolower(input$detail), parent=masterNode)
                newXMLNode('magic',
                           if (input$magic == TRUE) {'YES'},
                           parent=masterNode)
                newXMLNode('weight',input$weight, parent=masterNode)
                newXMLNode('text',input$text, parent=masterNode)
                newXMLNode('ac',input$ac, parent=masterNode)
                newXMLNode('strength', input$strength, parent=masterNode)
                newXMLNode('stealth',
                           if(input$stealth == TRUE) {'YES'},
                           parent=masterNode)
                newXMLNode('dmg1',input$dmg1, parent=masterNode)
                newXMLNode('dmg2',input$dmg2, parent=masterNode)
                newXMLNode('dmgType',input$dmgType, parent=masterNode)
                newXMLNode('property',paste(input$property,collapse=','), parent=masterNode)
                newXMLNode('range', input$range, parent=masterNode)
                
                if (length(insertedMod > 0)) {
                    for (mod in insertedMod) {
                        new <- newXMLNode('modifier',
                                          paste(input[[paste0(mod,'_target')]], 
                                                input[[paste0(mod,'_value')]]),
                                          parent=masterNode)
                        xmlAttrs(new) <- c('category' = input[[paste0(mod,'_category')]])
                    }
                }
                
                
            } else if (input$category == 'monster') {
                
                newXMLNode('size', input$size, parent=masterNode)
                newXMLNode('type', tolower(input$type), parent=masterNode)
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
                newXMLNode('conditionImmune', input$conditionImmune, parent=masterNode)
                newXMLNode('senses', input$senses, parent=masterNode)
                newXMLNode('languages', input$languages, parent=masterNode)
                
                # Add saves 
                #TODO Better format of saving modifiers
                newXMLNode('save', paste0('Strength +',input$str_save), parent=masterNode)
                newXMLNode('save', paste0('Dexterity +',input$dex_save), parent=masterNode)
                newXMLNode('save', paste0('Constitution +',input$con_save), parent=masterNode) 
                newXMLNode('save', paste0('Wisdom +',input$wis_save), parent=masterNode)
                newXMLNode('save', paste0('Intelligence +',input$int_save), parent=masterNode)
                newXMLNode('save', paste0('Charisma +',input$cha_save), parent=masterNode)
                
                
                
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
                    for (i in length(inserted):1) {
                        addChildren(masterNode, 
                                    newXMLNode(input[[paste0(inserted[i],'_type')]],
                                               newXMLNode('name',input[[paste0(inserted[i],'_name')]]), 
                                               newXMLNode('text',input[[paste0(inserted[i],'_desc')]]),
                                               newXMLNode('attack',input[[paste0(inserted[i],'_attack')]])
                                    )
                        )
                    }
                }
            } else if (input$category == 'spell') {
                newXMLNode('level', input$level, parent=masterNode)
                newXMLNode('school', input$school, parent=masterNode)
                newXMLNode('ritual', if (input$ritual == T) {'YES'} else {''}, parent=masterNode)
                newXMLNode('time', input$time, parent=masterNode)
                newXMLNode('range', input$range, parent=masterNode)
                newXMLNode('components', input$components, parent=masterNode)
                newXMLNode('duration', input$duration, parent=masterNode)
                newXMLNode('roll', input$roll, parent=masterNode)
                newXMLNode('classes', input$classes, parent=masterNode)
                
                
                # Add spell descriptions
                if (length(inserted > 0)) {
                    for (i in length(inserted):1) {
                        addChildren(masterNode, 
                                    newXMLNode('text', 
                                               input[[paste0(inserted[i],'_text')]]))
                    }
                }
            } else if (input$category == 'background') {
                skills_v <- c()
                
                for (ability in abilities) {
                    if (input[[ability]] == TRUE) {
                        skills_v <- append(skills_v, paste(ability, 'Saving Throws'))
                    }
                }
                
                for (skill in skills_simple) {
                    if ( input[[str_remove_all(skill,' ')]] == TRUE) {
                        skills_v <- append(skills_v, skill)
                    }
                }
                
                skills_v <- paste(skills_v, collapse = ', ')
                skills_v <- skills_v %>% str_remove_all(.,'(,)?[^,]* 0') %>% str_remove(.,'^,')
                
                newXMLNode('proficiency', skills_v, parent=masterNode)
                
                if (length(inserted > 0)) {
                    for (i in length(inserted):1) {
                        addChildren(masterNode, 
                                    newXMLNode('trait',
                                               newXMLNode('name',input[[paste0(inserted[i],'_name')]]), 
                                               newXMLNode('text',input[[paste0(inserted[i],'_text')]])
                                    )
                        )
                    }
                }
                
                if (length(insertedMod > 0)) {
                    for (mod in insertedMod) {
                        new <- newXMLNode('modifier',
                                          paste(input[[paste0(mod,'_target')]], 
                                                input[[paste0(mod,'_value')]]),
                                          parent=masterNode)
                        xmlAttrs(new) <- c('category' = input[[paste0(mod,'_category')]])
                    }
                }
                
                
                
                
            }
            
            # Clean output XML
            toString.XMLNode(xmlParse(toString.XMLNode(masterNode))) %>%
                str_remove('<\\?xml version="1.0"\\?>.*\\n') %>%
                str_remove_all(pattern = '[:blank:]*<.*\\/>.*\\n') %>%
                str_remove_all(pattern = '[:blank:]*<.*>NA</.*>\\n') %>%
                str_remove_all(pattern = '[:blank:]*<.*>.* \\+0</.*>\\\n') %>%
                str_remove_all('[:blank:]*$')
        }
    })
    
    
    observe({
        output$finalxml <- renderPrint(cat(new()))
    })
    
    
    xmlChunk <- reactive({
        toString.XMLNode(cat(new())) %>% 
            str_remove_all('NULL$') %>%
            str_remove_all('[:space:]*$')
    })
    
    observe({
        if(input$category != '') {
            output$copyButtonElement <- renderUI({
                rclipButton("copyButtonElement", 
                            paste("Copy",input$category), 
                            xmlChunk(), 
                            icon = if(input$category == 'item'){
                                icon("coins")
                            } else if (input$category == 'monster') {
                                icon("shield-alt")
                            } else if (input$category == 'spell') {
                                icon("magic")
                            }
                            else if (input$category == 'background') {
                                icon("history")
                            }
                            else { icon("clipboard")})
            })
        } else {
            output$copyButtonElement <- renderUI(HTML(''))}
    })
})
