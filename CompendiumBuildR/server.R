library(shiny)
library(shinyjs)

layout <- "data/layout.xml"
# layout <- "CompendiumBuildR/data/layout.xml"


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    
    
    observeEvent(c(input$category,input$type),{
        reset('form')
    })
    
     
    output$fields <- renderUI({
        if (input$category == 'item' & input$type == 'M') {
            list( h4('General'),
                  textInput('name','Name:',''),
                  radioButtons('magic',
                               'Magic Item:',
                               choices = c('Yes'=1,'No'=0),0),
                  h4('Damages'),
                  textInput('dmg1',
                            'Damage:',placeholder = '2d6+2'),'')
        } else if (input$category == 'item' & input$type == 'R') {
            reset('form')
            list(h4('General'),
                 textInput('name','Name:'),
                 radioButtons('magic',
                              'Magic Item:',
                              choices = c('Yes'=1,'No'=0), 0),
                 h4('Damages'),
                 textInput('dmg1',
                           'Damage:',placeholder = '2d6+2'),
                 textInput('range','Range:',placeholder = '20/60'))
        }
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

    
    observe({
        doc = xmlTreeParse(layout, useInternalNodes = T)     # PARSE STRING
        root = xmlRoot(doc)
        masterNode = newXMLNode(input$category, parent=root)
        newXMLNode('name',input$name, parent=masterNode)
        newXMLNode('type',input$type, parent=masterNode)
        newXMLNode('magic',input$magic, parent=masterNode)
        newXMLNode('dmg1',input$dmg1, parent=masterNode)
        newXMLNode('range', input$range, parent=masterNode)
        saveXML(doc,file = 'final.xml')
        output$finalxml <- renderPrint(xmlTreeParse('final.xml', useInternalNodes = T))
    })
    
    
    observeEvent(input$copyButton, {
        clipr::write_clip(readr::read_file(toString.XMLNode(xmlTreeParse('final.xml', useInternalNodes = T))))
    })
    
})
