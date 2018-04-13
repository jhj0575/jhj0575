# jhj0575
My statistics assignment(Jin huijie 14116112)
library(shiny)
ui<-shinyUI(pageWithSidebar(
            headerPanel("The relationship between percentage of survival and other factors"),  
            sidebarPanel(selectInput("variable", "Variable:",  list("pclass" = "pclass", 
                                                                     "age.g" = "age.g",   
                                                                      "sex" = "sex")),
            checkboxInput("outliers", "Show outliers", FALSE)  ), 
            mainPanel(    h3(textOutput("caption")),
                          plotOutput("titanicPlot")  )))
titanic$age.g <- as.integer(cut(titanic$age, 10)) 
library(plyr)
td <- ddply(titanic, c("pclass", "age.g", "sex"), summarise, total = length(survived), svv = length(survived[survived == 1]), ps = svv/total)
titanicData <- tdtitanicData$sex <- factor(titanicData$sex, labels = c("Famale", "Male"))
server<-shinyServer(function(input, output) { 
                   formulaText <- reactive({    paste("ps~", input$variable)  })
                   output$caption <- renderText({    formulaText()  }) 
                   output$titanicPlot <- renderPlot({    plot(as.formula(formulaText()), 
                                                         data = titanicData,   
                                                         outline = input$outliers)  })})
shinyApp(ui = ui, server = server)
                                                         
