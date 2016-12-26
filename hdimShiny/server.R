## hawaii_dimensions_shiny server.R
library(shiny)
library(hdimDB)

## 
db <- readGoogle(colEventsURL)
db2 <- db[1:1000, ]
errors <- dbChecker(db2)

## CURRENT VERSION ## 
function(input, output) {
    
    # choose columns to display
    output$table1 <- DT::renderDataTable({
        DT::datatable(db2)
    })
    output$table2 <- DT::renderDataTable({
        DT::datatable(errors)
    })
}

## STAGED UPDATE ## 
# function(input, output) {
#     output$table <- DT::renderDataTable(DT::datatable({
#         data <- db2
#         if (input$HDIM != "All") {
#             data <- data[data$HDIM == input$HDIM,]
#         }
#         if (input$Plot != "All") {
#             data <- data[data$Plot == input$Plot,]
#         }
#         if (input$Date != "All") {
#             data <- data[data$Date == input$Date,]
#         }
#         if (input$Colllector != "All") {
#             data <- data[data$Collector == input$Collector,]
#         }
#         if (input$Method != "All") {
#             data <- data[data$Method == input$Method,]
#         }
#         if (input$Whereabouts != "All") {
#             data <- data[data$Whereabouts == input$Whereabouts,]
#         }
#         if (input$SamplingRound != "All") {
#             data <- data[data$SamplingRound == input$SamplingRound,]
#         }
#         data
#         }))
##  output$table2 <- DT::renderDataTable({
##      DT::datatable(errors)
##      })
#}