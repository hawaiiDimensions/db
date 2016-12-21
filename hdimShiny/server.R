## dimensions_shiny server.R
library(shiny)
library(hdimDB)

db <- readGoogle(colEventsURL)
db2 = db[1:1000, ]
errors <- dbChecker(db2)

function(input, output) {
    
    # choose columns to display
    output$mytable1 <- DT::renderDataTable({
        DT::datatable(db2)
    })
    output$mytable2 <- DT::renderDataTable({
        DT::datatable(errors)
    })
}

# output$table <- DT::renderDataTable(DT::datatable({
#     data <- db2
#     if (input$Plot != "All") {
#         data <- data[data$manufacturer == input$man,]
#     }
#     if (input$Date != "All") {
#         data <- data[data$cyl == input$cyl,]
#     }
#     if (input$Colllector != "All") {
#         data <- data[data$trans == input$trans,]
#     }
#     if (input$Colllector != "All") {
#        data <- data[data$trans == input$trans,]
#     }
#     data
# }))
# 
# }