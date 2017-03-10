## hawaii_dimensions_shiny server.R
library(shiny)
library(hdimDB)

## 
db <- readGoogle(colEventsURL)
db2 <- db[1:1000, ]
# errors <- checkDb(db2)


## CURRENT VERSION ## 
function(input, output) {
    
    output$colEvents <- renderDataTable(readGoogle(colEventsURL))
#     output$errors <- renderDataTable(checkDb(db2))
    
    output$downloadData <- downloadHandler(
        filename = 'labels.pdf',
        content = function(file) {
           out = makeLabels(hdim = strsplit(input$hdimList, ", |,")[[1]],
                      dir = NULL,
                      sheetName = file,
                      repID = strsplit(input$repID, ", |,")[[1]])
           file.rename(out, file)
        }
    )
}

