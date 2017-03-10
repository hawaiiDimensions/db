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
        # content =  function(file) {
#            write.csv(readGoogle(colEventsURL), file)
        content = function(file) {
           out = makeLabels(hdim = strsplit(input$hdimList, ", ")[[1]],
                      dir = NULL,
                      sheetName = file,
                      repID = input$repID)
           file.rename(out, file)
        }
    )
}

