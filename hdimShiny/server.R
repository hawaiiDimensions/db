## dimensions_shiny server.R

library(shiny)
library(ggplot2)

function(input, output) {
    
    # choose columns to display
    db2 = db[sample(nrow(db), 1000), ]
    output$mytable1 <- DT::renderDataTable({
        DT::datatable(db2[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE))
    })
}
