## dimensions_shiny server.R

# devtools::install_github('hawaiiDimensions/db/hdimDB')
library(shiny)
library(datasets)
library(hdimDB)

function(input, output) {
    db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
    error.list <- dbChecker('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
    
    
    # Return the requested function(input, output) {
    output$contents <- renderTable({
        db
        # errors <- db[rapply(), ] 
    })
}