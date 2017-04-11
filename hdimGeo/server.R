library(shiny)
library(hdimDB)
library(leaflet)

sitesURL <- 'https://docs.google.com/spreadsheets/d/1ZT-PhfGT4S3-5rECBcrxjYJwcVnk1tKDUbWwlbpEeiI/pub?output=csv'
sites <- readGoogle(sitesURL)
siteLabels <- paste("Site", 
                    as.character(sites[, 'site.name']), 
                    "|", 
                    as.character(sites[, 'altitude..m.']), "meters")

server <- function(input, output) {
    output$sites <- renderDataTable(readGoogle(sitesURL))
    
    output$hdimGeo <- renderLeaflet({
        
        leaflet(sites) %>%
            addTiles() %>%  
            addMarkers(lng=~-longitude..W..DEG, 
                       lat=~latitude..N..DEG,
                       popup=siteLabels,
                       label=siteLabels) %>%
            addCircles(lng=~-longitude..W..DEG, 
                       lat=~latitude..N..DEG,
                       radius=50)
    })
}