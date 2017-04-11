library(shiny)
library(hdimDB)
library(leaflet)

ui <- fluidPage(
    titlePanel('Hawaii Dimensions Collection Sites'),
    p('Version 0.1 - UC Berkeley ESPM 2017'),
    p('Developed by Edward G. Huang'),
    mainPanel(
        tabsetPanel(
            id = 'dataset',
            tabPanel("hdimGeo", leafletOutput("hdimGeo")),
            tabPanel('sites', dataTableOutput(outputId = 'sites'))
        )
    )
)