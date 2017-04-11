## hawaii_dimensions_shiny ui.R
library(shiny)

## CURRENT VERSION ## 
fluidPage(
    titlePanel('Hawaii Dimensions Collection Events Database'),
    p('Version 0.1 - UC Berkeley ESPM 2016'),
    p('Developed by Edward G. Huang'),
    sidebarLayout(
        sidebarPanel(
            h3('Label Sheet Maker'),
            br(),
            textInput(inputId = 'hdimList', label = 'HDIM Numbers', value = NA,
                      placeholder = 'separate HDIMs with commas'),
            textInput(inputId = 'repID', label = 'Number of Labels per HDIM', value = NA,
                      placeholder = 'separate numbers with commas'),
            downloadButton('downloadData', 'Download Label Sheet')
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel('db', dataTableOutput(outputId = 'colEvents')),
                tabPanel('errors', dataTableOutput(outputId = 'errors'))
            )
        )
    )
)

