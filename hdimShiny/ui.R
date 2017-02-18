## hawaii_dimensions_shiny ui.R
library(shiny)
library(hdimDB)

## CURRENT VERSION ## 
fluidPage(
    titlePanel('Hawaii Dimensions Collection Events Database'),
        mainPanel(
            p("Version 0.0 - UC Berkeley ESPM 2016"),
            p("Developed by Edward G. Huang"),
            tabsetPanel(
                id = 'dataset',
                tabPanel('db', DT::dataTableOutput('table1')),
                tabPanel('errors', DT::dataTableOutput('table2'))
            )
        )
    )

