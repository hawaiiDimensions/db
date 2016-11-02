## dimensions_shiny ui.R
library(shiny)
library(ggplot2)
library(hdimDB)
db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
errors <- dbChecker('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')


fluidPage(
    title = 'Examples of DataTables',
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                'input.dataset === "db"',
                checkboxGroupInput('show_vars', 'Columns in diamonds to show:',
                                   names(db), selected = names(db))
            )
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel('db', DT::dataTableOutput('mytable1'))
            )
        )
    )
)
