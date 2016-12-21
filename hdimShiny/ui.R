## dimensions_shiny ui.R
library(shiny)
library(hdimDB)

fluidPage(
    title = 'HDIM Collection Events Database',
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel('db', DT::dataTableOutput('mytable1')),
                tabPanel('errors', DT::dataTableOutput('mytable2'))
            )
        )
    )


# fluidPage(
#     title = 'Examples of DataTables',
#     sidebarLayout(
#         sidebarPanel(
#             conditionalPanel(
#                 'input.dataset === "diamonds"',
#                 checkboxGroupInput('show_vars', 'Columns in diamonds to show:',
#                                    names(diamonds), selected = names(diamonds))
#             )
#         ),
#         mainPanel(
#             tabsetPanel(
#                 id = 'dataset',
#                 tabPanel('diamonds', DT::dataTableOutput('mytable1')),
#                 tabPanel('mtcars', DT::dataTableOutput('mytable2')),
#                 tabPanel('iris', DT::dataTableOutput('mytable3'))
#             )
#         )
#     )
# )