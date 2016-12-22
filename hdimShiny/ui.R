## dimensions_shiny ui.R
library(shiny)
library(hdimDB)

## CURRENT VERSION ## 
fluidPage(
    titlePanel('Hawaii Dimensions Collection Events Database'),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel('db', DT::dataTableOutput('table1')),
                tabPanel('errors', DT::dataTableOutput('table2'))
            )
        )
    )

## STAGED UPDATE ##
# db2 <- db[1:1000, ]

# fluidPage(
#     titlePanel("Hawaii Dimensions Collection Events Database"),
#     
#     # Create a new Row in the UI for selectInputs
#     fluidRow(
#         column(2,
#                selectInput("HDIM",
#                            "HDIM Identifier:",
#                            c("All",
#                              unique(as.character(db2$HDIM))))
#         ),
#         column(2,
#                selectInput("Plot",
#                            "Plot Name:",
#                            c("All",
#                              unique(as.character(db2$Plot))))
#         ),
#         column(2,
#                selectInput("Date",
#                            "Collection Date:",
#                            c("All",
#                              unique(as.character(db2$Date))))
#         ),
#         column(2,
#                selectInput("Collector",
#                            "Collector:",
#                            c("All",
#                              unique(as.character(db2$Collector))))
#         ),
#         column(2,
#                selectInput("Whereabouts",
#                            "Sample Location",
#                            c("All",
#                              unique(as.character(db2$Whereabouts))))
#         ),
#         column(2,
#                selectInput("SamplingRound",
#                            "Collection Round",
#                            c("All",
#                              unique(as.character(db2$SamplingRound))))
#         )
#     ),
#     # Create a new row for the table.
#     fluidRow(
#         DT::dataTableOutput("table")
#     )
# )
