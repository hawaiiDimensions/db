## dimensions_shiny ui.R

fluidPage(
    titlePanel("EvoLab Hawaii Dimensions Database Checker"),
   
    sidebarLayout(
        sidebarPanel(
            h6("University of California Berkeley - ESPM"),
            h6("Version 0.0.1 - 1 October 2016"),
            h6("Edward Greg Huang (edwardgh@berkeley.edu)"),
            h3("Database Errors"),
            tags$hr(),
            checkboxInput('all.errors', label = 'All Errors', TRUE),
        
            h4("Duplicates"),
            checkboxInput("dup.hdim", label = "Duplicate HDIM Numbers", value = TRUE),
        
            checkboxGroupInput("missing.values", 
                 label = h4("Missing Values"), 
                 choices = list("Column Entries" = 1, 
                                "Contingent Entries" = 2),
                                   selected = NA),
            
            checkboxGroupInput("misspellings", 
                 label = h4("Missppellings"), 
                 choices = list("Column Entries" = 1), 
                                #"Contingent Entries" = 2, ),
                                   selected = NA),

           checkboxGroupInput("invalid.time", 
                 label = h4("Invalid Time Entries"), 
                 choices = list("Date" = 1), 
#                                "TimeBegin and TimeEnd" = 2,
#                                "DateEnd" = 3, ),
                                   selected = NA),

            tags$hr(),
            p('You can download the database as a .csv ',
              a(href = 'https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv',
                       'here.'))
        ),
        mainPanel(
            br(),
            br(),
            tableOutput('contents')
        )
    )
)