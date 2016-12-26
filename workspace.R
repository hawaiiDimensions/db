###################
## KOKUA: hdimDB ##
###################
devtools::install_github('hawaiiDimensions/db/hdimDB') # installation script
library(hdimDB) # load package
##############################
db <- readGoogle(colEventsURL) # load database
errOut <- dbChecker(db) # diagnose errors - 12.09.16 runtime -> 22.04574 mins

## Plotting errTags ##
tags <- as.character(errOut$errMessage) # convert factors to characters
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0)) 
errPlot <- barplot(table(tags), names.arg = unique(tags), 
                   horiz = TRUE, las = 1, cex.names = 0.4, border = NA, 
                   main = 'Types of ColEvents Database Errors') 
                   # sub = 'Huang, E.G., 2016. UC Berkeley')

## hdimDB Testing ## 
fakeData <- read.csv("fake_data.csv", as.is=TRUE) # load fake dataset
dbChecker(fakeData) # run checker


## checkEmpty Update ##

# Make checkEmpty screen the unused contingency columns for filled entries
foo <- function(method, vector, db){
    method.ind <- which(db$Method == method)
    method.vec <- apply(db[vector], 2, function(x) which(x == ''))
    # contingencies <- c('Plant', 'BeatingDuration', 'TimeBegin','TimeEnd', 'DateEnd', 'PitFallSlice') # all possible contingent columns
    # empty.columns <- setdiff(contingencies, vector) # all contingency columns that are supposed to be empty
    # nonempty <- apply(db[empty.columns], 2, function(x) which(x != '')) # finds indices of invalid filled contingent entries
    empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE))) # needs to be modified
    return(db[unique(empty.ind[duplicated(empty.ind)]), ]$HDIM)
}


###############
## hdimShiny ##
###############

## example apps
system.file("examples", package="shiny")

runExample("01_hello") # a histogram
runExample("02_text") # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg") # global variables
runExample("05_sliders") # slider bars
runExample("06_tabsets") # tabbed panels
runExample("07_widgets") # help text and submit buttons
runExample("08_html") # Shiny app built from HTML
runExample("09_upload") # file upload wizard
runExample("10_download") # file download wizard
runExample("11_timer") # an automated timer

## SHINY.IO DEPLOYMENT ##
# install.packages('rsconnect') # shiny.io
library(rsconnect)
# rsconnect::setAccountInfo(name='edwardhuang', token='BC78E54A24F464E0C7E125EDC1FAC215', secret='OfD5Ks58EnClYHcrn3vtdHjqqRod2X4kwdZsXzTu')
rsconnect::deployApp('/Users/EdwardH/Dropbox/hawaiiDimensions/db/hdimShiny') # deploy
# App URL: https://edwardhuang.shinyapps.io/hdimshiny/



