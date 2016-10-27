##########################
## Install and load hdimDB
devtools::install_github('hawaiiDimensions/db/hdimDB')
library(hdimDB)
##########################
## Load database and scan for errors
db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
errors <- dbChecker('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
##########################

########################
## FAKE DATABASE TEST ## 
# db <- read.csv("fake_data.csv", as.is=TRUE)
# test.results <- list(duplicatedHDIM = dupHDIM(db), empty = checkEmpty(db), misspell = checkMisspell(db),wrongTime = checkTime(db))
# test.results
########################

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

#####################
## ROMINGER SCRIPT ##
#####################
## turn nested list structure into a single vector of all the HDIMs with errors
errors <- unlist(errors)

## extract the error type (which is stored as the name of each element in the vector)
## use gsub to remove the sequential numbers from the names 
## (e.g. turn 'duplicatedHDIM123' into 'duplicatedHDIM')
errType <- gsub('[[:digit:]]', '', names(errors))
names(errors) <- NULL

## now we have all the error HDIMs and what type of error is associated with them
head(data.frame(errors, errType))
########################
## checkMisspell
########################
## Original stand-in correction vector
cor.plot <- c(unique(db$Plot), "")
cor.plot
## helper function to extract synonym values
.synValues <- function(url){
    return(unique(readGoogle(url)[, 2]))
}
## synonym extraction
syn.plot <- .synValues('https://docs.google.com/spreadsheets/d/1Q8rFjF4n828ZVRTl7KkCQao5G0Emtwmm88MLZSoHcbA/pub?output=csv')
syn.plot 
#########################
errors
