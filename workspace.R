#######################
## R PACKAGE: hdimDB ##
#######################
devtools::install_github('hawaiiDimensions/db/hdimDB') # installation script
library(hdimDB) # Loading script
####################################
## Load database and diagnose errors
db <- readGoogle(colEventsURL)
errOut <- dbChecker(db)
## Plotting errortypes
tags <- as.character(errOut$errMessage) # converting factor vectors to characters
errPlot <- barplot(table(tags), names.arg = unique(tags), 
                   horiz = TRUE, las = 1, cex.names = 0.4, border = NA, 
                   main = 'ColEvents Database Error Distribution', 
                   sub = 'Huang, E.G., 2016. Evolab')
########################
## FAKE DATABASE TEST ## 
# fakeData <- read.csv("fake_data.csv", as.is=TRUE)
# results <- dbChecker(fakeData)

##########################
## SHINY IMPLEMENTATION ##
##########################

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

############################
## ROMINGER SCRIPT FOR SHINY

## turn nested list structure into a single vector of all the HDIMs with errors
errors <- unlist(errors)

## extract the error type (which is stored as the name of each element in the vector)
## use gsub to remove the sequential numbers from the names 
## (e.g. turn 'duplicatedHDIM123' into 'duplicatedHDIM')
errType <- gsub('[[:digit:]]', '', names(errors))
names(errors) <- NULL

## now we have all the error HDIMs and what type of error is associated with them
head(data.frame(errors, errType))
############################
## Dataframe of errors and their type
errKey <- data.frame(errors, errType)
## Copy over db 
errBase <- readGoogle(colEventsURL)

## Consolidate errKey by HDIM
errSumm <- tapply(errKey$errType, errKey$errors, paste, collapse=',')
errSumm <- data.frame(HDIM = names(errSumm), errMessage = as.character(errSumm))

## Create new column in errBase with error tags from errSumm
errBase$errTag <- as.character(errSumm$errMessage[match(errBase$HDIM, errSumm$HDIM)])
errBase$errTag[is.na(errBase$errTag)] <- ''
head(errBase)

