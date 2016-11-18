####################
## hdimDB PACKAGE ##
####################

## Install and load hdimDB
devtools::install_github('hawaiiDimensions/db/hdimDB')
library(hdimDB)
####################################
## Load database and scan for errors
db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
errors <- dbChecker('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
########################
## FAKE DATABASE TEST ## 
# fake.db <- read.csv("fake_data.csv", as.is=TRUE)
# test.results <- list(duplicatedHDIM = dupHDIM(db), empty = checkEmpty(db), misspell = checkMisspell(db),wrongTime = checkTime(db))
###############
## UNIT TEST ##
# install.packages('testthat')
# library(testthat)
##########################
## AUTOMATED CORRECTION ##
library(RecordLinkage)

# closestMatch <- function(string, stringVector){
#     distance <- levenshteinSim(string, stringVector)
#     stringVector[distance == max(distance)]
# }
# closestMatch("kohala", syn.plot)

correctMispell <- function(db){
    ## Returns new columns of corrected entries in the
    ## database based on Levinshtein distance
    
    ## Vectors of correct spelling to match against
    syn.method <- .synValues('https://docs.google.com/spreadsheets/d/1MIXM5OzUtWUj4w_9dzf51Z1aRNV2mTCLUNgVBvkZYuE/pub?output=csv')
    syn.plot <- .synValues('https://docs.google.com/spreadsheets/d/1Q8rFjF4n828ZVRTl7KkCQao5G0Emtwmm88MLZSoHcbA/pub?output=csv')
    
    ## Initial creation of database columns for suggested corrections
    db$corMethod <- db$Method
    db$corPlot <- db$Plot
    
    ## List of synonym vectors and columns to be checked
    syn.list <- list(syn.method, syn.plot)
    ver.columns <- list("corMethod", "corPlot")
    
    ## Wrapper
    mapply(ver.columns, syn.list, MoreArgs=list(db))
    return(head(db))
}

.corColumn <- function(db, column, syn.vector){
    for (string in column){
        distance <- levenshteinSim(string, syn.vector)
        for (match in syn.vector[distance == max(distance)]){
            paste(db[string, column], match)
        }
    }
    return(head(db))
}

.corColumn(db, "corPlot", syn.plot)
head(db)
correctMispell(db)



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
errBase <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')

## Consolidate errKey by HDIM
errSumm <- tapply(errKey$errType, errKey$errors, paste, collapse=',')
errSumm <- data.frame(HDIM = names(errSumm), errMessage = as.character(errSumm))

## Create new column in errBase with error tags from errSumm
errBase$errTag <- as.character(errSumm$errMessage[match(errBase$HDIM, errSumm$HDIM)])
errBase$errTag[is.na(errBase$errTag)] <- ''
head(errBase)


