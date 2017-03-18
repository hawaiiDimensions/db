#' @title Checks Dimensions Database for invalid time entries
#'  
#' @description \code{checkTime} runs through the online database and returns a dataframe corresponding to invalid time entries in the database
#' 
#' @details Time entries will be checked, based on column name, for correct date and hour:minute formatting.
#' 
#' @param db The database for which time entries are to be checked
#' 
#' @return Dataframe with HDIM identifier, invalid time error tag, verbatim entry, and suggested correction.
#' 
# @example 
# ## Load the Database
# db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
# 
# ## check times
# checkTime(db)
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

## 1. checkTime
##  a. Date / Date End
##  b. Valid range for collection dates: March 1, 2014 to January 1, 2016. 
##     Format should be %m/%d/YYYY (e.g. 7/8/2015 or 07/08/2015 are both valid, 
##     but 7/8/15 would not be valid). 


checkTime <- function(db){
    dateCheck <- .dateColumn(db)
    timeCheck <- .badHDIM(db)
    out <- (c(dateCheck, timeCheck))
    extractOut <- .extractErr(db, out, 'time.Date')
    return(.assignCorr(extractOut))
}

## =================================================================
## ++++++++++++++++++++++ STAGED UPDATE ++++++++++++++++++++++++++++
# .emptyTime <- function(db){ # checks TimeBegin / TimeEnd entries are in the correct locations
#     # beating samples should have TimeBegin/TimeEnd entries
#     beatingIndices <- which(tolower(db['Method']) == 'beating')
#     bErrors <- which(db[beatingIndices, 'TimeEnd'] == '' |
#                      db[beatingIndices, 'TimeBegin'] == '')
#     # nonbeating samples should not have TimeBegin/TimeEnd entries
#     otherIndices <- which(tolower(db['Method']) != 'beating')
#     oErrors <- which(db[beatingIndices, 'TimeEnd'] != '' |
#                      db[beatingIndices, 'TimeBegin'] != '')
#     # compile results
#     errIndices <- unique(c(bErrors, oErrors))
#     return(db[errIndices]$HDIM)
# }

.badTime <- function(db){ # checks TimeBegin / TimeEnd entries are correctly formatted.
    # isolate indices of correctly located TimeBegin / TimeEnd entries
    beatingIndices <- which(tolower(db[, 'Method']) == 'beating'
                                  & db[, 'TimeEnd'] != '' 
                                  & db[, 'TimeBegin'] != '')
    # checking that entries are in 24hr/military time with regular expressions
    begIndices <- which(!grepl('^([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$', db[, 'TimeBegin'])) # HDIMs where no match
    endIndices <- which(!grepl('^([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$', db[, 'TimeEnd']))
    errIndices <- c(beatingIndices, unique(c(begIndices, endIndices))) 
    return(db[errIndices[duplicated(errIndices)], 'HDIM']) # duplicate indices are errors
} # FINISHED & TESTED 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## =======================================================================

## hidden helper functions
.dateColumn <- function(db){
    db[is.na(db)] <- ''
    dates <- (as.Date(db[, 'Date'], format = '%m/%d/%Y' ))
    dates.indices <- which(is.na(as.character(dates)) == 'TRUE')
    return(db[dates.indices,]$HDIM)
}

## Standin format checker function, please replace
.dateContin <- function(date.column, date.format){
  empty.dates <- which(db[, date.column] != "")
  dates <- as.Date(db[, date.column], format = date.format )
  dates.indices <- which(is.na(as.character(dates)))
  dates.vector <- c(empty.dates, dates.indices)
  return(db[unique(dates.vector[duplicated(dates.vector)]), ]$HDIM)
}
