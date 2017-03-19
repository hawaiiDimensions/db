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


checkTime <- function(db){ 
    db[is.na(db)] <- ''
    dateOut <- .badDate(db)
    timeOut <- .badTime(db)
    extractDate <- .extractErr(db, dateOut, 'time.Date')
    extractTime <- .extractErr(db, timeOut, 'time.TimeBegin')
    return(.assignCorr(rbind(extractDate, extractTime)))
}


## hidden helper functions 

.badTime <- function(db){ # checks TimeBegin / TimeEnd entries are correctly formatted.
    # isolate indices of correctly located TimeBegin / TimeEnd entries
    beatingIndices <- which(tolower(db[, 'Method']) == 'beating' # TimeBegin & TimeEnd only relevant for beating samples
                                  & db[, 'TimeEnd'] != '' 
                                  & db[, 'TimeBegin'] != '')
    # checking that entries are in 24hr/military time with regular expressions
    begIndices <- which(!grepl('^([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$', db[, 'TimeBegin'])) # indices where there are no matches
    endIndices <- which(!grepl('^([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$', db[, 'TimeEnd']))
    errIndices <- c(beatingIndices, unique(c(begIndices, endIndices))) 
    return(db[errIndices[duplicated(errIndices)], 'HDIM']) # duplicate indices are errors
} 


.badDate <- function(db){ # checks whether entries in the date column are correctly formatted
    
    # indices of target subsets
    dateIndices <- which(db[, 'Date'] != '' ) 
    endIndices <- which(db[, 'Method'] == 'pitfall'  # only pitfall samples use DateEnd
                      & db[, 'DateEnd'] != '')
    
    # establish valid date range
    firstDay <- as.numeric(as.Date('2014/3/1')) # March 1, 2014
    lastDay <- as.numeric(as.Date('2016/1/1')) # January 1, 2016
    
    # convert all date entries to numeric datetime objects
    dateNumerics <- as.numeric(Map(as.Date, db[,'Date'], format = '%m/%d/%Y')) # must do each column seperately
    endNumerics <- as.numeric(Map(as.Date, db[,'DateEnd'], format = '%m/%d/%Y')) # to preserve indices
    
    # compare against date range
    dateErr <- which(dateNumerics[] <= firstDay | dateNumerics[] >= lastDay) 
    endErr <- which(endNumerics[] <= firstDay | endNumerics[] >= lastDay)
        
    allErr <- unique(c(dateErr, endErr)) # compile errors
    allInd <- unique(c(dateIndices, endIndices)) # compile indices
    mixedInd <- c(allErr, allInd)
    
    # compare against all filled date entries
    return(db[mixedInd[duplicated(mixedInd)], 'HDIM']) # duplicate indices are where errors are located
} 

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
