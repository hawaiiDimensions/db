#' @title Checks Dimensions Database for invalid time entries
#'  
#' @description \code{checkEmpty} runs through the online database and returns a list of HDIM numbers corresponding to invalid time entries in the database
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
    out <- (.dateColumn(db))
    extractOut <- .extractErr(db, out, "time.Date")
    return(.assignCorr(extractOut))
}

## hidden helper functions
.dateColumn <- function(db){
    db[is.na(db)] <- ""
    dates <- (as.Date(db[, "Date"], format = "%m/%d/%Y" ))
    dates.indices <- which(is.na(as.character(dates)) == "TRUE")
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
