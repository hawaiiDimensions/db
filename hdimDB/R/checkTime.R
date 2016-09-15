#' @title Checks Dimensions Database for invalid time entries
#'  
#' @description \code{checkEmpty} runs through the online database and returns a list of HDIM numbers corresponding to invalid time entries in the database
#' 
#' @details Time entries will be checked, based on column name, for correct date and hour:minute formatting.
#' 
#' @param None
#' 
#' @return List of character vectors of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

## Load the Database
db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoew
                    itaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
db[is.na(db)] <- ""

.dateColumn <- function(){
  dates <- (as.Date(db[, "Date"], format = "%m/%d/%Y" ))
  dates.indices <- which(is.na(as.character(dates)) == "TRUE")
  return(db[dates.indices,]$HDIM)
}

## Standin format checker function, please replace
.dateContin <- function(date.column, date.format){
  empty.dates <- which(db[, date.column] != "")
  dates <- as.Date(db[, date.column], format = date.format )
  dates.indices <- which(is.na(as.character(dates)) == "TRUE")
  dates.vector <- c(empty.dates, dates.indices)
  return(db[unique(dates.vector[duplicated(dates.vector)]), ]$HDIM)
}

#####################################################
## METHOD CONTINGENT TIME ENTRY CHECKER FUNCTION HERE
#####################################################

#######################################
## DATEEND COLUMN CHECKER FUNCTION HERE
#######################################

checkTime <- function(){
  return(.datecolumn())
}
