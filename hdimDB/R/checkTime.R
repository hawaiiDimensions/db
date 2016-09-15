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

#####################################################
## METHOD CONTINGENT TIME ENTRY CHECKER FUNCTION HERE
#####################################################

#######################################
## DATEEND COLUMN CHECKER FUNCTION HERE
#######################################

checkDate <- function(){
    return(.datecolumn())
}

