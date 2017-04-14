#' @title Checks Dimensions Database for errors
#'  
#' @description \code{checkDb} processes the online database and returns a dataframe of errors and suggested corrections
#' 
#' @details Developed specifically for the Dimensions in Biodiversity Evolab Database.
#' 
#' @param db The database to be checked
#' @param match The autocorrection method to be used with misspelled entries
#' 
# @example 
# checkDb(readGoogle(colEventsURL), match = 'index')
#' 
#' @return Dataframe with HDIM identifier, error type, verbatim entry, and suggested correction.
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkDb <- function(db, match = 'index'){
    duplicatedHDIM <-dupHDIM(db)
    empty <- checkEmpty(db)
    misspelled <- checkMisspell(db, match)
    wrongTime <- checkTime(db)
    wrongDuration <- checkDuration(db)
    return(rbind(duplicatedHDIM, empty, misspelled, wrongTime, wrongDuration))
} 
