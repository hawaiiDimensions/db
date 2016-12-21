#' @title Checks Dimensions Database for errors
#'  
#' @description \code{dbChecker} runs through the online database and returns a dataframe associated with specific errors
#' 
#' @details Developed specifically for the Dimensions in Biodiversity Evolab Database.
#' 
#' @param db The database to be checked
#' 
# @example 
# dbChecker('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
#' 
#' @return Dataframe with HDIM identifier, error type, verbatim entry, and suggested correction.
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

dbChecker <- function(db){
    duplicatedHDIM <-dupHDIM(db)
    empty <- checkEmpty(db)
    misspelled <- checkMisspell(db)
    wrongTime <- checkTime(db)
    wrongDuration <- checkDuration(db)
    return(rbind(duplicatedHDIM, empty, misspelled, wrongTime, wrongDuration))
} 
