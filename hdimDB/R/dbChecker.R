#' @title Checks Dimensions Database for errors
#'  
#' @description \code{dbChecker} runs through the online database and returns a list of HDIM numbers associated with specific errors
#' 
#' @details Developed specifically for the Dimensions in Biodiversity Evolab Database.
#' 
#' @param url The url where the goodle drive database lives
#' 
#' @example 
#' dbChecker('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
#' 
#' @return A multi-leveled list of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

dbChecker <- function(url){
    db <- readGoogle(url)
    return(list(duplicatedHDIM = dupHDIM(db), empty = checkEmpty(db), misspell = checkMisspell(db), 
                wrongTime = checkTime(db)))
} 
