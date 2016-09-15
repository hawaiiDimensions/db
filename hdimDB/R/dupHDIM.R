#' @title Checks Dimensions Database for duplicate HDIM numbers
#'  
#' @description \code{dupHDIM} runs through the online database and returns a vector of repeated HDIM numbers 
#' 
#' @details only valid when called on a Dimensions Database with a column "HDIM"
#' 
#' @param None
#' 
#' @return character vector of duplicate HDIM numbers in the Dimensions Database
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export


dupHDIM <- function(){
  db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoew
                    itaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
  db[is.na(db)] <- ""
  return(db[which(duplicated(db[, "HDIM"])),]$HDIM)
}
