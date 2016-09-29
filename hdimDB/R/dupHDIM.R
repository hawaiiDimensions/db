#' @title Checks Dimensions Database for duplicate HDIM numbers
#'  
#' @description \code{dupHDIM} runs through the online database and returns a vector of repeated HDIM numbers 
#' 
#' @details Only valid when called on a Dimensions Database with a column "HDIM"
#' 
#' @param db the database to be checked
#' 
# @example 
# ## Load the Database
# db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
# 
# ## check duplicate HDIM
# dupHDIM(db)
#' 
#' @return Character vector of duplicate HDIM numbers in the Dimensions Database
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

dupHDIM <- function(db){
  return(db[which(duplicated(db[, "HDIM"])),]$HDIM)
}
