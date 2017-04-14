#' @title Checks Dimensions Database for duplicate HDIM numbers
#'  
#' @description \code{dupHDIM} processes the online database and returns a dataframe of repeated HDIM numbers 
#' 
#' @details Only valid when called on a Dimensions Database with a column "HDIM"
#' 
#' @param db the database to be checked
#' 
# @example 
# ## Load the Database
# db <- readGoogle(colEventsURL)
# 
# ## check duplicate HDIM
# dupHDIM(db)
#' 
#' @return Dataframe with HDIM identifier and duplicate HDIM error tag.
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

dupHDIM <- function(db){
  out <- db[which(duplicated(db[, 'HDIM'])),]$HDIM
  extractOut <- .extractErr(db, out, 'dupHDIM') 
  return(.assignCorr(extractOut))
}
