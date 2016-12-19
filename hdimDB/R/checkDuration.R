#' @title Checks Dimensions Database for invalid BeatingDuration values
#'  
#' @description \code{checkEmpty} runs through the online database and returns a list of HDIM numbers corresponding to invalid beating duration entries in the database
#' 
#' @details The sum of all values in the BeatingDuration column for any given plot should equal 420.
#' 
#' @param db the database to be checked
#' 
# @example 
# ## Load the Database
# db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
# 
# ## check BeatingDuration entries
# checkDuration(db)
#' 
#' @return List of character vectors of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkDuration <- function(db){
    plots <- .synValues(synPlotURL)
    out <- list()
    for (p in plots){
        site <- db[db$Plot == p & db$Method == 'beating', 'BeatingDuration']
        if (length(site) == 0){
            # no relevant rows found
        } else if (Reduce('+', as.numeric(site)) != 420){
            out <- list(out, list(db[site, ]$HDIM))
        }
    } 
    extractOut <- .extractErr(db, out, 'beatduration')
    return(.assignCorr(extractOut))
}

