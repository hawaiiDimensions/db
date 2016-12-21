#' @title Checks Dimensions Database for invalid BeatingDuration values
#'  
#' @description \code{checkDuration} runs through the online database and returns a dataframe corresponding to invalid beating duration entries in the database
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
#' @return Dataframe with HDIM identifier, beating duration error tag, verbatim entry, and suggested correction.
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkDuration <- function(db){
    plots <- .synValues(synPlotURL) # plot names
    out <- list() # initialized list
    for (site in plots){
        values <- db[db$Plot == site & db$Method == 'beating', 'BeatingDuration']
        if (length(values) == 0){
            # no relevant rows found
        } else if (Reduce('+', as.numeric(values)) != 420){
            out <- list(out, list(db[values, ]$HDIM))
        }
    } 
    extractOut <- .extractErr(db, out, 'beatduration')
    return(.assignCorr(extractOut))
}

