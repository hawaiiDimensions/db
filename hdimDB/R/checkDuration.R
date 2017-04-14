#' @title Checks Dimensions Database for invalid BeatingDuration values
#'  
#' @description \code{checkDuration} processes the online database and returns a dataframe corresponding to invalid beating duration entries in the database
#' 
#' @details The sum of all values in the BeatingDuration column for any given plot should equal 420. The suggested correction describes the number of seconds over or under the 420 quota. 
#' 
#' @param db the database to be checked
#' 
# @example 
# ## Load the Database
# db <- readGoogle(colEventsURL)
# ## check BeatingDuration entries
# checkDuration(db)
#' 
#' @return Dataframe with HDIM identifier, beating duration error tag, verbatim entry, and suggested correction.
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkDuration <- function(db) {
    plots <- .synValues(synPlotURL) # plot names
    errHDIM <- c() # initialized list
    for (site in plots) {
        values <- db[db$Plot == site & db$Method == 'beating', 'BeatingDuration']
        if (length(values) == 0) {
            # no relevant rows found
        } else if ("" %in% values | NA %in% values) {
            # correction unable to be calculated
        } else if (Reduce('+', as.numeric(values)) != 420) {
            errHDIM <- c(errHDIM, db[db$Plot == site & db$Method == 'beating', ]$HDIM)
        }
    } 
    extractOut <- .extractErr(db, errHDIM, 'BeatingDuration')
    return(.assignCorr(extractOut, db = db))
}

