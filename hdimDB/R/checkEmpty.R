#' @title Checks Dimensions Database for invalid empty entries
#'  
#' @description \code{checkEmpty} runs through the online database and returns a dataframe corresponding to invalid empty entries in the database
#' 
#' @details Only certain columns are checked for empty entries, factor columns contingent to certain collection methods are considered.
#' 
#' @param db the database to be checked
#' 
# @example 
# ## Load the Database
# db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
# 
# ## check empty entries
# checkEmpty(db)
#' 
#' @return Dataframe with HDIM identifier and empy entry error tag.
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkEmpty <- function(db) {
    db[is.na(db)] <- ''
    
    ## Vectors of Factor Column Names - REPLACE WITH SYNONYM TABLE VALUES
    beat.vector <- c('Plant', 'BeatingDuration', 'TimeBegin', 'TimeEnd') # beating
    gmal.vector <- c('DateEnd', 'PitFallSlice') # ground malaise 
    cmal.vector <- c('DateEnd', 'PitFallSlice') # canopy malaise 
    leaf.vector <- c('PitFallSlice') # leaf litter
    pit.vector <- c( 'PitFallSlice', 'DateEnd') # pitfall trap
    zook.vector <- c('PitFallSlice') # InsectaZooka
    soil.vector <- c('PitFallSlice') # soil extraction
    # canopy clipping has no contingent columns, does not need to be checked.
    
    contin.list <- list(beat.vector, pit.vector, leaf.vector,
                        cmal.vector, gmal.vector, zook.vector, 
                        soil.vector)
    
    # Compiled vectors of column names and contingent column names.
    empty.col <- c('HDIM', 'Plot', 'Date', 'Collector', 'Method', 
                   'Whereabouts', 'SamplingRound') 
    methods <- c('beating', 'pitfall', 'leaf litter', 'canopy malaise', 
                 'ground malaise', 'Insectazooka', 'soil extraction') 
    
    ## Synonym Table Implementation
    # syn.method <- .synValues(synMethodURL)
    
    out <- list(column = mapply(.emptyColumn, empty.col, MoreArgs=list(db)),
                contingency = mapply(.emptyContin, methods, contin.list, MoreArgs=list(db)),
                misplaced = mapply(.misplacedContin, methods, contin.list, MoreArgs=list(db))
                )
    extractOut <- .extractErr(db, out, 'empty')
    return(.assignCorr(extractOut))
}

## Hidden functions
.emptyColumn <- function(column, db) {
    return(db[which(db[, column] == ''), 'HDIM'])
}

.emptyContin <- function(method, vector, db){
    method.ind <- which(db$Method == method)
    method.vec <- apply(db[vector], 2, function(x) which(x == ''))
    empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE))) 
    return(db[unique(empty.ind[duplicated(empty.ind)]), ]$HDIM)
}

################################################################################
##++++++++++++++++++++++++++++ STAGED ++++++++++++++++++++++++++++++++++++++++++
.misplacedContin <- function(method, vector, db) {
    methodInd <- which(db$Method == method)
    emptyColumns <- setdiff(c('Plant', 
                               'BeatingDuration', 
                               'TimeBegin', 
                               'TimeEnd', 
                               'DateEnd', 
                               'PitFallSlice'), vector) # all contingency columns that are supposed to be empty
    nonempty <- apply(db[emptyColumns], 2, function(x) which(x != '')) # finds indices of invalid filled contingent entries
    emptyInd <- c(methodInd, unique(unlist(nonempty, recursive = TRUE))) # needs to be modified
    return(db[emptyInd[duplicated(emptyInd)], 'HDIM'])
}