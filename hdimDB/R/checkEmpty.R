#' @title Checks Dimensions Database for invalid empty entries
#'  
#' @description \code{checkEmpty} runs through the online database and returns a list of HDIM numbers corresponding to invalid empty entries in the database
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
#' @return List of character vectors of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkEmpty <- function(db){
    db[is.na(db)] <- ""
    
    #####################################################################
    ## Vectors of Factor Column Names - REPLACE WITH SYNONYM TABLE VALUES
    #####################################################################
    
    ## beating (beat.vector)                   
    beat.vector <- c("Plant", "BeatingDuration", "TimeBegin", "TimeEnd")
    ## ground malaise (gmal.vector)
    gmal.vector <- c("DateEnd", "PitFallSlice")
    ## canopy malaise (cmal.vector)
    cmal.vector <- c("DateEnd", "PitFallSlice")
    ## leaf litter (leaf.vector)
    leaf.vector <- c("PitFallSlice")
    ## pitfall trap (pit.vector)
    pit.vector <- c("DateEnd", "PitFallSlice")
    ## InsectaZooka (zook.vector)
    zook.vector <- c("PitFallSlice")
    ## soil extraction (soil.vector)
    soil.vector <- c("PitFallSlice")  
    ## canopy clipping has no contingent columns, does not need to be checked
    contin.list <- list(beat.vector, pit.vector, leaf.vector,
                        cmal.vector, gmal.vector, zook.vector, 
                        soil.vector)
    
    ## Compiling vectors of column names and contingent column names
    empty.col <- c("HDIM", "Plot", "Date", "Collector", "Method", 
                   "Whereabouts", "SamplingRound") 
    methods <- c("beating", "pitfall", "leaf litter", "canopy malaise", 
                 "ground malaise", "Insectazooka", "soil extraction") 
    
    ####################################
    ## IMPLEMENTING SYNONYM TABLE VALUES 
    ####################################
  
    # .synValues <- function(url){
    #     return(c(readGoogle(url)[, 1]))
    # }
    # 
    # methods <- .synValues(methods.url)

    ###################################################################
    
    out <- list(column = mapply(.emptyColumn, empty.col, MoreArgs=list(db)),
                contingency = mapply(.emptyContin, methods, contin.list, MoreArgs=list(db)))
    return(.extractErr(out, "empty"))
}

## Helper functions
.emptyColumn <- function(column, db){
    return(db[which(db[, column] == ""),]$HDIM)
}

.emptyContin <- function(method, vector, db){
    method.ind <- which(db$Method == method)
    method.vec <- apply(db[vector], 2, function(x) which(x == ""))
    empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE)))
    return(db[unique(empty.ind[duplicated(empty.ind)]), ]$HDIM)
}
