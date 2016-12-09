#' @title Checks Dimensions Database for misspelled empty entries
#'  
#' @description \code{checkEmpty} runs through the online database and returns a list of HDIM numbers corresponding to misspelled entries in the database
#' 
#' @details Existing entries are compared against approved entries as listed in the synonym tables
#' 
#' @param db the database to be checked
#' 
# @example 
# ## Load the Database
# db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
# 
# ## check misspellings
# checkMisspell(db)
#' 
#' @return List of character vectors of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkMisspell <- function(db){
    db[is.na(db)] <- ""
    
    ## Non-Synonym Correction Vectors ##

    cor.sample <- c(1:2, "")
    
    ## Synonym Sourced Correction Vectors ##
    
    # 'Plot
    syn.plot <- .synValues(synPlotURL)
    # 'Collector' 
    syn.collect <- .synValues(synCollectURL)
    # 'Method'
    syn.method <- .synValues(synMethodURL)
    # 'Plant'
    syn.plant <- .synValues(synPlantURL)
    # 'PitFallSlice'
    syn.pit <- .synValues(synPitURL)
    # 'Whereabouts'
    syn.where <- .synValues(synWhereURL)
    
    # List of the vectors of possible valid entries to columns 
    # being checked for misspellings.

    # 'syn-' synonym values.
    # 'cor-' stand-in values.
    cor.list <- list(syn.plot, syn.collect, syn.method, 
                     syn.plant, syn.pit, syn.where, cor.sample)
    
    # Vector of the different column names of columns checked 
    # for misspellings.
    misspelled.columns <- c('Plot', 'Collector', 'Method', 'Plant', 
                            'PitFallSlice', 'Whereabouts', 'SamplingRound')
    
    out <- mapply(.misColumn, misspelled.columns, cor.list, MoreArgs=list(db))
    extractOut <- .extractErr(db, out, "misspelled")
    return(.assignCorr(extractOut))
}

## Hidden functions
.misColumn <- function(column, vector, db){
    indice.misspelled <- (which(!db[, column] %in% vector))
    return(db[indice.misspelled,]$HDIM)
}

.synValues <- function(url){
    return(unique(readGoogle(url)[, 2]))
}
