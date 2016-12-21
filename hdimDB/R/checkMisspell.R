#' @title Checks Dimensions Database for misspelled empty entries
#'  
#' @description \code{checkMisspell} runs through the online database and returns a dataframe corresponding to misspelled entries in the database
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
#' @return Dataframe with HDIM identifier, misspelling error tag, verbatim entry, and suggested correction.
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkMisspell <- function(db){
    db[is.na(db)] <- ''
    
    ## Non-Synonym Correction Vectors - 'cor.'
    cor.sample <- c(1:2, '') # 'SamplingRound'
    
    ## Synonym Sourced Correction Vectors - 'syn.'
    syn.plot <- .synValues(synPlotURL) # 'Plot'
    syn.collect <- .synValues(synCollectURL) # 'Collector'
    syn.method <- .synValues(synMethodURL) # 'Method'
    syn.plant <- .synValues(synPlantURL) # 'Plant'
    syn.pit <- .synValues(synPitURL) # 'PitFallSlice'
    syn.where <- .synValues(synWhereURL) # 'Whereabouts'
    
    # List of the vectors of possible valid entries to columns 
    # being checked for misspellings.

    cor.list <- list(syn.plot, syn.collect, syn.method, 
                     syn.plant, syn.pit, syn.where, cor.sample)
    
    # Vector of the different column names of columns checked 
    # for misspellings.
    misspelled.columns <- c('Plot', 'Collector', 'Method', 'Plant', 
                            'PitFallSlice', 'Whereabouts', 'SamplingRound')
    
    out <- mapply(.misColumn, misspelled.columns, cor.list, MoreArgs=list(db))
    extractOut <- .extractErr(db, out, 'misspelled')
    return(.assignCorr(extractOut))
}

## Hidden functions
.misColumn <- function(column, vector, db){ 
    # vector <- c(vector, '') # add empty string to synonym vector to avoid 
    #                           redundantly tagging empty entries as misspellings
    indice.misspelled <- (which(!db[, column] %in% vector))
    return(db[indice.misspelled, ]$HDIM)
}

.synValues <- function(url){ 
    return(unique(readGoogle(url)[, 2]))
}
