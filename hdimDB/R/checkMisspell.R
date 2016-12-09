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
    
    ####################################
    ## Non-Synonym Correction Vectors ##
    ####################################
    
    ## "Plot" (cor.plot) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    ## cor.plot <- c(unique(db$Plot), "")
    
    ## "Collector" (cor.collect) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    ## cor.collect <- c(unique(db$Collector), "")
    
    ## "Method" (cor.method) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    ## cor.method <- c("canopy malaise", "ground malaise", "beating", 
    ##                 "pitfall", "canopy clipping", "leaf litter",
    ##                 "InsectaZooka", "soil extraction", "")
    
    ## "Plant" (cor.plant) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    ## cor.plant <- c(unique(db$Plant), "")
    
    ## "BeatingDuration" (cor.beat)
    cor.beat <- c(0:300, "")
    
    ## "PitFallSlice" (cor.pit) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    ## cor.pit <- c("up", "down", "A", "B", "C", "D", 
    ##              "E", "F", "ground", "")
    
    ## "Whereabouts" (cor.where)(IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    ## cor.where <- c("UHH", "Hilgard 220", "NEED TO FIND", "")
    
    ## "SamplingRound" (cor.sample)
    cor.sample <- c(1:2, "")
    
    ########################################
    ## Synonym Sourced Correction Vectors ##
    ########################################
    
    .synValues <- function(url){
        return(unique(readGoogle(url)[, 2]))
    }
    
    ## "Plot" (cor.plot) 
    syn.plot <- .synValues('https://docs.google.com/spreadsheets/d/1Q8rFjF4n828ZVRTl7KkCQao5G0Emtwmm88MLZSoHcbA/pub?output=csv')
    # syn.plot <- .synValues(synPlotURL)
    
    ## "Collector" (cor.collect)
    syn.collect <- .synValues('https://docs.google.com/spreadsheets/d/1_KGLPEcOneLvcRR8--CjEVeKTPDXJkI7YqwSTM2BVJc/pub?output=csv')
    # syn.collect <- .synValues(synCollectURL)
    
    ## "Method" (cor.method)
    syn.method <- .synValues('https://docs.google.com/spreadsheets/d/1MIXM5OzUtWUj4w_9dzf51Z1aRNV2mTCLUNgVBvkZYuE/pub?output=csv')
    # syn.method <- .synValues(synMethodURL)

    ## "Plant" (cor.plant) 
    syn.plant <- .synValues('https://docs.google.com/spreadsheets/d/1SxbSt_SRiycfaihkOg-m4tz-nrtZNKtqJDE2OmG700Q/pub?output=csv')
    # syn.plant <- .synValues(synPlantURL)
    
    ## "PitFallSlice" (cor.pit)
    syn.pit <- .synValues('https://docs.google.com/spreadsheets/d/19eHQCQLKN_At10iUDmXYX2w1FA7EomHK9jZbq7lOSYg/pub?output=csv')
    # syn.pit <- .synValues(synPitURL)

    ## "Whereabouts" (cor.where)
    syn.where <- .synValues('https://docs.google.com/spreadsheets/d/1sKJpNgcghZySIGQiw2o9t6Vt_Q06IVQo0GLf5YUb4-M/pub?output=csv')
    # syn.where <- .synValues(synWhereURL)
    ###################################################################
    
    ## List of the vectors of possible valid entries to columns being checked for misspellings
    ## 'syn' prefix indicates calues sourced from synonym tables.
    ## 'cor' prefix indicates stand-in values.
    cor.list <- list(syn.plot, syn.collect, syn.method, 
                     syn.plant, cor.beat, syn.pit, 
                     syn.where, cor.sample)
    
    ## Vector of the different column names, excluding columns not being checked for misspellings
    misspelled.columns <- c("Plot", "Collector", "Method", "Plant", 
                            "BeatingDuration", "PitFallSlice",
                            "Whereabouts", "SamplingRound")
    
    out <- mapply(.misColumn, misspelled.columns, cor.list, MoreArgs=list(db))
    return(.extractErr(db, out, "misspelled"))
}

## Helper function
.misColumn <- function(column, vector, db){
    indice.misspelled <- (which(!db[, column] %in% vector))
    return(db[indice.misspelled,]$HDIM)
}

