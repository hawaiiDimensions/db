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
    
    ##################################################################
    ## Vectors of Approved Entries - CORRECT WITH SYNONYM TABLE VALUES
    ##################################################################
    
    ## "Plot" (cor.plot) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    # cor.plot <- c(unique(db$Plot), "")
    ## "Collector" (cor.collect) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    cor.collect <- c(unique(db$Collector), "")
    ## "Method" (cor.method) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    cor.method <- c("canopy malaise", "ground malaise", "beating", "pitfall", "canopy clipping", 
                    "leaf litter", "InsectaZooka", "soil extraction", "")
    ## "Plant" (cor.plant) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    cor.plant <- c(unique(db$Plant), "")
    ## "BeatingDuration" (cor.beat)
    cor.beat <- c(0:300, "")
    ## "PitFallSlice" (cor.pit) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    cor.pit <- c("up", "down", "A", "B", "C", "D", 
                 "E", "F", "ground", "")
    ## "Whereabouts" (cor.where)(IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    cor.where <- c("UHH", "Hilgard 220", "NEED TO FIND", "")
    ## "SamplingRound" (cor.sample)
    cor.sample <- c(1:2, "")
    
    ## List of the vectors of possible valid entries to columns being checked for misspellings
    cor.list <- list(cor.plot, cor.collect, cor.method, 
                     cor.plant, cor.beat, cor.pit, 
                     cor.where, cor.sample)
    
    ## Vector of the different column names, excluding columns not being checked for misspellings
    misspelled.columns <- c("Plot", "Collector", "Method", "Plant", 
                            "BeatingDuration", "PitFallSlice",
                            "Whereabouts", "SamplingRound")
    
    ####################################
    ## IMPLEMENTING SYNONYM TABLE VALUES 
    ####################################
    
    .synValues <- function(url){
        return(readGoogle(url)[, 2])
    }
    #     ## "Plot" (cor.plot) 
          cor.plot <- .synValues('https://docs.google.com/spreadsheets/d/1Q8rFjF4n828ZVRTl7KkCQao5G0Emtwmm88MLZSoHcbA/pub?output=csv')
    #     ## "Collector" (cor.collect)
    #     cor.collect <- .synValues('https://docs.google.com/spreadsheets/d/1_KGLPEcOneLvcRR8--CjEVeKTPDXJkI7YqwSTM2BVJc/edit#gid=0')
    #
    #     ## "Method" (cor.method)
    #     cor.method <- .synValues('https://docs.google.com/spreadsheets/d/1MIXM5OzUtWUj4w_9dzf51Z1aRNV2mTCLUNgVBvkZYuE/edit#gid=0')
    #
    #     ## "Plant" (cor.plant) 
    #     cor.plant <- .synValues(https://docs.google.com/spreadsheets/d/1SxbSt_SRiycfaihkOg-m4tz-nrtZNKtqJDE2OmG700Q/edit#gid=0)
    #
    #     ## "PitFallSlice" (cor.pit)
    #     cor.pit <- .synValues(https://docs.google.com/spreadsheets/d/19eHQCQLKN_At10iUDmXYX2w1FA7EomHK9jZbq7lOSYg/edit#gid=0)
    #
    #     ## "Whereabouts" (cor.where)
    #     cor.where <- .synValues(https://docs.google.com/spreadsheets/d/1sKJpNgcghZySIGQiw2o9t6Vt_Q06IVQo0GLf5YUb4-M/edit#gid=0)
    # 
    #
    
    
    ###################################################################
    
    return(mapply(.misColumn, misspelled.columns, cor.list, MoreArgs=list(db)))
}

## Helper function
.misColumn <- function(column, vector, db){
    indice.misspelled <- (which(!db[, column] %in% vector))
    return(db[indice.misspelled,]$HDIM)
}

