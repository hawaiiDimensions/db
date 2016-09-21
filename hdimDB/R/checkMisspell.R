#' @title Checks Dimensions Database for misspelled empty entries
#'  
#' @description \code{checkEmpty} runs through the online database and returns a list of HDIM numbers corresponding to misspelled entries in the database
#' 
#' @details Existing entries are compared against approved entries as listed in the synonym tables
#' 
#' @param None
#' 
#' @example 
#' ## Load the Database
#' db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
#' 
#' ## check misspellings
#' checkMisspell(db)
#' 
#' @return List of character vectors of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

checkMisspell <- function(db){
    
    ##################################################################
    ## Vectors of Approved Entries - CORRECT WITH SYNONYM TABLE VALUES
    ##################################################################
    
    ## "Plot" (cor.plot) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    cor.plot <- c(unique(colEvent$Plot), "")
    ## "Collector" (cor.collect) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    cor.collect <- c(unique(colEvent$Collector), "")
    ## "Method" (cor.method)
    cor.method <- c("canopy malaise", "ground malaise", "beating", "pitfall", "canopy clipping", 
                    "leaf litter", "Insectazooka", "soil extraction", "")
    ## "Plant" (cor.plant) (IGNORE, MUST UPDATE FROM SYNONYM TABLES)
    cor.plant <- c(unique(colEvent$Plant), "")
    ## "BeatingDuration" (cor.beat)
    cor.beat <- c(0:300, "")
    ## "PitFallSlice" (cor.pit)
    cor.pit <- c("up", "down", "A", "B", "C", "D", 
                 "E", "F", "ground", "")
    ## "Whereabouts" (cor.where)
    cor.where <- c("UHH", "Hilgard 220", "NEED TO FIND", "")
    ## "SamplingRound" (cor.sample)
    cor.sample <- c(1:2, "")
    ## "NoOfVials" (cor.vial)
    cor.vial <- c(1:3, "")
    
    ## List of the vectors of possible valid entries to columns being checked for misspellings
    cor.list <- list(cor.plot, cor.collect, cor.method, 
                     cor.plant, cor.beat, cor.pit, 
                     cor.where, cor.sample, cor.vial)
    
    ## Vector of the different column names, excluding columns not being checked for misspellings
    misspelled.columns <- c("Plot", "Collector", "Method", "Plant", 
                            "BeatingDuration", "PitFallSlice",
                            "Whereabouts", "SamplingRound", "NoOfVials")
    
    return(mapply(.misColumn, misspelled.columns, cor.list))
}

## Helper function
.misColumn <- function(column, vector){
  indice.misspelled <- (which(!db[, column] %in% vector))
  return(db[indice.misspelled,]$HDIM)
}

