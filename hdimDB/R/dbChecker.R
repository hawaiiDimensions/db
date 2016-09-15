#' @title Checks Dimensions Database for errors
#'  
#' @description \code{dbChecker} runs through the online database and returns a list of HDIM numbers associated with specific errors
#' 
#' @details Developed specifically for the Dimensions in Biodiversity Evolab Database.
#' 
#' @param None
#' 
#' @return A multi-leveled list of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export
#' 


dbChecker <- function(){
    # Customized for Dimensions in Biodiversity database 'colEvent'.
    # Thoroughly checks the Dimensions database for invalid and missing entries.
    #
    # Args;
    #   dataframe: The name of the target dataframe; 'colEvent'.
    #
    # Returns:
    #   List of vectors of HDIM numbers corresponding to invalid database entries.
    duplicate.hdim <- IndiceDuplicated(dataframe, "HDIM")
    empty.columns <- c("HDIM", "Plot", "Date", "Collector", "Method", 
                       "Whereabouts", "SamplingRound", "NoOfVials")
    misspelled.columns <- colEvent[c("Plot", "Collector", "Method", "Plant", 
                                     "BeatingDuration", "PitFallSlice", 
                                     "Whereabouts", "SamplingRound", "NoOfVials")]
    correct.list <- list(correct.plot, correct.collector, correct.method, 
                         correct.plant, correct.beatingduration, 
                         correct.pitfallslice, correct.where, correct.samplerd,
                         correct.vialno)
    methods <- c("beating", "pitfall", "litter", "canopy malaise", 
                 "ground malaise", "Insectazooka", "soil extraction")
    contingent.list <- list(beating.columns, pitfall.columns, litter.columns,
                            canopy.malaise.columns, ground.malaise.columns,
                            Insectazooka.columns, soil.extraction.columns) 
    empty.list <- ListEmptyHDIM(colEvent, empty.columns)
    empty.method <- ListEmptyMethod(methods, contingent.list)
    misspelled.list <- ListMisspelledHDIM(misspelled.columns, correct.list)
    invalid.time <- list(InvalidDateHDIM(colEvent, "Date")
                         , InvalidMethodDateHDIM(colEvent, "DateEnd", "%m/%d/%Y")
                         , InvalidMethodDateHDIM(colEvent, "TimeBegin", "h:m")
                         , InvalidMethodDateHDIM(colEvent, "TimeEnd", "h:m"))
    results <- (list(duplicated = duplicate.hdim, empty = empty.list, 
                     emptyMethod = empty.method, misspelled = misspelled.list, 
                     invalidTime = invalid.time))
    return(results)
}  