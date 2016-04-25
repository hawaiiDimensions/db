# =============================================================================
IndiceEmpty # Extracts row indices of all empty entries by column.
HDIMempty # Extracts HDIM numbers of empty entries within a target column.
# =============================================================================
IndiceMethod # Extracts row indices of empty entries contingent to method.
HDIMmethod # Extracts HDIM numbers of empty entries contingent to method.
# =============================================================================
IndiceMisspelled # Extracts row indices of misspelled entries by column.
HDIMmisspelled # Extracts HDIM numbers of misspelled entries by column.
# =============================================================================
InvalidDateInd # Extracts indices of invalid date entries in the date column of a dataframe.
InvalidDateHDIM # Retrieves HDIM numbers of invalid date entries in a dataframe date column.
InvalidMethodDateHDIM # Retrieves HDIM numbers of invalid date entries in a method column.
# =============================================================================
ListEmptyIndice # Creates list of row indices of empty entries in multiple columns.
ListEmptyHDIM # Creates list of HDIM numbers of empty entries in multiple columns.
ListMisspelledHDIM # Extracts HDIM numbers of misspelled entries by columns.
ListEmptyMethod # Finds HDIM numbers of empty entries contingent to collection methods
# =============================================================================
StoreDb # Imports .csv from a URL as a database; formats for use with db package.
PatternHDIM # Finds HDIM locations of pattern mismatches within a colEvent column.
colEventMethod # Evolab specific version of HDIMmethod with fewer arguments.
# =============================================================================
DiagnoseDb # Throroughly checks a Dimensions database for invalid and missing entries.
DiagnoseDimensions # Throroughly checks the colEvent database for invalid and missing entries.
# =============================================================================
IndiceDuplicated # Extracts vector of row indices of duplicate entries within a target column.
HDIMduplicated # Extracts vector of HDIM indices of duplicate entries within a target column.
# =============================================================================
CorrectColumn # Makes new dataframe of corrected misspellings within a dataframe column.

CorrectDataframe <- function(dataframe, columns, correct.list){
  # Makes new dataframe of corrected misspellings within a dataframe column.
  #
  # Args:
  #   dataframe: Name of the target dataframe.
  #   columns: Vector of names of the target columns.
  #   correct.list: A list of vectors of correct spellings corresponding to 
  #                 the names of the target columns
  #   
  # Returns:
  #   Defined dataframe column corrected for misspellings in the global environment.
  corrected.dataframe <- mapply(CorrectColumn, dataframe = dataframe, column=columns, correct.vector=correct.list)
  return(corrected.dataframe)
 }
CorrectDataframe(colEvent, foo, correct.list)
foo <- c("Plot", "Collector", "Method", "Plant", 
         "BeatingDuration", "PitFallSlice", 
         "Whereabouts", "SamplingRound", "NoOfVials")

correct.list

corrected.dataframe <- dataframe{
corrected.list <- sapply(correct.list, function(x){
    as.character(x[x != c("")])
})
sapply(columns, function(x){
    mapply(CorrectColumn, corrected.dataframe[, x], corrected.list)
}, dataframe = corrected.dataframe)
return(corrected.dataframe)
}
