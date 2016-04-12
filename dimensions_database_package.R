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
UniqueEntries # Extracts all unique elements of a column within a dataframe.
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

CorrectColumn <- function(dataframe, column, correct.vector){
    # Makes new dataframe of corrected misspellings within a dataframe column.
    #
    # Args:
    #   dataframe: Name of the target dataframe.
    #   column: Name of the target column.
    #   
    # Returns:
    #   Defined datarame column corrected for misspellings in the global environment.
    corrected.dataframe <- dataframe
    corrected.vector <-   as.character(correct.vector[correct.vector != c("")])
    sapply(corrected.vector, function(x) {
        m <- agrep(x, corrected.dataframe[, column])
        corrected.dataframe[, column][m] <<- x
    })
    return(corrected.dataframe[, column])
}
CorrectColumn(colEvent, "Whereabouts", correct.where)
corrected.dataframe["Whereabouts"]
HDIMmisspelled(corrected.dataframe, "Whereabouts", correct.where)
# =============================================================================

corrected.dataframe[, "Whereabouts"]
corrected.where <- as.character(correct.where[correct.where != c("")])
sapply(corrected.where, function(x) {
    m <- agrep(x, corrected.dataframe[, "Whereabouts"])
    corrected.dataframe[m, "Whereabouts"] <<- x
})

# matches <- agrep(correct.vector[x], corrected.dataframe[, column], max.distance = 0.1)
# corrected.dataframe[matches, column] <- replace(corrected.dataframe[matches, column], 
#                                                 c(0:length(corrected.dataframe[matches, column])), 
#                                                 correct.vector[x])

corrected.dataframe <- colEvent
corrected.where

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
  corrected.dataframe <- dataframe
  corrected.list <- sapply(correct.list, function(x){
      as.character(x[x != c("")])
      })
  sapply(columns, function(x){
      mapply(CorrectColumn, corrected.dataframe[, x], corrected.list, 
             MoreArgs = dataframe = corrected.dataframe)
  })
  return(corrected.dataframe)
 }
