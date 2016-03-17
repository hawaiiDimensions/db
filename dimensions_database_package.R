IndiceEmpty # Extracts row indices of all empty entries by column.
IndiceMethod # Extracts row indices of empty entries contingent to method.
IndiceMisspelled # Extracts row indices of misspelled entries by column.
ListEmptyIndice # Creates list of row indices of empty entries in multiple columns.

HDIMempty # Extracts HDIM numbers of empty entries within a target column.
HDIMmethod # Extracts HDIM numbers of empty entries contingent to method.
HDIMmisspelled # Extracts HDIM numbers of misspelled entries by column.
ListEmptyHDIM # Creates list of HDIM numbers of empty entries in multiple columns.

UniqueEntries # Extracts all unique elements of a column within a dataframe.
InvalidDateInd # Extracts indices of invalid date entries in the date column of a dataframe.
InvalidDateHDIM # Retrieves HDIM numbers of invalid date entries in a dataframe date column.
StoreDb # Imports .csv from a URL as a database; formats for use with db package.
ListMisspelledHDIM # Extracts HDIM numbers of misspelled entries by columns.
PatternHDIM # Finds HDIM location of pattern mismatches within colEvent column.
PrimerMethod # Finds HDIM numbers of empty entries of a column contingent to a method.
ListEmptyMethod # Finds HDIM numbers of empty entries contingent to collection methods.


DiagnoseDb <- function(dataframe, empty.columns, misspelled.columns, method.columns){ # customized for colEvent
# Throroughly checks the Dimensions database for invalid and missing entries.
#
# Args;
#   dataframe: The name of the target dataframe.
#   empty.columns: Columns marked to be checked for missing entries.
#   misspelled.columns: Columns marked to be checked for misspellings.
#   method.columns: Columns marked for special columns.
#
# Returns:
#   List of vectors of HDIM numbers corresponding to invalid database entries.
empty.list <- ListEmptyHDIM(dataframe, empty.columns)
empty.method <- ListEmptyMethod(methods, contingent.list)
mispelled.list <- ListMisspelledHDIM(colEvent, misspelled.columns, correct.list)
invalid.date <- InvalidDateHDIM(colEvent, "Date")
invalid.method <- HDIMmisspelledMethod(contingent.list, correct.list)
return(list(empty.list, empty.mispelling, empty.method, invalid.date, invalid.method))
}  

MultiplePatternHDIM <- function(list1, list2){
# Finds HDIM locations of pattern mispatches in multiple lists.
#
# Args:
#   list1:
#   list2:
# 
# Returns:
#   List of HDIM indices of pattern mismatches in multiple column arrangements.
return(mapply(PatternHDIM, list1, list2))
}
MultiplePatternHDIM(misspelled.columns, correct.list)

HDIMmisspelledMethod <- function(contingent.list, correct.list){
return(by(colEvent[c("Plant", "BeatingDuration", "TimeBegin", 
                       "TimeEnd", "DateEnd", "PitFallSlice")], 
          colEvent["Method"], function(x) MultiplePatternHDIM(contingent.list, correct.list)))
}

str(HDIMmisspelledMethod(misspelled.columns, correct.list))

c("Plant", "BeatingDuration", "TimeBegin", 
   "TimeEnd", "DateEnd", "PitFallSlice"  
HDIMmisspelledMethod <- function(method, contingent.columns, correct.list){
# Finds HDIM numbers of misspellings within contingent method columns.
#  
# Args:
#
method.ind <- which(colEvent[, "Method"] == method)
method.vec <- apply(dataframe[contingent.columns], 2, 
                    function(x) which(!dataframe[, contingent.columns] %in% correct.list))
empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE)))
empt.met <- (dataframe[unique(empty.ind[duplicated(empty.ind)]),]$HDIM)
return(empt.met)
}

ListInvalidMethod <- function(method.columns, contingent.columns, correct.list){
    # Finds list of HDIM numbers of misspellings within contingent method columns.
    # 
    # Args:
    #   dataframe: The name of the target dataframe.
    #   mispelled.columns: The name of the target columns within the dataframe.
    #   correct.list: A list of the accepted entries for the target column.
    #   
    # Returns: 
    #   List of vectors of HDIM numbers of misspelled entries by column.
    return(mapply(PatternHDIM, misspelled.columns, correct.list))
}

misspelled.columns <- colEvent[c("Plot", "Collector", "Method", "Whereabouts", "SamplingRound", "NoOfVials")]
correct.plot <- c(UniqueEntries(siteInfo, "Plot"), "")
correct.collector <- c(UniqueEntries(colEvent, "Collector"), "")
correct.method <- c(UniqueEntries(colEvent, "Method"), "")
correct.where <- c("BERKELEY", "Berkeley", "UHH", "Hilgard 220", "Hilo Boys", "Hilo Boys (in packing box)", "NNNNN",  "")
correct.samplerd <- c(1:2, "")
correct.vialno <- c(1:2, "")
correct.list <- list(correct.plot, correct.collector, correct.method, correct.where, correct.samplerd, correct.vialno)
ListMisspelledHDIM(misspelled.columns, correct.list)
