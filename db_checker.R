# package to interface with web
library(RCurl)

# download packages to play around with dataframes
# install.packages("funModeling")
# install.packages("pryr")

# retrieve Collection Events from Dimensions Google drive as .csv
colEvent <- getURL('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?gid=0&single=true&output=csv')
colEvent <- read.csv(textConnection(colEvent))
colEvent[] <- lapply(colEvent, as.character)
colEvent[is.na(colEvent)] <- ""

# retrieve Site Info from Dimensions Google Drive as .csv
siteInfo <- getURL('https://docs.google.com/spreadsheets/d/1EGeeVTpk4wPxigOwrI2TGviZram9FSo87BKbPBED7gw/pub?gid=0&single=true&output=csv')
siteInfo <- read.csv(textConnection(siteInfo))
siteInfo[] <- lapply(siteInfo, as.character)
siteInfo[is.na(siteInfo)] <- ""

# diagnostic functions to keep in handy
sapply(colEvent, class)
str(colEvent)
head(colEvent)
db.status <- df_status(colEvent)

# create dataframes of columns with empty values in "Plot, Date, Collector,
# Method, Whereabouts, SamplingRound, NoOfVials" return corrected indices of
# the rows of empty column
empt.hdi <- which(colEvent[,"HDIM"] == "") + 1
empt.plo <- which(colEvent[,"Plot"] == "") + 1
empt.dat <- which(colEvent[,"Date"] == "") + 1
empt.col <- which(colEvent[,"Collector"] == "") + 1
empt.met <- which(colEvent[,"Method"] == "") +1
empt.whe <- which(colEvent[,"Whereabouts"] == "") + 1
empt.sam <- which(colEvent[,"SamplingRound"] == "") + 1
empt.via <- which(colEvent[,"NoOfVials"] == "") + 1

# create dataframe of beating entries; create vector of row indices of said 
# entries
beat.ind <- which(colEvent[,"Method"] == "beating")
# find indices of empty entries of any of the four beating information columns
beat.pla <- which(colEvent[,"Plant"] == "")
beat.dur <- which(colEvent[,"BeatingDuration"] == "")
beat.beg <- which(colEvent[,"TimeBegin"] == "")
beat.end <- which(colEvent[,"TimeEnd"] == "")
# combine vectors of indices of empty entries of beating information colums;
# create vector of unique index values
beat.var <- unique(c(beat.end, beat.beg, beat.dur, beat.pla))
# combine vector of the indices of empty beating information indices with the
# beating rows indices and isolate the duplicate values; adjust for accuracy
beat.emp <- c(beat.ind, beat.var)
empt.bea <- sort(unique(beat.emp[duplicated(beat.emp)]) + 1)

# =============================================================================

IndiceEmpty <- function(dataframe, column) {
  # Extracts row indices of all empty entries by column.
  # 
  # Args: 
  #   dataframe: The name of the target dataframe.
  #   column: The name of the target column within the dataframe.
  #
  # Returns:
  #   Vector of sorted indices of empty entries in a column. 
    return(which(dataframe[, column] == "") + 1)
}
IndiceEmpty(colEvent, "Whereabouts")

# =============================================================================

IndiceMethod <- function(dataframe, column, method, vector) {
  # Extracts row indices of empty entries contingent to method.
  #
  # Args:
  #   dataframe: The name of the target dataframe.
  #   column: The name of the method column within the dataframe.
  #   method: The name of the target method in the method column.
  #   vector: The vector of the names of the contingent columns
  #           to the target method.
  #   
  # Returns:
  #   Vector of sorted row indices of empty entries in all columns contingent
  #   to the target method.
  method.ind <- which(dataframe[, column] == method)
  method.vec <- apply(dataframe[vector], 2, function(x) which(x == ""))
  empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE)))
  empty.met <- sort(unique(empty.ind[duplicated(empty.ind)]) + 1)
  return(empty.met)
}
beat.vector <- c("Plant", "BeatingDuration", "TimeBegin", "TimeEnd")
IndiceMethod(colEvent, "Method", "beating", beat.vector)

# =============================================================================

IndiceMisspelled <- function(dataframe, column, vector){
  # Extracts row indices of misspelled entries by column.
  # 
  # Args:
  #   dataframe: The name of the target dataframe.
  #   column: The name of the target column within the dataframe.
  #   vector: A vector of the accepted entries for the target column.
  #   
  # Returns: 
  #   Vector of sorted row indices of misspelled entries within a column.
  return(which(!dataframe[, column] %in% vector) + 1)
}
correct.where <- c("BERKELEY", "Berkeley", "UHH", "Hilgard 220", "Hilo Boys", "Hilo Boys (in packing box)", "NNNNN",  "")
IndiceMisspelled(colEvent, "Whereabouts", correct.where)

# =============================================================================

ListEmptyIndice <- function(dataframe, vector){
  # Creates list of row indices of empty entries in multiple columns.
  # 
  # Args:
  #   dataframe: The name of the target dataframe.
  #   vector: The vector of names of target columns within the dataframe.
  # 
  # Returns:
  #   List of vectors of sorted empty row indices named by the targeted column.
  return(apply(dataframe[, vector], 2, function(x) which(x == "") + 1))
}
empty.vector <- c("HDIM", "Plot", "Date", "Collector", "Method", "Whereabouts", "SamplingRound", "NoOfVials")
ListEmptyIndice(colEvent, empty.vector)

# =============================================================================

HDIMempty <- function(dataframe, column){
  # Extracts HDIM numbers of empty entries within a target column.
  # 
  # Args: 
  #   dataframe: The name of the target dataframe.
  #   column: The name of the target column.
  # 
  # Returns: 
  #   Vector of HDIM numbers of empty entries within a column.
  return(dataframe[which(dataframe[, column] == ""),]$HDIM)
}
HDIMempty(colEvent, "Whereabouts")

# =============================================================================

HDIMmethod <- function(dataframe, column, method, vector){
  # Extracts HDIM numbers of empty entries contingent to method.
  #
  # Args:
  #   dataframe: The name of the target dataframe.
  #   column: The name of the method column within the dataframe.
  #   method: The name of the target method in the method column.
  #   vector: The vector of the names of the contingent columns
  #           to the target method.
  #   
  # Returns:
  #   Vector of HDIM numbers of empty entries in all columns contingent
  #   to the target method.
  method.ind <- which(dataframe[, column] == method)
  method.vec <- apply(dataframe[vector], 2, function(x) which(x == ""))
  empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE)))
  empt.met <- (dataframe[unique(empty.ind[duplicated(empty.ind)]), ]$HDIM)
  return(empt.met)
}
HDIMmethod(colEvent, "Method", "beating", beat.vector)

# =============================================================================

HDIMmisspelled <- function(dataframe, column, vector){
  # Extracts HDIM numbers of misspelled entries by column.
  # 
  # Args:
  #   dataframe: The name of the target dataframe.
  #   column: The name of the target column within the dataframe.
  #   vector: A vector of the accepted entries for the target column.
  #   
  # Returns: 
  #   Vector of HDIM numbers of misspelled entries within a column.
  indice.misspelled <- (which(!dataframe[, column] %in% vector))
  return(dataframe[indice.misspelled,]$HDIM)
}
correct.where <- c("BERKELEY", "Berkeley", "UHH", "Hilgard 220", "Hilo Boys", "Hilo Boys (in packing box)", "NNNNN",  "")
HDIMmisspelled(colEvent, "Whereabouts", correct.where)

# =============================================================================

ListEmptyHDIM <- function(dataframe, vector){
  # Creates list of HDIM numbers of empty entries in multiple columns.
  # 
  # Args:
  #   dataframe: The name of the target dataframe.
  #   vector: The vector of names of target columns within the dataframe.
  # 
  # Returns:
  #   List of vectors of HDIM numbers named by the targeted column.
    return(apply(dataframe[, vector], 2, 
                 function(x) dataframe[which(x == ""), ]$HDIM))
}
empty.columns <- c("HDIM", "Plot", "Date", "Collector", "Method", 
                   "Whereabouts", "SamplingRound", "NoOfVials")
ListEmptyHDIM(colEvent, empty.columns)

# =============================================================================

InvalidDateInd <- function(dataframe, date.column){
  # Extracts indices of invalid date entries in the date column of a dataframe.
  #
  # Args:
  #   dataframe: The name of the target dataframe.
  #   date.column: The name of the target date column.
  #  
  # Returns:
  #   Numerical vector of indices of invalid date entries in the columm.
  dates <- (as.Date(dataframe[, date.column], format = "%m/%d/%Y" ))
  return(which(is.na(as.character(dates)) == "TRUE") + 1)
}
InvalidDateInd(colEvent, "Date") # Only a rudimentary date format check.

# =============================================================================

InvalidDateHDIM <- function(dataframe, date.column){
  # Retrieves HDIM numbers of invalid date entries in a dataframe date column.
  #
  # Args:
  #   dataframe: The name of the target dataframe.
  #   date.column: The name of the target date column.
  #  
  # Returns:
  #   Numerical vector of HIDM numbers of invalid date entries in the columm.
  dates <- (as.Date(dataframe[, date.column], format = "%m/%d/%Y" ))
  dates.indices <- which(is.na(as.character(dates)) == "TRUE")
  return(dataframe[dates.indices,]$HDIM)
}
InvalidDateHDIM(colEvent, "Date")

# =============================================================================

StoreDb <- function(dataframe, url){
  # Imports .csv from a URL as a database; formats for use with db package.
  #
  # Args:
  #   database: The name that the dataframe will be called.
  #   url: The web address of the .csv file.
  # 
  # Returns:
  #   A formatted dataframe from the database file hosted online.
  library(RCurl)
  dataframe <- getURL(url)
  dataframe <- read.csv(textConnection(dataframe))
  dataframe[] <- lapply(dataframe, as.character)
  dataframe[is.na(dataframe)] <- ""
  return(dataframe)
}
StoreDb(siteInfo, 'https://docs.google.com/spreadsheets/d/1EGeeVTpk4wPxigOwrI2TGviZram9FSo87BKbPBED7gw/pub?gid=0&single=true&output=csv')

# =============================================================================

ListMisspelledHDIM <- function(mispelled.columns, correct.list){
  # Extracts HDIM numbers of misspelled entries by columns.
  # 
  # Args:
  #   dataframe: The name of the target dataframe.
  #   mispelled.columns: The list of target columns within the dataframe.
  #   correct.list: A list of the accepted entries for the target column.
  #   
  # Returns: 
  #   List of vectors of HDIM numbers of misspelled entries by column.
  return(mapply(PatternHDIM, misspelled.columns, correct.list))
}
misspelled.columns <- colEvent[c("Plot", "Collector", "Method", "Plant", 
                                 "BeatingDuration", "PitFallSlice", 
                                 "Whereabouts", "SamplingRound", "NoOfVials")]
correct.plot <- c(unique(siteInfo$Plot), "")
correct.collector <- c(unique(colEvent$Collector), "")
correct.method <- c(unique(colEvent$Method), "")
correct.plant <- c(unique(colEvent$Plant), "")
correct.beatingduration <- c(0:120, "")
correct.pitfallslice <- c("Up", "Down", "A", "B", "C", "D", "E", "F", "Ground"
                          , "")
correct.where <- c("BERKELEY", "Berkeley", "UHH", "Hilgard 220", "Hilo Boys",
                   "Hilo Boys (in packing box)", "NNNNN",  "")
correct.samplerd <- c(1:2, "")
correct.vialno <- c(1:2, "")
correct.list <- list(correct.plot, correct.collector, correct.method, 
                     correct.plant, correct.beatingduration, 
                     correct.pitfallslice, correct.where, correct.samplerd,
                     correct.vialno)
ListMisspelledHDIM(misspelled.columns, correct.list)

# =============================================================================

PatternHDIM <- function(column, vector){
  # Finds HDIM locations of pattern mismatches within a colEvent column.
  #
  # Args:
  #   column: The targeted colEvent column
  #   vector: The pattern to be matched, as a vector.
  #  
  # Returns: HDIM indices of pattern matches within the vector.
  return(colEvent[which(!column %in% vector), ]$HDIM)
}
PatternHDIM(colEvent[, "Whereabouts"], correct.where)

# =============================================================================

colEventMethod <- function(method, contingent.list){
  # colEvent specific version of HDIMmethod with fewer arguments.
  # 
  # Args:
  #   method: The name of the target collection method.
  #   contingent.list: Vector of contingent columns to the target method.
  #
  # Returns: 
  #   Vector of HDIM numbers of the empty entries corresponding to a method.
  method.ind <- which(colEvent[, "Method"] == method)
  method.vec <- apply(colEvent[contingent.list], 2, function(x) which(x == ""))
  empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE)))
  return(colEvent[unique(empty.ind[duplicated(empty.ind)]),]$HDIM)
}
colEventMethod("beating", beating.columns)

# =============================================================================

ListEmptyMethod <- function(method, contingent.list){
  # Finds HDIM numbers of empty entries contingent to collection methods.
  #
  # Args:
  #   method: The name of the target collection method.
  #   contingent.column: Vector of contingent columns to the target method.
  #
  # Returns:
  #   A list of vectors corresponding to HDIM numbers of the empty entries 
  #   contingent to collection method.
  return(mapply(colEventMethod, method, contingent.list))
}
methods<- c("beating", "pitfall", "litter", "canopy malaise", "ground malaise",
            "Insectazooka", "soil extraction")
beating.columns <- c("Plant", "BeatingDuration", "TimeBegin", "TimeEnd")
pitfall.columns <- c("DateEnd", "PitFallSlice")
litter.columns <- "PitFallSlice"
canopy.malaise.columns <- c("DateEnd", "PitFallSlice")
ground.malaise.columns <- c("DateEnd", "PitFallSlice")
Insectazooka.columns <- "PitFallSlice"
soil.extraction.columns <- "PitFallSlice"
contingent.list <- list(beating.columns, pitfall.columns, litter.columns,
                        canopy.malaise.columns, ground.malaise.columns,
                        Insectazooka.columns, soil.extraction.columns)
ListEmptyMethod(methods, contingent.list)

# =============================================================================

InvalidMethodDateHDIM <- function(dataframe, date.column, date.format){
    # Retrieves HDIM numbers of invalid date entries in a method column.
    #
    # Args:
    #   dataframe: The name of the target dataframe.
    #   date.column: The name of the target date column.
    #   date.format: The format of the date for which is being tested.
    #  
    # Returns:
    #  Vector of HIDM numbers of invalid date entries in the columm.
    empty.dates <- which(dataframe[, date.column] != "")
    dates <- as.Date(dataframe[, date.column], format = date.format )
    dates.indices <- which(is.na(as.character(dates)) == "TRUE")
    dates.vector <- c(empty.dates, dates.indices)
    return(dataframe[unique(dates.vector[duplicated(dates.vector)]), ]$HDIM)
}
InvalidMethodDateHDIM(colEvent, "DateEnd", "%m/%d/%Y")

# =============================================================================

DiagnoseDb <- function(dataframe, empty.columns, misspelled.columns, 
                       correct.list, methods, contingent.list){
  # Customized for colEvent
  # Thoroughly checks the Dimensions database for invalid and missing entries.
  #
  # Args;
  #   dataframe: The name of the target dataframe.
  #   empty.columns: Columns marked to be checked for missing entries.
  #   misspelled.columns: Columns marked to be checked for misspellings.
  #   correct.list: List of correct entries to check for misspellings.
  #   methods: Vector of method names to be checked.
  #   contingent.list: Contingent columns to the methods to be checked.
  #
  # Returns:
  #   List of vectors of HDIM numbers corresponding to invalid database entries.
  duplicate.hdim <- IndiceDuplicated(dataframe, "HDIM")
  empty.list <- ListEmptyHDIM(dataframe, empty.columns)
  empty.method <- ListEmptyMethod(methods, contingent.list)
  misspelled.list <- ListMisspelledHDIM(misspelled.columns, correct.list)
  invalid.time <- list(InvalidDateHDIM(colEvent, "Date")
                       , InvalidMethodDateHDIM(dataframe, "DateEnd", "%m/%d/%Y")
                       , InvalidMethodDateHDIM(dataframe, "TimeBegin", "h:m")
                       , InvalidMethodDateHDIM(dataframe, "TimeEnd", "h:m"))
  results <- (list(duplicate.hdim, empty.list, empty.method, misspelled.list, invalid.time))
  return(results)
}  
empty.columns <- c("HDIM", "Plot", "Date", "Collector", "Method", 
                   "Whereabouts", "SamplingRound", "NoOfVials")
misspelled.columns <- colEvent[c("Plot", "Collector", "Method", "Plant", 
                                 "BeatingDuration", "PitFallSlice", 
                                 "Whereabouts", "SamplingRound", "NoOfVials")]
correct.list <- list(correct.plot, correct.collector, correct.method, 
                     correct.plant, correct.beatingduration, 
                     correct.pitfallslice, correct.where,correct.samplerd,
                     correct.vialno)
methods <- c("beating", "pitfall", "litter", "canopy malaise", "ground malaise", 
             "Insectazooka", "soil extraction")
contingent.list <- list(beating.columns, pitfall.columns, litter.columns,
                        canopy.malaise.columns, ground.malaise.columns,
                        Insectazooka.columns, soil.extraction.columns) 
DiagnoseDb(colEvent, empty.columns, misspelled.columns, 
           correct.list, methods, contingent.list)

str(DiagnoseDb(colEvent, empty.columns, misspelled.columns, correct.list, 
               methods, contingent.list))

# =============================================================================

IndiceDuplicated <- function(dataframe, column){
    # Extracts row indices of duplicate entries within a target column.
    # 
    # Args: 
    #   dataframe: The name of the target dataframe.
    #   column: The name of the target column.
    # 
    # Returns: 
    #   Vector of indices of duplicated entries within a column.
    return(which(duplicated(dataframe[, column])))
}
IndiceDuplicated(colEvent, "HDIM")

# =============================================================================

DiagnoseDimensions <- function(dataframe){
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
# DiagnoseDimensions(colEvent)
# 
# str(DiagnoseDb(colEvent, empty.columns, misspelled.columns, correct.list, methods, contingent.list))
# str(DiagnoseDimensions(colEvent))
# 
# # =============================================================================
#  
# HDIMduplicated <- function(dataframe, column){
#   # Extracts HDIM indices of duplicate entries within a target column.
#   # 
#   # Args: 
#   #   dataframe: The name of the target dataframe.
#   #   column: The name of the target column.
#   # 
#   # Returns: 
#   #   Vector of HDIM numbers of duplicated entries within a column.
#   return(dataframe[which(duplicated(dataframe[, column])),]$HDIM)
# }
# HDIMduplicated(colEvent, "HDIM")
# 
# # =============================================================================
# 
# CorrectColumn <- function(dataframe, column, correct.vector){
#   # Makes new dataframe of correctehttps://blog.ouseful.info/2014/12/12/seven-ways-of-running-ipython-notebooks/d misspellings within a dataframe column.
#   #
#   # Args:
#   #   dataframe: Name of the target dataframe.
#   #   column: Name of the target column.
#   #   
#   # Returns:
#   #   Defined datarame column corrected for misspellings in the global environment.
#   corrected.dataframe <- dataframe
#   corrected.vector <-   as.character(correct.vector[correct.vector != c("")])
#   sapply(corrected.vector, function(x) {
#       m <- agrep(x, corrected.dataframe[, column])
#       corrected.dataframe[, column][m] <- x
#     })
#   return(corrected.dataframe[, column])
# }
# CorrectColumn(colEvent, "Whereabouts", correct.where)
# 
# # =============================================================================
# 
# ## SUMMER 2016 WORKSPACE
# 
# # =============================================================================
# 
# install.packages("googlesheets")
# 
