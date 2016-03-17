# ackage to interface with web
library(RCurl)

# download packages to play around with dataframes
install.packages("funModeling")
install.packages("pryr")

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

# 03.10.16 NOTES FROM MEETING WITH LIM - VLSB 5056
#   
# modfy functions to return HDIM number instead of row indices
# modify empty_list function to return adjusted row indices
# look at Google R Style Guide 
# date.mispelling function -> library(stringr) 
# str_split() can unpack date entries into a new dataframe for analysis
# Use Jupyter notebook to initialize code and to introduce package to laymen
# Comprehensive list of Plot names located in Siteinfo Google Drive file 
# use source() to initialize all relevant function from a seperate script 
# file (.R) 
# output invalid entry information in a comprehensive list that can be 
# returned with a wrapper function
# 
# 03.10.16 NOTES FROM MEETING WITH ROMINGER - HILGARD 305
# Function to return list of all mispellings and empty entries by HDIM number 
# Ways to source functions
# list.files(file_location)
# oldwd() < - setwd(new_directory)
# files2load <- c(file1, file2, file3)
# lapply(files2load, source)
# source(), setwd(oldwd)
# STOP 

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
  empt.met <- (dataframe[unique(empty.ind[duplicated(empty.ind)]),]$HDIM)
  return(empt.met)
}
HDIMmethod(colEvent, "Method", "beating", beat.vector)

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
HDIMmisspelled(colEvent, "Plot", correct.where)  # Output indicates inconsistent data entry format

ListEmptyHDIM <- function(dataframe, vector){
  # Creates list of HDIM numbers of empty entries in multiple columns.
  # 
  # Args:
  #   dataframe: The name of the target dataframe.
  #   vector: The vector of names of target columns within the dataframe.
  # 
  # Returns:
  #   List of vectors of HDIM numbers named by the targeted column.
    return(apply(dataframe[, vector], 2, function(x) dataframe[which(x == ""), ]$HDIM))
}
columnvector <- c("HDIM", "Plot", "Date", "Collector", "Method", "Whereabouts", "SamplingRound", "NoOfVials")
ListEmptyHDIM(colEvent, columnvector)

UniqueEntries <- function(dataframe, column){
  # Extracts all unique elements of a column within a dataframe.
  # 
  # Args:
  #   dataframe: The name of the target dataframe.
  #   column: The name of the column from which the values are to be extracted.
  #
  # Returns:
  #   Vector of unique elements of the target column within the dataframe.
  return(unique(c(unlist(dataframe[, column]))))
}
UniqueEntries(siteInfo, "Plot")
correct.plot <- unique(c(unlist(siteInfo[, "Plot"]))) 
# Vector of correct plots from Site Info file - dissimilar to plots listed in
# the google drive file "Collection Events".
correct.method <- unique(c(unlist(colEvent[, "Method"]))) 
# Vector of unvalidated correct method names

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


