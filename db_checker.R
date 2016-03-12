# package to interface with web
library(RCurl)
# package to play around with dataframes
install.packages("funModeling")
# retrive sheets from google drive (requires sheet be published as a CSV)
colEvent <- getURL('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?gid=0&single=true&output=csv')
colEvent <- read.csv(textConnection(colEvent))
# convert factors to characters
colEvent[] <- lapply(colEvent, as.character)
# convert null values to 0
colEvent[is.na(colEvent)] <- ""
# diagnostic functions to keep in handy
sapply(colEvent, class)
str(colEvent)
head(colEvent)
db_status <- df_status(colEvent)

# create dataframes of columns with empty values in "Plot, Date, Collector,
# Method, Whereabouts, SamplingRound, NoOfVials" return corrected indices of
# the rows of empty column
empt_hdi <- which(colEvent[,"HDIM"] == "") + 1
empt_plo <- which(colEvent[,"Plot"] == "") + 1
empt_dat <- which(colEvent[,"Date"] == "") + 1
empt_col <- which(colEvent[,"Collector"] == "") + 1
empt_met <- which(colEvent[,"Method"] == "") +1
empt_whe <- which(colEvent[,"Whereabouts"] == "") + 1
empt_sam <- which(colEvent[,"SamplingRound"] == "") + 1
empt_via <- which(colEvent[,"NoOfVials"] == "") + 1

# create dataframe of beating entries; create vector of row indices of said 
# entries
beat_ind <- which(colEvent[,"Method"] == "beating")
# find indices of empty entries of any of the four beating information columns
beat_pla <- which(colEvent[,"Plant"] == "")
beat_dur <- which(colEvent[,"BeatingDuration"] == "")
beat_beg <- which(colEvent[,"TimeBegin"] == "")
beat_end <- which(colEvent[,"TimeEnd"] == "")
# combine vectors of indices of empty entries of beating information colums;
# create vector of unique index values
beat_var <- unique(c(beat_end, beat_beg, beat_dur, beat_pla))
# combine vector of the indices of empty beating information indices with the
# beating rows indices and isolate the duplicate values; adjust for accuracy
beat_emp <- c(beat_ind, beat_var)
empt_bea <- sort(unique(beat_emp[duplicated(beat_emp)]) + 1)

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
metavector <- c("Plant", "BeatingDuration", "TimeBegin", "TimeEnd")
IndiceMethod(colEvent, "Method", "beating", metavector)

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
correct_where <- c("BERKELEY", "Berkeley", "UHH", "Hilgard 220", "Hilo Boys", "Hilo Boys (in packing box)", "NNNNN",  "")
IndiceMisspelled(colEvent, "Whereabouts", correct_where)

ListInvalid <- function(dataframe, vector){
  # Creates list of row indices of empty entries in multiple columns
  # 
  # Args:
  #   dataframe: The name of the target dataframe.
  #   vector: The vector of names of target columns within the dataframe.
  # 
  # Returns:
  #   List of vectors of sorted empty row indices named by the targeted column.
  return(apply(dataframe[, vector], 2, function(x) which(x == "") + 1))
}
columnvector <- c("HDIM", "Plot", "Date", "Collector", "Method", "Whereabouts", "SamplingRound", "NoOfVials")
ListInvalid(colEvent, columnvector)

# 03.10.16 NOTES FROM MEETING WITH LIM - VLSB 5056
#   
# modfy functions to return HDIM number instead of row indices - 
# modify empty_list function to return adjusted row indices - DONE
# look at Google R Style Guide - DONE
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
HDIMmethod(colEvent, "Method", "beating", metavector)

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
    indice.misspelled <- (which(!dataframe[, column] %in% vector) + 1)
    return(dataframe[indice.misspelled,]$HDIM)
}
correct_where <- c("BERKELEY", "Berkeley", "UHH", "Hilgard 220", "Hilo Boys", "Hilo Boys (in packing box)", "NNNNN",  "")
HDIMmisspelled(colEvent, "Whereabouts", correct_where)

