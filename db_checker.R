## package to interface with web
library(RCurl)
## package to play around with dataframes
install.packages("funModeling")
## retrive sheets from google drive (requires sheet be published as a CSV)
colEvent <- getURL('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?gid=0&single=true&output=csv')
colEvent <- read.csv(textConnection(colEvent))
## convert to dataframe
colEvent <- table(colEvent)
## convert factors to characters
colEvent[] <- lapply(colEvent, as.character)
## convert na values to 0
colEvent[is.na(colEvent)] <- ""
## diagnostic functions to keep in handy
sapply(colEvent, class)
str(colEvent)
head(colEvent)
db_status <- df_status(colEvent)
## create dataframe of row with empty values in "Plot"
## return corrected indices of the rows of empty plot columns
empt_plo <- colEvent[,"Plot"] == ""
EMPTY_PLOT <- which(empt_plo, arr.ind = TRUE, useNames = TRUE) + 1
EMPTY_PLOT
## create dataframe of row with empty values in "Date"
## return corrected indices of the rows of empty date columns
empt_dat <- colEvent[,"Date"] == ""
EMPTY_DATE <- which(empt_dat, arr.ind = TRUE, useNames = TRUE) + 1
EMPTY_DATE
## create dataframe of rows with empty values in "Collector"; 
## return corrected indices of the rows of empty collector columns
empt_col <- colEvent[,"Collector"] == ""
EMPTY_COLLECTOR <- which(empt_col, arr.ind = TRUE, useNames = TRUE) + 1
EMPTY_COLLECTOR
## create dataframe of rows with empty values in "Method";
## return corrected indices of the row of empty method columns
empt_met <- colEvent[,"Method"] == ""
EMPTY_METHOD <- which(empt_met, arr.ind = TRUE, useNames = TRUE) +1
EMPTY_METHOD
## create dataframe of rows with empty values in "Whereabouts"; 
## return corrected indices of the rows of empty whereabouts columns
empt_whe <- colEvent[,"Whereabouts"] == ""
EMPTY_WHEREABOUTS <- which(empt_whe, arr.ind = TRUE, useNames = TRUE) + 1
EMPTY_WHEREABOUTS
## create dataframe of rows with empty values in "SamplingRound"; 
## return corrected indices of the rows of empty smapling round columns
empt_sam <- colEvent[,"SamplingRound"] == ""
EMPTY_SAMPLINGROUND <- which(empt_sam, arr.ind = TRUE, useNames = TRUE) + 1
EMPTY_SAMPLINGROUND
## create dataframe of rows with empty values in "NoOfVials"; 
## return corrected indices of the rows of empty vial number columns
empt_via <- colEvent[,"NoOfVials"] == ""
EMPTY_VIALNUM <- which(empt_via, arr.ind = TRUE, useNames = TRUE) + 1
EMPTY_VIALNUM
## create dataframe of beating entries, create vector of 
## row indices of said entries
beat_col <- colEvent[,"Method"] == "beating"
beat_ind <- which(beat_col, arr.ind = TRUE, useNames = TRUE)
## find indices of empty entries of any of the four beating 
## information columns
beat_pla <- colEvent[,"Plant"] == ""
empt_pla <- which(beat_pla, arr.ind = TRUE, useNames = TRUE)
beat_dur <- colEvent[,"BeatingDuration"] == ""
empt_dur <- which(beat_dur, arr.ind = TRUE, useNames = TRUE)
beat_beg <- colEvent[,"TimeBegin"] == ""
empt_beg <- which(beat_beg, arr.ind = TRUE, useNames = TRUE)
beat_end <- colEvent[,"TimeEnd"] == ""
empt_end <- which(beat_end, arr.ind = TRUE, useNames = TRUE)
## combine vectors of indices of empty entries of
## beating information colums and create vector of 
## unique index values
beat_var <- c(empt_end, empt_beg, empt_dur, empt_pla)
uni_beat_var <- unique(beat_var)
## combine vector of the indices of empty beating information
## indices with the beating rows indices and isolate the 
## duplicate values; adjust for accuracy
empt_beat <- c(beat_ind, uni_beat_var)
EMPTY_BEATING <- unique(empt_beat[duplicated(empt_beat)]) + 1
EMPTY_BEATING
## function to extract indices of empty row entries
empty_indices <- function(column, dataframe) {
    empty_rows <- dataframe[,column] == ""
    indices <- which(empty_rows, arr.ind = TRUE, useNames = TRUE) + 1
    return(indices)
}
## function to extract row indices of any empty entries in relevant
## columns to the set of rows that have a certain entry in another
## column; e.g. the row indices of empty entries of columns relevant
## to the method "beating".
empty_method <- function(column, method, dataframe, metavector) {
    method_col <- dataframe[,column] == method
    method_ind <- which(method_col, arr.ind = TRUE, useNames = TRUE)
    method_vec <- dataframe[,metavector] == ""
    vector_ind <- which(method_vec, arr.ind = TRUE, useNames = TRUE)
    unique_vec <- unique(vector_ind)
    empty_ind <- c(method_ind, unique_vec)
    empty_met <- unique(empty_ind[duplicated(empty_ind)]) + 1
    return(empty_met)
}

empty_method("Method", "beating", colEvent, "TimeEnd")

