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
EMPTY_PLOT <- which(empt_plo) + 1
EMPTY_PLOT

## create dataframe of row with empty values in "Date"
## return corrected indices of the rows of empty date columns
empt_dat <- colEvent[,"Date"] == ""
EMPTY_DATE <- which(empt_dat) + 1
EMPTY_DATE

## create dataframe of rows with empty values in "Collector"; 
## return corrected indices of the rows of empty collector columns
empt_col <- colEvent[,"Collector"] == ""
EMPTY_COLLECTOR <- which(empt_col) + 1
EMPTY_COLLECTOR

## create dataframe of rows with empty values in "Method";
## return corrected indices of the row of empty method columns
empt_met <- colEvent[,"Method"] == ""
EMPTY_METHOD <- which(empt_met) +1
EMPTY_METHOD

## create dataframe of rows with empty values in "Whereabouts"; 
## return corrected indices of the rows of empty whereabouts columns
empt_whe <- colEvent[,"Whereabouts"] == ""
EMPTY_WHEREABOUTS <- which(empt_whe) + 1
EMPTY_WHEREABOUTS

## create dataframe of rows with empty values in "SamplingRound"; 
## return corrected indices of the rows of empty smapling round columns
empt_sam <- colEvent[,"SamplingRound"] == ""
EMPTY_SAMPLINGROUND <- which(empt_sam) + 1
EMPTY_SAMPLINGROUND

## create dataframe of rows with empty values in "NoOfVials"; 
## return corrected indices of the rows of empty vial number columns
empt_via <- colEvent[,"NoOfVials"] == ""
EMPTY_VIALNUM <- which(empt_via) + 1
EMPTY_VIALNUM

## create dataframe of beating entries, create vector of 
## row indices of said entries
beat_col <- colEvent[,"Method"] == "beating"
beat_ind <- which(beat_col)
## find indices of empty entries of any of the four beating 
## information columns
beat_pla <- colEvent[,"Plant"] == ""
empt_pla <- which(beat_pla)
beat_dur <- colEvent[,"BeatingDuration"] == ""
empt_dur <- which(beat_dur)
beat_beg <- colEvent[,"TimeBegin"] == ""
empt_beg <- which(beat_beg)
beat_end <- colEvent[,"TimeEnd"] == ""
empt_end <- which(beat_end)
## combine vectors of indices of empty entries of
## beating information colums and create vector of 
## unique index values
beat_var <- c(empt_end, empt_beg, empt_dur, empt_pla)
uni_beat_var <- unique(beat_var)
## combine vector of the indices of empty beating information
## indices with the beating rows indices and isolate the 
## duplicate values; adjust for accuracy
empt_beat <- c(beat_ind, uni_beat_var)
EMPTY_BEATING <- sort(unique(empt_beat[duplicated(empt_beat)]) + 1)
EMPTY_BEATING

## function to extract indices of empty row entries
empty_indices <- function(dataframe, column) {
    # logical vector of whether entry is empy or not
    empty_rows <- dataframe[,column] == ""
    # indices, adjusted
    indices <- which(empty_rows) + 1
    return(indices)
}
empty_indices(colEvent, "Whereabouts")

## function to extract row indices of any empty entries in relevant
## columns to the subset of rows that have a certain entry in another
## column; e.g. the row indices of empty entries of columns relevant
## to the method "beating".
empty_method <- function(dataframe, column, method, metavector) {
    # logical vector of entries corresponding to method
    method_col <- dataframe[,column] == method
    # unadjusted indices of entries
    method_ind <- which(method_col)
    # list of vectors of unadjusted indices of empty entries in factor columns
    method_vec <- apply(dataframe[metavector], 2, function(x) which(x == ""))
    # split the list into seperate vector elements
    unique_vec <- unique(unlist(method_vec, recursive = TRUE))
    # combine the unadjusted indices of the method entries and factor entries
    empty_ind <- c(method_ind, unique_vec)
    # find unique indices of the combined vector and adjust for accuracy
    empty_met <- unique(empty_ind[duplicated(empty_ind)]) + 1
    # sort the indices, descending = FALSE
    empty_met <- sort(empty_met)
    return(empty_met)
}
metavector <- c("Plant", "BeatingDuration", "TimeBegin", "TimeEnd")
empty_method(colEvent, "Method", "beating", metavector)

# function to retrieve row indices of entries not matching a predetermined 
# list of possible valid entries in a certain column.
misspelled_indices <- function(dataframe, column, correctvector){
    return(which(!dataframe[,column] %in% correctvector ) + 1)
}
correct_where <- c("BERKELEY", "Berkeley", "UHH", "Hilgard 220", "Hilo Boys", "Hilo Boys (in packing box)", "NNNNN",  "")
misspelled_indices(colEvent, "Whereabouts", correct_where)

# function to create list with vectors corresponding to certain columns where
# empty entries are an issue and rows containing indice entries of empty row entries
empty_list <- function(dataframe, columnvector){
    return(apply(dataframe[columnvector], 2, function(x) which(x == "")))
}
columnvector <- c("Plot", "Date", "Collector", "Method", "Whereabouts", "SamplingRound", "NoOfVials")
empty_list(colEvent, columnvector)

# function to create dataframe with vectors corresponding to certain columns where
# empty entries are an issue and rows containing indice entries of empty row entries
empty_frame <- function(dataframe, columnvector){
    split_list <- list2env(empty_list(dataframe, columnvector),.GlobalEnv)
    padded_list <- rbind.fill(split_list)
    return(padded_list)
}
    # return(data.frame(padded_list[c(1:length(padded_list))]))

empty_frame(colEvent, columnvector)
str(rbind(empty_list(colEvent, columnvector)))

INVALID_colEvent <- rbind.fill(data.frame(EMPTY_PLOT), data.frame(EMPTY_DATE), data.frame(EMPTY_COLLECTOR), 
                               data.frame(EMPTY_METHOD), data.frame(EMPTY_WHEREABOUTS), 
                               data.frame(EMPTY_SAMPLINGROUND), data.frame(EMPTY_VIALNUM), 
                               data.frame(EMPTY_BEATING))
head(INVALID_colEvent)
