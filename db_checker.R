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
## convert null values to 0
colEvent[is.na(colEvent)] <- ""
## diagnostic functions to keep in handy
sapply(colEvent, class)
str(colEvent)
head(colEvent)
db_status <- df_status(colEvent)

## create dataframe of row with empty values in "Plot, Date, 
## Collector, Method, Whereabouts, SamplingRound, NoOfVials"
## return corrected indices of the rows of empty column
empt_hdi <- which(colEvent[,"HDIM"] == "") + 1
empt_plo <- which(colEvent[,"Plot"] == "") + 1
empt_dat <- which(colEvent[,"Date"] == "") + 1
empt_col <- which(colEvent[,"Collector"] == "") + 1
empt_met <- which(colEvent[,"Method"] == "") +1
empt_whe <- which(colEvent[,"Whereabouts"] == "") + 1
empt_sam <- which(colEvent[,"SamplingRound"] == "") + 1
empt_via <- which(colEvent[,"NoOfVials"] == "") + 1

## create dataframe of beating entries, create vector of 
## row indices of said entries
beat_ind <- which(colEvent[,"Method"] == "beating")
## find indices of empty entries of any of the four beating 
## information columns
beat_pla <- which(colEvent[,"Plant"] == "")
beat_dur <- which(colEvent[,"BeatingDuration"] == "")
beat_beg <- which(colEvent[,"TimeBegin"] == "")
beat_end <- which(colEvent[,"TimeEnd"] == "")
## combine vectors of indices of empty entries of
## beating information colums; create vector of 
## unique index values
beat_var <- unique(c(beat_end, beat_beg, beat_dur, beat_pla))
## combine vector of the indices of empty beating information
## indices with the beating rows indices and isolate the 
## duplicate values; adjust for accuracy
beat_emp <- c(beat_ind, uni_beat_var)
empt_bea <- sort(unique(beat_emp[duplicated(beat_emp)]) + 1)

## function to extract indices of empty row entries
## logical vector of whether entry is empy or not
## indices, adjusted
empty_indices <- function(dataframe, column) {
    return(which(dataframe[,column] == "") + 1)
}
empty_indices(colEvent, "Whereabouts")

## function to extract row indices of any empty entries in relevant
## columns to the subset of rows that have a certain entry in another
## column; e.g. the row indices of empty entries of columns relevant
## to the method "beating".
empty_method <- function(dataframe, column, method, metavector) {
    # logical vector of entries corresponding to method;
    # unadjusted indices of entries
    method_ind <- which(dataframe[,column] == method)
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

## function to retrieve row indices of entries not matching a predetermined 
## list of possible valid entries in a certain column.
misspelled_indices <- function(dataframe, column, correctvector){
    # logical vector of pattern matches of correctvector in dataframe column
    # return adjusted row indices from logical vector
    return(which(!dataframe[,column] %in% correctvector ) + 1)
}
correct_where <- c("BERKELEY", "Berkeley", "UHH", "Hilgard 220", "Hilo Boys", "Hilo Boys (in packing box)", "NNNNN",  "")
misspelled_indices(colEvent, "Whereabouts", correct_where)

## function to create list with vectors corresponding to certain columns where
## empty entries are an issue and rows containing indice entries of empty row entries
empty_list <- function(dataframe, columnvector){
    # apply empty_indices to each column of dataframe and return list of results
    return(apply(dataframe[columnvector], 2, function(x) which(x == "")))
}
columnvector <- c("HDIM", "Plot", "Date", "Collector", "Method", "Whereabouts", "SamplingRound", "NoOfVials")
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

INVALID_colEvent <- rbind.fill(data.frame(empt_hdi), data.frame(empt_plo), data.frame(empt_dat), 
                               data.frame(empt_col), data.frame(empt_met), data.frame(empt_whe), 
                               data.frame(empt_sam), data.frame(empt_via), data.frame(empt_bea))
head(INVALID_colEvent)
df_status(INVALID_colEvent)
