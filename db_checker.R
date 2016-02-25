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
## diagnostic functions to keep in handy
sapply(colEvent, class)
str(colEvent)
head(colEvent)
db_status <- df_status(colEvent)
## create dataframe of rows with empty values in "Collector"; 
## return corrected indices of the rows of invalid collector columns
inval_col <- colEvent[,"Collector"] == ""
INVALID_COLLECTOR <- which(inval_col, arr.ind = TRUE, useNames = TRUE) + 1
## create dataframe of row with empty values in "Location"
## return corrected indices of the rows of invalid location columns
inval_plo <- colEvent[,"Plot"] == ""
INVALID_PLOT <- which(inval_plo, arr.ind = TRUE, useNames = TRUE) + 1
## create dataframe of row with empty values in "Date"
## return corrected indices of the rows of invalid date columns
inval_dat <- colEvent[,"Date"] == ""
INVALID_DATE <- which(inval_dat, arr.ind = TRUE, useNames = TRUE) + 1
