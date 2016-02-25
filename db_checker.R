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
## create dataframe of rows with empty values in "Collector"
inval_col <- colEvent[,"Collector"] == ""
## return indices of the rows of invalid collector columns
which(inval_col, arr.ind = TRUE, useNames = TRUE)