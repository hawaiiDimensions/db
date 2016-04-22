## ======================================================================
## script to pull unique values from database to make synonym tables from
## ======================================================================

setwd('~/Dropbox/hawaiiDimensions/db')

db <- RCurl::getURL('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
db <- read.csv(textConnection(db), as.is=TRUE)

## correct these columns
col2correct <- c('Plot', 'Collector', 'Method', 'Plant', 'PitFallSlice', 'Whereabouts')

## write tables of unique values
for(i in col2correct) 
    write.table(unique(db[, i]), 
                file=sprintf('synonymTable%s.csv', i), 
                row.names=FALSE, col.names=sprintf('verbatim%s', i))
