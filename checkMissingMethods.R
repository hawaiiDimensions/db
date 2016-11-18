library(hdimDB)
library(plyr)
library(reshape2)

setwd('~/Dropbox/hawaiiDimensions/db')

## read in dimensions db
db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')

## add column that has pitfall slice info, etc
db$fullMethod <- paste(db$Method, db$PitFallSlice, sep = '')

## subset by sampling round
db1 <- db[db$SamplingRound == 1, ]
db2 <- db[db$SamplingRound == 2, ]

## summarize methods by plot
methSumm <- ddply(db2, c('Plot', 'fullMethod'), function(x) {
    if(any(grepl('beat', x$fullMethod, ignore.case = TRUE))) {
        return(sum(as.numeric(x$BeatingDuration)))
    } else {
        return(nrow(x))
    }
})

methSummWide <- acast(methSumm, Plot ~ fullMethod, value.var = 'V1')
