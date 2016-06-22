setwd('~/Dropbox/hawaiiDimensions/db')

## package to interface with web
library(RCurl)

## function to read-in google drive tables
readGoogle <- function(u) {
    dat <- getURL(u)
    read.csv(textConnection(dat), as.is=TRUE)
}

## function to correct one column (assumes colEvent exists in parent environment)
correctSyn <- function(syn) {
    thisCol <- gsub('verbatim', '', colnames(syn)[1])
    colEvent[, thisCol] <<- syn[match(colEvent[, thisCol], syn[, 1]), 2]
}

## synonomy tables
synTab <- c('https://docs.google.com/spreadsheets/d/1_KGLPEcOneLvcRR8--CjEVeKTPDXJkI7YqwSTM2BVJc/pub?output=csv',
            'https://docs.google.com/spreadsheets/d/1MIXM5OzUtWUj4w_9dzf51Z1aRNV2mTCLUNgVBvkZYuE/pub?output=csv',
            'https://docs.google.com/spreadsheets/d/19eHQCQLKN_At10iUDmXYX2w1FA7EomHK9jZbq7lOSYg/pub?output=csv',
            'https://docs.google.com/spreadsheets/d/1SxbSt_SRiycfaihkOg-m4tz-nrtZNKtqJDE2OmG700Q/pub?output=csv',
            'https://docs.google.com/spreadsheets/d/1Q8rFjF4n828ZVRTl7KkCQao5G0Emtwmm88MLZSoHcbA/pub?output=csv',
            'https://docs.google.com/spreadsheets/d/1sKJpNgcghZySIGQiw2o9t6Vt_Q06IVQo0GLf5YUb4-M/pub?output=csv')

## the database (read it and write it as old version)
colEvent <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?gid=0&single=true&output=csv')
write.table(colEvent, file=sprintf('db_uncorrected_%s.csv', Sys.Date()), sep=',', row.names=FALSE)

## loop over synonomy tables and correct
for(i in synTab) correctSyn(readGoogle(i))

write.table(colEvent, file=sprintf('db_corrected_%s.csv', Sys.Date()), sep=',', row.names=FALSE)
