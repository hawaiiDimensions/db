setwd('~/Dropbox/hawaiiDimensions/db')
source('db_checker.R', verbose = FALSE)

## run diagnostics
check <- DiagnoseDimensions(colEvent)

## extract out needing info for checking
colEventCheck <- colEvent
colEventCheck$check <- 'z'
colEventCheck$check[colEventCheck$HDIM %in% check$empty$Plot] <- 'empty plot'
colEventCheck$check[colEventCheck$HDIM %in% colEvent$HDIM[check$duplicated]] <- 'duplicated' 
colEventCheck$check[colEventCheck$HDIM %in% check$emptyMethod] <- 'empty method'

## order by what needs checking and write it out
colEventCheck <- colEventCheck[order(colEventCheck$check, colEventCheck$HDIM), ]
colEventCheck$check[colEventCheck$check == 'z'] <- NA
write.csv(colEventCheck, file = 'dbCleaning_2016-09-02.csv', row.names = FALSE)


## evaluate possible missing things on db that doesn't need checking
colEventGood <- colEventCheck[is.na(colEventCheck$check), ]