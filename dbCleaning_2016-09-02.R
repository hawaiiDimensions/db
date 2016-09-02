setwd('~/Dropbox/hawaiiDimensions/db')
source('db_checker.R', verbose = FALSE)

check <- DiagnoseDimensions(colEvent)

colEventCheck <- colEvent
colEventCheck$check <- ''
check$empty$Plot
colEvent$HDIM[check$duplicated]
check$emptyMethod

