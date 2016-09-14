library(reshape2)
library(plyr)

setwd('~/Dropbox/hawaiiDimensions/db')
source('db_checker.R')

colEvent$Date <- as.Date(colEvent$Date, '%m/%d/%Y')

## round 2 starts June 1, 2016
colEvent$SamplingRound <- ifelse(colEvent$Date >= as.Date('2015-06-01'), 2, 1)

## run diagnostics
check <- DiagnoseDimensions(colEvent)

## extract out needing info for checking
colEventCheck <- colEvent
colEventCheck$check <- 'z'
colEventCheck$check[colEventCheck$HDIM %in% check$empty$Plot] <- 'empty plot'
colEventCheck$check[colEventCheck$HDIM %in% check$empty$Method] <- 'empty method'
colEventCheck$check[colEventCheck$HDIM %in% colEvent$HDIM[check$duplicated]] <- 'duplicated' 
colEventCheck$check[colEventCheck$HDIM %in% check$emptyMethod] <- 'empty method info'

## order by what needs checking and write it out
colEventCheck <- colEventCheck[order(colEventCheck$check, colEventCheck$HDIM), ]
colEventCheck$check[colEventCheck$check == 'z'] <- ''
write.csv(colEventCheck, file = 'dbCleaning_2016-09-02.csv', row.names = FALSE)

## =================================================================
## evaluate possible missing things on db that doesn't need checking
## =================================================================

colEventGood <- colEventCheck[colEventCheck$check == '', ]

## get number of events by plot, method, round
ceSum <- ddply(colEventGood[grep('beat|malaise|leaf|pit', colEventGood$Method), ], 
               c('Plot', 'Method', 'SamplingRound'), 
               function(x) {
                   if(x$Method[1] == 'beating') {
                       return(sum(as.numeric(x$BeatingDuration), na.rm = TRUE))
                   } else {
                       return(nrow(x))
                   }
               })
names(ceSum) <- c('plot', 'method', 'round', 'nEvent')
ceSum$round <- as.factor(ceSum$round)

## organize by plots as rows and methods by cols
temp <- acast(melt(ceSum), plot ~ method ~ round, fill = 0, fun.aggregate = sum)
ce1Sum <- temp[, , 1] # sampling round 1
ce2Sum <- temp[, , 2] # sampling round 2

## targets for each method
target <- array(0, dim = dim(ce2Sum))
rownames(target) <- rownames(ce2Sum)
colnames(target) <- colnames(ce2Sum)
target[, 'beating'] <- 420
target[, 'canopy malaise'] <- 2
target[, 'ground malaise'] <- 1
target[, 'leaf litter'] <- 3
target[, 'pitfall'] <- 3

## missing
ce1Mis <- ce1Sum - target
ce2Mis <- ce2Sum - target

## things for grace to look for
graceToDo <- apply(ce2Mis, 1, function(x) paste(names(which(x < 0)), collapse = ', '))
graceToDo <- paste(names(graceToDo), as.character(graceToDo), sep = ': ')
graceToDo <- graceToDo[!grepl('waikamoi', graceToDo)]
cat(paste(graceToDo, collapse = '\n'))

