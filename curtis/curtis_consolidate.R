library(hdimDB)

setwd('~/Dropbox/hawaiiDimensions/db')

## read in curtis db and dimensions db
curt <- read.csv('curtis/curtis_clean.csv', as.is = TRUE)
curt <- curt[curt$Plot != '', ]
curt$source <- 'curtis'

db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
db <- db[, names(db) != 'X']
db$source <- 'db'

## combine and sort by HDIM
dbCombo <- rbind(db, curt)
dbCombo <- dbCombo[order(dbCombo$HDIM), ]

## get duplicated and unique HDIM
dupHDIM <- dbCombo$HDIM[duplicated(dbCombo$HDIM)]
unqHDIM <- dbCombo$HDIM[!duplicated(dbCombo$HDIM)]

write.csv(dbCombo[dbCombo$HDIM %in% dupHDIM, ], 
          file = 'curtis/curtis_consolidate_dup.csv', row.names = FALSE, na = '')


## read back in corrected duplicates and combine with non-duplicates
dupCheck <- read.csv('curtis/curtis_consolidate_dupCheck.csv', as.is = TRUE)
dbClean <- rbind(dupCheck, dbCombo[dbCombo$HDIM %in% unqHDIM, -which(names(dbCombo) == 'source')])


## clean up date

dbClean$Date[dbClean$HDIM %in% c(7121, 7124)] <- '6/17/2015'
dbClean$Date[dbClean$HDIM == 9002] <- '8/19/2015'

yr <- gsub('.*/|.*-', '', dbClean$Date)

cleanDate <- rep(as.Date('0000/1/1'), nrow(dbClean))
cleanDate[grep('-', dbClean$Date)] <- as.Date(dbClean$Date[grep('-', dbClean$Date)], '%d-%b-%y')
cleanDate[grepl('/', dbClean$Date) & nchar(yr) == 2] <- 
    as.Date(dbClean$Date[grepl('/', dbClean$Date) & nchar(yr) == 2], '%m/%d/%y')
cleanDate[grepl('/', dbClean$Date) & nchar(yr) == 4] <- 
    as.Date(dbClean$Date[grepl('/', dbClean$Date) & nchar(yr) == 4], '%m/%d/%Y')

if(!any(cleanDate == as.Date('0000/1/1'))) {
    dbClean$Date <- cleanDate
    
    badYear <- as.character(dbClean$Date[dbClean$Date < as.Date('2014-1-1')])
    substring(badYear, 1, 4) <- as.character(as.numeric(substring(badYear, 1, 4)) + 4)
    dbClean$Date[dbClean$Date < as.Date('2014-1-1')] <- as.Date(badYear)
}


## if all HDIM accounted for, write it back out along with original version of db

if(all(dbCombo$HDIM %in% dbClean$HDIM)) {
    write.csv(dbClean, file = sprintf('db_consolidated_%s.csv', Sys.Date()), row.names = FALSE)
    write.csv(db, file = sprintf('db_origVersion_%s.csv', Sys.Date()), row.names = FALSE)
}






## get plot synonyms
plotNames <- readGoogle('https://docs.google.com/spreadsheets/d/1Q8rFjF4n828ZVRTl7KkCQao5G0Emtwmm88MLZSoHcbA/pub?output=csv')

## clean up curtis's plot names
i <- match(tolower(curt$Plot), tolower(plotNames$verbatimPlot))
curt$Plot[!is.na(i)] <- plotNames$correctedPlot[i[!is.na(i)]]

badPlot <- curt$Plot[is.na(i)]
badPlotSite <- gsub('_.*', '', badPlot)
j <- match(tolower(badPlotSite), tolower(gsub('_.*', '', plotNames$correctedPlot)))
badPlotSite[!is.na(j)] <- gsub('_.*', '', plotNames$correctedPlot)[j[!is.na(j)]]

badPlotSite[is.na(j) & grepl('laup', badPlotSite, ignore.case = TRUE)] <- 'laupOld'
badPlotSite[is.na(j) & grepl('escape', badPlotSite, ignore.case = TRUE)] <- 'escape'

badPlotRep <- gsub('.*_', '', badPlot)