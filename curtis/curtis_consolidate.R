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
          file = 'curtis/curtis_consolidate.csv', row.names = FALSE, na = '')


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