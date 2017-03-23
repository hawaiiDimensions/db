
###################
## KOKUA: hdimDB ##
###################
devtools::install_github('hawaiiDimensions/db/hdimDB') # installation script
library(hdimDB) # load package
##############################
db <- readGoogle(colEventsURL) # load database
errOut <- checkDb(db[1:1000, ]) # diagnose errors - 12.09.16 runtime - 22.04574 mins

## Plotting errTags ##
tags <- as.character(errOut$errMessage) # convert factors to characters
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0)) 
errPlot <- barplot(table(tags), names.arg = unique(tags), 
                   horiz = TRUE, las = 1, cex.names = 0.4, border = NA, 
                   main = 'Types of ColEvents Database Errors') 
                   # sub = 'Huang, E.G., 2016. UC Berkeley')

## TIME BENCHMARKS ## 
x <- proc.time()
checkDb(db2)
proc.time() - x

########################################
## hdimShiny / ## SHINY.IO DEPLOYMENT ##
########################################
# install.packages('rsconnect') # shiny.io
library(rsconnect)
# rsconnect::setAccountInfo(name='edwardhuang', 
#                           token='BC78E54A24F464E0C7E125EDC1FAC215', 
#                           secret='OfD5Ks58EnClYHcrn3vtdHjqqRod2X4kwdZsXzTu')
rsconnect::deployApp('/Users/EdwardH/Dropbox/hawaiiDimensions/db/hdimShiny') # deploy
# App URL: https://edwardhuang.shinyapps.io/hdimshiny/
rsconnect::showLogs()

