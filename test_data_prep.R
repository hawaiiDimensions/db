setwd('~/Dropbox/hawaiiDimensions/db')
testData <- read.csv('test_data.csv', header = TRUE, as.is = TRUE)
save(testData, file = 'hdimDB/data/testData.RData')
