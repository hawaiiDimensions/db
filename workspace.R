##########################
## Install and load hdimDB
devtools::install_github('hawaiiDimensions/db/hdimDB')
library(hdimDB)
##########################
## Load database and scan for errors
db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
errors <- dbChecker('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
####################################

############################################
## Fake database 
# db <- read.csv("fake_data.csv", as.is=TRUE)
# test.results <- list(duplicatedHDIM = dupHDIM(db), empty = checkEmpty(db), misspell = checkMisspell(db),wrongTime = checkTime(db))
# test.results
############################################
checkMisspell(db)

system.file("examples", package="shiny")

runExample("01_hello") # a histogram
runExample("02_text") # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg") # global variables
runExample("05_sliders") # slider bars
runExample("06_tabsets") # tabbed panels
runExample("07_widgets") # help text and submit buttons
runExample("08_html") # Shiny app built from HTML
runExample("09_upload") # file upload wizard
runExample("10_download") # file download wizard
runExample("11_timer") # an automated timer

str(errors)
db[errors$duplicatedHDIM, ]
?checkboxGroupInput
str(errors)
install.packages('DT')
library('DT')
foo <- db$HDIM == errors

########################
## script to 
foobar <- rapply(errors, function(x)db[db$HDIM %in% x, ])
########################

head(foobar)
class(foobar)
length(foobar)
length(errors)