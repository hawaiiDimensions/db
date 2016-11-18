db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
errors <- dbChecker('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')

# ROMINGER SCRIPT
## turn nested list structure into a single vector of all the HDIMs with errors
errors <- unlist(errors)

## extract the error type (which is stored as the name of each element in the vector)
## use gsub to remove the sequential numbers from the names 
## (e.g. turn 'duplicatedHDIM123' into 'duplicatedHDIM')
errType <- gsub('[[:digit:]]', '', names(errors))
names(errors) <- NULL

## now we have all the error HDIMs and what type of error is associated with them
head(data.frame(errors, errType))
############################
## Dataframe of errors and their type
errKey <- data.frame(errors, errType)

extractErr <- function(errHDIM, errTag){
    
    errHDIM <- unlist(errHDIM)
    errType <- gsub('[[:digit:]]', '', names(errHDIM))
    names(errHDIM) <- NULL
    errMess <- paste(errTag, errType, sep = ".")
    
}
errType
    

    
    