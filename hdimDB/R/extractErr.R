## Assigns a given error type string to an HDIM number as a dataframe.
.extractErr <- function(db, errHDIM, errTag){ 
    errHDIM <- unlist(errHDIM)
    if (is.null(names(errHDIM))){
        errMessage <- errTag
    } else {
        errType <- gsub('[[:digit:]]', '', names(errHDIM))
        names(errHDIM) <- NULL
        errMessage <- paste(errTag, errType, sep = ".")
    }
    errColumn <- gsub('.*\\.', '', errMessage) 
    verbatim <- mapply(hdim = errHDIM, col = errColumn, FUN = function(hdim, col){
        paste(unique(db[db$HDIM == hdim, col]), collapse = ';')
    })
    return(data.frame(errHDIM, errMessage, verbatim = verbatim))
}

## Input is output of .extractErr
.assignCorr <- function(extractOut){
    errTag <- gsub('\\..*', '', extractOut$errMessage)[1]
    browser()
    if (errTag == "dupHDIM"){
        corr <- NA
    }
    if (errTag == "empty"){
        corr <- NA
    }
    if (errTag == "misspelled"){
        errColumn <- gsub('.*\\.', '', extractOut$errMessage)
        corr <- mapply(verbatim = extractOut$verbatim, column = errColumn, .closestMatch)
    }
    if (errTag == "time"){
        corr <- NA
    }
    return(data.frame(extractOut, corr))
}

.closestMatch <- function(verbatim, column){
# .closestMatch <- function(verbatim, synVector){
#     if(column == '...') { ## fill this in
#         synVector <- NULL
#     }
    synVector <- switch(column)
    distance <- RecordLinkage::levenshteinSim(verbatim, synVector)
    corr <- synVector[distance == max(distance)]
    if (length(corr) > 1){
        corr <- paste(corr, collapse = ';')
    }
    return(corr)
}