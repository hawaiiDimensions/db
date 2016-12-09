## Assigns a given error type string to an HDIM number as a dataframe.
.extractErr <- function(db, errHDIM, errTag){ 
    errHDIM <- unlist(errHDIM)
    if (length(errHDIM) == 0){
        return(data.frame(errHDIM = NA, errMessage = NA, verbatim = NA))
    } else {
        if (is.null(names(errHDIM))){
            errMessage <- errTag
        } else {
            errType <- gsub('[[:digit:]]', '', names(errHDIM))
            names(errHDIM) <- NULL
            errMessage <- paste(errTag, errType, sep = ".")
        }
        errColumn <- gsub('.*\\.', '', errMessage) 
        verbatim <- try(mapply(hdim = errHDIM, col = errColumn, FUN = function(hdim, col){
            paste(unique(db[db$HDIM == hdim, col]), collapse = ';')
        }))
        if(class(verbatim) == 'try-error') browser()
        return(data.frame(errHDIM, errMessage, verbatim = verbatim))
    }
}

## Input is output of .extractErr
.assignCorr <- function(extractOut){
    if (any(is.na(extractOut$errHDIM))){
        corr <- NA
    } else {
        errTag <- gsub('\\..*', '', extractOut$errMessage)[1]
        if (errTag == "dupHDIM"){
            corr <- NA
        }
        if (errTag == "empty"){
            corr <- NA
        }
        if (errTag == "misspelled"){
            errColumn <- gsub('.*\\.', '', extractOut$errMessage)
            corr <- mapply(verbatim = as.character(extractOut$verbatim), column = errColumn, .closestMatch)
        }
        if (errTag == "time"){
            corr <- NA
        }
    } 
    return(data.frame(extractOut, corr))
}

.closestMatch <- function(verbatim, column){
    synVector <- switch(column,
                        'Plot' = .synValues(synPlotURL),
                        'Collector' = .synValues(synCollectURL),
                        'Method' = .synValues(synMethodURL),
                        'Plant' = .synValues(synPlantURL),
                        'PitFallSlice' = .synValues(synPitURL),
                        'Whereabouts' = .synValues(synWhereURL),
                        'SamplingRound' = c(1:2))
    synVector <- synVector[synVector != '']
    distance <- RecordLinkage::levenshteinSim(verbatim, synVector)
    corr <- synVector[distance == max(distance)]
    if (length(corr) > 1){
        corr <- paste(corr, collapse = ';')
    }
    return(corr)
}