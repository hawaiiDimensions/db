## Assigns a given error type string to an HDIM number as a dataframe.
.extractErr <- function(db, errHDIM, errTag){ 
    errHDIM <- unlist(errHDIM)
    if (length(errHDIM) == 0){ # NA result if no errors found
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
        verbatim <- mapply(hdim = errHDIM, col = errColumn, FUN = function(hdim, col){
            paste(unique(db[db$HDIM == hdim, col]), collapse = ';')
        })
        return(data.frame(errHDIM, errMessage, verbatim = verbatim))
    }
}

## Input is output of .extractErr
.assignCorr <- function(extractOut){
    if (any(is.na(extractOut$errHDIM))){
        corr <- NA
    } else {
        errTag <- gsub('\\..*', '', extractOut$errMessage)[1]
        if (errTag == 'dupHDIM'){
            corr <- NA
        }
        if (errTag == 'empty'){
            corr <- NA
        }
        if (errTag == 'misspelled'){
            errColumn <- gsub('.*\\.', '', extractOut$errMessage)
            corr <- mapply(verbatim = as.character(extractOut$verbatim), column = errColumn, .closestMatch)
            # corr <- mapply(verbatim = as.character(extractOut$verbatim), column = errColumn, .regexMatch) # regex update
        }
        if (errTag == 'time'){
            corr <- NA
        }
        if (errTag == 'beatduration'){ 
            corr <- 420
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

.regexMatch <- function(verbatim, column){
    synVector <- switch(column,
                        'Plot' = .synValues(synPlotURL),
                        'Collector' = .synValues(synCollectURL),
                        'Method' = .synValues(synMethodURL),
                        'Plant' = .synValues(synPlantURL),
                        'PitFallSlice' = .synValues(synPitURL),
                        'Whereabouts' = .synValues(synWhereURL),
                        'SamplingRound' = c(1:2))
    synVector <- synVector[synVector != '']
    corr <- grep(verbatim, synVector, ignore.case=TRUE, value = TRUE) # specific search
    if (length(corr) == 0){ # if no match
        corr <- grep(gsub('_[[:digit:]]', '', verbatim), synVector, ignore.case=TRUE, value = TRUE) # general search
        if (length(corr) == 0){ # if still no match
            corr <- NA # give up 
        }
    }
    if (length(corr) > 1){ # if multiple matches
        corr <- paste(corr, collapse = ';')
    }
return(corr)
}
