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
.assignCorr <- function(extractOut, match = 'index', db = NULL){
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
            method <- switch(match, 
                             'levenshtein' = .levenshteinMatch,
                             'index' = .indexMatch,
                             'regex' = .regexMatch)
            
            synList = list(list(readGoogle(synPlotURL), 
                                readGoogle(synCollectURL),
                                readGoogle(synMethodURL), 
                                readGoogle(synPlantURL),
                                readGoogle(synPitURL), 
                                readGoogle(synWhereURL), c(1:2)))
            corr <- mapply(verbatim = as.character(extractOut$verbatim), 
                           column = errColumn, frameList = synList, method)
            
            corr <- unlist(lapply(corr, function(x) ifelse(length(x) == 0, NA, x)))
        }
        if (errTag == 'time'){
            corr <- mapply(.timeCorr, value = as.character(extractOut$verbatim))
        }
        if (errTag == 'BeatingDuration'){ 
            corr <- .durationCorr(extractOut$errHDIM, db)
        }
    } 
    return(data.frame(extractOut, corr))
}

## AUTOCORRECTION METHOD FUNCTIONS ##

.levenshteinMatch <- function(verbatim, column, frameList){ # levenshtein distance
    synVector <- switch(column,
                       'Plot' = frameList[[1]],
                       'Collector' = frameList[[2]],
                       'Method' = frameList[[3]],
                       'Plant' = frameList[[4]],
                       'PitFallSlice' = frameList[[5]],
                       'Whereabouts' = frameList[[6]],
                       'SamplingRound' = frameList[[7]])
    
    synVector <- synVector[synVector != '']
    distance <- RecordLinkage::levenshteinSim(verbatim, synVector)
    corr <- synVector[distance == max(distance)]
    if (length(corr) > 1){
        corr <- paste(corr, collapse = ';')
    }
    return(corr)
}

.indexMatch <- function(verbatim, column, frameList){ # synonym table indexing
    synFrame <- switch(column,
                       'Plot' = frameList[[1]],
                       'Collector' = frameList[[2]],
                       'Method' = frameList[[3]],
                       'Plant' = frameList[[4]],
                       'PitFallSlice' = frameList[[5]],
                       'Whereabouts' = frameList[[6]],
                       'SamplingRound' = frameList[[7]])
        
    corr <- synFrame[synFrame[1] == verbatim, ][[2]]
    if (length(corr) > 1){ # if multiple matches
        corr <- paste(corr, collapse = ';')
    }
    return(corr)
}

.regexMatch <- function(verbatim, column, frameList){ # in development; regular expression matching
    synVector <- switch(column,
                        'Plot' = frameList[[1]],
                        'Collector' = frameList[[2]],
                        'Method' = frameList[[3]],
                        'Plant' = frameList[[4]],
                        'PitFallSlice' = frameList[[5]],
                        'Whereabouts' = frameList[[6]],
                        'SamplingRound' = frameList[[7]])
    
    synVector <- synVector[synVector != '']
    corr <- grep(verbatim, synVector, ignore.case=TRUE, value=TRUE) # specific search
    if (length(corr) == 0){ # if no match
        corr <- grep(gsub('_[[:digit:]]', '', verbatim), synVector, ignore.case=TRUE, value=TRUE) # general search
        if (length(corr) == 0){ # if still no match
            corr <- NA # give up 
        }
    }
    if (length(corr) > 1){ # if multiple matches
        corr <- paste(corr, collapse = ';')
    }
    return(corr)
}

.durationCorr <- function(errHDIM, db) {
    corr <- c()
    for (hdim in errHDIM) {
        site <- db[db['HDIM'] == hdim, 'Plot']
        if (length(site) > 1) {
            corr <- c(corr, 'NA')
        } else {
            values <- db[db$Plot == site & db$Method == 'beating', 'BeatingDuration']
            corr <- c(corr, Reduce('+', as.numeric(values)) - 420)
        }
    }
    return(corr)
}

.timeCorr <- function(value) {
    if (grepl(pattern = ';', x = value)) {
        return('Fix duplicate entries')
    } else if (grepl(pattern = '/', x = value)) {
        return('Change format to m/d/yyyy')
    } else if (grepl(pattern = ':', x = value)) {
        return('Change format of TimeBegin or TimeEnd to 24-hour time')
    }
    return(NA)
}

## AUTOCORRECTION SCRIPTS END ## 

.synValues <- function(url) { # synonym value extraction
    return(unique(readGoogle(url)[, 2]))
}