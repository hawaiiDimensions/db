## Assigns a given error type string to an HDIM number as a dataframe.
.extractErr <- function(errHDIM, errTag){
    errHDIM <- unlist(errHDIM)
    if (is.null(names(errHDIM))){
        errMessage <- errTag
    } else {
        errType <- gsub('[[:digit:]]', '', names(errHDIM))
        names(errHDIM) <- NULL
        errMessage <- paste(errTag, errType, sep = ".")
    }
    return(data.frame(errHDIM, errMessage))
}

.assignCorr <- function(errOut){
    errTag <- gsub('\\..*', '', errOut$errMessage)[1]
    if (errTag == "dupHDIM"){
        corr <- NA
    }
    if (errTag == "empty"){
        corr <- NA
    }
    if (errTag == "misspelled"){
        corr <- NA
    }
    if (errTag == "time"){
        corr <- NA
    }
    return(data.frame(err.Out, corr))
}