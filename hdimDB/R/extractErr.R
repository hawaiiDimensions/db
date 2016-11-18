## Assigns a given error type string to an HDIM number as a dataframe.
extractErr <- function(errHDIM, errTag){
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

