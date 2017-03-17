#' @title Make collection event or specimen labels
#'  
#' @description \code{makeLabels} reads the online database and prints labels requested by 
#' unique identifier (HDIM or EMEC number)
#' 
# @details See example
#' 
#' @param hdim the unique identifier(s) to be printed
#' @param dir the directory in which to save the labels
#' @param sheetName the file name to give the sheet of labels
#' @param repID the number of labels for each unique identifier (can be a single value or 
#' a vector of length equal to length of \code{hdim})
#' 
#' @return A data.frame of the google sheet
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @export

## main function takes vector of HDIM numbers and directory of DB

## helper functions need to pull info from database and organize that into a label

makeLabels <- function(hdim, dir = NULL, sheetName, repID=1) {
    ## set a default year in case missing
    defaultYear <- 2015
    
    ## load data base
    db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoewitaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
    
    ## subset database to only desired HDIM numbers
    badHDIM <- hdim[!(hdim %in% db$HDIM)]
    
    if(length(repID) == 1) {
        repID <- rep(repID, length(hdim))
    }

    hdim <- rep(hdim[!(hdim %in% badHDIM)], repID[!(hdim %in% badHDIM)])
    db <- db[match(hdim, db$HDIM), ]
    
    ## allow .makeOneLabel function to see objects from local environment
    environment(.makeOneLabel) <- environment()
    
    ## loop over db and make labels
    out <- character(nrow(db))
    for(i in 1:nrow(db)) {
        this.label <- .makeOneLabel(db[i, ])
        
        if(i %% 6 == 0 & i != nrow(db)) {
        	this.label <- paste(this.label, '\n\\vspace{-0.075in} \n\n\\noindent')
        }
        
        out[i] <- this.label
    }
    
    out <- c('---',
             'header-includes:',
             '- \\usepackage{graphicx}',
             '- \\usepackage{setspace}',
             '- \\usepackage{amssymb}',
             '- \\usepackage{amsmath}',
             '- \\usepackage{epstopdf}',
             'geometry: margin=0.5in',
             'output: pdf_document',
             '---',
             '\n',
             '\\noindent', 
             '\\raggedright',
             out)
    
    tempLabels <- file.path(tempdir(), 'mkdownLabels.Rmd')
    writeLines(out, con = tempLabels)
    
    if(is.null(dir)) {
        outpath <- rmarkdown::render(tempLabels, envir = new.env(parent = globalenv()),
                          output_file = paste(sheetName, 'pdf', sep = '.'))
    } else {
        outpath <- rmarkdown::render(tempLabels, output_dir = dir, envir = new.env(parent = globalenv()),
                          output_file = paste(sheetName, 'pdf', sep = '.'))
    }
    

    # if(length(badHDIM) > 0) warning('missing HDIMs:', paste(badHDIM, collapse=', '))
    # invisible(badHDIM)
    return(outpath)
}

.makeOneLabel <- function(x) {
    month <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug',
               'sep', 'oct', 'nov', 'dec')
    
    with(x, {
        coll <- Collector
        coll <- coll[!(is.na(coll) | coll == '')]
        if(length(coll) < 1) {
        	coll <- '\\rule{0ex}{0ex}\\hspace{6em}'
        }

        if(grepl('beat', Method, ignore.case=TRUE) & !(is.na(TimeBegin) | TimeBegin == '')) {
            endt <- paste('--', TimeEnd, sep='')
        } else {
            endt <- ''
        }
        
        if(is.na(Date) | Date == '') {
        	date <- defaultYear
        } else {
            d <- strsplit(Date, '/')[[1]]
            date <- paste(ifelse(nchar(d[3]) == 2, paste('20', d[3], sep=''), d[3]), 
                          month[as.numeric(d[1])], 
                          d[2], 
                          sep='-')
        }

        if(is.na(BeatingDuration) | BeatingDuration == '') {
            BeatingDuration <- ''
        } else {
            BeatingDuration <- paste(' ', BeatingDuration, ' sec. ', sep='')
        }
        
        paste('\\parbox{0.16\\textwidth}{\\tiny ', '\\raggedright ', 
              '\\rule[-0.3\\baselineskip]{0pt}{10pt}',
              paste('HDIM', HDIM, sep=''), '; ', gsub('_', '\\\\_', Plot), '\\\\ ',
              Method, ifelse(is.na(PitFallSlice) | PitFallSlice=='', '', 
                             paste(' ', PitFallSlice, sep='')),
              ifelse(is.na(Plant) | Plant=='', '', paste(' ', Plant, sep='')), BeatingDuration,
              TimeBegin, endt, '; ', date, '\\\\ ',
              paste(coll, collapse=', '), ifelse(length(coll) > 1, ' colls.', ' coll.'), 
              '}',
              sep='')
    })

}
