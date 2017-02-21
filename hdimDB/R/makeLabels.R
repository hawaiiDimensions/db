#' @title Make collection event or specimen labels
#'  
#' @description \code{makeLabels} reads the online database and prints labels requested by unique identifier (HDIM or EMEC number)
#' 
# @details See example
#' 
#' @param hdim the unique identifier(s) to be printed
#' @param dir the directory in which to save the labels
#' @param sheetName the file name to give the sheet of labels
#' @param defaultYear the year to be given to labels with a missing year
#' @param repID the number of labels for each unique identifier (can be a single value or a vector of length equal to length of \code{hdim})
#' 
#' @return A data.frame of the google sheet
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @export

## main function takes vector of HDIM numbers and directory of DB

## helper functions need to pull info from database and organize that into a label

makeLabels <- function(hdim, dir, sheetName, defaultYear=2015, repID=1) {
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
        	this.label <- paste(this.label, '\\\\ \n\\vspace{0.001in} \n\n\\noindent')
        }
        
        out[i] <- this.label
    }
    
    out <- c('\\documentclass[2pt]{extarticle}',
             '\\usepackage[margin=0.5in]{geometry}',
             '\\geometry{letterpaper}',
             '\\usepackage{graphicx}',
             '\\usepackage{setspace}',
             '\\usepackage{amssymb}',
             '\\usepackage{amsmath}',
             '\\usepackage{epstopdf}',
             '\n',
             '\\begin{document}',
             '\\noindent', 
             '\\raggedright',
             out,
             '\\end{document}')
    
    writeLines(out, paste(dir, '/', sheetName, '.tex', sep=''))
    
    ######### not working right yet!!!! ############
    
    system(sprintf('%s %s/%s.tex', system('which pdflatex', intern=TRUE), dir, sheetName))
    system(sprintf('rm %s/%s.aux %s/%s.log', dir, sheetName, dir, sheetName))

    if(length(badHDIM) > 0) warning('missing HDIMs:', paste(badHDIM, collapse=', '))
    invisible(badHDIM)
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
        
        # if(length(coll) > 1) {
        	# coll <- substring(coll, 1, 4)
        	# coll <- gsub('\\. ', '', coll)
        	# coll <- paste(coll, collapse=', ')
        # }

        if(grepl('beat', Method, ignore.case=TRUE) & !(is.na(TimeBegin) | TimeBegin == '')) {
            endt <- paste('--', TimeEnd, sep='')
        } else {
            endt <- ''
        }
        
#         if(is.na(TimeBegin) | TimeBegin == '') {
#             TimeBegin <- '\\rule{0ex}{0ex}\\hspace{4.5em}'
#         }
        
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
        
        paste('\\parbox{0.16\\textwidth}{\\tiny ', '\\raggedright ', '\\rule[-0.3\\baselineskip]{0pt}{10pt}',
              paste('HDIM', HDIM, sep=''), '; ', gsub('_', '\\\\_', Plot), '\\\\ ',
              Method, ifelse(is.na(PitFallSlice) | PitFallSlice=='', '', paste(' ', PitFallSlice, sep='')),
              ifelse(is.na(Plant) | Plant=='', '', paste(' ', Plant, sep='')), BeatingDuration, # '\\\\ ',
              TimeBegin, endt, '; ', date, '\\\\ ',
              paste(coll, collapse=', '), ifelse(length(coll) > 1, ' colls.', ' coll.'), 
              # 
              '}',
              sep='')
    })

}

## fakeLabels Implementation ##

fakelabels <- function(hdims, numLabels) {
    ## desired number of rows and columns
    nrow <- 24
    ncol <- 6
    
    ## replicate all hdims by number of desired labels
    allHDIM <- rep(hdims, numLabels)
    
    ## figure out how to pack it into a matrix
    if(length(allHDIM) < nrow * ncol) {
        allHDIM <- c(allHDIM, rep('', nrow * ncol - length(allHDIM)))
    } else if(length(allHDIM) > nrow * ncol) {
        addRow <- ceiling((length(allHDIM) - nrow * ncol) / (nrow * ncol))
        nrow <- nrow + nrow*addRow
        if(length(allHDIM) < nrow * ncol) {
            allHDIM <- c(allHDIM, rep('', nrow * ncol - length(allHDIM)))
        }
    }
    
    ## make the matrix
    labelMatrix <- matrix(allHDIM, nrow = nrow, ncol = ncol, byrow = TRUE)
    
    # knitr::kable \ to make matrix markdown table: http://stackoverflow.com/questions/15488350/programmatically-creating-markdown-tables-in-r-with-knitr
    mkdownLabels <- knitr::kable(labelMatrix, format = "markdown", col.names = rep('', ncol)) # convert to Rmd file
    tempLabels <- file.path(tempdir(), 'mkdownLabels.Rmd')
    writeLines(mkdownLabels, con = tempLabels)
    
    # rmarkdown::render \ render table to html: http://stackoverflow.com/questions/28507693/call-rmarkdown-on-command-line-using-a-r-that-is-passed-a-file
    rmarkdown::render(tempLabels)
}
# return html file
# embed into Shiny App https://shiny.rstudio.com/articles/generating-reports.html

x <- fakelabels(5000:5001, numLabels = 2:3)
