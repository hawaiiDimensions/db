#' @title Read a google sheet
#'  
#' @description \code{readGoogle} reads a google sheet into a data.frame
#' 
#' @details The google sheet must be published as a csv.  The function forces strings to be read as characters, not factors
#' 
#' @param u the URL of the google sheet
#' 
#' @return A data.frame of the google sheet
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @export

readGoogle <- function(u) {
    dat <- RCurl::getURL(u)
    read.csv(textConnection(dat), as.is=TRUE)
}