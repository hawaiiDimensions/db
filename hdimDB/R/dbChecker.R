#' @title Make collection event or specimen labels
#'  
#' @description \code{makeLabels} reads the online database and prints labels requested by unique identifier (HDIM or EMEC number)
#' 
#' @details See example
#' 
#' @param hdim the unique identifier(s) to be printed
#' @param dir the directory in which to save the labels
#' @param sheetName the file name to give the sheet of labels
#' @param defaultYear the year to be given to labels with a missing year
#' @param repID the number of labels for each unique identifier (can be a single value or a vector of length equal to length of \code{hdim})
#' 
#' @return A data.frame of the google sheet
#' 
#' @examples 
#' x <- 1:10
#' x
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @export

