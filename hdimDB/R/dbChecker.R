#' @title Checks Dimensions Database for errors
#'  
#' @description \code{makeLabels} runs through the online database and returns a list of HDIM numbers associated with specific errors
#' 
#' @details See example
#' 
#' @param hdim the unique identifier(s) to be printed
#' @param dir the directory in which to save the labels
#' @param sheetName the file name to give the sheet of labels
#' @param defaultYear the year to be given to labels with a missing year
#' @param repID the number of labels for each unique identifier (can be a single value or a vector of length equal to length of \code{hdim})
#' 
#' @return A multi-leveled list of HDIM numbers
#' 
#' @examples 
#' dimensions
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

