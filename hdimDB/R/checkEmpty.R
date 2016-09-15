#' @title Checks Dimensions Database for invalid empty entries
#'  
#' @description \code{checkEmpty} runs through the online database and returns a list of HDIM numbers corresponding to invalid empty entries in the database
#' 
#' @details Only certain columns are checked for empty entries, factor columns contingent to certain collection methods are considered.
#' 
#' @param None
#' 
#' @return List of character vectors of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

## Load the Database
db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoew
                    itaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
db[is.na(db)] <- ""

## Helper functions
.emptyColumn <- function(column){
  return(db[which(db[, column] == ""),]$HDIM)
}

.emptyContin <- function(method, vector){
  method.ind <- which(db$Method == method)
  method.vec <- apply(db[vector], 2, function(x) which(x == ""))
  empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE)))
  return(db[unique(empty.ind[duplicated(empty.ind)]), ]$HDIM)
}

#####################################################################
## Vectors of Factor Column Names - REPLACE WITH SYNONYM TABLE VALUES
## define inside functions
#####################################################################

## beating (beat.vector)                   
beat.vector <- c("Plant", "BeatingDuration", "TimeBegin", "TimeEnd")
## ground malaise (gmal.vector)
gmal.vector <- c("DateEnd", "PitFallSlice")
## canopy malaise (cmal.vector)
cmal.vector <- c("DateEnd", "PitFallSlice")
## leaf litter (leaf.vector)
leaf.vector <- c("PitFallSlice")
## pitfall trap (pit.vector)
pit.vector <- c("DateEnd", "PitFallSlice")
## InsectaZooka (zook.vector)
zook.vector <- c("PitFallSlice")
## soil extraction (soil.vector)
soil.vector <- c("PitFallSlice")  
## canopy clipping has no contingent columns, does not need to be checked
contin.list <- list(beat.vector, pit.vector, leaf.vector,
                    cmal.vector, gmal.vector, zook.vector, 
                    soil.vector)

## Compiling vectors of column names and contingent column names
empty.col <- c("HDIM", "Plot", "Date", "Collector", "Method", 
               "Whereabouts", "SamplingRound", "NoOfVials") 
methods <- c("beating", "pitfall", "leaf litter", "canopy malaise", 
             "ground malaise","Insectazooka", "soil extraction") 


checkEmpty <- function(){
  return(list(mapply(.emptyColumn, empty.col),
              mapply(.emptyContin, methods, contin.list)))
}

