#' @title Checks Dimensions Database for errors
#'  
#' @description \code{dbChecker} runs through the online database and returns a list of HDIM numbers associated with specific errors
#' 
#' @details Developed specifically for the Dimensions in Biodiversity Evolab Database.
#' 
#' @param None
#' 
#' @return A multi-leveled list of HDIM numbers
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export
#' 

## Import Database
db <- readGoogle('https://docs.google.com/spreadsheets/d/1Ve2NZwNuGMteQDOoew
                  itaANfTDXLy8StoHOPv7uGmTM/pub?output=csv')
db[is.na(db)] <- ""

## dupHDIM
dupHDIM <- function(){
  return(db[which(duplicated(db[, "HDIM"])),]$HDIM)
}

## checkEmpty
.emptyColumn <- function(column){
  return(db[which(db[, column] == ""),]$HDIM)
}

.emptyContin <- function(method, vector){
  method.ind <- which(db$Method == method)
  method.vec <- apply(db[vector], 2, function(x) which(x == ""))
  empty.ind <- c(method.ind, unique(unlist(method.vec, recursive = TRUE)))
  return(db[unique(empty.ind[duplicated(empty.ind)]), ]$HDIM)
}
                
beat.vector <- c("Plant", "BeatingDuration", "TimeBegin", "TimeEnd")
gmal.vector <- c("DateEnd", "PitFallSlice")
cmal.vector <- c("DateEnd", "PitFallSlice")
leaf.vector <- c("PitFallSlice")
pit.vector <- c("DateEnd", "PitFallSlice")
zook.vector <- c("PitFallSlice")
soil.vector <- c("PitFallSlice")  

contin.list <- list(beat.vector, pit.vector, leaf.vector,
                    cmal.vector, gmal.vector, zook.vector, 
                    soil.vector)

empty.col <- c("HDIM", "Plot", "Date", "Collector", "Method", 
               "Whereabouts", "SamplingRound", "NoOfVials") 
methods <- c("beating", "pitfall", "leaf litter", "canopy malaise", 
             "ground malaise","Insectazooka", "soil extraction") 

checkEmpty <- function(){
    return(list(mapply(.emptyColumn, empty.col),
                mapply(.emptyContin, methods, contin.list)))
}

## checkMisspell
.misColumn <- function(column, vector){
    indice.misspelled <- (which(!db[, column] %in% vector))
    return(db[indice.misspelled,]$HDIM)
}

cor.plot <- c(unique(colEvent$Plot), "")
cor.collect <- c(unique(colEvent$Collector), "")
cor.method <- c("canopy malaise", "ground malaise", "beating", "pitfall", "canopy clipping", 
                "leaf litter", "Insectazooka", "soil extraction", "")
cor.plant <- c(unique(colEvent$Plant), "")
cor.beat <- c(0:300, "")
cor.pit <- c("up", "down", "A", "B", "C", "D", 
             "E", "F", "ground", "")
cor.where <- c("UHH", "Hilgard 220", "NEED TO FIND", "")
cor.sample <- c(1:2, "")
cor.vial <- c(1:3, "")

cor.list <- list(cor.plot, cor.collect, cor.method, 
                 cor.plant, cor.beat, cor.pit, 
                 cor.where, cor.sample, cor.vial)

misspelled.columns <- c("Plot", "Collector", "Method", "Plant", 
                        "BeatingDuration", "PitFallSlice",
                        "Whereabouts", "SamplingRound", "NoOfVials")

checkMisspell <- function(){
  return(mapply(.misColumn, misspelled.columns, cor.list))
}

#checkTime
.dateColumn <- function(){
    dates <- (as.Date(db[, "Date"], format = "%m/%d/%Y" ))
    dates.indices <- which(is.na(as.character(dates)) == "TRUE")
    return(db[dates.indices,]$HDIM)
}

.dateContin <- function(date.column, date.format){
    empty.dates <- which(db[, date.column] != "")
    dates <- as.Date(db[, date.column], format = date.format )
    dates.indices <- which(is.na(as.character(dates)) == "TRUE")
    dates.vector <- c(empty.dates, dates.indices)
    return(db[unique(dates.vector[duplicated(dates.vector)]), ]$HDIM)
}

checkTime <- function(){
    return(list(.dateColumn(), .dateContin("DateEnd", "%m/%d/%Y")
                        , .dateContin("TimeBegin", "h:m")
                        , .dateContin("TimeEnd", "h:m")))
}

###################
## Database Checker
###################
dbChecker <- function(){
    return(list(dupHDIM(), checkEmpty(), checkMisspell(), checkTime))
}  