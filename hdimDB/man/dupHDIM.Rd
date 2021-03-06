% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dupHDIM.R
\name{dupHDIM}
\alias{dupHDIM}
\title{Checks Dimensions Database for duplicate HDIM numbers}
\usage{
dupHDIM(db)
}
\arguments{
\item{db}{the database to be checked}
}
\value{
Dataframe with HDIM identifier and duplicate HDIM error tag.
}
\description{
\code{dupHDIM} processes the online database and returns a dataframe of repeated HDIM numbers
}
\details{
Only valid when called on a Dimensions Database with a column "HDIM"
}
\author{
Edward Greg Huang <edwardgh@berkeley.edu>
}
