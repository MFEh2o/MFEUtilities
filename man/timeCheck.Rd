% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{timeCheck}
\alias{timeCheck}
\title{Check \code{sampleID} time against the time portion of \code{dateTimeSample}}
\usage{
timeCheck(df)
}
\arguments{
\item{df}{A data frame to check}
}
\value{
ID's of rows that have mismatched dates.
}
\description{
\code{timeCheck} checks whether all the times in the \code{sampleID} column match up with the times in the \code{dateTimeSample} column. If they don't, returns numeric indices of the rows that contain mismatches. This function also verifies that \code{df} is a data frame and that it has the columns \code{sampleID} and \code{dateTimeSample}.
}
