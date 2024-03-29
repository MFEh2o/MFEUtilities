% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensordbTable.R
\name{sensordbTable}
\alias{sensordbTable}
\title{Sensor DB Table
Retrieving a table from the sensor database. Created by JAZ, 2014-12-10; updated 2017-03-08}
\usage{
sensordbTable(
  table,
  fpath = sensor_dbdir,
  dbname = sensor_db,
  lakeID = c(),
  minDepth_m = c(),
  maxDepth_m = c(),
  minDate = c(),
  maxDate = c(),
  dateFormat = c()
)
}
\arguments{
\item{table}{The data table to be returned. \emph{Not} case-sensitive.}

\item{dbname}{Name of the database file. Default is whatever you've set to the variable \code{sensor_db} in your script.}

\item{lakeID}{Vector of lakeID's that you want; default returns all lakeID's.}

\item{minDepth_m}{Numeric value of minimum sensor depth}

\item{maxDepth_m}{Numeric value of maximum sensor depth}

\item{minDate}{Character string of minimum sample date in standard unambiguous format (i.e. YYYY-MM-DD, YYYY-MM-DD HH:MM, or YYYY-MM-DD HH:MM:SS)}

\item{maxDate}{Character string of maximum sample date in standard unambiguous format (i.e. YYYY-MM-DD, YYYY-MM-DD HH:MM, or YYYY-MM-DD HH:MM:SS)}

\item{dateFormat}{Character string of date format if not in standard unambiguous format (i.e. '\%y/\%m/\%d')}

\item{depthClass}{Vector of depthClass values that you want; default returns all depthClasses (i.e. PML, hypo, etc...)}
}
\value{
Database table, pulled from the sensor database, filtered by the specified parameters if applicable.
}
\description{
Sensor DB Table
Retrieving a table from the sensor database. Created by JAZ, 2014-12-10; updated 2017-03-08
}
