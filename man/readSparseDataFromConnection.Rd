% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readSparseDataFromConnection.R
\name{readSparseDataFromConnection}
\alias{readSparseDataFromConnection}
\title{Read sparse data from an open connection.}
\usage{
readSparseDataFromConnection(con, zeroBased = FALSE, verbose = FALSE)
}
\arguments{
\item{con}{Connection to read from}

\item{zeroBased}{Do the indicies in the file start with 0, e.g. -1 0:2 1:4 ...?}

\item{verbose}{Be verbose?}
}
\value{
L		list containing input data and labels
}
\description{
Read sparse data from an open connection.
}

