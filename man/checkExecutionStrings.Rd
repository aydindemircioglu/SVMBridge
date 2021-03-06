% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkExecutionStrings.R
\name{checkExecutionStrings}
\alias{checkExecutionStrings}
\title{Check whether a binary's output contains given strings.}
\usage{
checkExecutionStrings(trainBinaryPath = NULL, patterns = NULL,
  verbose = FALSE)
}
\arguments{
\item{trainBinaryPath}{Path of binary to check.}

\item{patterns}{List of patterns to check for. Checking is done via grep.}

\item{verbose}{Be verbose?}
}
\value{
True, if all patterns matches, False if not.
}
\description{
Given a pattern, this function will check whether the given binary has these patterns
in its output string, when called without any arguments. This helps all wrappers in identifying 
the correct binary.
}
\note{
To make sure that the binary is correct, it will be executed!
binary is really not exchangeable).

Our problem child, svmperf, waits for an key. therefore a 
flag applyKeyFix was handed before to simulate a keypress. but this 
seemed to be platform dependent. On non-unix platforms this simply 
did not work out. Instead now this function will load the whole binary 
into memory and grep for the strings there. But by reading docs-- rtfm--
it became clear that system2 has an 'input' parameter that just sends
the simulated key to the binary-- this must be platform independent.
so we remove applyKeyFix for the user, and later from code too.
}

