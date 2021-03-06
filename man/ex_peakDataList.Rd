% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ex_peakDataList}
\alias{ex_peakDataList}
\title{Example peakDataList - value input to calculteElutionShift and calculateRetentionTimeConsistency}
\format{
A list of lists. Each nested list contains the following entries: \code{mz}, \code{mzmin}, \code{mzmax}, \code{rt},
\code{rtmin},  \code{rtmax}, \code{into}, \code{intb}, \code{maxo}, \code{sn}, \code{sample}, and \code{is_filled}.
}
\usage{
ex_peakDataList
}
\description{
An example of the input for the peakDataList argument for calculteElutionShift and calculateRetentionTimeConsistency. Each entry
in the list is represents data for a sample for the peak of interest.
}
\keyword{datasets}
