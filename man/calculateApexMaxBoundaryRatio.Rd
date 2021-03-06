% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateApexMaxBoundary.R
\name{calculateApexMaxBoundaryRatio}
\alias{calculateApexMaxBoundaryRatio}
\title{Calculate Apex-Max Boundary Ratio (of a Chromatographic Peak)}
\usage{
calculateApexMaxBoundaryRatio(peakData, pts)
}
\arguments{
\item{peakData}{A vector containing characteristic information about a chromatographic peak - including the retention time range}

\item{pts}{A 2D matrix containing the retention time and intensity values of a chromatographic peak}
}
\value{
The apex-max boundary ratio (double)
}
\description{
Calculates the Apex-Max Boundary Ratio of the integrated region of a chromatographic peak. The Apex-Max Boundary Ratio is found by
taking the ratio of the intensity of the peak apex over the intensity of the maximum of the two boundary intensities.
}
\details{
This function repurposed from TargetedMSQC. Toghi Eshghi, S., Auger, P., & Mathews, W. R. (2018). Quality assessment and
interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics, 15.
https://doi.org/10.1186/s12014-018-9209-x
}
\examples{
# Calculate Apex Max-Boundary Ratio for a peak
data(ex_pts)
data(ex_peakData)
apexMaxBoundary <- calculateApexMaxBoundaryRatio(peakData = ex_peakData, pts = ex_pts)

}
