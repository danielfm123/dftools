% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMode.R
\name{getMode}
\alias{getMode}
\title{calculate the mode}
\usage{
getMode(v)
}
\arguments{
\item{values}{values}

\item{append_dv}{should the calculated DV be apended to the rut or just return the DV}
}
\description{
calculates the mode or most frequent value
}
\examples{

values = c(1,2,3,4,2,3,2,1)
getMode(values)
}
