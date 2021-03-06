% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiplot.R
\name{multiplot}
\alias{multiplot}
\title{Print multiple plots inside one}
\usage{
multiplot(..., plotlist=NULL, file="", cols=1, layout=NULL)
}
\arguments{
\item{...}{single ggplot plots separated by comas.}

\item{plotlist}{a list of ggplot plots.}

\item{file}{a character with the file's names that would be storaged.}

\item{cols}{the number of columns of the final layout. Number of rows will
adapt to it.}

\item{layout}{a matrix with a custom layout.}
}
\value{
It returns the multiple plot of classes \code{"gtable","gTree","grob","gDesc"}
}
\description{
Take multiple ggplot objects, print them together as you wish and storage the result
 as pdf file.
}
\details{
Storagement is made by dev.print(pdf,file), so it follows its ways to change
 directories.
}
\author{
Martin Vicencio
}
