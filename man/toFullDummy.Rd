% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toFullDummy.R
\name{toFullDummy}
\alias{toFullDummy}
\title{turns all variables into dummy}
\usage{
toFullDummy(dataset)
}
\arguments{
\item{dataset}{Data array}
}
\value{
It returns a matrix
}
\description{
This function has no documentation yet
}
\details{
this function execute this code:\cr
 \code{toFullDummy = function(dataset){\cr
dataset = alply(dataset, 2, function(x) {\cr
model.matrix( ~. -1 , data = x)\cr
})\cr
names(dataset) = attr(dataset,"split_labels")[,1]\cr
data.frame(do.call(cbind,dataset))\cr
}}
}
\author{
Daniel Fischer
}
