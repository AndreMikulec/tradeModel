% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxilary.R
\name{fredEomData}
\alias{fredEomData}
\title{get the latest value in the month from FRED}
\usage{
fredEomData(Symbol = NULL)
}
\arguments{
\item{Symbol}{FRED symbol as a string}
}
\value{
xts object of the last observation with the
month date rounded up to the last day of the month
}
\description{
...
}
\examples{
\dontrun{
EXAMPLE
# # weekly
# > ff <- head(fredEomData("FF"),1)
# > ff,
#              ff
# 1954-07-31 0.63
}
}
