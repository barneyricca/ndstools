% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nl_cross_pred.R
\name{nl_cross_pred}
\alias{nl_cross_pred}
\title{nl_cross_pred}
\usage{
nl_cross_pred(ts, nseg = 5, m.max = 6)
}
\arguments{
\item{ts}{character vector}

\item{nseg}{number of segments to cross validate on}

\item{m.max}{maximum embedding dimension to consider}
}
\description{
nl_cross_pred() uses the Nash-Sutcliffe efficiency coefficient to test
support for stationarity in a signal.
}
\details{
nl_cross_pred() uses the Nash-Sutcliffe efficiency coefficient to estimate
how well any one segment of a time-series predicts other segments of the
same series. Consistent high cross-prediction supports the hypothesis that
the signal is stationary.
}
\author{
Barney Ricca barneyricca@gmail.com
}
\keyword{cross-prediction}
\keyword{nonlinear,}
