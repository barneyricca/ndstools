% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca_embed.R
\name{pca_embed}
\alias{pca_embed}
\title{pca_embed}
\usage{
pca_embed(ts, dstep = 1, m.max = 20, do.plot = TRUE)
}
\arguments{
\item{ts}{numeric vector to embed}

\item{dstep}{(constant) step size for delays}

\item{m.max}{maximum embedding dimension to consider}

\item{do.plot}{boolean value; display the scree plot or not?}
}
\value{
The function returns a list with three elements, the estimated
embedding dimension (obtained by a segmented regression of the scree), the
PCA object, and the embedded data. The PCA object is returned to allow the
user to refine the estimation of embedding dimension and embedded data.
}
\description{
This function computes the delay state space embedding of a
time series, using principal component analysis.
}
\details{
A potential embedding matix of ts is created and then principal component
analysis is used to return an appropriate embedding dimension. m.max should
be set to a value much larger than the anticipated embedding dimension and
dstep should be set to a small (positive integer) value, unless there are
concerns that the data are oversampled.
}
\author{
Barney Ricca <barneyricca@gmail.com>
}
\keyword{embedding}
