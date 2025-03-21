% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vf_plot.R
\name{vf_plot}
\alias{vf_plot}
\title{Plot a vector field}
\usage{
vf_plot(df, arrow_size = 0.2, arrow_scale = 1)
}
\arguments{
\item{df}{A data frame of data to fit. The first four columns must be the
(x,y) coordinates and the x- and y-directional derivatives, respectively.}

\item{arrow_size}{The base size of the arrow heads, in centimeters.}

\item{arrow_scale}{A scaling factor for the arrow lengths. Larger scales
produce shorter arrows.}
}
\value{
A ggplot2 object with the vector field plot and path. Labels, main
    title, etc., may be added after the return.
}
\description{
This function takes a data frame of directional derivatives and creates a
    vector field plot. The current version DOES NOT check for appropriate
    input, so be careful. phaseR::flowField() may be a better way to do
    this.
}
\seealso{
[vfp_plot()]
}
