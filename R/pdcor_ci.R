#' Estimate the partial distance correlation, with CI, between two vectors.
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as x.
#' @param R The number of replications. Default is 1000.
#'
#' @description
#' This function returns the partial distance correlation of two
#'     vectors along with a bootstrapped 95% confidence interval. It does so by
#'     removing the linear fit between the vectors before estimating the
#'     distance correlation.
#'
#' @return A named vector with the partial distance correlation ($mn) of the
#'     two vectors x and y, along with the lower ($lCI) and upper ($uCI)
#'     confidence interval limits.
#'
#' @export
#'
#'
#'
#'
#'
pdcor_ci <- function(x,
                     y,
                     R = 1000) {

  replicate(R,                              #
            pdcor(sample(x),y)) ->          #
    pd
  mean(pd) -> mn                            # Saving before returning names
  stats::quantile(pd, 0.025) -> lCI         #  the elements of the return
  stats::quantile(pd, 0.975) -> uCI         #  vector.
  return(c(mn, lCI, uCI))
}
