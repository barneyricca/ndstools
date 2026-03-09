#' Estimate the partial distance correlation, with CI, between two vectors.
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as x.
#' @param R The number of replications. Default is 1000.
#'
#' @description
#' This function returns the partial distance correlation of two
#'     vectors along with a bootstrapped 95 percent confidence interval. It does
#'     so by removing the linear fit between the vectors before estimating the
#'     distance correlation.
#'
#' This function, like pdcor, only uses pairwise complete observations.
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

  # First, only sample from complete cases
  data.frame(x = x,
             y = y) ->
    df
  colnames(df) ->
    cndf
  df[complete.cases(df),] ->
    df
  if(nrow(df) == 0) {
    warning(
      paste0(
        "No non-NA cases for ",
        cndf[1],
        "-",
        cndf[2],
        " correlation. Exiting pdcor_ci().\n"))
    return((rep(NA_integer_, 3)))
  }
  df[,1] -> x
  df[,2] -> y

  replicate(R,                              #
            pdcor(sample(x),y)) ->          #
    pd
  mean(pd) -> mn                            # Saving before returning names
  stats::quantile(pd, 0.025) -> lCI         #  the elements of the return
  stats::quantile(pd, 0.975) -> uCI         #  vector.
  stats::setNames(c(mn, lCI, uCI),
                  c("mn", "lCI", "uCI")) ->
    pdc
  return(pdc)
}
