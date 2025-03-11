#' mi_ksg
#'
#' @param ts numeric vector of time series data
#' @param k number of nearest neighbors to use
#' @param lag.max Maximum lag to use
#'
#' @keywords mutual information
#' @description
#'
#' mi_ksg() uses the approach of Kraskov, Stögbauer, and Grassberger (KSG) to
#' estimate the mutual information in a time-series.
#'
#' @export
#'
#' @details
#' mi_ksg() uses the approach of Kraskov, Stögbauer, and Grassberger (KSG) to
#' estimate the mutual information in a time-series. It returns a vector of
#' mutual information estimates at each lag up to lag.max(), starting with
#' a lag of zero. (Hence, the lags are one less than the indices of the
#' vector.)
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#'
#' @references Kraskov, A., Stögbauer, H., & Grassberger, P. (2004).
#  Estimating mutual information.
#'
#'
mi_ksg <- function(ts,                      # A numeric vector of a timeseries
                   k = 3,                   # Number of nearest neighbors
                   lag.max = 100) {         # Maximum lag
  # Some day, after implementing KDE approaches, etc., deprecate this.
  #  The body of this will have to be something like:
  #  {return(ami(ts, method = "KSG", k = k))} to call the full ami calculation.

  # Check ts (numeric vector)

  # Check lag.max (positive integer)

  if((mode(k) == "numeric" |                # Make certain k is a positive
      mode(k) == "integer") &               #  integer
     k %% 1 == 0 &
     k >= 1) {

    min(length(ts) - k - 1,
        lag.max) ->
      lag.max

    if(lag.max <= 0) {
      cat("Invalid lag.max in mi_ksg!\n")
      return(NULL)
    }

    vector(mode = "numeric",
           length = lag.max + 1) ->
      mi
    for(index in 0:lag.max) {
      # Too many ties creates a big problem. So, add a little noise to
      #  avoid that.
      FNN::mutinfo(ts[1:(length(ts) - index)] +
                     stats::rnorm(length(ts) - index,
                           0,
                           min(1e-5,
                               stats::sd(ts, na.rm = TRUE) / 1e3)),
                   ts[(index+1):(length(ts))],
                   k = k) ->
        mi[index + 1]
    }
    return(mi)
  } else {
    cat("k must be a positive integer!\n")
    return(NULL)
  }
}
