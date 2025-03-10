#' const_delay_embed
#'
#' @param x numeric vector (representing time series)
#' @param m.max maximum embedding dimension to consider
#' @param max.delay maximum delay to consider
#' @param lag.max maximum lag to consider for autocorrelation
#' @param tol Default tolerance for "closeness" of false nearest neighbors
#'
#' @keywords delay, embedding, embedding dimension, shadow state space
#'
#' @description
#' const_delay_embed() determines a constant delay reconstruction state space
#' and embeds the time series in it.
#'
#' @export
#' @details
#' const_delay_embed() estimates appropriate values for delay (using mutual
#' information), embedding dimension (using autocorrelation), and Theiler
#' window, (using a false-nearest neighbors approach) to determine the
#' appropriate embedding of a time series into a reconstructed state space.
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#'
const_delay_embed <- function(x,
                              m.max = 6,  # Maximum embedding dimension
                              max.delay = 20,
                              lag.max = 100,
                              tol = 0.15) {
  # Issue Number 0: Is this correct????

  # Issue Number 1:
  # tseriesChaos::mutual() and nonlinearTseries::mutualInformation() give
  #  different AMI estimates for the Hare embedding. This is problematic
  #  because they also give different estimates of the delay. The former
  #  yields a delay of 4, while the latter yields a delay of 3.
  # Hence, I wrote my own mutual information routine.
  #
  mean(x,
       na.rm = TRUE) ->
    x[is.na(x) == TRUE]

  mi_ksg(x) -> mi

  # First minimum, from jamie.f.olson. See details at:
  # https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
  which(diff(diff(mi)>0)>0)[1] ->           # This is the one we want.
    d

  acf.out <- stats::acf(x,
                 lag.max,
                 plot = FALSE)$acf

  # First minimum, from jamie.f.olson. See details at:
  # https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
  which(diff(diff(acf.out)>0)>0)[1] ->      # This is the one we want.
    tw

  min(m.max,
      floor(length(x)/d) - 1) ->
    m.dim
  if(m.dim < 0) {
    cat("Dimension in embed_local must be positive!\n")
    return(NULL)
  }

  min(length(x) - m.dim * d - 1,
      tw) ->
    tw
  if(tw < 0) {
    cat("Theiler window in embed_local must be positive!\n")
    return(NULL)
  }

  tseriesChaos::false.nearest(series = x,   # Possible error for short series?
                              m = m.dim,
                              d = d,
                              t = tw) ->    #
    fn.out
  fn.out[is.na(fn.out)] <- 0                # set NA in fn.out to zero

  fn.out[2*(1:m.max)-1] ->                  # Output vector of fnn percentages
    fnp                                     #  from fn.out.
  ifelse(                                   # Minimum delay that is < tol
    suppressWarnings(
      is.infinite(min(which(fnp < tol)))),
    m.max,
    min(which(fnp < tol))) ->
    m                                       # Embedding dimension

  ifelse(m <= 1,                            # Must embed in more than 1 dim
         2,
         m) ->
    m
  tseriesChaos::embedd(x,m,d) ->            # Embed time series (Mx)
    Mx

  return(list("delay" = d,
              "dim" = m ,
              "tw" = tw,
              "embedded" = Mx))
}
