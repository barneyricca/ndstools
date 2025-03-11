#' max_ortho_embed
#'
#' @param ts numeric vector of the time series to evaluate
#' @param max_lag maximum lag to consider
#' @param max_dim maximum dimension to allow
#' @param method method choice; not yet implemented so do not use.
#'
#' @keywords embedding
#'
#' @description This function computes the delay state space embedding of a
#' time series, based on the method proposed by Pecora et al. (2008).
#'
#' @details
#' max_ortho_embed returns a (possibly non-constant) delay state space
#' embedding of ts. It does this by determining the maximally independent
#' delay vector at each step. The maximum embedding dimenions for the naive
#' approach (the only one implemented so far) is 6. Eventually, this will
#' include 3 methods; the mutual information method implemented here, the
#' method proposed by Pecora et al., and an approximation of the Pecora method.
#' The naive approach (method = "mutual") is a slight extension of the constant
#' delay embedding approach.
#'
#' @export
#'
#' @author Barney Ricca <barneyricca@gmail.com>
#'
#' @references Pecora, L. M., Moniz, L., Nichols, J., & Carroll, T. L. (2007).
#' A unified approach to attractor reconstruction. Chaos, 17(1),
#' 013110-1-013110–013119. https://doi.org/10.1063/1.2430294
#'
max_ortho_embed <- function(ts,                 # Time series vector
                            max_lag = 20,       #
                            max_dim = 6,        #
                            method = "mutual") { # Not yet used; do not change!

  if(is.null(ts) == TRUE) {
    cat("ts is NULL in max_ortho_embed!/n")
    return(NULL)
  }
  if(is.numeric(ts) == FALSE) {             # Is ts a numeric vector?
    cat("ts in max_ortho_embed must be numeric!/n")
    return(NULL)
  }

  length(ts) ->
    len_ts
  min(max_lag, len_ts-1) ->
    max_lag
  min(max_dim,                              # Maximum dimension must allow for
      len_ts - max_dim) ->                  #  vector creation
    max_dim
  min(max_dim, 6) ->
    max_dim
  vector(mode = "numeric",
         length = max_dim) ->
    delays

  # ==== First dimension is always the data without delay ============== #
  0 ->                                      #
    delays[1]                               #
  ts ->                                     #
    ts1                                  #

  # if(method == "ortho") {
    # 1 -> best_cos_theta                     # Second dimension
    # for(poss_del in 1:max_dim) {            #
    #   ts[-c(1:poss_del)] ->
    #     test
    #   ts[-c((len_ts + 1 - poss_del):len_ts)] ->
    #     check
    #   test * check / (sqrt(sum(test^2)) * sqrt(sum(check^2))) ->
    #     cos_theta
    #   if(cos_theta < best_cos_theta) {
    #     cos_theta -> best_cos_theta
    #     poss_del -> delays[1]
    #   }
    # }
    # ts[-c(1:delays[1])] ->
    #   ts2 ->
    #   mat[1:(nrow(mat) - delays[1]),2]
    # # Third dimension
    # # Create the cross product of the first two vectors and find the MAXIMUM
    # #  dot product (i.e., minimum angle)
    # for(poss_delay in delays[1]:max_lag) {
    #   ts1[1:length(ts2)] ->
    #     x
    #   ts2 ->
    #     y
    # }
    # # Fourth dimension
    # # Fifth dimension
    # # Sixth dimension
    #
  # }
#  if(method == "mutual") {
    # ==== Second dimension by mutual information minimum ============== #
    mi_ksg(ts1) ->                          # A vector of mutual information
      mi                                    #  for lags 0:100 (or length(ts1)).
    which(diff(diff(mi)>0)>0)[1] ->         # First local minimum; adjusted for
      delays[2]                             #  0-indexing of lags.
    ts[-c(1:delays[2])] ->                  # Remove first n = delays[2] data
      ts2
    # ==== Third dimension by mutual information minimum =============== #
    vector(mode = "numeric",
           length = max_lag) ->
      ar2
    for(poss_delay in 1:max_lag) {
      if(poss_delay != delays[2]) {
        ts[-c(1:poss_delay)] ->
          t3
        ts1[1:length(t3)] -> t1
        ts2[1:length(t3)] -> t2
        summary(stats::lm(t3 ~ t1 + t2))$adj.r.squared ->
          ar2[poss_delay]
      }
    }
    # First minimum, from jamie.f.olson. See details at:
    # https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
    which(diff(diff(ar2[-c(1:delays[2])])>0)>0)[1] + 1 +
      delays[2] ->      # This is the one we want.
      delays[3]
    ts[-c(1:delays[3])] ->
      ts3
    # ==== Fourth dimension by mutual information minimum ============== #
    vector(mode = "numeric",
           length = max_lag) ->
      ar2
    for(poss_delay in 1:max_lag) {
      if(poss_delay != delays[2] &
         poss_delay != delays[3]) {
        ts[-c(1:poss_delay)] ->
          t4
        ts1[1:length(t4)] -> t1
        ts2[1:length(t4)] -> t2
        ts3[1:length(t4)] -> t3
        summary(stats::lm(t4 ~ t1 + t2 + t3))$adj.r.squared ->
          ar2[poss_delay]
      }
    }
    which(diff(diff(ar2[-c(1:delays[3])])>0)>0)[1] + 1 +
      delays[3] ->      # This is the one we want.
      delays[4]
    ts[-c(1:delays[4])] ->
      ts4
    # ==== Fifth dimension by mutual information minimum =============== #
    vector(mode = "numeric",
           length = max_lag) ->
      ar2
    for(poss_delay in 1:max_lag) {
      if(poss_delay != delays[2] &
         poss_delay != delays[3] &
         poss_delay != delays[4]) {
        ts[-c(1:poss_delay)] ->
          t5
        ts1[1:length(t5)] -> t1
        ts2[1:length(t5)] -> t2
        ts3[1:length(t5)] -> t3
        ts4[1:length(t5)] -> t4
        summary(stats::lm(t5 ~ t1 + t2 + t3 + t4))$adj.r.squared ->
          ar2[poss_delay]
      }
    }
    which(diff(diff(ar2[-c(1:delays[4])])>0)>0)[1] + 1 +
      delays[4] ->      # This is the one we want.
      delays[5]
    ts[-c(1:delays[5])] ->
      ts5
    # ==== Sixth dimension by mutual information minimum =============== #
    vector(mode = "numeric",
           length = max_lag) ->
      ar2
    for(poss_delay in 1:max_lag) {
      if(poss_delay != delays[2] &
         poss_delay != delays[3] &
         poss_delay != delays[4] &
         poss_delay != delays[5]) {
        ts[-c(1:poss_delay)] ->
          t6
        ts1[1:length(t6)] -> t1
        ts2[1:length(t6)] -> t2
        ts3[1:length(t6)] -> t3
        ts4[1:length(t6)] -> t4
        ts5[1:length(t6)] -> t5
        summary(stats::lm(t6 ~ t1 + t2 + t3 + t4 + t5))$adj.r.squared ->
          ar2[poss_delay]
      }
    }
    which(diff(diff(ar2[-c(1:delays[5])])>0)>0)[1] + 1 +
      delays[5] ->      # This is the one we want.
      delays[6]
    ts[-c(1:delays[6])] ->
      ts6

    matrix(c(ts1[1:length(ts6)],
             ts2[1:length(ts6)],
             ts3[1:length(ts6)],
             ts4[1:length(ts6)],
             ts5[1:length(ts6)],
             ts6),
           nrow = length(ts6),
           ncol = max_dim,
           byrow = FALSE) ->
      mat

  # } else {
  #   cat("Indicated method not implemented in max_ortho_embed!\n")
  #   return(NULL)
  # }
  # For method Pecora:
  # The first dimension is ts1 and tau_lag = 0
  # To find the second dimension:
  # - For each subsequent possible time lag (tau_1...tau_max)
  #   - For each of a sample of points from the time series. (For short enough
  #     series, use all points?)
  #     - Pick a neighborhood of K points (ball of delta_prime radius) about
  #       ts[i].
  #     - Find the smallest epsilon with L points mapped from K, set such that
  #       L/K just fails the significance test. Set this to epsilon_try.
  #     - Reduce K by 1 and repeat the previous step, reducing epsilon_try if
  #       possible.
  #     - if epsilon_try cannot be reduced, epsilon_try -> epsilon_star
  #     - Go to the next sampled point from the time series
  #   - Compute the mean epsilon_star for that lag
  # - Go to the next time lag
  # - Find the maximum mean epsilon_star across the lags to identify tau_add
  # - Now, is it possible to add ts(t + tau_lag) independently to the existing
  #   dimension, or must we stop here? The way we do that is:
  #

  return(list(dimensions = length(delays),  # Number of dimensions
              delays = delays,              # Delay for each dimension
              embedding = mat))             # Embedded data
}
