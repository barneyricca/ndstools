#' max_ortho_embed
#'
#' @param <ts> character or numeric vector; time series to evaluate
#' @keywords embedding
#' @description This function computes the delay state space embedding of a
#' time series, based on the method proposed by Pecora et al. (2008).
#' @details
#' #' Currently, this function works only with numerical data.
#' It returns
#' @export
#' @author Barney Ricca <barneyricca@gmail.com>
#' @references Pecora, L. M., Moniz, L., Nichols, J., & Carroll, T. L. (2007).
#' A unified approach to attractor reconstruction. Chaos, 17(1),
#' 013110-1-013110–013119. https://doi.org/10.1063/1.2430294

#' @examples
#' max_ortho_embed(Mayport)
#'
max_ortho_embed <- function(ts1) {                   # Time series vector
  #
  if(is.null(ts1) == TRUE) {
    cat("ts1 is NULL!/n")
    return(NULL)
  }
  if(is.character(ts1) == TRUE) {           # Is ts1 a character vector?
    "Character" -> ts_type
  } else {
    if(is.numeric(ts1) == TRUE) {           # Is ts1 a numeric vector?
      "Continuous" -> ts_type
    } else {                                # If neither, abort
      cat("ts1 must be numeric!/n")
      return(NULL)
    }
  }
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

  # embed_ls[[1]] - vector 1:N
  # embed_ls[[2]] - delay for each dimension
  # embed_ls[[3]] - data frame of coordinates, with dimensions (length(ts) - N
  #                 + 1) by N, where N = number of dimensions
  return(embed_ls)
}
