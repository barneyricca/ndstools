#' Estimate the partial distance correlation of two vectors.
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as x.
#'
#' @description
#' This function returns the partial distance correlation of two vectors. It
#'     does so by removing the linear fit between the vectors before estimating
#'     the distance correlation.
#'
#' @return The partial distance correlation of the two vectors x and y.
#'
#' @export
#'
#'
#'
#'
#'
pdcor <- function(x,                        # First data vector
                  y) {                      # Second data vector

  if(length(x) != length(y)) {
    stop("Vectors must be the same length!\n")
  }

  data.frame("X" = x, "Y" = y) ->
    df
  df[complete.cases(df),] ->                # Listwise deletion
    df
  lm(Y ~ X,
     data = df) ->                          # Fit a line
    remove_lm
  df$Y -                                    # Original value minus
    remove_lm$coef[1] -                     #  Fit intercept minus
    remove_lm$coef[2] * df$X ->             #  Fit slope times IV
    df$Y
  return(energy::dcor(df$X, df$Y))                  # dcor()
}
