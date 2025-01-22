#' Estimate the fixed points of a 1-dimensional nonlinear system.
#'
#' @param Level_data A vector of the level (0th derivative) data.
#' @param Velocity_data A vector of the velocity (1st derivative) data.
#' @param span Smoothing for the loess() function. (Default is 0.3.)
#' @param numPred Size of grid on which to predict velocity as a function of
#'     level. (Default is 100.)
#'
#' @description
#' This function estimates the zeroes in the function velocity(level).
#'
#' @return A data frame with columns "Level" indicating the estimated fixed
#'     point and "Stability" indicating whether the fixed point is stable of
#'     unstable.
#'
#' @export
#'
#'
#'
#'
#'
fixed_1d <- function(Level_data,            # Vector of level data
                     Velocity_data,         # Vector of velocity data
                     span = 0.3,            # Default smoothing for loess()
                     numPred = 100) {       # Number of points to predict

  # This function will use loess() to model a uni-dimensional time sequence,
  #  find the fixed points and their stability.

  loess(Velocity_data ~ Level_data,
        span = span,
        method = "loess",
        family = "symmetric") ->
    loess_model

  seq(min(Level_data, na.rm = TRUE),
      max(Level_data, na.rm = TRUE),
      length.out = numPred) ->
    predict_points

  predict(loess_model,
          data.frame(Level_data = predict_points)) ->
    predicted

  ifelse(predicted[1] > 0, 1, -1) ->
    currentSign
  list(c(0,0)) ->
    zeroes
  1 ->
    zero_num

  for(index in 2:numPred) {
    if(predicted[index] * currentSign < 0) {
      c(index, currentSign) -> zeroes[[zero_num]]
      -1 * currentSign -> currentSign
      zero_num + 1 -> zero_num
    }
  }

  data.frame("Level" = rep(0, length(zeroes)),
             "Stability" = rep(0, length(zeroes))) ->
    fp_df

  for(index in 1:nrow(fp_df)) {
    zeroes[[index]][1] +                    # Find location of zeroes
      predicted[zeroes[[index]][1]] /
      (predicted[zeroes[[index]][1]-1] -
         predicted[zeroes[[index]][1]]) ->
      fp_df$Level[index]

    ifelse(zeroes[[index]][2] == 1, "Unstable", "Stable") ->
      fp_df$Stability[index]
  }

  predict_points[1] +                       # Convert from location to level
    (predict_points[numPred] - predict_points[1]) *
    fp_df$Level /
    numPred ->
    fp_df$Level

  return(fp_df)                             #
}
