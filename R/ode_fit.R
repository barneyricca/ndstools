#' Estimate unknown coefficients of a first order differential equation
#'
#' @param df A data frame of data to fit. The first column must contain the
#'     times.
#' @param user_func The ODE function. Must correspond to the requirements
#'     of the deSolve::ode() function.
#' @param parm_init Initial coefficient estimates
#' @param parm_names The parameter names. These must match those used in
#'     user_func, but do not need to match the column names of df.
#' @param init_step_size Initial step size for each iteration
#' @param max_iter The maximum number of steps to take.
#' @param rtol The smallest step size
#' @param tol Smallest RMSE improvement to continue iterating
#'
#' @return A list of estimated parameters and process information.
#' @export
#'
#'
#'
#'
#'

## For testing:
# read.csv("../Data/Lynx and Hare.csv") -> lh_df
# lh_df -> df
#
# mahaffy <- function(t,
#                     y,
#                     parameters) {
#   y[1] -> Hare                              # This is a bit awkward, but it is
#   y[2] -> Lynx                              #  safer and works better with
#   parameters[1] -> b_H                      #  package phaseR.
#   parameters[2] -> d_H                      # Make sure these names match the
#   parameters[3] -> d_L                      #  data and initial parameter
#   parameters[4] -> b_L                      #  names.
#
#   b_H * Hare - d_H * Hare * Lynx ->         # dHare is the Hare velocity
#     dHare
#   - d_L * Lynx + b_L * Hare * Lynx ->       # dLynx is the Lynx velocity
#     dLynx
#   return(list(c(dHare, dLynx)))
# }
# mahaffy -> user_func
#
#
# c(b_H = 0.5,                                # These were reported by Mahaffy
#   d_H = 0.02,                               #  and make good starters.
#   d_L= 0.9,
#   b_L= 0.03) ->
#   m_parm
# m_parm -> parm_init
# names(m_parm) -> parm_names
#
# init_step_size = 0.05
# max_iter = 1000
# rtol = 1e-3
# tol = 1e-1
#

ode_fit <- function(df,                     # Data to fit. 1st column must be
                    #  the time variable!
                    user_func,              # ode function
                    parm_init,              # Initial parameter guesses
                    parm_names,             # Parameter names (match func)
                    init_step_size = 0.05,  # Step size each iteration
                    max_iter = 1000,        # Maximum iteration steps
                    rtol = 1e-3,            # Smallest step size
                    tol = 1e-1) {           # Smallest RMSE improvement to
  #  continue

  # This is a function that works similarly to nls() or other iterative
  #  approaches, but uses ode() to generate comparison data from the current
  #  parameter estimates. It then uses a steepest descent method to find
  #  the best parameter estimates.
  #
  # The alternative is, I suppose, to estimate the derivatives - finite
  #  differences are probably sufficient for things like populations, but
  #  something else may be needed for other situations - and then estimate
  #  the parameters from the equations with known derivatives. I think that
  #  the approach used here will be less biased.
  #
  # Notes:
  #  1. This currently does NOT work with missing data or irregularly spaced
  #     data.
  #  2. It is a bit clunky, and probably doesn't have all of the safeguards
  #     that it should. Caveat inquisitor! (And report problems to BPR.)

  # Check inputs:
  #  Check for NA in data frame.

  # This changes the order of parameters to allow apply() to be used below.
  # Probably should go to map or something to do better than apply and not
  #  need this.
  # ode_wrap <- function(parms, y, times, func) {
  #   require(deSolve)
  #   return(ode(y,times, func, parms))}

  df[1,1] ->
    time_offset
  df[,1] ->
    data_vec
  for(index in 2:ncol(df)) {
    c(data_vec, df[,index]) ->
      data_vec
  }

  parm_init ->
    current_parm

  as.vector(t(df[1,-1])) ->
    init_data
  colnames(df)[-1] ->
    attr(init_data, "names")

  init_step_size ->
    current_step_size

  1 -> iterations
  TRUE -> continue

  while(continue == TRUE) {

    list() ->
      grid_ls
    for(index in 1:length(current_parm)) {
      unname(c(current_parm[index] *
                 (1 - current_step_size),
               current_parm[index],
               current_parm[index] *
                 (1 + current_step_size))) ->
        grid_ls[[index]]
    }
    expand.grid(grid_ls) ->
      parm_grid

    parm_names ->
      colnames(parm_grid)
    NULL ->                                   # Clean up
      attr(parm_grid, "out.attrs")
    nrow(parm_grid) ->                        # Number of parameter combos to
      numChecks                               #  check. Should be 3 ^ (number
    #  of parameters).

    # Because of potential convergence issues, we do NOT want to use
    #  apply(). Hence, we will look through things as they appear.
    vector("list",
           length = numChecks) ->
      ode_ls
    for(index in 1:numChecks) {
      #if(index %%10 == TRUE) {cat("Index:", index, '\n')}
      ode(y = init_data,
          times = 0:(nrow(df)-1),
          func = user_func,
          parms = parm_grid[index,]) ->
        ode_ls[[index]]
    }

    #     ode_ls[[1]][,1] ->                      # Can move outside the loop
    #       t_vec
    #
    #     for(index in 1:length(init_data)) {
    #       matrix(NA,
    #              nrow = length(t_vec),
    #              ncol = numChecks) ->
    #         v_mat
    # #      for(parmNum in 1:numChecks) {
    #      for(parmNum in 1:numChecks) {
    #        as.vector(ode_ls[[parmNum]][,(index+1)]) ->
    #          v_mat[,parmNum]
    #      }
    #       assign(x = v_mat,
    #         value = names(init_data)[1],)
    #     }

    # lapply(List, '[[', 3)  # This returns a list with only the third element
    # unlist(lapply(List, '[[', 3)) # This returns a vector with the third element
    #
    # unlist(lapply(ode_ls, '[[', 1)) -> dum1

    #    suppressWarnings(                      # Avoid convergence failure message.
    # apply(parm_grid,
    #     1,
    #     ode_wrap,
    #     init_data,
    #     0:(nrow(df)-1),
    #     user_func) -> #) ->
    # ode_mat                               # Each column corresponds to a row
    #  of parm_grid. The first nrow(df)
    #  rows are the times (without
    #  offset), the next nrow(df) rows
    #  are the first variable (Hares),
    #  and the last nrow(df) rows are
    #  the second variable (Lynx), etc.
    # nrow(ode_mat) ->
    #   nr
    #
    # # Need to deal with problems in ode_mat having some NA due to non-
    # #  convergence of ode().
    # data.frame(kr1 = rep(
    #   complete.cases(ode_mat[1:nrow(df),]),3),
    #            kr2 = rep(
    #              complete.cases(ode_mat[(nrow(df)+1):(2*nrow(df)),]),3),
    #            kr3 = rep(
    #              complete.cases(ode_mat[(2*nrow(df)+1):(3*nrow(df))]),3)) ->
    #   keep_df
    #
    # ode_mat[which(complete.cases(keep_df) == TRUE),] ->
    #   ode_mat
    # Should do the next with map or apply or something...eventually...
    vector(mode = "numeric",
           length = numChecks) ->
      rmse_vec
    for(index in 1:numChecks) {
      for(varNum in 1:length(init_data)) {
        rmse_vec[index] +
          sqrt(
            sum((ode_ls[[index]][,(varNum + 1)] -
                   df[,(varNum + 1)]) ^ 2,
                na.rm = TRUE)) ->
          rmse_vec[index]
      }
    }
    # lapply(ode_ls,
    #        function(x) {
    #          sum(
    #           apply(x[,-1],
    #                 2,
    #                 function(y) {
    #
    #                 })
    #          )
    #        })
    #
    #     apply(ode_mat[-c(1:(nrow(df))),],         # Now, get an rmse for each
    #           2,                                #  parameter value.
    #           function(x) {
    #             sqrt(sum((x - data_vec[-c(1:nrow(df))])^2))}) ->
    #       rmse_vec
    #
    which(rmse_vec == min(rmse_vec)) ->
      rmse_min
    rmse_vec[(1 + numChecks) / 2] ->
      rmse_current

    if(rmse_vec[rmse_min] < rmse_current - tol) {
      names(current_parm) ->
        names_keep
      as.vector(t(parm_grid[rmse_min,])) ->
        current_parm
      names_keep ->
        names(current_parm)
      current_step_size * 2 ->               # 2 is arbitrary, but NRiC says
        current_step_size                    #  "substantial factor." 10 causes
      #  many warnings
    } else {
      current_step_size / 2 ->              # 2 is arbitrary
        current_step_size
    }

    if(current_step_size < rtol) {            # Stop if step size too small
      FALSE -> continue
    }
    if(iterations >= max_iter) {               # Stop if too long
      FALSE -> continue
    }
    #    cat(paste("RMSE", rmse_current, '\n'))

    iterations + 1 ->
      iterations
  }

  ##### Fix this return. Must calculate the "PredictedData"

  list("Parameters" = current_parm,
       "Iterations" = iterations,
       "RMSE" = rmse_vec[rmse_min],
       "Step" = current_step_size,
       "PredictedData" =
         data.frame("time" = df[,1],
                    "x" = ode_ls[[rmse_min]][,2],
                    "y" = ode_ls[[rmse_min]][,3])) ->
    fit_ls
  colnames(df) ->
    colnames(fit_ls$PredictedData)

  return(fit_ls)
}
