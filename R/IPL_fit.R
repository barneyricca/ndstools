#' IPL_fit
#'
#' @param x numeric or character
#' @param estimator Estimation method ("CSN", "Hill", "moments" or "peaks", or "Pickands")
#' @param gof Goodness of fit measure (pseudo-R-squared, KS, or loglikelihood)
#'
#' @keywords power-law
#'
#' @description This function estimates the (inverse) power-law fit to a
#' dataset using one of several estimators.
#'
#' @details
#' IPL_fit returns the fit parameters and a goodness of fit measure for a
#' power-law fit to data.
#'
#' @export
#'
#' @author Barney Ricca <barneyricca@gmail.com>
#'
#' @references Nair, J., Wierman, A., & Zwart, B. (2022). The Fundamentals of
#'  heavy tails: Properties, emergence, and estimation. Cambridge University
#'  Press. (See Chapter 9.)
#'
IPL_fit <- function(x,                  #
                    estimator = "CSN", #
                    gof = "logLik") {   #

  if(is.null(x) == TRUE) {
    cat("x is NULL in IPL_fit!/n")
    return(NULL)
  }
  if(is.numeric(x) == FALSE &               # Correct data?
     is.character(x) == FALSE) {            #
    cat("x in IPL_fit must be numeric or character!/n")
    return(NULL)
  }

  NA -> fit_parm                            # Return NA if something goes
  NA -> logLik                              #  wrong.

  if(estimator == "CSN") {
    igraph::fit_power_law(x = x) ->
      ipl
    -ipl$alpha -> fit_parm
    if(gof == "logLik") {
      ipl$logLik -> logLik
    } else {
      if(gof == "KS") {
        ipl$KS.stat -> logLik
      }
    }
  }

  if(is.na(fit_parm) == TRUE &
     is.na(logLik) == TRUE) {
       cat("Estimator method in IPL_fit is not implemented!\n")
       cat("Returning NA\n")
  }

  return(c(fit_parm, logLik))               # Number of dimensions
}
