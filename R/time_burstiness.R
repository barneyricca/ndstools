#' time_burstiness
#'
#' @param times numeric vector
#' @param min_time the time at which the data sequence begins (assumed to be
#' 0)
#' @param min_iet minimum interevent sequence spacing
#' @keywords burstiness
#' @description time_burstiness() estimates the burstiness coefficient for a
#' sequence conataining the series of times at which an event occurs.
#'
#' See burstiness() to find the burstiness of a time-series of categorical
#' data.
#'
#' There is a known problem with a non-NULL minimum inter-event time. That
#' needs to be corrected.
#' @export
#' @details
#' time_burstiness() returns the burstiness number for a sequence of event
#' times (e.g., the successive times at which an event occurs).
#'
#' Standard errors for the resulting burstiness coefficients can be
#' obtained using block resampling, but that has not been implemented here yet.
#' (It seems that, unlike burstiness(), boot:tsboot() will not return the
#' correct standard errors for time_burstiness().)
#'
#' There may still be a bug in the non-NULL minimum inter-event times
#' parameter.
#'
#' @author Barney Ricca barneyricca@gmail.com
#' @references Kim, E.-K., & Jo, H.-H. (2016). Measuring burstiness for finite
#' event sequences. Physical Review E, 94(3), 032311.
#' https://doi.org/10.1103/PhysRevE.94.032311
#'
time_burstiness <- function (times,
                             min_time = 0,
                             min_iet = NULL) {

  # ==== Validate data ============================================== #
  if (is.numeric(times) == FALSE) {
    cat("Invalid sequence type passed to time_burstiness().\n")
    cat("Did you mean to use burstiness()?\n")
    return(NULL)
  }
  if (length(times) == 0) {
    cat("Empty sequence.\n")
    return(NULL)
  }
  if (is.null(min_iet) == TRUE) {
    iet_vec <- diff(times, 1)
    mean_iet <- mean(iet_vec, na.rm = TRUE)
    sd_iet <- stats::sd(iet_vec, na.rm = TRUE)
    r <- sd_iet/mean_iet
    n <- length(iet_vec)
    B <- (r * sqrt(n + 1) - sqrt(n - 1)) /
      (r * (sqrt(n + 1) - 2) + sqrt(n - 1))
    return(B)
  }

  if (is.null(min_iet) == FALSE) {
     if (min_iet > 0) {

       y_tilde <- min_iet/(max(times) - min_time)

       iet_vec <- diff(times, 1)
       mean_iet <- mean(iet_vec, na.rm = TRUE)
       sd_iet <- stats::sd(iet_vec, na.rm = TRUE)
       r <- sd_iet/mean_iet
       n <- length(iet_vec)
       B <- ((n - 2) *
               (r * sqrt(n + 1) -
                  (1 - n * y_tilde) * sqrt(n - 1))) /
         (r * (n * sqrt(n + 1) - 2 * (n - 1)) +
            (1 - n * y_tilde) * sqrt(n - 1) *
            (n - 2 * sqrt(n + 1)))
       return(B)
     }
     cat("Invalid minimum inter-event time encountered!/n")
     cat("Minimum inter-event time must be NULL or positive./n")
     cat(paste(min_iet, "is invalid./n"))
     return(NULL)
   }
  cat("Other error/n")                      # Should never get here
  cat("Please report this message/n")
  return(NULL)
}

