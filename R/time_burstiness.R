#' time_burstiness
#'
#' @param ts numeric vector (representing event times).
#' @param min_iet minimum interevent sequence spacing
#'
#' @keywords burstiness
#'
#' @description
#' burstiness() estimates the burstiness coefficient for events.
#'
#' @export
#'
#' @details
#' time_burstiness() returns a vector with an estimated burstiness coefficient
#' for the events whose times are given in the data sequence.
#'
#' There is a known problem with a non-NULL minimum inter-event time. That
#' needs to be corrected.
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#'
#' @references
#' Kim, E.-K., & Jo, H.-H. (2016). Measuring burstiness for finite event
#' sequences. Physical Review E, 94(3), 032311.
#' https://doi.org/10.1103/PhysRevE.94.032311
#'
#'
#'
time_burstiness <- function(ts,              # vector of code sequence
                            min_iet = NULL) {# minimum inter-event time

  # ==== Validate parameters ========================================== #
  if(is.numeric(ts) == FALSE) {             # Validate sequence
    cat("Invalid sequence type passed to time_burstiness().\n")
    cat("Did you mean to use event_burstiness()?\n")
    return(NULL)
  }

  if(length(ts) > 0) {                      # If ts has entries
    sort(unique(ts)) ->                     # Unique codes
      ts_vec
  } else {
    cat("Empty sequence.\n")
    return(NULL)
  }

  if(is.null(min_iet) == FALSE) {
    if(min_iet < 0 |
       min_iet > max(code_vec)) {
         cat("Minimum inter-event time is invalid.\n")
         return(NULL)
    }
  }

  # ==== Case 1: NULL minimum inter-event time ==================== #

  if(is.null(min_iet) == TRUE) {            # Use Kim & Jo, eqn. 22
    # No minimum inter-event time.
    diff(ts_vec, 1) ->                      # Interevent times
      gaps_vec
    mean(gaps_vec, na.rm = TRUE) ->         # mean(interevent times)
      mean_iet
    stats::sd(gaps_vec, na.rm = TRUE) ->    # sd(interevent times)
      sd_iet
    sd_iet / mean_iet ->                    # Coefficient of variation
      r
    length(ts_vec) ->                       # Number of interevent times
      n
    (r * sqrt(n+1) - sqrt(n-1)) /           # Burstiness (K & J, eq. 22)
      (r * (sqrt(n+1) - 2) + sqrt(n-1)) ->
      B
    return(B)
  }

  # ==== Case 2: Minimum inter-event time =========================== #
  if(is.null(min_iet) == FALSE) {           # Minimum inter-event time
    if(min_iet > 0) {                       # Must be positive!
#      if(min_iet >= 1) {                   # Must be a positive integer!
      min_iet / length(ts_vec) ->           # For K & J, eq. 28
        y_tilde
      diff(ts_vec, 1) ->                    # Interevent times
        gaps_vec
      mean(gaps_vec, na.rm = TRUE) ->       # mean(interevent times)
        mean_iet
      stats::sd(gaps_vec, na.rm = TRUE) ->  # sd(interevent times)
        sd_iet
      sd_iet / mean_iet ->                  # Coefficient of variation
        r
      length(ts_vec) ->                     # Number of interevent times
        n
      ((n - 2) * (r * sqrt(n+1) - (1 - n * y_tilde) * sqrt(n-1))) /
        (r * (n * sqrt(n+1) - 2*(n-1)) +
           (1 - n * y_tilde) * sqrt(n-1) * (n - 2 * sqrt(n+1))) ->
        B                                   # Burstiness (K & J, eq. 28)
      return(B)
    } else{
      cat("Invalid minimum inter-event time encountered!/n") # Non-positive
      cat("Minimum inter-event time must be NULL or a positive integer./n")
      cat(paste(min_iet, "is invalid./n"))
      return(NULL)
    }
  }

  return(NULL)
}
