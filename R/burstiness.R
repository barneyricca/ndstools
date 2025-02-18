#' burstiness
#'
#' @param <ts> character vector
#' @param <min_iet> minimum interevent sequence spacing
#' @keywords burstiness
#' @description
#' burstiness() estimates the burstiness coefficient for each of the unique
#' codes in a time series.
#'
#' @export
#' @details
#' burstiness() returns a named vector with an estimated burstiness coefficient
#' for each unique code in the data sequence, including blank or missing codes.
#'
#' Standard errors for the resulting burstiness coefficients can be
#' obtained via boot::tsboot(ts, statistic = burstiness, R = 100, sim =
#' "fixed", l = length(ts)/2) or the like.
#'
#' See time_burstiness() to estimate the burstiness a vector containing the
#' times at which an event occurs.
#'
#' There is a known problem with a non-NULL minimum inter-event time. That
#' needs to be corrected.
#' #'
#' @author
#' Barney Ricca barneyricca@gmail.com
#' @references
#' Kim, E.-K., & Jo, H.-H. (2016). Measuring burstiness for finite event
#' sequences. Physical Review E, 94(3), 032311.
#' https://doi.org/10.1103/PhysRevE.94.032311
#' Ricca, B. P., & Jordan, M. E. (2022). Dynamical systems measures of small
#' group functioning. International Journal of Complexity in Education, 3(2),
#' 79–108.
#' @seealso
#' time_burstiness
#' @examples
#' burstiness(guastello)
#' burstiness(engineering)
#' burstiness(engineering, 2)
#'
burstiness <- function(ts,                  # vector of code sequence
                       min_iet = NULL) {    # minimum inter-event time
  # ==== Description ================================================== #
  # burstiness() takes a sequence of logical or character entries, and returns
  #  the Kim & Jo (2016) burstiness for each of the unique entries. It assumes
  #  that the data are a sequence of equally spaced (in time) measurements.
  #
  # This can be used with event-based sequences, subject to the usual caveats
  #  about doing so. When working with event-based sequences, contextual
  #  burstiness neds a minimum IET of 2. (I.e., the code must change and
  #  change back.) For collective burstiness, the minimum IET should be 1
  #  but that doesn't seem to be the norm yet. (See Ricca & Jordan, 2020.)

  # ==== Validate parameters ========================================== #
  if(is.character(ts) == FALSE &            # Validate sequence
     is.logical(ts) == FALSE) {
    cat("Invalid sequence type passed to event_burstiness().\n")
    cat("Did you mean to use time_burstiness()?\n")
    return(NULL)
  }

  if(length(ts) > 0) {                      # If ts has entries
    sort(unique(ts)) ->                     # Unique codes
      code_vec
  } else {
    cat("Empty sequence.\n")
    return(NULL)
  }

  if(length(code_vec) > 0) {                # If there are valid codes
    rep(0.0, length(code_vec)) ->           # Pre-allocation
      B
  } else {
    cat("No valid codes found.\n")
    return(NULL)
  }

  if(is.null(min_iet) == FALSE) {
    if(((as.integer(min_iet) - min_iet) >
        .Machine$double.eps^0.5) == TRUE) {
      cat("Minimum inter-event time is invalid.\n")
      return(NULL)
    }
  }

  # ==== Case 1: NULL minimum inter-event time ==================== #

  if(is.null(min_iet) == TRUE) {            # Use Kim & Jo, eqn. 22
    # No minimum inter-event time.
    for(index in 1:length(code_vec)) {      # For each code
      which(ts == code_vec[index]) ->       # Get the positions of the current
        code_pos_vec                        #  code.
      diff(code_pos_vec, 1) ->              # Interevent times
        gaps_vec
      mean(gaps_vec, na.rm = TRUE) ->       # mean(interevent times)
        mean_iet
      stats::sd(gaps_vec, na.rm = TRUE) ->  # sd(interevent times)
        sd_iet
      sd_iet / mean_iet ->                  # Coefficient of variation
        r
      length(code_pos_vec) ->               # Number of interevent times
        n
      (r * sqrt(n+1) - sqrt(n-1)) /         # Burstiness (K & J, eq. 22)
        (r * (sqrt(n+1) - 2) + sqrt(n-1)) ->
        B[index]
    }
    code_vec -> names(B)                    # Identify each B
    return(B)
  }

  # ==== Case 2: Minimum inter-event time =========================== #
  if(is.null(min_iet) == FALSE) {           # Minimum inter-event time
    if(min_iet %% 1 == 0) {                 # Must be an integer!
      if(min_iet >= 1) {                    # Must be a positive integer!
        min_iet / length(ts) ->             # For K & J, eq. 28
          y_tilde
        for(index in 1:length(code_vec)) {  # For each code
          which(ts == code_vec[index]) ->   # Get the positions of the current
            code_pos_vec                    #  code.
          diff(code_pos_vec, 1) ->          # Interevent times
            gaps_vec
          mean(gaps_vec, na.rm = TRUE) ->   # mean(interevent times)
            mean_iet
          stats::sd(gaps_vec, na.rm = TRUE) -> # sd(interevent times)
            sd_iet
          sd_iet / mean_iet ->              # Coefficient of variation
            r
          length(code_pos_vec) ->           # Number of interevent times
            n
          ((n - 2) * (r * sqrt(n+1) - (1 - n * y_tilde) * sqrt(n-1))) /
            (r * (n * sqrt(n+1) - 2*(n-1)) +
               (1 - n * y_tilde) * sqrt(n-1) * (n - 2 * sqrt(n+1))) ->
            B[index]                        # Burstiness (K & J, eq. 28)
        }
        code_vec -> names(B)
        return(B)
      }
      cat("Invalid minimum inter-event time encountered!/n") # Non-positive
      cat("Minimum inter-event time must be NULL or a positive integer./n")
      cat(paste(min_iet, "is invalid./n"))
      return(NULL)
    }
    cat("Invalid minimum inter-event time encountered!/n") # Non-integer
    cat("Minimum inter-event time must be NULL or a positive integer./n")
    cat(paste(min_iet, "is invalid./n"))
    return(NULL)
  }

  cat("Invalid data/n")                     # Should never get here
  cat("Did you mean to use time_burstiness()?/n")
  return(NULL)
}
