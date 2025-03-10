#' cobweb
#'
#' @param x numeric vector of time series data
#' @param title graph title
#' @keywords cobweb
#' @description
#' cobweb() creates a cobweb plot of the data.
#'
#' @export
#' @details
#' cobweb() creates a cobweb plot of the data. It plots the consecutive points
#' of the data, connects adjacent points with lines, and draws a diagonal.
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#'
#' @examples
#' 3.99999 -> R
#' vector(mode = "numeric", length = 500) -> x
#' 0.7 -> x[1]
#' for(i in 2:500) {R * x[i-1] * (1 - x[i-1]) -> x[i]}
#' cobweb(x, paste0("R = ",R))
#'
cobweb <- function(x,
                   title = NULL) {
  # ==== Description ================================================== #
  # cobweb() takes a sequence of numeric entries, and creates a cobweb plot.

  # ==== Validate parameters ========================================== #
  if(is.numeric(x) == FALSE) {
    cat("Invalid sequence type passed to cobweb().\n")
    return(NULL)
  }

  if(is.null(title) == FALSE) {
    if(is.character(title) == FALSE) {
      cat("Invalid title passed to cobweb().\n")
      return(NULL)
    }
  }

  if(length(x) < 2) {                      # If x is too short
    cat("Data passed to cobweb() is too short.\n")
    return(NULL)
  }

  length(x) - 1 ->
    len
  x[-1] -> xp1
  x[1:len] -> x

  rep(x, each = 2) ->                       #  double the x
    xr
  c(x[1],                                   # Except the first, double the y
    rep(xp1, each = 2)) ->
    xp1r
  xp1r[-length(xp1r)] ->
    xp1r

  graphics::par() ->
    par_keep
  graphics::par(pty = 's',
                mai = c(0.80, 0.05, 0.35, 0.05))
  {                                         # Plot
    plot(x, xp1,                            # The points
         pch = 16,
         cex = 0.6,
         xlab = expression(x[n]),
         ylab = expression(x[n+1]),
         main = ifelse(is.null(title) == TRUE,
                       "",
                       title))
    graphics::abline(0,1)                   # The diagonal
    graphics::lines(xr,xp1r,                # The cobweb
          lwd = 0.4)
  }
  suppressWarnings(
    graphics::par(par_keep))
  #return(NULL)                             # No return message if no
                                            #  return() command.

}
