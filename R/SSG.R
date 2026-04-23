#' SSG
#'
#' This function returns a numeric sequence corresponding to a text
#' file, suitable for RQA use.
#'
#' @param <x> text file name
#'
#' @keywords clean text
#'
#' @description
#' SSG creates a state space grid of the data
#'
#' @export
#'
#' @details
#' SSG creates and plots a state space grid of the data.
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#'
SSG <- function(ts1,              # time series
                 ts2,             # time series 2
                 times = NULL,    # ts times - not yet implemented
                 cats1 = NULL,
                 cats2 = NULL) {  # category vector
  require("scales")

  if(length(unique(ts1)) > 10 | length(unique(ts2)) > 10) {
    cat("More than 10 unique codes; cannot be plotted.\nAre data continuous?\n")
    return(NULL)
  }

  if(is.null(cats1) == TRUE) {          # Set up the axes labels
    1:length(unique(ts1)) -> cat_num1
    sort(unique(ts1)) -> names(cat_num1)
  } else {
    1:length(cats1) -> cat_num1
    cats1 -> names(cat_num1)
  }

  if(is.null(cats2) == TRUE) {          # Set up the axes labels
    1:length(unique(ts2)) -> cat_num2
    sort(unique(ts2)) -> names(cat_num2)
  } else {
    1:length(cats2) -> cat_num2
    cats2 -> names(cat_num2)
  }

  cat_num1[ts1] ->
    seq_num1
  cat_num2[ts2] ->
    seq_num2

  jitter(x = seq_num1,
         factor = 1.5,
         amount = NULL) -> jt1
  jitter(seq_num2,
         factor = 1.5,
         amount = NULL) -> jt2

  if(is.null(times) == TRUE) {
    rep(1, length(jt1)) -> pt_sizes
  }
  if(is.null(times) == FALSE) {
      diff(times) -> pt_sizes
    sqrt(pt_sizes * 4 / max(pt_sizes)) ->
      pt_sizes
    if(length(pt_sizes) ==
       (length(jt1) - 1)) {
      c(pt_sizes, 0.1) -> pt_sizes            # Placeholder for the last time
    }
  }
  {
    plot(jt1, jt2,
         pch = 16,
         xaxt = "n",  # No numbers on x-axis
         yaxt = "n",
         xlab = "First time series",
         ylab = "Second time series",
         cex = pt_sizes)
    grid(length(cat_num1), length(cat_num2))
    axis(1,
         at = 1:length(cat_num1),
#         at = seq(from = 0.5,
#                  to = length(cat_num1)+0.5,
#                  by = 1),
         lab = names(cat_num1))
    axis(2,
         at = 1:length(cat_num2),
#         at = seq(from = 0.5,
#                  to = length(cat_num1)+0.5,
#                  by = 1),
         lab = names(cat_num2))
    # xlab, ylab, change number to categories, change boxes,
    #  gridlines
#    x <- seq(-4, 4, len = 101)
#    plot(x,sin(x),type="l",xaxt="n", col="red",
#                                    xlab=expression(paste("Phase Angle ",phi)),
#                                    ylab=expression("sin "*phi))
#    axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
#         lab = expression(-pi, -pi/2, 0, pi/2, pi))
    lines(jt1, jt2,
          col = alpha("blue", 0.60),
          lwd = 0.7)
  }
  return(NULL)
}
