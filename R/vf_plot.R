#' Plot a vector field
#'
#' @param df A data frame of data to fit. The first four columns must be the
#'     (x,y) coordinates and the x- and y-directional derivatives, respectively.
#' @param data_df A data frame of the path (x,y) coordinates in temporal order.
#' @param arrow_size The base size of the arrow heads, in centimeters.
#' @param arrow_scale A scaling factor for the arrow lengths. Larger scales
#'     produce shorter arrows.
#' @description
#' This function takes a data frame of directional derivatives and creates a
#'     vector field plot. The current version DOES NOT check for appropriate
#'     input, so be careful. phaseR::flowField() may be a better way to do
#'     this.
#'
#' @return A ggplot2 object with the vector field plot and path. Labels, main
#'     title, etc., may be added after the return.
#' @export
#'
#' @seealso [vfp_plot()]
#'
#'
#'
vf_plot <- function(df,                     # data frame; see description
                    arrow_size = 0.2,       # In centimeters
                    arrow_scale = 1) {      # Scale arrow length

  df[,1:4] ->                               # Drop extraneous columns; not
    df                                      #  really necessary.

  c("x", "y", "dx", "dy") ->                # Set column names to what are
    colnames(df)                            #  expected.

  df %>%
    ggplot(aes(x = .data$x - .data$dx / arrow_scale,    # Set tail of arrows
               y = .data$y - .data$dy / arrow_scale)) +
    geom_segment(aes(xend = .data$x + .data$dx / arrow_scale,
                     yend = .data$y + .data$dy / arrow_scale),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    xlim(0.97 * min(df$x, na.rm = TRUE),    # Set graph limits
         1.03 * max(df$x, na.rm = TRUE)) +
    ylim(0.97 * min(df$y, na.rm = TRUE),
         1.03 * max(df$y, na.rm = TRUE)) +
    theme_bw() +
    theme(legend.position = "None") ->
    g

  return(g)
}


