#' Plot a vector field and a data path through that field
#'
#' @param df A data frame of data to fit. The first four columns must be the
#'     (x,y) coordinates and the x- and y-directional derivatives, respectively.
#' @param data_df A data frame of the path (x,y) coordinates in temporal order.
#' @param arrow_size The base size of the arrow heads, in centimeters.
#' @param arrow_scale A scaling factor for the arrow lengths. Larger scales
#'     produce shorter arrows.
#'
#' @description
#' This function takes a data frame of directional derivatives and a data
#'     frame of data points, and creates a vector field plot with the path
#'     superposed on it. The current version DOES NOT check for appropriate
#'     input, so be careful.
#'
#' @return A ggplot2 object with the vector field plot and path. Labels, main
#'     title, etc., may be added after the return.
#' @export
#'
#'
#'
#'
#'
vfp_plot <- function(df,                    # Arrow data
                     data_df,               # Path data
                     arrow_size = 0.2,      # In centimeters
                     arrow_scale = 1) {     # Scale arrow length

  # Need to do some error checking: existence of columns? numeric?
  #  not entirely constant x or y (for xlim and ylim)?

  c("x", "y", "dx", "dy") ->                # Set column names to what are
    colnames(df)[1:4]                       #  expected.
  c("x", "y") ->                            #
    colnames(data_df)[1:2]                  #

  ggplot(data = data_df,
         aes(x = .data$x,
             y = .data$y)) +
    geom_path(colour = "green",
              linewidth = 1.2) +
    geom_point(colour = "forestgreen",
               size = 2) +
    geom_segment(data = df,
                 inherit.aes = FALSE,
                 aes(x = .data$x - .data$dx / arrow_scale,
                     y = .data$y - .data$dy / arrow_scale,
                     xend = .data$x + .data$dx / arrow_scale,
                     yend = .data$y + .data$dy / arrow_scale),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    xlim(0.97 * min(df$x, na.rm = TRUE),    # Set graph limits
         1.03 * max(df$x, na.rm = TRUE)) +
    ylim(0.97 * min(df$y, na.rm = TRUE),
         1.03 * max(df$y, na.rm = TRUE)) +
    theme_bw() +
    theme(legend.position = "None") ->
    gp

  return(gp)
}
