#' Plot a vector field and a data path through that field
#'
#' @param df A data frame of data to fit. The first four columns must be the
#'     (x,y) coordinates and the x- and y-directional derivatives, respectively.
#' @param data_df A data frame of the path (x,y) coordinates in temporal order.
#' @param arrow_size The base size of the arrow heads, in centimeters.
#' @param arrow_scale A scaling factor for the arrow lengths. Larger scales
#'     produce shorter arrows. This will be changed at some point.
#'
#' @return A ggplot2 object with the vector field plot and path. Labels, main
#'     title, etc., may be added after the return.
#' @export
#'
#'
#'
#'
#'
# Fix these!
# Undefined global functions or variables:
#   C0 C1 CSE0f P0 P1 PTSS0f Var1 Var2 cse1_model_inter_ls ids index
# temp_df
#
# And in other functions:
#  dx dy ids ode_mat x y
#
gradient_field <- function(df,
                           xvar,
                           yvar,
                           gam_form,
                           inter = TRUE,
                           xdot = NULL,
                           ydot = NULL,
                           spar = 0.6,
                           arrow_scale = 10,
                           grid_size = 50,
                           display_grid_size = 20) {

  # generalize the stuff from Dolezal (and add some error checking). Either pass
  #  x and y, or x, y, xdot, and ydot to do this.

  # This function returns a ggplot of the vector field for the passed data.

  # Check: df is a data.frame, xvar and yvar are columns in the data frame,
  #  xdot and ydot are the same length as df, arrow_scale > 0,
  #  grid_size, dispaly_grid_size > 0, and inter is logical.

  if(is.null(xdot) == TRUE) {
    doremi::calculate.fda()
  }

  if(is.null(ydot) == TRUE) {
    doremi::calculate.fda()
  }

  # estimate the GAM; interaction or not is passed as a parameter
  temp_df %$%
    mgcv::gam(formula = PTSS1f ~ t2(PTSS0f) +
                t2(CSE0f) +
                t2(PTSS0f, CSE0f)) ->
    ptss1_model_inter_ls[[index]]


  # For each value of P & C, center an arrow that has components
  #  Pdot and Cdot (from the predicted)

  df %$%
    expand.grid(
      seq(min(CSE0f, na.rm = TRUE),
          max(CSE0f, na.rm = TRUE),
          length.out = display_grid_size),
      seq(min(PTSS0f, na.rm = TRUE),
          max(PTSS0f, na.rm = TRUE),
          length.out = display_grid_size)) %>%
    dplyr::mutate(CSE0f = Var1) %>%
    dplyr::mutate(PTSS0f = Var2) %>%
    dplyr::select(PTSS0f, CSE0f) ->
    df0

  data.frame(C0 = df0$CSE0f,
             P0 = df0$PTSS0f,
             C1 = stats::predict(object =
                                   cse1_model_inter_ls[[index]],
                                 newdata = df0),
             P1 = stats::predict(object =
                                   ptss1_model_inter_ls[[index]],
                                 newdata = df0)) ->
    df1

  ggplot2::ggplot(data = df,
                  aes(x = CSE0f,
                      y = PTSS0f
                  )) +
    ggplot2::geom_path(colour = "green",
                       linewidth = 1.2) +
    ggplot2::geom_point(colour = "forestgreen",
                        size = 2) +
    ggplot2::geom_segment(data = df1,                  # Center an arrow on each (C0,P0)
                          inherit.aes = FALSE,
                          aes(x = C0 - C1 / arrow_scale,
                              y = P0 - P1 / arrow_scale,
                              xend = C0 + C1 / arrow_scale,
                              yend = P0 + P1 / arrow_scale),
                          arrow = arrow(length = unit(0.2, "cm"))) +
    ggplot2::xlim(0.97 * df %$%
                    min(CSE0f, na.rm = TRUE),
                  1.03 * df %$%
                    max(CSE0f, na.rm = TRUE)) +
    ggplot2::ylim(0.97 * df %$%
                    min(PTSS0f, na.rm = TRUE),
                  1.03 * df %$%
                    max(PTSS0f, na.rm = TRUE)) +
    ggplot2::xlab("CSE Level") +
    ggplot2::ylab("PTSS Level") +
    ggplot2::ggtitle(paste("CSE-PTSS Gradient for Participant",
                           ids[index])) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "None") ->
    g

  return(g)
}
