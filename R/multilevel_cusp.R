#' mlmCusp
#'
#' @param <df> data frame with five columns
#' @param <column_names> name of the ID, time, control parameter 1, control
#' parameter 2, and outcome columns, respectively
#' @keywords cusp; catstrophe
#' @description
#' mlmCusp() estimates a cusp surface for nested data (e.g., for the time
#' series of multiple people).
#'
#' @export
#' @details
#' mlmCusp() extends cusp::cusp() to data that are both within case (e.g.,
#' longitudinal) and between-case (whereas cusp::cusp() only works with
#' between-case data). See Butner, et al., 2014, for more details.
#'
#' The data frame, df, passed to mlmCusp() should have five columns; if it has
#' more than five, then only the first five columns are used unless the column
#' names are given in the column_names parameter. The assumed order of the
#' columns is ID-time-control_1-control_2-outcome, again unless the column
#' names are given in the column_names parameter.
#'
#' Similar to cusp::cusp(), mlmCusp assumes that the asymmetry and bifurcation
#' parameters may be linear combinations of the control_1 and control_2
#' parameters passed to the function.
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#' @references
#' Butner, J. E., Dyer, H. L., Malloy, R., & Kranz, L. (2014). Uncertainty in
#' cost performance as a function of the cusp catastrophe in the NASA program
#' performance management system. Nonlinear Dynamics, Psychology, and Life
#' Sciences 18(4), 397-418.
#' Grasman, R. P. P. P., Maas, H. L. J. van der, & Wagenmakers, E.-J. (2009).
#' Fitting the cusp catastrophe in R: A cusp cackage primer. Journal of
#' Statistical Software, 32(8). https://doi.org/10.18637/jss.v032.i08
#' @seealso
#' cusp::cusp()
#' @examples
#' mlmCusp()
#'
mlmCusp <- function() {    #
  # ==== Description ================================================== #
  #
  # mlmCusp()
  #
  # ==== Validate parameters ========================================== #
  #
  # For now, limit this to five column data frames: ID, time, x & y (control
  #  parameters), and z (outcome).
  #
  # To do later: more than 2 control parameters.

  # Ugh...this probably has to be written from scratch. lme4 doesn't
  #  do longituidnal(????) and nlme doesn't do some of the things that
  #  are needed. Oh, well...one fewer dependence that way.
}
