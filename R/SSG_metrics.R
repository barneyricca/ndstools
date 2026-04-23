#' SSG_metrics
#'
#' This function returns a numeric sequence corresponding to a text
#' file, suitable for RQA use.
#'
#' @param <x> text file name
#'
#' @keywords clean text
#'
#' @export
#'
SSG_metrics <- function(ts1,             # time series
                        ts2,             # time series 2
                        times = NULL,    # ts times - not yet implemented
                        cats1 = NULL,
                        cats2 = NULL) {  # category vector

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

  cat_num1[ts1] -> seq_num1
  cat_num2[ts2] -> seq_num2

  jitter(ts1) -> jt1
  jitter(ts2) -> jt2

  if(is.null(times) == TRUE) {
    plot(jt1, jt2,
         pch = 16)
    lines(jt1, jt2)
  }


}

