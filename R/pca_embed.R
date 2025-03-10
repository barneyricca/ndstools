#' pca_embed
#'
#' @param <ts> numeric vector to embed
#' @param <dstep> (constant) step size for delays
#' @param <m.max> maximum embedding dimension to consider
#' @param <do.plot> boolean value; display the scree plot or not?
#' @keywords embedding
#' @description This function computes the delay state space embedding of a
#' time series, using principal component analysis.
#' @details
#' A potential embedding matix of ts is created and then principal component
#' analysis is used to return an appropriate embedding dimension. m.max should
#' be set to a value much larger than the anticipated embedding dimension and
#' dstep should be set to a small (positive integer) value, unless there are
#' concerns that the data are oversampled.
#' @returns The function returns a list with three elements, the estimated
#' embedding dimension (obtained by a segmented regression of the scree), the
#' PCA object, and the embedded data. The PCA object is returned to allow the
#' user to refine the estimation of embedding dimension and embedded data.
#' @export
#' @author Barney Ricca <barneyricca@gmail.com>
#'
pca_embed <- function(ts,                   # Time series vector
                      dstep = 1,            # Step size for delays
                      m.max = 20,           # Maximum dimension
                      do.plot = TRUE) {

  if(is.null(ts) == TRUE) {
    cat("ts passed to pca_embed() is NULL!\n")
    return(NULL)
  }

  if(is.numeric(ts) == FALSE) {           # Is ts a numeric vector?
    cat("ts passed to pca_embed() must be numeric!\n")
    return(NULL)
  }

  # Could force dstep and m.max to be positive integers
  if(m.max <= 0) {
    cat("Invalid maximum dimension in pca_embed!\n")
    return(NULL)
  }
  if(as.integer(dstep) <= 0) {
    cat("Invalid delay in pca_embed()\n")
    return(NULL)
  }

  # Create the matrix of delayed vectors; this is equivalent to the usual
  #  embedding call embedd(ts, m.max, dstep)
  matrix(NA,
         ncol = ceiling(m.max/dstep),
         nrow = length(ts) + 1 - ceiling(m.max/dstep)) ->
    full_embed

  for(column in 1:ncol(full_embed)) {
    (column-1) * delay + 1 ->
      start_val
    ts[start_val : (length(ts) + column - ceiling(m.max/dstep))] ->
      full_embed[, column]
  }

  stats::prcomp(full_embed) ->              # By default, centers!
    pca_res
  pca_res$sdev ->
    psd

  100 * (psd * psd) / sum(psd * psd) ->
    scree

  # Find the elbow
  data.frame(n = 1:length(scree),
             scree = scree) ->
    df
  lm(scree ~ n,
     data = df) ->
    lm1
  unname(
    round((segmented::segmented(lm1))$indexU$n, 0)) ->
    elbow

  if(do.plot == TRUE) {
    plot(df$n, df$scree,
         type = "b",
         xlab = "Component",
         ylab = "Variance explained")
  }

  return(list(emb_dim = elbow,
              pca = pca_res,
              embedded = predict(pca_res)[,1:elbow]))
}
