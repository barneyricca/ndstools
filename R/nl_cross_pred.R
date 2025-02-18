#' nl_cross_pred
#'
#' @param <ts> character vector
#' @param <nseg> number of segments to cross validate on
#' @param <m.max> maximum embedding dimension to consider
#' @keywords nonlinear, cross-prediction
#' @description
#' nl_cross_pred() uses the Nash-Sutcliffe efficiency coefficient to test
#' support for stationarity in a signal.
#'
#' @export
#' @details
#' nl_cross_pred() uses the Nash-Sutcliffe efficiency coefficient to estimate
#' how well any one segment of a time-series predicts other segments of the
#' same series. Consistent high cross-prediction supports the hypothesis that
#' the signal is stationary.
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#' @references
#' @seealso
#' @examples
#' nl_cross_pred(mayport)
#'

#
#
# FOR TESTING
#
#
ts = lh_df$Hare
nseg = 3
m.max = 6
#
#
#
#
# Hmm...does this work properly with Mayport data?
# Not as reported in HBR:
#
# Many digamma problems
# different (and negative) values!
# ah...problems with missingness.
read.csv(file = here("Data/Mayport.csv")) ->
  mp
mean(mp$Level) ->
  mp$Level[which(mp$Level < 0)]
mp$Level -> ts
5 -> nseg
6 -> m.max

#
# Better, but still does not correspond to HBR
#       [,1]  [,2]  [,3]  [,4]   [,5]
# [1,] 0.835 0.403 0.335 0.406 0.0876
# [2,] 0.613 0.902 0.654 0.662 0.2600
# [3,] 0.427 0.609 0.867 0.535 0.1550
# [4,] 0.399 0.562 0.488 0.840 0.2550
# [5,] 0.245 0.296 0.260 0.306 0.7156
# cv HBR Table 6.3
# First of all, the diagonals should be equal to 1, right? (Training on
#  a segment to predict that segment should be perfect.)

nl_cross_pred <- function(ts,               # time series signal
                          nseg = 5,         # number of segments
                          m.max = 6) {      # To allow for limiting the
  require(tseriesChaos)

  mean(ts, na.rm = TRUE) ->                 # Simple mean imputation of missing
    ts[is.na(ts)]                           #  data. Do this better! (Kalman?)

  # Step 1: Segment time series x(t)
  seg.length <- floor(length(ts)/nseg)      # length of each segment

  ts[1:(seg.length*nseg)] ->                # Possibly ignore a few data points
    ts                                      #  at the end (remainders)
  c(seg.length, nseg) ->                    # Store ts as a matrix with nseg
    dim(ts)                                 #  columns
  # matrix(ts[1:(seg.length*nseg)],
  #        ncol = nseg,
  #        nrow = seg.length,
  #        byrow = FALSE) ->
  #   seg.matrix

  #Step 2: Nonlinear cross prediction
  matrix(0,                                 # Matrix of Nash-Sutcliffe
         nseg,                              #  efficiences across the segments.
         nseg) ->
    nse.matrix
  for(i2 in 1:nseg) {           # loop over learning sets
    # Inner loop (i3) iterates over test sets to be predicted by each learning
    #  set. The matrix 'hold.nse' is a column vector storing Nash-Sutcliffe
    #  efficiencies for the predicted test sets (rows) by a given learning set.
ts -> ts.keep

print(paste0("i2: ", i2, sep = ""))
    hold.nse <- matrix(0, nseg, 1)
    for(i3 in 1:nseg) { #loop over test sets
print(paste0("i3: ", i3, sep = ""))

ts.keep -> ts

      # seg.matrix[,i2] ->                    # Learning set for forecast
      #   learn
      # seg.matrix[,i3] ->                    # Test set for forecast
      #   test
      ts[,i2] -> learn                        # Learning set for forecast
      ts[,i3] -> test                         # Test set for forecast

      #Step 2a: embedding
      const_delay_embed(learn) ->
        results.embed
      results.embed$delay -> d
      results.embed$dim -> m
      results.embed$embedded ->
        learn.em.0

      # I don't see any reason to not use const_delay_embed() here, except
      #  maybe for speed? const_delay_embed(test,m,d)$embedded should create
      #  test.em just fine, right? This simply renames some columns too
      tseriesChaos::embedd(test,m,d) ->     # initial embedded test set
        test.em
      rbind(learn.em.0,test.em) -> # Stack for use in prediction loop
        Mx

      #
      #
      # OK TO HERE
      #
      #
      #Step 2b: Nonlinear prediction
      results.np<-nse(Mx)

      nse<-results.np[[1]]
      hold.nse[i3,]<-nse
    } #end i3 loop through test sets
    nse.matrix[,i2]<-hold.nse
    nse.matrix[is.na(nse.matrix)] <- 1 #set any na's along diagonal = 1
  } #end i2 loop through learning sets
  #Plot of nse.matrix. Each line shows the Nash-Sutcliffe efficiencies for a given
  #learning set (column of nse.matrix) in 1-step prediction of each test set
  #(rows of nse.matrix) along horizontal axis.
  plot(nse.matrix[,1],type='l',
       xlab="test sets predicted",ylab="nse",ylim=c(0,1),
       font=2,font.lab=2,cex.axis=2,cex.lab=2)
  for(i4 in 2:ncol(nse.matrix)) {
    lines(nse.matrix[,i4],
          col = i4)
  } #end loop

  return(nse.matrix)
}
