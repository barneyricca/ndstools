#' nse
#'
#' @param ts (1-dimensional) numeric vector or (2-dimensional) matrix. If a
#' vector, then it is assumed ts is a data stream to be embedded in a shadow
#' state space. If a matrix, it is assumed that ts represents a data stream
#' embedded in a reconstructed state space.
#' @param frac.learn fraction of data in learning set
#' @keywords Nash Sutcliffe efficiency
#' @description
#' nse() estimates the Nash Sutcliffe efficiency for a time series.
#'
#' @export
#' @details
#' nse() returns a list which includes the Nash-Sutcliffe efficiency.
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#'
#'

nse <- function(ts,
                frac.learn = 0.5) { # fraction in learning set

  # Alternative: hydroGOF::NSE()? That works without emedding, though.

  if(mode(as.matrix(ts)) != "numeric") {
    cat("ts must be numeric!\n")
    return(NULL)
  }

  # Hmm...if ts is not a vector, then it is already embedded, and we should
  #  skip step 1?
  if(is.null(dim(ts))) {
    #Step 1: Time-delay embedding
    #Embed time series (ts)  l
    results.embed_udf<-const_delay_embed(ts)
    m<-results.embed_udf$dim #embedding dimension
    Mx<-results.embed_udf$embedded #embedded data matrix
  } else {
    if(length(dim(ts)) == 2) {
      ts -> Mx
      dim(Mx)[2] -> m
    } else {
      stop("Dimensionality issue with ts in nse()\n")
    }
  }

  # Ties are a problem, so add small random noise to the data
  Mx + stats::rnorm(n = nrow(Mx) * ncol(Mx),
             mean = 0,
             sd = min(stats::sd(Mx, na.rm = TRUE) / 1e3,
                      1e-3)) ->
    Mx

  #Step 2a: Partition Mx into learning and test sets
  # Original:
  # learn.rows<-round(frac.learn * nrow(Mx))
  # Revised:
  floor(frac.learn * nrow(Mx)) ->
    learn.rows

  # After the next, the top half of Mx should be in learn.em.0, while the
  #  bottom half of Mx should be in test.em
  learn.em.0<-Mx[1:learn.rows,] #initial learning set
  test.em<-Mx[(learn.rows+1):nrow(Mx),] #initial test set

  #Step 2b: Prediction
  hold.test<-matrix(0,learn.rows,1)
  hold.pred<-matrix(0,learn.rows,1)

  # Using the Mayport data (with the -99999, not with NA), we see this:
  # For i == 18, there are two dist.ref values that equal zero. Why?
  #  They are at 6 and 451. 451 is the reference point. But why 6?
  # The problem occurs with i = these values:
  # [1]  18 104 109 161 188 199
  #
  #
  # I think just using the smallest non-zero value for u.denom would be
  #  appropriate. At least, it would get past this for now.
  #
  #
  for(i in 1:learn.rows) {
    #print("i");print(i)
    Mx[1:((learn.rows + i) - 1),] ->
      learn.em
    #print("learn.em");print(learn.em)

    #Step 2b(1): Calculate nearest neighbours to last row in learning set
    #  (i.e., learn.em.0). Should this be learn.em, or should the ref point
    #  be learn.em.0?
    #Distance between points on the attractor
    ref.point <- nrow(learn.em) #index of reference point

    # Alternatives:
    # stat::dist(learn.em, diag = TRUE, upper = TRUE) gives the same things.
    # FNN:get.knn() gives almost the same information, but still needs to
    #  be normalized. (It is also sqrt(2) larger for the Hare data.)
    dist<-fields::rdist(learn.em) #distance matrix;

    dist.ref<-dist[,ref.point] #distances from reference point to other points
    sc<-max(dist.ref) #find maximum distance from reference point
    dist.ref.sc<-dist.ref/sc #scale distances from reference point to max distance

    o<-order(dist.ref.sc)  #Remove distance of reference point to itself
    remove<-which(o==nrow(learn.em))

    o1<-o[-remove]
    #Indices of m+1 smallest distances from reference point
    # (m=embedding dimension)
    o2<-o1[1:(m+1)]
    o2<-stats::na.omit(o2)
    dist.ordered<-dist.ref.sc[o2] #ordered distances
#dist.ordered
    # Alternative
    # library(FNN)
    # get.knn(data = cbind(learn.em, learn.em),
    #         k = 3) ->
    #   dum1
    # dum1$nn.index[27,]
    # dum1$nn.dist[27,]
    # # All are sqrt(2) longer distances than KBR method.
    # # Normalizing still must be done.

    #Increment neighbouring indices by 1 period for use in prediction algorithm
    # These are the next points for the nearest neighbors of the current point
    #  on the trajectory. These next points are used to predict the next point
    #  on this trajectory, which will later be compared to the actual next
    #  point on this trajectory.
    o.pred<-o2+1
    pred.ngh<-learn.em[o.pred,] #Nearest neighbours
    #Step 2b(2): Compute prediction as average of neighbouring points weighted by
    #distances from reference point
    # Compute weights (Sughihara et al., 2012)

    # Original:
    u.denom<-dist.ordered[1] #distance from reference point to nearest neighbour

    # This could be vectorized:
    # hold<-matrix(0,(m+1),1)
    # for(k in 1:(m+1)) { #summands in w.denom
    #   #      u.vector<-ifelse(
    #   #        u.denom == 0 & dist.ordered[k] != 0,
    #   #        0,
    #   u.vector <- exp(-dist.ordered[k]/u.denom)
    #   hold[k,1]<-u.vector
    # } #end loop k

  #  u.vector<-hold[1:(m+1)]
    exp(-dist.ordered/u.denom) ->           # This is vectorized
      u.vector # -> hold[,1]

    # w.denom<-sum(u.vector)
    w.vector<-u.vector / sum(u.vector)
#w.vector
    #Prediction of next point on attractor (row in test.em)
    # HBR does not cast the data frame to a matrix; that must be done. (That
    #  might have been a change with R 4.0.0, IIRC.)
    pred.point<-w.vector %*% as.matrix(pred.ngh)
#pred.point
    #Prediction of time series observation is first (unlagged) element of
    #pred.point
    pred.ts<-pred.point[1]
    #print("pred.ts");print(pred.ts)
    #Step 2b(3): Test point on attractor (row in test.em)
    test.point<-test.em[i,]

    #print("test.point");print(test.point)
    #Time series observation to be validated is first (unlagged) element of
    #  test.point
    test.ts<-test.point[1]
    hold.test[i,]<-test.ts #1st element is original data point
    hold.pred[i,]<-pred.ts
#hold.test[i,]
#hold.pred[i,]
  } #end i loop through Mx

  # Hmm...the first element consistently seems to be orders of magnitude
  #  different than the others. I don't know why, but removing that brings
  #  things more into line. Let's see if that works for now.
  1 - (sum((hold.test[-1] - hold.pred[-1])^2) /     # Estimate the NSE
         sum((hold.test[-1] - mean(hold.test[-1]))^2)) ->
    nse
  #print("nse");print(hold.mnse)
  #results<-cbind(learn.0,hold.test,hold.pred)
  #print("learn,test,pred");print(results)
  results<-list("nse" = nse,
                "test" = hold.test,
                "predicted" = hold.pred)
  return(results)
} #end function
