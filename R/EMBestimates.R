#' Create a Empirical Bayes estimate of derivatives
#'
#' @param DataM A data sequence
#' @param Nobs Embedding dimension
#' @param maxorder The highest order of derivative to estimate
#' @param dt NA means all time intervals are equal
#' @param TimeM Times of the data (not used of dt is NA)
#'
#' @return A matrix of derivatives, in columns
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' 1:100 -> t
#' 4 * exp(-t/20) * sin(pi * t / 5) + rnorm(100, 0, 0.1) -> x
#' EMBestimates(x, 2, 2) -> dx
EMBestimates <- function(DataM,             # Data sequence
                         Nobs,              # Embedding dimension
                         maxorder=2,        # "2" for 2nd derivative
                         dt=NA,             # If all time intervals equal
                         TimeM=NA) {        # Times of data
  # This function is actually Jon Butner's modification of Pascal Deboeck's
  #  work, cleaned up a bit by BPR for use here.

  # ==== CHeck inputs ================================================= #
  if((is.na(dt))&(sum(!is.na(TimeM))==0)) {
    return("Error: dt or TimeM must be provided.")
  }
  if(is.vector(DataM)) {
    DataM <- matrix(DataM,nrow=1)
  }
  if(is.na(dt)) {
    if(is.vector(TimeM)) {
      TimeM <- matrix(TimeM,nrow=1)
    }
    if(sum(dim(TimeM)==dim(DataM))!=2) {
      return("Error: DataM and TimeM dimensions do not match.")
    }
  }

  # ==== Create long data ============================================== #
  LongData <- rep(NA,3+maxorder)
  uIDcreate <- (10^(nchar(dim(DataM)[2])+1))
  for(i in 1:dim(DataM)[1]) {
    tmp.DataM <- embed(DataM[i,],Nobs)[,Nobs:1]
    if(is.na(dt)) {
      tmp.TimeVec <- embed(TimeM[i,],Nobs)[,Nobs:1]
    } else {
      tmp.TimeVec <- embed(seq(1,by=dt,length.out=dim(DataM)[2]),Nobs)[,Nobs:1]
    }

    tmp.TimeVec <- as.vector(t(apply(tmp.TimeVec,1,function(x){x-mean(x,na.rm=T)})))
    tmp.TimeM <- tmp.TimeVec^0
    if(maxorder>=1) {
      for(k in 1:maxorder) {
        tmp.TimeM <- cbind(tmp.TimeM,(tmp.TimeVec^k)/factorial(k))
      }
    }

    tmp.ncol <- apply(embed(1:dim(DataM)[2],Nobs)[,Nobs:1],1,mean)
    uniqueid <- rep(tmp.ncol,Nobs)+(i*uIDcreate)
    tmp.new <- cbind(as.vector(uniqueid),as.vector(tmp.DataM),tmp.TimeM)
    LongData <- rbind(LongData,tmp.new[order(tmp.new[,1]),])
  }
  LongData <- LongData[-1,]
  colnames(LongData) <- c("uniqueid","Y",paste("d",0:maxorder,sep=""))
  LongData <- data.frame(LongData)

  # ==== Create, Run and Return lme4 Model============================== #
  Model <- lme4::lmer(formula(
    paste("Y~",
          paste(names(LongData)[3:ncol(LongData)], collapse="+"),
          "-1+(-1+",
          paste(names(LongData)[3:ncol(LongData)], collapse="+"),
          "|uniqueid)")),
    data=LongData)

  ids <- as.numeric(rownames(coef(Model)$uniqueid))
  #
  # The next was originally from package::Matrix. Must it be?
  # The form that should be used would be (I think!):
  #  Matrix::`.__T__%/%:base`
  #  Matrix::`.__T__%%:base`
  #
  # usethis::check() gives a Note about this that I haven't figure out.
  ids <- cbind(ids %/% uIDcreate,
               ids %% uIDcreate)

  return(
    list(Model=Model,
         Derivatives=coef(Model)$uniqueid,
         nrow=ids[,1],ncol=ids[,2]))
}
