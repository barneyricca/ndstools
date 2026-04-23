#' ccm
#'
#' @param x numeric vector of time series data
#' @param title graph title
#'
#' @keywords cobweb plot
#'
#' @description
#' ccm() estimated convergent cross mapping prediciton skill
#'
#' @export
#'
#' @details
#' ccm() is from HBR, Code 8.1, modified by BPR
#'
#' @author
#' Barney Ricca barneyricca@gmail.com
#'
#'@references Huffaker, Ray; Bittelli, Marco; Rosa, Rodolfo. Nonlinear Time Series Analysis with R (p. 224). (Function). Kindle Edition.
ccm <- function(ts,
                model = 1        # Model to run
                ) {

  # ==== Validate parameters ========================================== #
  if(is.numeric(x) == FALSE) {
    cat("Invalid sequence type passed to cobweb().\n")
    return(NULL)
  }

#  dir.results<-as.character("C:/Results")  #Load user-defined functions
#  setwd(dir.udf)
#  dump("embed_udf", file="embed_udf.R")
#  source("embed_udf.R")  #Load libraries
#  library(tseriesChaos)
#  library(multispatialCCM)
  #Select model to run
  # model.1<-TRUE #TRUE means run model.1
  # model.2<-FALSE #TRUE means run model.2
  # if(model == 1) {
  #   TRUE -> model.1
  #   FALSE -> model.2
  # } else {
  #   FALSE -> model.1
  #   TRUE -> model.2
  # }
  #Solve ODE system to generate data
  system<-function(v0,
                   w0,
                   nstep,
                   a,
                   b,
                   c,
                   d,
                   model){
    vt<-numeric()
    wt<-numeric()
    v<-v0
    w<-w0
    vt[1]<-v
    wt[1]<-w
    for(i in 2:nstep){
      if(model == 1){
        v1<-v*(a-(a*v)-(b*w))
        #System (1)
        w1<-w*(c-(c*w)-(d*v))
      }
      if(model == 2){
        v1<-v*(a-(a*v))
        #System (2)
        w1<-w*(c-(c*w)-(d*v))
      }
      v<-v1
      w<-w1
      vt[i]<-v
      wt[i]<-w
    } #end loop i
    solutions<-cbind(vt,wt)
    return(solutions)
  } #end user-defined function

  if(model == 1){ #run System (1)
    solutions<-system(0.2,0.4,1000,3.78,0.07,3.77,0.08)
  }

  if(model == 2) { #run System (2)
    results<-solutions(0.2,0.4,1000,3.8,0,3.1,0.8)
  }

  v<-solutions[,1]
  w<-solutions[,2]
  data<-cbind(v,w)
  names<-colnames(data)  #Parameter required for ccm_udf
  #frac' is a parameter used in ccm_udf to compute the 'weighted
  # adjacency matrix'. (1-frac) sets the fraction of points used to calculate
  # the asymptote of the ccm curve (plotting the correlation coefficient
  # against the library of points on the attractor used to cross-predict
  # the other variables.
  frac<-0.66
  #Formulate user-defined function 'ccm_udf' to run ccm
  ccm_udf <-function(data,
                     frac){
    #Loop j specifies each variable in turn as the response process (x),
    #and loop k iterates over the remaining variables as forcing (driving)
    #processes (y)
    wam<-matrix(0,ncol(data),ncol(data)) #weighted adjacency matrix
    for(j in 1:ncol(data)) { #response process x
      print("j")
      print(j)
      x<-data[,j]

  #Step 1: Set parameters required by CCM_boot(multispatialCCM)
      #results.embed<-embed_udf(x)
      #In general, we use this to embed response process x
      E<-2 #E<-results.embed[[2]]
      tau<-1 #tau<-results.embed[[1]]
      lib<-((E*(tau-1)+(E+1)):length(x)-E+2) #ccm library
      lib.size<-length(lib)
      print("lib.size")
      print(lib.size)
      #ccm correlations where x is the response variable and columns are driving variables (y)
      corr.mat.x<-matrix(0,lib.size,ncol(data))
      for(k in 1:ncol(data)){ #forcing process y, response process x
        if(j==k) next #don't cross-map to same attractor
        print("k");print(k)
        y<-data[,k]
        #Step 2: Run CCM_boot(multispatialCCM)
        ccm.yx<-multispatialCCM::CCM_boot(y,x,
                                          E,
                                          tau,
                                          DesiredL=lib,
                                          iterations=100)
        corr.yx<-ccm.yx$rho #ccm correlation coefficients over libraries
        length.corryx<-length(corr.yx)
        print("corr.yx");print(corr.yx)
        #Step 2a: Export corr.yx plots for x xmap y
        setwd(dir.results)
        plot(corr.yx,type='l',xlab="library",ylab="rho",ylim=c(0,1),
             lwd=3,cex.axis=1.1,font=2,font.lab=2,cex.lab=1.1)
        file<-as.character("")
        names.var<-as.matrix(names)
        file<- as.character(paste(substr(file,1,nchar(file)-4),
                                  names.var[j],"_xmap_",names.var[k],".png",
                                  sep=""))
        dev.copy(png,filename=paste(file,".png",sep=""))
        dev.off()
        #Step 3: Store corr.yx as columns in 'corr.mat.x'. Each corr.yx may
        #have a different library length, since each response series can have
        #different embedding dimensions and delays. So, we might have to store
        #vectors of different lengths.
        #Set length of corr.yx at least as long as the library length
        #associated with jth response variable (lib.size)
        length(corr.yx)<-lib.size
        #The storage matrix will contain "NA" if length(corr.yx)<lib.size
        corr.mat.x[,k]<-corr.yx #columns are ccm coefficients
        corr.x<-corr.mat.x[1:length.corryx,] #remove "NA"'s from corr.mat.x
        } #end loop k
      #Step 3a: Export 'corr.x' matrix for each response variable (x)
      file<-as.character("")  #file<-as.character("ccm.csv")
      names.var<-as.matrix(colnames(data))
      file<- as.character(paste(substr(file,1,nchar(file)-4),
                                names.var[j],"_as response variable",".csv",
                                sep=""))
      write.table(corr.x,file,col.names=colnames(data),
                  row.names=FALSE,sep=",")
      #write.table(corr.mat.x,file,col.names=colnames(data),
      #row.names=FALSE,sep=",")
      #Step 4: Prepare results to be portrayed in network diagram
      #Convert corr.mat.x matrices into weighted adjacency matrix 'wam'.
      n<-frac*nrow(corr.x);n<-round(n)

  corr.x.1<-corr.x[(n+1):nrow(corr.x),]
  corr.ave<-colMeans(corr.x.1)
  wam[j,]<-corr.ave #weighted adjacency matrix (driving variables along column)
  wam<-na.omit(wam)  } #end loop j
    #Step 4a: Export 'wam' matrix after inserting row and column labels
    names.var<-as.matrix(colnames(data))
    row.names <- c(names.var)
    col.names<-c("",names.var)
    matrix1<-cbind(row.names,wam)
    matrix2<-rbind(col.names,matrix1)
    write.table(matrix2,"wam.csv",
                col.names=FALSE,row.names=FALSE,sep=",")
    results<-list(wam) #wam is output of user-defined function
    return(results)
    } #end user-defined function
  #Step 5: Run ccm_udf
  results_ccm<-ccm_udf(data,frac)
  wam<-results_ccm[[1]] #weighted adjacency matrix
  #Step 6: Convert the 'wam' matrix into a scaled adjacency matrix 'adjacency'
  #by assigning a scale of integers to the approximated asymptotes in 'wam'. For
  #example, asymptotes greater than 0.65 are assigned 1, with 0 otherwise.
  mat.1<-wam
  for(row in 1:nrow(wam)){#row
    for(col in 1:nrow(wam)){#column
      if(mat.1[row,col]>=0.65){
        mat.1[row,col]=1
      }
      if(mat.1[row,col]<0.65){
        mat.1[row,col]=0
        }
      } #end col  } #end row
    #Reading file: Driving variable listed in column.
    adjacency<-mat.1
    #Step 6a: Export 'adjacency' matrix after inserting row and column labels
    row.names <- c(names)
    col.names<-c("",names)
    mat1<-cbind(row.names,adjacency)
    mat2<-rbind(col.names,mat1)
    write.table(mat2,"adjacency.csv",
                col.names=FALSE,row.names=FALSE,sep=",")



  }
}

