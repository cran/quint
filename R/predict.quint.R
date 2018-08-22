#' Predictions for new data with a QUINT object
#'
#' Predicts for (new) subjects the treatment subgroups (P1, P2 or P3) based on a fitted
#'   \code{quint} object. The meaning of the subgroups are based on the two treatment categories
#'   used to fit the \code{quint} object.
#'
#' @param object an object of the class \dQuote{quint}.
#' @param newdata a data frame with data on new subjects for whom predictions should be made.
#'   The data frame should contain at least the variables used in the splits of the fitted tree.
#'   It is not necessary to include the treatment variable.
#' @param type character string denoting the type of predicted object to be returned. The default is
#'   set to \code{type="class"}: a vector with predicted treatment subgroup classes per subject
#'   is returned. If set to \code{"matrix"}, a matrix is returned with the leaf and
#'   corresponding node of the tree to which a subject is assigned.
#' @param \dots optional additional arguments.
#'
#' @return One of the following objects is returned depending on output type specified
#'   in the function:
#'
#'   If \code{type="class"}:
#'   vector of predicted treatment classes for every individual in the data set. Returns NA
#'   for subjects with missing values on one or more of the splitting variables.
#'
#'   If \code{type="matrix"}:
#'   a matrix with predicted locations of subjects within the fitted tree. The leaf numbers are
#'   in the first column and the corresponding node numbers in the second column. Returns NA
#'   for subjects with missing values on one or more of the splitting variables.
#'
#' @seealso \code{\link{quint}}, \code{\link{prune.quint}}
#'
#' @examples data(bcrp)
#' formula1<- I(cesdt1-cesdt3)~cond | nationality+marital+wcht1+age+
#'   trext+comorbid+disopt1+uncomt1+negsoct1
#'
#' set.seed(10)
#' control1<-quint.control(maxl=5,B=2)
#' quint1<-quint(formula1, data= subset(bcrp,cond<3),control=control1) #Grow a QUINT tree
#'
#' prquint1<-prune(quint1) #Prune QUINT tree to optimal size
#'
#' #Predict for the same data set the treatment classes for patients individually:
#' predquint1<-predict(prquint1, newdata=subset(bcrp,cond<3), type='class')
#' predquint1
#'
#' @importFrom stats as.formula model.frame
#' @export
predict.quint<-function(object,newdata,type='class',...){
  nsplit<-dim(object$si)[1]
  form<-as.formula(paste(names(object$data)[1],"~",paste(c(object$si[1:nsplit,3]),collapse="+" )))
  ytxna <- model.frame(form, data=newdata, na.action=NULL)
  ytx<-model.frame(form,data=newdata) # NA omitted data frame required for procedure
  y<-as.matrix(ytx[,1])
  root<-rep(1,dim(ytx)[1]);
  # Gmat<-makeGchmat(root,ytx[,object$si[1,3]],object$si[1,4])
  if(is.factor(ytx[,object$si[1,3]])==F){
    Gmat <- makeGchmat(gm=root, varx=ytx[,object$si[1,3]], splitpoint=object$si[1,4])  }
  if(is.factor(ytx[,object$si[1,3]])==T){
    possibleSplits <- determineSplits(x=ytx[,object$si[1,3]], gm=root)
    assigMatrix <- makeCatmat(x=ytx[,object$si[1,3]], gm=root, z=possibleSplits[[1]], splits=possibleSplits[[2]])
    Gmat <- makeGchmatcat(gm=root, splitpoint=object$si[1,4], assigMatrix=assigMatrix)  }

  nnum<-c(2,3)
  if (nsplit>1){
    for (i in 2:nsplit){
      o<-which(nnum==object$si[i,1])
      #Gmatch<-makeGchmat(Gmat[,o],ytx[,object$si[i,3]],object$si[i,4])
      if(is.factor(ytx[,object$si[i,3]])==F){
        Gmatch <- makeGchmat(gm=Gmat[,o], varx=ytx[,object$si[i,3]], splitpoint=object$si[i,4])  }
      if(is.factor(ytx[,object$si[i,3]])==T){
        possibleSplits <- determineSplits(x=ytx[,object$si[i,3]], gm=Gmat[,o])
        assigMatrix <- makeCatmat(x=ytx[,object$si[i,3]], gm=Gmat[,o], z=possibleSplits[[1]], splits=possibleSplits[[2]])
        Gmatch <- makeGchmatcat(gm=Gmat[,o], splitpoint=object$si[i,4], assigMatrix=assigMatrix)  }

      nnum<-c(nnum[-o],object$si[i,1]*2,object$si[i,1]*2+1)
      Gmat<-cbind(Gmat[,-o],Gmatch)
    }
  }
  #check number of missings
  nmis<-sum(is.na(ytxna))
  index <- c(1:dim(ytxna)[1])
  if(nmis!= 0){
  naindex <- which(is.na(ytxna)) # which subjects have NA on required variables
  index <- c(1:dim(ytxna)[1])[-naindex] # which subjects have no NA on required variables
  }

  if(type=="matrix") {
    nodemat <- matrix(0, nrow=dim(newdata)[1], ncol=2)
    for(i in 1:length(nnum)){
      nodemat[index[which(Gmat[,i]==1)],1] <- which(object$li[,1]==nnum[i])
      nodemat[index[which(Gmat[,i]==1)],2] <- nnum[i]
    }
    if(nmis!= 0){
    nodemat[naindex,c(1:2)] <- NA
    }
    colnames(nodemat) <- c("Leaf", "Node")
    #rownames(nodemat) <- as.numeric(rownames(ytxna)) # give subjects numbers of original dataset
    return(nodemat)
  }

  if(type=="class"){
    classmat <- numeric(dim(newdata)[1])
       for(i in 1:length(nnum)) {
      classmat[index[which(Gmat[,i]==1)]] <- object$li[which(nnum[i]==object$li[,1]),10]
    }
    if(nmis!= 0){
    classmat[naindex] <- NA
    }
    names(classmat) <- 1:dim(newdata)[1]
    #names(classmat) <- as.numeric(rownames(ytxna)) # give subjects numbers of original dataset (alternative to 1:dim(newdat)[1])
    return(classmat)
  }

}


