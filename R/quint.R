#' Qualitative Interaction Trees
#'
#' This is the core function of the package. It performs a subgroup analysis
#' by QUalitative INteraction Trees (QUINT; Dusseldorp & Van Mechelen, 2014) and
#' is suitable for data from a two-arm randomized controlled trial. Ingredients
#' of the analysis are: one continuous outcome variable \eqn{Y} (the effect
#' variable), one dichotomous treatment variable \eqn{T} (indicating two treatment
#' conditions, e.g., A and B), and several background characteristics \eqn{X1,\dots,XJ}.
#' These background characteristics are measured at baseline and may have a numeric or
#' ordinal measurement level (i.e., in R a numeric or integer variable) or a nominal measurement
#' level (i.e., in R a factor). They are used to identify the following subgroups (i.e., partition
#' classes): Subgroup 1: Those patients for whom Treatment A is better than
#' Treatment B (P1); Subgroup 2: Those for whom Treatment B is better than
#' Treatment A (P2), and Subgroup 3: Those for whom it does not make any difference (P3).
#'
#' @param formula a description of the model to be fit. The format is \code{Y ~ T | X1 + \dots + XJ},
#'   where the variable before the | represents the dichotomous treatment variable \eqn{T}
#'   and the variables after the | are the baseline characteristics used for partitioning.
#'   If the data are in the order \code{Y, T, X1,\dots, XJ}, no formula is needed.
#'   The lay-out of this formula is based on Zeileis & Croissant (2010).
#' @param data a dataframe containing the variables in the model. The treatment variable can be
#'   a numeric or a factor variable with two values (or levels).
#' @param control a list with control parameters as returned by \code{\link{quint.control}}.
#'
#' @details The method QUINT uses a sequential partitioning algorithm. The algorithm
#'   starts with a tree consisting of a single node, that is, the root node containing
#'   all patients. Next, it follows a stepwise binary splitting procedure. This
#'   procedure implies that in each step a node, a baseline characteristic, a split
#'   of that characteristic, and an assignment of the leaves of the current tree to
#'   partition classes 1, 2, and 3 (P1 to P3) are chosen that maximize the
#'   partitioning criterion. Note that this means that after each split, all leaves
#'   of the tree are re-assigned afresh to the partition classes P1, P2, and P3.
#'
#' @return Returns an object of class \code{quint} with components:
#'  \item{call}{the call that created the object.}
#'  \item{crit}{the partitioning criterion used to grow the tree. The default is
#'    the Effect size criterion. Use crit="dm" for the Difference in means criterion.}
#'  \item{control}{the control parameters used in the analysis.}
#'  \item{fi}{the fit information of the final tree.}
#'  \item{si}{the split information of the final tree.}
#'  \item{li}{the leaf information of the final tree. Treatment A is denoted with \eqn{T}=1, and treatment
#'    B is denoted with \eqn{T}=2. Can display either the output for Difference
#'    in Means (crit='dm') or Cohen's \emph{d} effect size (crit='es').}
#'  \item{data}{the data used to grow the tree.}
#'  \item{nind}{an \eqn{N} x \eqn{L} matrix indicating leaf membership.}
#'  \item{siboot}{an \eqn{L} x 9 x \eqn{B} array with split information for each bootstrap sample:
#'    C_boot = value of \eqn{C};
#'    C_compdif = value of Difference in treatment outcome component;
#'    checkdif = indicates if pooled Difference in treatment outcome component in
#'      test set (i.e., original sample) is positive, with  values: 0 = yes,1 = negative
#'      in P1, 2 = negative in P2, 3 = negative in P1 and P2;
#'    C_compcard = value of Cardinality component;checkcard = indicates if value of pooled
#'      cardinality in test set is zero, with values: 0 =no,1 = zero in P1, 2 = zero in P2,
#'      3 = zero in P1 and P2;
#'    opt = value of optimism (C_boot\emph{-}C_orig).}
#'  \item{indexboot}{an \eqn{N} x \eqn{B} matrix indicating bootstrap sample membership.}
#'
#' @references Dusseldorp, E., Doove, L., & Van Mechelen, I. (2016). Quint:
#'   An R package for the identification of subgroups of clients who differ in
#'   which treatment alternative is best for them. \emph{Behavior Research Methods,
#'   48}(2), 650-663. DOI 10.3758/s13428-015-0594-z
#'
#'   Dusseldorp E. and Van Mechelen I. (2014). Qualitative interaction trees:
#'   a tool to identify qualitative treatment-subgroup interactions.
#'   \emph{Statistics in Medicine, 33}(2), 219-237. DOI: 10.1002/sim.5933.
#'
#'   Zeileis A. and Croissant Y. (2010). Extended model formulas in R: Multiple parts and
#'   multiple responses. \emph{Journal of Statistical Software, 34}(1), 1-13.
#'
#'   van der Geest M. (2018). Decision Trees: Amelioration, Simulation, Application. Can be found in:
#'  https://openaccess.leidenuniv.nl/handle/1887/65935
#'
#' @seealso \code{\link{summary.quint}}, \code{\link{quint.control}},
#'   \code{\link{prune.quint}}, \code{\link{bcrp}}
#'
#' @examples #EXAMPLE with data from the Breast Cancer Recovery Project
#' data(bcrp)
#' #Start with expliciting the model for quint
#' #The outcome Y is a change score between timepoint 3 and timepoint 1
#' #A positive Y value indicates an improvement in depression (i.e., a decrease)
#'
#' formula1<- I(cesdt1-cesdt3)~cond | nationality+marital+wcht1+age+
#'   trext+comorbid+disopt1+uncomt1+negsoct1
#'
#' #Perform a quint analysis
#' #The BCRP data contain 3 conditions. Quint only works now for 2 conditions.
#' #For the example, we disregard the control condition
#' #To save computation time, we also adjust the control parameters
#'
#' set.seed(2)
#' control1<-quint.control(maxl=5,B=2) #The recommended number of bootstraps is 25.
#' quint1<-quint(formula1, data= subset(bcrp,cond<3),control=control1)
#' quint1pr<-prune(quint1)
#'
#' #Inspect the main results of the analysis:
#' summary(quint1pr)
#'
#' #plot the tree
#' plot(quint1pr)
#'
#' @keywords tree
#' @keywords cluster
#'
#' @import Formula
#' @importFrom stats model.frame IQR na.omit sd terms var
#' @importFrom utils combn
#'
#' @exportClass quint
#' @export
quint<- function(formula, data, control=NULL){
  #Dataformat without use of formula:
  #dat:data; first column in dataframe = the response variable
  #second column in dataframe = the dichotomous treatment vector
  #(coded with treatment A=1 and treatment B=2)
  #rest of the columns in dataframe are the predictors
  #maxl: maximum total number of leaves (terminal nodes) of the final tree: Lmax

  dat <- as.data.frame(data)
  if (missing(formula)) {
    y <- dat[, 1]
    tr <- dat[, 2]
    Xmat <- dat[, -c(1, 2)]
    dat <- na.omit(dat)
    if (length(levels(as.factor(tr))) != 2) {
      stop("Quint cannot be performed. The number of treatment conditions does not equal 2.")
    }
  } else {
    F1 <- Formula(formula)
    mf1 <- model.frame(F1, data = dat)
    y <- as.matrix(mf1[, 1])
    origtr <- as.factor(mf1[, 2])
    tr <- as.numeric(origtr)
    if (length(levels(origtr)) != 2) {
      stop("Quint cannot be performed. The number of treatment conditions does not equal 2.")
    }
    Xmat <- mf1[, 3:dim(mf1)[2]]
    dat <- cbind(y, tr, Xmat)
    dat <- na.omit(dat)
    cat("Treatment variable (T) equals 1 corresponds to",
        attr(F1, "rhs")[[1]], "=", levels(origtr)[1], "\n")
    cat("Treatment variable (T) equals 2 corresponds to",
        attr(F1, "rhs")[[1]], "=", levels(origtr)[2], "\n")
    names(dat)[1:2] <- names(mf1)[1:2]
  }
  cat("The sample size in the analysis is", dim(dat)[1], "\n")

  N<-length(y)
  if(is.null(control)) {
    control <- quint.control()  #Use default control parameters and criterion
  }

  #specify criterion , parameters a and b  (parvec),  weights and maximum number of leaves:
  crit <- control$crit
  parvec <- control$parvec
  w <- control$w
  maxl <- control$maxl

  #if no control argument was specified ,use default parameter values
  #Default parameters a1 and a2 for treatment cardinality condition:
  if(length(parvec)<2){
    if(length(parvec)==1){
      warning("a1 or a2 is NULL. Default values have been used for both variables.")
    }
    a1 <- max(2,round(sum(tr==1)/10))
    a2 <- max(2,round(sum(tr==2)/10))
    parvec <- c(a1, a2)
    control$parvec <- parvec
  }else{
    if(parvec[1]<2 || parvec[2]<2){
      warning("a1 and a2 should be grater or equal than 2.")
    }
  }

  if(is.null(w)){
    #edif=expected mean difference between treatment and control; default value for effect size criterion: edif = 3 (=Cohen's d),
    #and for difference in means criterion: edif= IQR(Y)
    edif <- ifelse(crit=="es", 3, IQR(y))
    w1 <- 1/log(1+edif)
    #bal= balance (ratio) between "difference in treatment outcomes component" and "cardinality component"
    w2 <- 1/log(length(y)/2)
    w <- c(w1, w2)
    control$w <- w
  }

  ##Create matrix for results
  allresults <- matrix(0, nrow=maxl-1, ncol=6)
  splitpoints <- matrix(0, nrow=maxl-1, ncol=1)
  ## create a vector for true split points

  ##Start of the tree growing: all persons are in the rootnode.   L=1  ; Criterion value (cmax)=0
  root <- rep(1, length(y))
  cmax <- 0

  #Step 1
  #Generate design matrix D with admissable assignments after first split
  dmat1 <- matrix(c(1,2,2,1), nrow=2)

  #Select the optimal triplet for the first split: the triplet resulting in the maximum value of the criterion (critmax1)
  #use the rootnode information: cardinality t=1, cardinality t=2, meant1 ,meant0
  rootvec <- c(sum(tr==1), sum(tr==2), mean(y[tr==1])-mean(y[tr==2]))

  critmax1 <- bovar(y, Xmat, tr, gm=root, dmatsg=dmat1, dmatsel=rep(1,nrow(dmat1)), parents=rootvec, parvec, w, nsplit=1, crit=crit)

  #Make the first split
  if(is.factor(Xmat[,critmax1[1]])==FALSE){
    Gmat <- makeGchmat(root, Xmat[,critmax1[1]], critmax1[2])  }
  if(is.factor(Xmat[,critmax1[1]])==TRUE){
    possibleSplits <- determineSplits(x=Xmat[,critmax1[1]], gm=root)
    assigMatrix <- makeCatmat(x=Xmat[,critmax1[1]], gm=root, z=possibleSplits[[1]], splits=possibleSplits[[2]])
    Gmat <- makeGchmatcat(gm=root, splitpoint=critmax1[2], assigMatrix=assigMatrix)  }

  cat("split 1","\n")
  cat("#leaves is 2","\n")

  ##Keep the child node numbers  nnum; #ncol(Gmat) is current number of leaves
  ##(=number of candidate parentnodes)=L ;    #ncol(Gmat)+1 is total number of
  ##leaves after the split  (Lafter)
  nnum <- c(2,3)
  L <- ncol(Gmat)

  ##Keep the results (split information, fit information, end node information) after the first split
  if(critmax1[4]!=0){
    allresults[1,] <- c(1,critmax1[-3])
    #Keep the splitpoints
    ifelse(is.factor(Xmat[,critmax1[1]])==F, splitpoints[1] <- critmax1[2], splitpoints[1] <- paste(as.vector(unique(sort(Xmat[Gmat[,1]==1, critmax1[1]]))), collapse=", "))
    dmatrow<-dmat1[critmax1[3],]
    cmax <- allresults[1, 4]
    endinf <- ctmat(Gmat,y,tr,crit=crit) ####changed
  } else { ##if there is no optimal triplet for the first split:
    Gmat <- Gmat*0
    dmatrow <- c(0,0)
    endinf <- matrix(0, ncol=8, nrow=2)
  }

#  ##Check the qualitative interaction condition:  Cohen's d in the leafs after the first split >=dmin
#  qualint <- "Present"
#  if(abs(endinf[1,7])<control$dmin | abs(endinf[2,7])<control$dmin) {
#    L <- maxl
#    stop("The qualitative interaction condition is not satified: One or both of the effect sizes are lower than absolute value",control$dmin,". There is no clear qualtitative interaction present in the data.","\n")
#  }


# Return an object of Length 1 when the criterion C is 0.

  if (cmax == 0){
    print("Quint method cannot be performed. There is no qualitative treatment-subgroup interaction.")

    Gmat<-as.matrix(rep(1,dim(dat)[1]))
    colnames(Gmat)<-c("1")
    leaf.info<-ctmat(Gmat,y=dat[,1],tr=dat[,2],crit=crit)
    leaf.info<-leaf.info[1,]
    class_quint<-ifelse(leaf.info[7]>=0,1,2)
    node<-0
    leaf.info<-as.data.frame(matrix(c(node,leaf.info,class_quint),nrow = 1))
    colnames(leaf.info) <- c("node","#(T=1)", "meanY|T=1", "SD|T=1","#(T=2)", "meanY|T=2","SD|T=2","d","se","class")
    rownames(leaf.info) <- c("Leaf 1")
    object <- list(call = match.call(), crit = crit, control = control,
                   indexboot = NULL, data = dat, si = NULL, fi = NULL, li = leaf.info, nind = Gmat,
                   siboot = NULL, pruned=FALSE)
    class(object)<-"quint"
    return(object)
  }else{

  ##Perform bias-corrected bootstrapping for the first split:
  if(control$Boot==TRUE&cmax!=0){
    #initiate bootstrap with stratification on treatment groups:
    indexboot <- Bootstrap(y, control$B, tr)
    critmax1boot <- matrix(0, ncol=6, nrow=control$B)

    #initialize matrices to keep results
    Gmattrain <- array(0, dim=c(N,maxl,control$B))
    Gmattest <- array(0, dim=c(N,maxl,control$B))
    allresultsboot <- array(0, dim=c(maxl-1,9,control$B))
    #find best first split for the k training sets
    for (b in 1:control$B) {
      cat("Bootstrap sample ",b,"\n")
      ##use the bootstrap data as training set
      critmax1boot[b,]<-bovar(y[indexboot[,b]],Xmat[indexboot[,b],],tr[indexboot[,b]],root,dmat1,rep(1,nrow(dmat1)),rootvec,parvec,w,1,crit=crit)

      if(is.factor(Xmat[,critmax1boot[b,1]])==FALSE){
        Gmattrain[,c(1:2),b]<-makeGchmat(gm=root, varx=Xmat[indexboot[,b],critmax1boot[b,1]], splitpoint=critmax1boot[b,2])
        ##use the original data as testset
        Gmattest[,c(1:2),b]<-makeGchmat(gm=root, varx=Xmat[,critmax1boot[b,1]],splitpoint=critmax1boot[b,2])
      }

      if(is.factor(Xmat[,critmax1boot[b,1]])==TRUE){
        possibleSplits <- determineSplits(x=Xmat[indexboot[,b], critmax1boot[b,1]], gm=root)
        assigMatrixTrain <- makeCatmat(x=Xmat[indexboot[,b], critmax1boot[b,1]], gm=root, z=possibleSplits[[1]], splits=possibleSplits[[2]])
        Gmattrain[,c(1:2),b]<-makeGchmatcat(gm=root, splitpoint=critmax1boot[b,2], assigMatrix=assigMatrixTrain)
        ##use the original data as testset
        assigMatrixTest <- makeCatmat(x=Xmat[,critmax1boot[b,1]], gm=root, z=possibleSplits[[1]], splits=possibleSplits[[2]])
        Gmattest[,c(1:2),b]<-makeGchmatcat(gm=root, splitpoint=critmax1boot[b,2], assigMatrix=assigMatrixTest)
      }

      End <- cpmat(Gmattest[,c(1:2),b], y, tr, crit=crit)
      #select the right row in the design matrix
      dmatsel <- t(dmat1[critmax1boot[b,3],])

      allresultsboot[1,c(1:8),b] <- c(1,critmax1boot[b,c(1:2)],computeCtest(End, dmatsel, w))
      allresultsboot[1,9,b] <- critmax1boot[b,4]-allresultsboot[1,4,b]
      if(critmax1boot[b,4]==0) {allresultsboot[1,,b]<-NA}
    }
  }

  #Repeat the tree growing procedure
  stopc <- 0

  while(L<maxl){
    cat("current value of C", cmax,"\n")
    cat("split", L, "\n")
    Lafter <- ncol(Gmat)+1
    cat("#leaves is", Lafter, "\n")
    ##make a designmatrix (dmat) for the admissible assignments of the leaves after the split
    dmat <- makedmat(Lafter)
    dmatsg <- makedmats(dmat)
    #make parentnode information matrix, select best observed parent node (with optimal triplet)
    parent <- cpmat(Gmat,y,tr,crit=crit)
    critmax <- bonode(Gmat,y,Xmat,tr,dmatrow,dmatsg,parent,parvec,w,L,crit=crit)

    ##Perform the best split and keep results
    if(is.factor(Xmat[,critmax[2]])==FALSE){
      Gmatch <- makeGchmat(Gmat[,critmax[1]], Xmat[,critmax[2]], critmax[3])  }
    if(is.factor(Xmat[,critmax[2]])==TRUE){
      possibleSplits <- determineSplits(x=Xmat[,critmax[2]], gm=Gmat[,critmax[1]])
      assigMatrix <- makeCatmat(x=Xmat[,critmax[2]], gm=Gmat[,critmax[1]], z=possibleSplits[[1]], splits=possibleSplits[[2]])
      Gmatch <- makeGchmatcat(gm=Gmat[,critmax[1]], splitpoint=critmax[3], assigMatrix=assigMatrix)    }

    Gmatnew <- cbind(Gmat[,-critmax[1]], Gmatch)
    allresults[L,] <- c(nnum[critmax[1]], critmax[2:3], critmax[5:7])
    ifelse(is.factor(Xmat[,critmax[2]])==F, splitpoints[L] <- round(critmax[3], digits = 2), splitpoints[L] <- paste(as.vector(unique(sort(Xmat[Gmatch[,1]==1,critmax[2]]))), collapse=", "))
    dmatrownew <- dmatsg[critmax[4],]

    #check if cmax new is higher than current value
    if(allresults[L,4]<=cmax){
      cat("splitting process stopped after number of leaves equals",L,"because new value of C was not higher than current value of C","\n")
      stopc<-1
    }

    ##repeat this procedure for the bootstrap samples
    if(control$Boot==TRUE & stopc!=1){
      critmaxboot<-matrix(0,nrow=control$B,ncol=7)
      for (b in 1:control$B){
        cat("Bootstrap sample ",b,"\n")
        #make parentnode information matrix pmat
        parent <- cpmat(Gmattrain[,c(1:(Lafter-1)),b], y[indexboot[,b]], tr[indexboot[,b]], crit=crit)
        critmaxboot[b,] <- bonode(Gmat=Gmattrain[,c(1:(Lafter-1)),b], y=y[indexboot[,b]], Xmat=Xmat[indexboot[,b],], tr=tr[indexboot[,b]], dmatrow, dmatsg, parent, parvec, w, nsplit=L, crit=crit)

        #best predictor and node of this split for the training samples
        if(is.factor(Xmat[,critmaxboot[b,2]])==FALSE){
          Gmattrainch <- makeGchmat(Gmattrain[, critmaxboot[b,1],b], Xmat[indexboot[,b], critmaxboot[b,2]], critmaxboot[b,3])
          Gmattestch <- makeGchmat(Gmattest[,critmaxboot[b,1],b], Xmat[, critmaxboot[b,2]], critmaxboot[b,3])
        }
        if(is.factor(Xmat[,critmaxboot[b,2]])==TRUE){
          possibleSplits <- determineSplits(x=Xmat[indexboot[,b], critmaxboot[b,2]], gm=Gmattrain[,critmaxboot[b,1],b])
          assigMatrixTrain <- makeCatmat(x=Xmat[indexboot[,b], critmaxboot[b,2]], gm=Gmattrain[,critmaxboot[b,1],b], z=possibleSplits[[1]], splits=possibleSplits[[2]])
          Gmattrainch <- makeGchmatcat(gm=Gmattrain[,critmaxboot[b,1],b], splitpoint=critmaxboot[b,3], assigMatrix=assigMatrixTrain)

          assigMatrixTest <- makeCatmat(x=Xmat[,critmaxboot[b,2]], gm=Gmattest[,critmaxboot[b,1],b], z=possibleSplits[[1]], splits=possibleSplits[[2]])
          Gmattestch <- makeGchmatcat(gm=Gmattest[,critmaxboot[b,1],b], splitpoint=critmaxboot[b,3], assigMatrix=assigMatrixTest)
        }

        Gmattrain[,c(1:Lafter),b] <- cbind(Gmattrain[,c(1:(Lafter-1))[-critmaxboot[b,1]],b], Gmattrainch)
        Gmattest[,c(1:Lafter),b] <- cbind(Gmattest[,c(1:(Lafter-1))[-critmaxboot[b,1]],b], Gmattestch)

        ##compute criterion value for the test sets
        End <- cpmat(Gmattest[,c(1:Lafter),b],y,tr,crit=crit)
        #select the right row in the design matrix
        if(critmaxboot[b,5]!=0){
          dmatsel<-t(dmatsg[critmaxboot[b,4],])
          allresultsboot[L,c(1:8),b] <- c(nnum[critmaxboot[b,1]],critmaxboot[b,2],critmaxboot[b,3],computeCtest(End, dmatsel, w))
          allresultsboot[L,9,b]<-critmaxboot[b,5]-allresultsboot[L,4,b]
        }
        if(critmaxboot[b,5]==0){
          allresultsboot[L,,b] <-NA
        }
      }

      if(sum(is.na(allresultsboot[L,9,]))/control$B > .10 ){
        warning("After split ",L,", the partitioning criterion cannot be computed in more than 10 percent of the bootstrap samples. The split is unstable." )
      }
    }

    #update the parameters after the split:
    if(stopc==0) {
      Gmat <- Gmatnew
      dmatrow <- dmatrownew
      cmax <- allresults[L,4]
      L <- ncol(Gmat)
      nnum <- c(nnum[-critmax[1]],nnum[critmax[1]]*2,nnum[critmax[1]]*2+1)
    } else {L <- maxl}

    #end of while loop
  }

  Lfinal <- ncol(Gmat)  #Lfinal=final number of leaves of the tree

  #create endnode information of the tree
  endinf <- matrix(0,nrow=length(nnum),ncol=10)
  if(cmax!=0){
    endinf[,c(2:9)] <- ctmat(Gmat,y,tr,crit=crit)} ####changed
  endinf <- data.frame(endinf)
  endinf[,10] <- dmatrow
  endinf[,1] <- nnum
  index <- leafnum(nnum)
  endinf <- endinf[index,]
  rownames(endinf) <- paste("Leaf ",1:Lfinal,sep="")
  if(crit == 'es'){ ### this was added/changed
    colnames(endinf) <- c("node","#(T=1)", "meanY|T=1", "SD|T=1","#(T=2)", "meanY|T=2","SD|T=2","d", "se", "class")}
  if(crit == 'dm'){ ### this was added
    colnames(endinf) <- c("node","#(T=1)", "meanY|T=1", "SD|T=1","#(T=2)", "meanY|T=2","SD|T=2","diff", "se", "class")}
  if(Lfinal==2){allresults <- c(2,allresults[1,])}
  if(Lfinal>2){
    allresults <- cbind(2:Lfinal, allresults[1:(Lfinal-1),])
  }

  #compute final estimate of optimism and standard error:
  if(control$Boot==TRUE){
    #raw mean and sd:
    opt <- sapply(1:(Lfinal-1), function(kk,allresultsboot){mean(allresultsboot[kk,9,],na.rm=TRUE)}, allresultsboot=allresultsboot)
    se_opt <- sapply(1:(Lfinal-1), function(kk,allresultsboot){sd(allresultsboot[kk,9,],na.rm=TRUE)/sqrt(sum(!is.na(allresultsboot[kk,9,])))}, allresultsboot=allresultsboot)
    if(sum(is.na(se_opt))>0){
      stop("The standard error obtained through bootstrap cannot be computed. Consider increasing the number of bootstraps (a minimum of 25 is recommended).")
    }

    if(Lfinal==2){allresults <- c(allresults[1:5], allresults[5]-opt,opt, se_opt, allresults[6:7])
    allresults <- data.frame(t(allresults))
    }
    if(Lfinal>2){
      allresults <- cbind(allresults[,1:5], allresults[,5]-opt,opt, se_opt, allresults[,6:7])
      allresults <- data.frame(allresults)
    }
    allresults[,3] <- colnames(Xmat)[allresults[,3]]
    splitnr <- 1:(Lfinal-1)
    allresults <- cbind(splitnr, allresults)
    colnames(allresults) <- c("split", "#leaves", "parentnode", "splittingvar", "splitpoint", "apparent", "biascorrected", "opt", "se","compdif","compcard")
  }

  if(control$Boot==FALSE){
    if(Lfinal>2){
      allresults <- data.frame( allresults)
    }
    if(Lfinal==2){
      allresults <- data.frame(t(allresults))
    }
    allresults[,3] <- colnames(Xmat)[allresults[,3]]
    splitnr <- 1:(Lfinal-1)
    allresults <- cbind(splitnr, allresults)
    colnames(allresults) <- c("split", "#leaves", "parentnode", "splittingvar", "splitpoint", "apparent","compdif","compcard")
  }
  colnames(Gmat) <- nnum

  ##split information (si): also include childnode numbers
  si <- allresults[,3:5]
  cn <- paste(si[,1]*2, si[,1]*2+1, sep=",")
  si <- cbind(parentnode=si[,1], childnodes=cn, si[,2:3], truesplitpoint=splitpoints[1:nrow(si)])
  rownames(si) <- paste("Split ", 1:(Lfinal-1), sep="")

  if(control$Boot==FALSE){
    object <- list(call=match.call(), crit=crit, control=control,
                   data=dat, si=si, fi=allresults[,c(1:2,6:8)], li=endinf, nind=Gmat[,index], pruned=FALSE)
  }
  if(control$Boot==TRUE){
    nam <- c("parentnode", "splittingvar", "splitpoint",
             "C_boot", "C_compdif", "checkdif", "C_compcard",
             "checkcard", "opt")
    dimnames(allresultsboot) <- list(NULL, nam, NULL)
    object <- list(call = match.call(), crit = crit, control = control,
                   indexboot = indexboot, data = dat, si = si, fi = allresults[, c(1:2, 6:11)], li = endinf, nind = Gmat[, index],
                   siboot = allresultsboot, pruned=FALSE)                                               #11
  }
  class(object) <- "quint"
  return(object)
}
}
