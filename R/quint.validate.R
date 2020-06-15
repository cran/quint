#' Validation of a Qualitative Interaction Tree
#'
#' A bootstrap-based validation procedure to estimate the optimism in the effect sizes of a
#' QUINT tree which gives insight in the generalizability of the results.
#'
#' @param object a (pruned) QUINT tree object of class \code{quint}.
#' @param B number of bootstrap samples. Default number is 10; for better accuracy \eqn{B}=1000
#'  is recommended.
#' @param allresults option to return an extended list of output. Default is set to FALSE.
#'   See \emph{Value} section for details.
#'
#' @details In this procedure bootstrap trees are grown of the same leaf size as the (pruned)
#'   QUINT tree. The bootstrap samples are drawn from the data used to grow the original tree. For every
#'   bootstrap tree the largest and smallest (i.e., largest negative) treatment mean differences
#'   (or treatment effect sizes) of two leaves are saved. Treatment mean differences in the
#'   leaves are then predicted using the original data set as input for each bootstrapped tree.
#'   From these predictions, the largest and smallest treatment mean differences are saved.
#'   For each bootstrap tree, the largest predicted treatment effect is subtracted from
#'   the largest treatment effect in the bootstrap sample. The average of these values
#'   is the bias (i.e., the optimism) for the largest treatment effects. This is done likewise for
#'   the smallest treatment effects. Subsequently, the bias is computed as the difference between
#'   the bias for the largest effects minus the bias for the smallest effects.
#'
#'   The details of this validaton procedure are described in Appendix C of Dusseldorp & Van Mechelen (2014).
#'
#' @return Returns a list with the following components:
#'   \item{estopt}{the estimated optimism for either the treatment effect size (biasd) or the
#'     raw treatment mean difference (biasdif).}
#'   \item{li}{a data frame with leaf information output similar to the leaf information output
#'     of the (pruned) QUINT tree object. An extra column is added for the bias-corrected
#'     differences in treatment outcomes (d or diff). The bias-corrected values are only computed
#'     for the leaves with the most extreme values, i.e. the largest and smallest treatment effects.
#'     Hence, the other leaves get the value NA in this column.}
#'   \item{optd}{a matrix with computed estimated optimism of the treatment effect size per
#'     bootstrapp tree. The first column contains the difference between the largest and smallest
#'     effect size of the bootstrapped tree. The second column contains the difference between the
#'     largest and smallest predicted effect size. Returned when \code{allresults} is set to TRUE and
#'     \code{crit='es'} is specified in the QUINT object.}
#'   \item{optdif}{a matrix with computed estimated optimism of the raw mean difference bootstrapped tree.
#'     The first column contains the difference between the largest and smallest raw mean difference of
#'     the bootstrapped tree. The second column contains the difference between the
#'     largest and smallest predicted raw mean difference. Returned when \code{allresults} is set to
#'     TRUE and \code{crit='es'} is specified in the QUINT object.}
#'   \item{resultd}{a vector with the estimated overall mean optimism, the mean bias for the smallest
#'     and for the largest effect size. Returned when \code{allresults} is set to
#'     TRUE and \code{crit="es"}.}
#'   \item{resultdif}{a vector with the estimated overall mean optimism, the mean bias for the smallest and
#'     largest raw mean difference. Returned when \code{allresults} is set to TRUE and \code{crit="dm"}.}
#'
#' @references Dusseldorp E. and Van Mechelen I. (2014). Qualitative interaction trees:
#'   a tool to identify qualitative treatment-subgroup interactions.
#'   \emph{Statistics in Medicine, 33}(2), 219-237. DOI: 10.1002/sim.5933.
#' @seealso \code{\link{quint}}, \code{\link{prune.quint}}, \code{\link{quint.control}}, \code{\link{quint.bootstrapCI}}
#'
#' @examples
#' \dontrun{data(bcrp)
#' formula1<- I(cesdt1-cesdt3)~cond | nationality+marital+wcht1+age+
#'   trext+comorbid+disopt1+uncomt1+negsoct1
#'
#' set.seed(10)
#' control1<-quint.control(maxl=5,B=2)
#' quint1<-quint(formula1, data= subset(bcrp,cond<3),control=control1) #Grow a QUINT tree
#'
#' prquint1<-prune(quint1) #Prune tree to optimal size
#'
#' set.seed(3)
#' valquint1<-quint.validate(prquint1, B = 5) #estimate the optimism by bootstrapping 5 times
#' valquint1}
#'
#' @importFrom stats sd na.omit
#' @importFrom Formula Formula
#' @export
quint.validate <-function(object, B=10, allresults=FALSE){

  if(is.null(object$si)){
    warning("quint.validate() does not work with quint objects without splitting information (si) ")
    print("No validation is possible. The tree contains only one leaf.")
  }else{
    ## Construct bootstrap samples
    y<-as.matrix(object$data[,1])
    origtr<-as.factor(object$data[,2])
    tr<-as.numeric(origtr)
    indexboot <- Bootindex(y,B,tr)#number of bootstrap samples
    Xmat <- object$data[, 3:dim(object$data)[2]]
    controlB <- object$con
    controlB$Boot <- FALSE
    controlB$dmin <- 0.0001
    controlB$maxl <- dim(object$li)[1]
    origdat <- cbind(y, tr, Xmat)
    origdat <- na.omit(origdat)

    if(object$crit=='dm'){
      optdif<-matrix(0,nrow=B,ncol=2)#optimism in difference of means
      optdifmin<-matrix(0,nrow=B,ncol=2)
      optdifmax<-matrix(0,nrow=B,ncol=2)
      for (i in 1:B){
        print(i)
        quintboot<-quint(data=origdat[indexboot[,i],], control=controlB)
        if(is.null(quintboot$si)){
          optdif[i,]<-NA;optdifmin[i,]<-NA;optdifmax[i,]<-NA

        }else if(quintboot$si[1,1]!=0){
          origli<-predval(object=quintboot,newdata=origdat) #original data through bootstrap trees
          esbootdif<-quintboot$li$diff

          ##bias in difference in means
          nnummaxdif<-quintboot$li$node[which(esbootdif==max(esbootdif))]  #search max and min raw diffs
          nnummindif<-quintboot$li$node[which(esbootdif==min(esbootdif))]
          optdifmin[i,]<-c( min(esbootdif),origli[origli[,2]==nnummindif,5])
          optdifmax[i,]<-c( max(esbootdif),origli[origli[,2]==nnummaxdif,5])
          optdif[i,]<-c( (max(esbootdif)-min(esbootdif)),(origli[origli[,2]==nnummaxdif,5]-origli[origli[,2]==nnummindif,5]))
        } else {
          optdif[i,]<-NA;optdifmin[i,]<-NA;optdifmax[i,]<-NA
        }
      }

      missingBs<-sum(is.na(optdif[,1]))

      resultdif<-c(biasdif=mean(optdif[,1]-optdif[,2],na.rm=TRUE),
                   biasdifmin=mean(optdifmin[,1]-optdifmin[,2],na.rm=TRUE),
                   biasdifmax=mean(optdifmax[,1]-optdifmax[,2],na.rm=TRUE))
      mindex <- which.min(object$li[,8])
      maxdex <- which.max(object$li[,8])

      bc_diff <- numeric(dim(object$li)[1])
      bc_diff[mindex] <- object$li[mindex, 8]-as.numeric(resultdif['biasdifmin'])
      bc_diff[maxdex] <- object$li[maxdex, 8]-as.numeric(resultdif['biasdifmax'])
      bc_diff[-c(mindex, maxdex)] <- NA
      object$li <- cbind(object$li, bc_diff=bc_diff)
      object<-list(estopt=resultdif['biasdif'], li=object$li,finalBootstraps=(B-missingBs))
      if(allresults==FALSE) return(object)
      if(allresults==TRUE) {
        extendedobject <- list(optdif=optdif, resultdif=resultdif, li=object$li,finalBootstraps=(B-missingBs))
        return(extendedobject)
      }
    }

    if(object$crit=='es'){
      optd<-matrix(0,nrow=B,ncol=2)#optimism in effect size
      optdmin<-matrix(0,nrow=B,ncol=2)
      optdmax<-matrix(0,nrow=B,ncol=2)
      for (i in 1:B){
        print(i)
        quintboot<-quint(data=origdat[indexboot[,i],], control=controlB)

        if(is.null(quintboot$si)){
          optd[i,]<-NA;optdmin[i,]<-NA;optdmax[i,]<-NA

        }else if(quintboot$si[1,1]!=0){
          origli<-predval(object=quintboot,newdata=origdat) #original data through bootstrap trees
          esbootd<-quintboot$li$d

          ##bias in effect size
          nnummaxd<-quintboot$li$node[which(esbootd==max(esbootd))]  #search max and min effect sizes
          nnummind<-quintboot$li$node[which(esbootd==min(esbootd))]
          optdmin[i,]<-c( min(esbootd),origli[origli[,2]==nnummind,5])
          optdmax[i,]<-c( max(esbootd),origli[origli[,2]==nnummaxd,5])
          optd[i,]<-c( (max(esbootd)-min(esbootd)),(origli[origli[,2]==nnummaxd,5]-origli[origli[,2]==nnummind,5]))
        } else {
          optd[i,]<-NA;optdmin[i,]<-NA;optdmax[i,]<-NA
        }
      }

      missingBs<-sum(is.na(optd[,1]))

      resultd<-c(biasd=mean(optd[,1]-optd[,2],na.rm=TRUE),
                 biasdmin=mean(optdmin[,1]-optdmin[,2],na.rm=TRUE),
                 biasdmax=mean(optdmax[,1]-optdmax[,2],na.rm=TRUE))
      mindex <- which.min(object$li[,8])
      maxdex <- which.max(object$li[,8])
      bc_d <- numeric(dim(object$li)[1])
      bc_d[mindex] <- object$li[mindex, 8]-as.numeric(resultd['biasdmin'])
      bc_d[maxdex] <- object$li[maxdex, 8]-as.numeric(resultd['biasdmax'])
      bc_d[-c(mindex, maxdex)] <- NA
      object$li <- cbind(object$li, bc_d=bc_d)
      object<-list(estopt=resultd['biasd'], li=object$li, finalBootstraps=(B-missingBs))
      if(allresults==FALSE) return(object)
      if(allresults==TRUE) {
        extendedobject <- list(optd=optd, resultd=resultd, li=object$li, finalBootstraps=(B-missingBs))
        return(extendedobject)
      }
    }
  }
}


