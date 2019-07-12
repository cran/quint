#' Pruning of a Qualitative Interaction Tree
#'
#' Determines the optimally pruned size of the tree by applying the one
#' standard error rule to the results from the bias-corrected bootstrap procedure.
#' At the end of the pruning procedure, it is checked whether the pruned tree satisfies
#' the qualitative interaction condition. If this condition is not met, it is concluded
#' that there is no qualitative tratment-subgroup interaction and a tree containing
#' only the root node is returned.
#'
#' @param tree fitted tree of the class \code{quint}.
#' @param pp pruning parameter, the constant (\eqn{c}) to be used in the \eqn{c*}standard
#'   error rule. The default value is 1.
#' @param \dots optional additional arguments.
#'
#' @details The pruning algorithm of \code{quint} is explained in Dusseldorp
#'   & Van Mechelen (2014), Appendix B of the online supplementary material. It is
#'   based on the bias-corrected bootstrap pruning procedure (Le Blanc & Crowley, 1993)
#'   and the one standard error rule (Breiman, Friedman, Olshen, & Stone, 1984).
#'   The one standard error rule for \code{quint} uses the estimates of the bias-corrected
#'   criterion value (\eqn{C}) and its standard error for each value of \eqn{L}
#'   (= maximum number of leaves). The optimally pruned tree corresponds to the
#'   smallest tree with a bias-corrected \eqn{C} higher or equal to the maximum
#'   bias-corrected \eqn{C} minus its standard error.
#'
#' @return Returns an object of class \code{quint}. The number of leaves of this object is
#'   equal to the optimally pruned size of the tree.
#'
#' @references Breiman L., Friedman J.H., Olshen R.A. and Stone C.J. (1984).
#'   \emph{Classification and Regression Trees}. Chapman & Hall/CRC: Boca Raton.
#'
#'   Dusseldorp E. and Van Mechelen I. (2014). Qualitative interaction trees:
#'   a tool to identify qualitative treatment-subgroup interactions.
#'   \emph{Statistics in Medicine, 33}(2), 219-237. DOI: 10.1002/sim.5933.
#'
#'   LeBlanc M. and Crowley J. (1993). Survival trees by goodness of split.
#'   \emph{Journal of the American Statistical Association, 88,} 457-467.
#'
#' @seealso \code{\link{quint.control}}, \code{\link{quint}}
#'
#' @examples data(bcrp)
#' formula2 <- I(cesdt1-cesdt3)~cond |age+trext+uncomt1+disopt1+negsoct1
#' #Adjust the control parameters only to save computation time in the example;
#' #The default control parameters are preferred
#' control2 <- quint.control(maxl=5,B=2)
#' set.seed(2) #this enables you to repeat the results of the bootstrap procedure
#' quint2 <- quint(formula2, data= subset(bcrp,cond<3),control=control2)
#' quint2pr <- prune(quint2)
#' summary(quint2pr)
#'
#' @keywords tree
#'
#' @importFrom rpart prune
#' @exportClass quint
#' @export

prune.quint <- function(tree,pp=1,...){
  object <- tree
  if(is.null(object$si)) {
    besttree <- list(call = match.call(), crit = object$crit, control = object$control,
                     data = object$data, si = object$si, fi = object$fi, li = object$li, nind = object$nind,
                     siboot = object$siboot, pruned=TRUE)
    class(besttree) <- "quint"
    return(besttree)

  } else {

    #pp=pruning parameter
    if(names(object$fi)[4]%in%c("Difcomponent","compdif")){
      stop("Pruning is not possible; The quint object lacks estimates of the biascorrected
           criterion. Grow again a large tree using the bootstrap procedure." )}

    object$fi[is.na(object$fi[,4]),4]<-0
    object$fi[is.na(object$fi[,5]),5]<-0
    maxrow<-which(object$fi[,4]==max(object$fi[,4]))[1]
    if(is.na(object$fi[maxrow,6])) maxrow <- maxrow -1
    bestrow<-min( which(object$fi[,4]>= (object$fi[maxrow,4]-pp*object$fi[maxrow,6]) ) )
    con<-object$control
    con$Boot<-FALSE
    con$maxl <- bestrow + 1
    besttree <- quint(data = object$data, control = con)
    besttree$fi <- object$fi[1:bestrow, ]
    objboot <- list(siboot = object$siboot[1:bestrow, , ])
    besttree <- c(besttree, objboot)
    besttree$control$Boot <- object$control$Boot

    # Check whether there is a qualitative interaction
    if(con$crit=="es"){  # criterium is es
      if( ( any(abs(besttree$li$d[besttree$li$class==1]) >= con$dmin) &
          any(abs(besttree$li$d[besttree$li$class==2]) >= con$dmin) ) == FALSE) {

        Gmat<-as.matrix(rep(1,dim(object$dat)[1]))
        colnames(Gmat)<-c("1")
        leaf.info<-ctmat(Gmat,y=object$dat[,1],tr=object$dat[,2],crit=object$crit)
        leaf.info<-leaf.info[1,]
        class_quint<-ifelse(leaf.info[7]>=0,1,2)
        node<-0
        leaf.info<-as.data.frame(matrix(c(node,leaf.info,class_quint),nrow = 1))
        colnames(leaf.info) <- c("node","#(T=1)", "meanY|T=1", "SD|T=1","#(T=2)", "meanY|T=2","SD|T=2","d","se","class")
        rownames(leaf.info) <- c("Leaf 1")
        besttree <- list(call = match.call(), crit = object$crit, control = object$control,
                        data = object$dat, si = NULL, fi = NULL, li = leaf.info, nind = Gmat,
                         siboot = NULL, pruned=TRUE)
        class(besttree)<-"quint"
        warning("Best tree is the root node.")
        return(besttree)
      }
    } else {  # criterium is dm
      if((any(abs(subset(besttree$li, class == 1, diff) /
                  sqrt(((besttree$li[besttree$li[,10]==1, 2] - 1) * besttree$li[besttree$li[,10]==1, 4] ^ 2 +
                        (besttree$li[besttree$li[,10]==1, 5] - 1) * besttree$li[besttree$li[,10]==1, 7] ^ 2) /
                       (sum(besttree$li[besttree$li[,10]==1, c(2, 5)]) - 2))) >= con$dmin) &
          any(abs(subset(besttree$li, class == 2, diff) /
                  sqrt(((besttree$li[besttree$li[,10]==2, 2] - 1) * besttree$li[besttree$li[,10]==2, 4] ^ 2 +
                        (besttree$li[besttree$li[,10]==2, 5] - 1) * besttree$li[besttree$li[,10]==2, 7] ^ 2) /
                       (sum(besttree$li[besttree$li[,10]==2, c(2, 5)]) - 2))) >= con$dmin)) == FALSE) {

        Gmat<-as.matrix(rep(1,dim(object$dat)[1]))
        colnames(Gmat)<-c("1")
        leaf.info<-ctmat(Gmat,y=object$dat[,1],tr=object$dat[,2],crit=object$crit)
        leaf.info<-leaf.info[1,]
        class_quint<-ifelse(leaf.info[7]>=0,1,2)
        node<-0
        leaf.info<-as.data.frame(matrix(c(node,leaf.info,class_quint),nrow = 1))
        colnames(leaf.info) <- c("node","#(T=1)", "meanY|T=1", "SD|T=1","#(T=2)", "meanY|T=2","SD|T=2","d","se","class")
        rownames(leaf.info) <- c("Leaf 1")
        besttree <- list(call = match.call(), crit = object$crit, control = object$control,
                         data = object$dat, si = NULL, fi = NULL, li = leaf.info, nind = Gmat,
                         siboot = NULL,pruned=TRUE)
        class(besttree)<-"quint"
        return(besttree)
      }
    }
    besttree$pruned<-TRUE
    class(besttree) <- "quint"
    return(besttree)
  }
  }

