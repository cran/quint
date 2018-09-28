#' Summarizing Qualitative Interaction Tree Information
#'
#' Summary method for an object of class \code{quint}.
#'
#' @param object a \code{quint} object. This can be the output of \code{\link{quint}}.
#' @param \dots optional additional arguments.
#' @param digits specified number of decimal places (default is 2).
#'
#' @return prints a summarized version of the \code{quint} output.
#'
#' @details This function is a method for the generic function summary for class
#'   \code{quint}. It extracts the following essential components from a \code{quint}
#'   object: 1) Specification of the partitioning criterion; 2) Fit information;
#'   3) Split information, and 4) Leaf information.
#'
#' @examples data(bcrp)
#' formula1<- I(cesdt1-cesdt3)~cond | nationality+marital+wcht1+
#'   age+trext+comorbid+disopt1+uncomt1+negsoct1
#' control1<-quint.control(maxl=5,Bootstrap=FALSE)
#' quint1<-quint(formula1, data= subset(bcrp,cond<3),control=control1 )
#' summary(quint1)
#' 
#' ##############################################3
#' # Example with only root node tree as outcome
#' data(SimData_1)
#' formula<- Y~A |X1+X2+X3+X4+X5
#' #Adjust the control parameters only to save computation time in the example;
#' #The default control parameters are preferred
#' control<-quint.control(maxl=5,B=2)
#' set.seed(2) #this enables you to repeat the results of the bootstrap procedure
#' quint_1<-quint(formula, data= SimData_1,control=control)
#' quint_1pr<-prune(quint_1)
#' summary(quint_1pr)
#'
#' @keywords summary
#'
#' @export
summary.quint<-function(object,digits=2,...){
  #digits=number of digits at decimal points
  if(object$pruned==FALSE && !is.null(object$si)){
    warning("This tree is the initial one and it is not pruned. Thus, the qualitative interaction condition has not been checked yet. We advise to prune the tree and then summarize its information.")
  }
  
  if(object$crit=="es"){
    cat("Partitioning criterion: Effect size criterion","\n","\n")}
  else{
    cat("Partitioning criterion: Difference in treatment means criterion","\n","\n")}
  
  if(is.null(dim(object$fi)[2])){
    #cat("\n")
  }else if (dim(object$fi)[2]==5){
    cat("Fit","information:", "\n")
    cat(c(rep("",22),"Criterion") ,"\n" )
    cat(c(rep("",16),paste(rep("-",15),sep="")),"\n")
    print(round(object$fi[,c(1:3)],digits=digits),row.names=FALSE) }
  else {
    cat("Fit","information:", "\n")
    cat(c(rep("",15),"Criterion") ,"\n" )
    cat(c(rep("",16),paste(rep("-",4),sep="")),"\n")
    print( round(object$fi[,c(1:4,6)],digits=digits), row.names=FALSE) }
  cat("\n")

  if(is.null(dim(object$si)[2])){
    cat("Only","root", "node.", "\n")
  }else if (dim(object$si)[2]==4) {
    object$si <- cbind(object$si[,1:3], splitpoint = round(object$si[,4], digits = digits)) }
  else {
    object$si <- cbind(object$si[,1:3], splitpoint = object$si[,5])
  }

  if(is.null(dim(object$si)[2])){
    #cat("\n")
  }else{  
  cat("Split information:","\n")
  print(object$si,row.names=TRUE)
  }
  cat("\n")
  cat("Leaf information:","\n")
  #options(warn=-1)
  print(round(object$li[,c(2:10)],digits=digits))
}
