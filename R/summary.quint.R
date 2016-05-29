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
#' @keywords summary
#'
#' @export
summary.quint<-function(object,digits=2,...){
  #digits=number of digits at decimal points
  if(object$crit=="es"){
    cat("Partitioning criterion: Effect size criterion","\n","\n")}
  else{
    cat("Partitioning criterion: Difference in treatment means criterion","\n","\n")}
  cat("Fit","information:", "\n")
  if (dim(object$fi)[2]==5){
    cat(c(rep("",22),"Criterion") ,"\n" )
    cat(c(rep("",16),paste(rep("-",15),sep="")),"\n")
    print(round(object$fi[,c(1:3)],digits=digits),row.names=FALSE) }
  else {
    cat(c(rep("",15),"Criterion") ,"\n" )
    cat(c(rep("",16),paste(rep("-",4),sep="")),"\n")
    print( round(object$fi[,c(1:4,6)],digits=digits), row.names=FALSE) }
  cat("\n")

  if (dim(object$si)[2]==4) {
    object$si <- cbind(object$si[,1:3], splitpoint = round(object$si[,4], digits = digits)) }
  else {
    object$si <- cbind(object$si[,1:3], splitpoint = object$si[,5])
  }

  cat("Split information:","\n")
  print(object$si,row.names=TRUE)
  cat("\n")
  cat("Leaf information:","\n")
  #options(warn=-1)
  print(round(object$li[,c(2:10)],digits=digits))
}
