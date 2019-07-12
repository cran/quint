makeGchmat<-function(gm,varx,splitpoint){
  #creates an indicator matrix of the child nodes
  #gm = indicator vector of persons in the parent node that is split
  #varx =  unsorted datavector of splitting predictor X
  #splitpoint is optimal threshold on varx used for splitting
  Gchmat<-matrix(nrow=length(gm),ncol=2)
  Gchmat<-cbind(ifelse(gm==1&varx<=splitpoint,1,0),ifelse(gm==1&varx>splitpoint,1,0))
  return(Gchmat)}


### Function that creates indicator matrix of left childnodes for categorical variable
makeCatmat <- function(x,gm,z,splits){
  if(length(z)>1){
    if(length(z)==2) {assigMatrix <- as.matrix(sapply(1:length(x), function(p) x[p]%in%splits))}
    if(length(z)==3) {assigMatrix <- sapply(1:ncol(splits), function(k) sapply(1:length(x), function(p) x[p]%in%splits[,k]))}
    if(length(z)>=4) {assigArray <- sapply(1:length(splits), function(w) sapply(1:ncol(splits[[w]]), function(k) sapply(1:length(x), function(p) x[p]%in%splits[[w]][,k])))
    assigMatrix <- do.call(cbind, assigArray)} #indicator column for every possible splitpoint
  } else {assigMatrix <- matrix(rep(c(1,0)), nrow=length(gm), ncol=2, byrow=T)}
  return(assigMatrix)}

#Function for splits of categorical variable
makeGchmatcat<-function(gm,splitpoint,assigMatrix){
  Gchmat<-matrix(nrow=length(gm),ncol=2)
  if(splitpoint!=0){Gchmat <- cbind(ifelse(gm==1&assigMatrix[,splitpoint], 1, 0), ifelse(gm==1&!assigMatrix[,splitpoint],1,0))}
  if(splitpoint==0){Gchmat <- matrix(rep(c(1,0)),nrow=length(gm),ncol=2,byrow=TRUE)}
  return(Gchmat)
}
