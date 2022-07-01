### Function to define possible splits of categorical predictor when growing the tree
determineSplits <- function(x,gm){
  z<-unique(sort(x[gm==1]))
  splits <- sapply(1:floor(length(z)/2), FUN=function(p) combn(as.vector(z),p,simplify=T))
  if(length(z)%%2==0 & length(z)!=2) {splits[[(length(z)/2)]] <- splits[[(length(z)/2)]][,1:(ncol(splits[[(length(z)/2)]])/2)]}
  if(length(z)<=2) {splits <- splits[[1]][1]}
  if(length(z)==3) {splits <- t(splits)}
  list(z,splits)
}

###Function to use in predict.quint: the number of possible splits should be based on the original categorical variable
determineSplits2 <- function(x){
  z<-unique(sort(x))
  splits <- sapply(1:floor(length(z)/2), FUN=function(p) combn(as.vector(z),p,simplify=T))
  if(length(z)%%2==0 & length(z)!=2) {splits[[(length(z)/2)]] <- splits[[(length(z)/2)]][,1:(ncol(splits[[(length(z)/2)]])/2)]}
  if(length(z)<=2) {splits <- splits[[1]][1]}
  if(length(z)==3) {splits <- t(splits)}
  list(z,splits)
}
