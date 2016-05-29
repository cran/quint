predval<-function(object,newdata,...){
  nsplit<-dim(object$si)[1]
  form<-as.formula(paste(names(object$data)[1],"~",paste(c(names(object$data)[2],object$si[1:nsplit,3]),collapse="+" )))
  ytx<-model.frame(form,data=newdata)
  y<-as.matrix(ytx[,1])
  tr<-as.numeric(as.factor(ytx[,2]))
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

  End<-cpmat(Gmat,y,tr,crit=object$crit)
  Leaf <- class <- numeric(dim(object$li)[1])
  for(i in 1:dim(object$li)[1]) {
    Leaf[i] <- which(nnum[i]==object$li[,1])
    class[i] <- object$li[which(nnum[i]==object$li[,1]), 10]
  }
  obj<-cbind(Leaf,nnum,End,class)
  obj<-as.data.frame(obj)
  if(object$crit=='dm'){names(obj)=c("Leaf","Node","#(T=1)","#(T=2)","diff","class")}
  if(object$crit=='es'){names(obj)=c("Leaf","Node","#(T=1)","#(T=2)","d","class")}
  return(obj)
}
