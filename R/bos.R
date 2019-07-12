ctc<-function(pmat,parvec){
  #check treatment cardinality condition; parvec contains the cardinality restrictions of resp. a1 and a2
  #a1 cardinality T=1 in a node ;   #a2 cardinality T=2 in a node
  cond<-sapply(1:nrow(pmat),function(kk,pmat,parvec){ifelse(sum(pmat[kk,1:2]>=parvec)==2,1,0)},pmat=pmat, parvec=parvec)
  condvec<-ifelse(sum(cond)==nrow(pmat),1,0)
  #if condvec==1 then the cardinality conditions for each node after the split are met
  return(condvec)}

computeC<-function(pmat,dmats3,w){
  #compute criterion
  #dmats3: designmatrix with admissible assignments of the nodes to the partition classes
  #compute value of partitioning criterion
  selp1<-dmats3==1
  selp2<-dmats3==2
  weight<-pmat[,1]+pmat[,2]
  pmat<-cbind(pmat,weight)
  #weighted average of the effect sizes of the regions belonging to p1 for each possible partition (rows of dmats3)
  dif1<- sapply(1:nrow(dmats3),function(kk,pmat,sel){(t(pmat[sel[kk,],4])%*%pmat[sel[kk,],3])/sum(pmat[sel[kk,],4])  },pmat=pmat,sel=selp1)
  #weighted average of the effect sizes of the regions belonging to p1 for each possible partition (rows of dmats3)
  dif2<- sapply(1:nrow(dmats3),function(kk,pmat,sel){(t(pmat[sel[kk,],4])%*%-pmat[sel[kk,],3])/sum(pmat[sel[kk,],4]) },pmat=pmat,sel=selp2)
  #compute "Difference in treatment outcomes" component
  crit1<- sapply(1:length(dif1),function(kk,dif1,dif2,w){ w[1]*log(1+dif1[kk])+w[1]*log(1+dif2[kk])},dif1=dif1,dif2=dif2,w=w)
  #compute "Difference in cardinality" component
  crit2<-sapply(1:nrow(dmats3),function(kk,pmat,w,sel1,sel2){
    w[2]*log(sum(pmat[sel1[kk,],4]))+w[2]*log(sum(pmat[sel2[kk,],4]))},pmat=pmat,w=w,sel1=selp1,sel2=selp2)
  crittot<-apply(rbind(crit1,crit2),2,sum)
  return(list(crittot=crittot,critdif=crit1,critcard=crit2))}

cmd<-function(pmat,dmats2){
  #check mean difference per node condition
  selp1<-dmats2==1
  selp2<-dmats2==2
  #vec p1 is sum of nodes belonging to p1 with effect size or mean dif that is not > 0
  vecp1<- sapply(1:nrow(dmats2),function(kk,pmat,sel){sum(pmat[sel[kk,],3]<=0)},pmat=pmat,sel=selp1)
  #vecp2 is sum of nodes belonging to p2 with effect size or mean dif that is not > 0
  vecp2<- sapply(1:nrow(dmats2),function(kk,pmat,sel){sum(-pmat[sel[kk,],3]<=0)},pmat=pmat,sel=selp2)
  condvec<-ifelse(vecp1==0 &vecp2==0,1,0)
  return(condvec)}


##function for best observed first split
bos<-function(y,x,tr,gm,dmats,parents,parvec,w,nsplit,crit="es",plotc=FALSE){
  #computes best observed split for a splitting variable candidate
  #ouput is result (rowvector of length 5) with: optimal split point, rownumber of dmats, max value of C, Cdif, and Ccard
  #y = outcome variable
  #x = predictor to be used for the split
  #tr = treatment variable(1=treatment A; 2=treatment B)
  #gm = indicator vector of persons in the particular parent node that is split
  #parents contains information of rest of the parent nodes (cardinalities t0, cardinalities t1, and sum y|t=0, sum y|t=1)
  #dmats=designmatrix with admissible assignments
  #parvec=parameter a1 and a2;
  #w=vector with weights w1,and w2.
  #nsplit: number of split
  #if plotc is T, then the value of the criterium is plotted for all splitpoints and for all possible partitions
  if(is.factor(x)==TRUE){
    z <- unique(sort(x[gm==1]))
    n <- ((2^length(z))-2)/2

    #n is  the number of distinct values (split points) of predictor x
    crittot<-matrix(0,nrow=dim(dmats)[1],ncol=n)
    crit1<-matrix(0,nrow=dim(dmats)[1],ncol=n)
    crit2<-matrix(0,nrow=dim(dmats)[1],ncol=n)
    #crittot is a matrix with number of rows is number of possible assignments (rows of Ds)
    #and number of columns is n = number of splitpoints; the cells contain the value of the criterion C

    #Perform for each possible split point vik:
    if(n>0){
      possibleSplits <- determineSplits(x, gm)
      assignMatrix <- makeCatmat(x=x, gm=gm, z=possibleSplits[[1]], splits=possibleSplits[[2]])
      #For every possible split determine whether person goes to left node(=1) or right node(=0)
      for(i in 1:n){
        Gch<- makeGchmatcat(gm,i,assignMatrix)
        child<-cpmat(Gch,y,tr,crit=crit)
        if(nsplit==1){End<-child}
        if(nsplit>1){
          End<-as.matrix(rbind(parents,child) )}
        dimnames(End)<-NULL
        check1<-ctc(End,parvec)
        if(check1==1){
          check2<-cmd(End,dmats)
          if( sum(check2)!=0) {
            dmats2<-dmats[check2==1,]
            if( sum(check2)==1) {    dmats2<-t(dmats2)}
            cdat<-computeC(End,dmats2,w)
            crittot[ check2==1,i]<-cdat$crittot
            crit1[ check2==1,i]<-cdat$critdif
            crit2[ check2==1,i]<-cdat$critcard } }  }

      ccmax<-apply(crittot,2,max)
      colstar<-which(ccmax==max(crittot))[1]
      rcmax<-apply(crittot,1,max)
      rowstar<- which(rcmax==max(crittot))[1]

      if(plotc==T){
        vec<-1:dim(dmats)[1]
        par(mfrow=c(1,2))
        plot(z[ccmax!=0],ccmax[ccmax!=0])
        plot(vec,rcmax) }

      splitpoint <- colstar
      result<-c(splitpoint,rowstar,crittot[rowstar,colstar],crit1[rowstar,colstar],crit2[rowstar,colstar])}
    if (n==0){result<-numeric(5)}
  }

  if(is.factor(x)==FALSE){
    z<-unique(sort(x[gm==1]))
    n<-length(z)

    #n is  the number of distinct values (split points) of predictor x
    crittot<-matrix(0,nrow=dim(dmats)[1],ncol=n)
    crit1<-matrix(0,nrow=dim(dmats)[1],ncol=n)
    crit2<-matrix(0,nrow=dim(dmats)[1],ncol=n)
    #crittot is a matrix with number of rows is number of possible assignments (rows of Ds)
    #and number of columns is n = number of splitpoints; the cells contain the value of the criterion C
    #Perform for each possible split point vik:
    if(n>1){
      for  (i in 1:(n-1)){
        #print(i)
        #create indicator matrix of child nodes (Gch) after split on z[i]:
        splitpoint<-(z[i]+z[i+1]) /2
        Gch<- makeGchmat(gm,x,splitpoint)
        #child will be filled with information of the two childnodes (cardinalities t0, cardinalities t1, and sum y|t=0, sum y|t=1)
        child<-cpmat(Gch,y,tr,crit=crit)
        if(nsplit==1){End<-child}
        if(nsplit>1){
          End<-as.matrix(rbind(parents,child) )}
        dimnames(End)<-NULL
        #print(End)
        #End is matrix E that contains the information of all the end nodes after a split (cardinalities t0, cardinalities t1, and sum y|t=0, sum y|t=1)
        ##check conditions for each row of dmats and if these are met compute the value of C which will be collected in matrix crittot
        check1<-ctc(End,parvec)
        #cat("treatment cardinality cond", check1, "\n")
        if(check1==1){
          check2<-cmd(End,dmats)
          #cat("mean difference cond", check2, "\n")
          if( sum(check2)!=0) {
            dmats2<-dmats[check2==1,]
            if( sum(check2)==1) {    dmats2<-t(dmats2)}
            cdat<-computeC(End,dmats2,w)
            crittot[ check2==1,i]<-cdat$crittot
            crit1[ check2==1,i]<-cdat$critdif
            crit2[ check2==1,i]<-cdat$critcard }
        }
      }

      ccmax<-apply(crittot,2,max)
      #ccmax is vector with maximum value of C for each split point
      colstar<-which(ccmax==max(crittot))[1]
      #colstar is the columnnumber referring to the splitpoint on variable Xk that results in the highest value of C
      rcmax<-apply(crittot,1,max)
      #rcmax is vector with maximum value of C for each row of the design matrix Ds
      rowstar<- which(rcmax==max(crittot))[1]
      if(plotc==T){
        vec<-1:dim(dmats)[1]
        par(mfrow=c(1,2))
        plot(z[ccmax!=0],ccmax[ccmax!=0])
        plot(vec,rcmax) }
      #rowstar is the rownumber referring to the row of dmats (Ds) that results in the highest value of C for this particular predictor variable
      splitpoint<- (z[colstar]+z[colstar+1] )/2
      result<-c(splitpoint,rowstar,crittot[rowstar,colstar],crit1[rowstar,colstar],crit2[rowstar,colstar])}
    if (n==1){result<-numeric(5)}
  }
  return(result)}
