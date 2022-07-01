# All subfunctions relevant for displaying node information:

csigmap<-function(n1,sd1,n2,sd2, weight = TRUE){
  #computes estimate of pooled sigma , see Cohen (1988, p. 66)
  # sd1 , sd 2 and n1, n2: standard deviation and sample size for group 1 and 2 respectively
  sigmap<-sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2))      #denominator: -2 !
  if (weight == FALSE) {sigmap <- sqrt((sd1^2 + sd2^2)/2)}
  return(sigmap)}


computeD<-function(n1, mean1,sd1,n2,mean2,sd2){
  # for crit = 'es':
  #computes Cohen's D see Cohen (1988, p. 66) and Hedges (1982, p. 111-112)
  sigmap <- csigmap(n1, sd1, n2, sd2)
  dval <- (mean1 - mean2)/sigmap
  mi <- (n1 + n2 - 2)
  ni <- (n1 * n2)/(n1 + n2)
  fac <- mi/((mi - 2) * ni)

  # Way to compute it up to version 2.0.0
  # cm <- 1 - (3/(4*mi - 1)) # approx of cm value instead of using gamma function for large sample sizes
  # var <- (2/mi) * (1 + dval^2/4)
  # if (mi <= 240) {
  #   cm <- (gamma(mi/2))/(sqrt(mi/2) * gamma((mi - 1)/2))
  #   var <- (fac * (1 + ni * dval^2)) - dval^2/cm^2
  # }

  cm <- exp(lgamma(mi/2)-log(sqrt(mi/2))-lgamma((mi-1)/2))
  var <- (fac * (1 + ni * dval^2)) - dval^2/cm^2

  sedval <- sqrt(var)
  # for crit = 'dm':
  diff <- mean1 - mean2
  seDiff <- sqrt((sigmap^2)*(1/n1 + 1/n2))
  obj <- list(dval = dval, se = sedval, diff = diff, seDiff = seDiff)
  return(obj)}


ctmat<-function(Gmat,y,tr,crit){
  #creates a matrix (tmat) with final information of each terminal node of the tree (each column of Gmat)
  #tmat= I*7 matrix
  #Gmat = nodeindicator matrix
  #y = outcome variable, column vector
  #tr = treatmentvariable with two values (1: T=1; 2: T=2)
  ## cardinalities t1, cardinalities t2, and mean  and var y|t=1, mean and var y|t=2
  #each row of tmat gives this information for each column of Gmat
  #thus number of rows of pmat corresponds to number of columns of Gmat
  rownum <- ncol(Gmat)
  t2mat <- matrix(0, ncol = 2, nrow = rownum)
  t1mat <- matrix(0, ncol = 2, nrow = rownum)
  tmat <- matrix(0, ncol = 2, nrow = rownum)
  dat <- data.frame(cbind(y = y, tr = tr))
  t1mat[, 1] <- sapply(1:rownum, function(kk, Gmat, y, tr) {
    ifelse(sum(Gmat[, kk] == 1 & tr == 1) == 0, NA, mean(y[Gmat[,kk] == 1 & tr == 1]))}, Gmat = Gmat, y = y, tr = tr)
  t1mat[, 2] <- sapply(1:rownum, function(kk, Gmat, y, tr) {
    ifelse(sum(Gmat[, kk] == 1 & tr == 1) == 0, NA, sqrt(var(y[Gmat[, kk] == 1 & tr == 1])))}, Gmat = Gmat, y = y, tr = tr)
  t2mat[, 1] <- sapply(1:rownum, function(kk, Gmat, y, tr) {
    ifelse(sum(Gmat[, kk] == 1 & tr == 2) == 0, NA, mean(y[Gmat[,kk] == 1 & tr == 2]))}, Gmat = Gmat, y = y, tr = tr)
  t2mat[, 2] <- sapply(1:rownum, function(kk, Gmat, y, tr) {
    ifelse(sum(Gmat[, kk] == 1 & tr == 2) == 0, NA, sqrt(var(y[Gmat[,kk] == 1 & tr == 2])))}, Gmat = Gmat, y = y, tr = tr)
  tmat <- cbind(apply(as.matrix(Gmat[tr == 1, ]), 2, sum), t1mat, apply(as.matrix(Gmat[tr == 2, ]), 2, sum), t2mat)

  if(crit=="es") {
    es <- sapply(1:rownum, function(kk, tmat) {
      ifelse(is.na(sum(tmat[kk, c(2:3, 5:6)])), NA, computeD(tmat[kk,1], tmat[kk, 2], tmat[kk, 3], tmat[kk, 4], tmat[kk, 5], tmat[kk, 6])$dval) }, tmat = tmat)
    se <- sapply(1:rownum, function(kk, tmat) {
      ifelse(is.na(sum(tmat[kk, c(2:3, 5:6)])), NA, computeD(tmat[kk,1], tmat[kk, 2], tmat[kk, 3], tmat[kk, 4], tmat[kk,5], tmat[kk, 6])$se)}, tmat = tmat)
    tmat <- cbind(tmat, es, se)
    colnames(tmat) <- c("nt1", "meant1", "sdt1", "nt2", "meant2","sdt2", "d", "se")
  }

  if(crit=="dm") {
    diff <- sapply(1:rownum, function(kk, tmat) {
      ifelse(is.na(sum(tmat[kk, c(2:3, 5:6)])), NA, computeD(tmat[kk,1], tmat[kk, 2], tmat[kk, 3], tmat[kk, 4], tmat[kk, 5], tmat[kk, 6])$diff) }, tmat = tmat)
    se <- sapply(1:rownum, function(kk, tmat) {
      ifelse(is.na(sum(tmat[kk, c(2:3, 5:6)])), NA, computeD(tmat[kk,1], tmat[kk, 2], tmat[kk, 3], tmat[kk, 4], tmat[kk,5], tmat[kk, 6])$seDiff)}, tmat = tmat)
    tmat <- cbind(tmat, diff, se)
    colnames(tmat) <- c("nt1", "meant1", "sdt1", "nt2", "meant2","sdt2", "diff", "se")
  }
  return(as.matrix(tmat))}


cpmat<-function(Gmat,y,tr,crit){
  #creates pmat = candidate parent nodes information matrix
  #pmat= I * 3 matrix ; I = number of candidate parent nodes
  #columns of pmat: per node: cardinality t1, cardinality t2, cohen's d or difference in absolute means
  #Gmat = N*I matrix=indicator matrix of all candidate parent nodes
  #y = outcome variable , column vector
  #tr = treatment-variable with values 1 and 2, column vector
  #crit="es": effect size criterion;  crit="dm": difference in means
  rownum<-ncol(Gmat)
  pmat<-matrix(0,ncol=3,nrow=rownum)
  nt2<-apply(Gmat[tr==2,],2,sum)
  nt1<-apply(Gmat[tr==1,],2,sum)
  meant2<-numeric(rownum)
  meant1<-numeric(rownum)
  #compute mean value of y for T=0
  meant2<-sapply(1:rownum,function(kk,Gmat,y,tr){mean(y[Gmat[,kk]==1&tr==2]) },Gmat=Gmat,y=y,tr=tr)
  #compute mean value of y for T=1
  meant1<-sapply(1:rownum,function(kk,Gmat,y,tr){mean(y[Gmat[,kk]==1&tr==1]) },Gmat=Gmat,y=y,tr=tr)
  #create pmat with crit = dm (no extra computations)
  pmat<-as.matrix(cbind(nt1,nt2,meant1-meant2))
  #create pmat with crit = es:
  if(crit=="es"){
    sigmap<-numeric(rownum)
    sigmap<-sapply(1:rownum,function(kk,Gmat,y,tr,nt1,nt2){
      ifelse(nt1[kk]<=1|nt2[kk]<=1,0,csigmap(nt1[kk],sqrt(var(y[Gmat[,kk]==1&tr==1])) ,
                                             nt2[kk],sqrt(var(y[Gmat[,kk]==1&tr==2]))) )},Gmat=Gmat,y=y,tr=tr,nt1=nt1,nt2=nt2)
    pmat[,3]<-pmat[,3]/sigmap
    pmat[,3][pmat[,3]==Inf]<-0
    pmat[,3][pmat[,3]==-Inf]<-0 }
  pmat[,3][is.na(pmat[,3])]<-0
  dimnames(pmat)<-NULL
  return(pmat)}
