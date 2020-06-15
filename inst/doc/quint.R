## ----echo=FALSE,include=FALSE--------------------------------------------
library(quint)

## ----echo=TRUE, include=TRUE---------------------------------------------
data(bcrp)
head(bcrp)
bcrp2arm<-subset(bcrp,bcrp$cond<3)
head(bcrp2arm)

## ----echo=TRUE, include=TRUE---------------------------------------------
summary(bcrp2arm)

## ----echo=TRUE, include=TRUE---------------------------------------------
control1 <- quint.control(crit="dm",maxl = 5,B = 10)

## ----echo=TRUE, include=TRUE---------------------------------------------
formula1<- I(cesdt1-cesdt3) ~ cond | nationality+marital+wcht1+  age+trext+comorbid+disopt1+uncomt1+negsoct1

## ----echo=TRUE, include=TRUE---------------------------------------------
set.seed(2)
quint1<-quint(formula1, data= bcrp2arm, control=control1 )

## ----echo=TRUE, include=TRUE---------------------------------------------
quint1pr <- prune(quint1)

## ----echo=TRUE, include=TRUE---------------------------------------------
quint1pr_bootCI <- quint.bootstrapCI(quint1pr,n_boot = 5)

## ----echo=TRUE, include=TRUE---------------------------------------------
summary(quint1pr)  #without bootstrap-based confidence intervals 
summary(quint1pr_bootCI$tree) #with bootstrap-based confidence intervals 

## ----echo=TRUE, include=TRUE, fig.width=7.5, fig.height=6----------------
plot(quint1pr) #without bootstrap-based confidence intervals 
plot(quint1pr_bootCI$tree) #with bootstrap-based confidence intervals 

