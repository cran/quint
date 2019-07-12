## ----echo=FALSE,include=FALSE--------------------------------------------
library(quint)

## ----echo=TRUE, include=TRUE---------------------------------------------
data(bcrp)
head(bcrp)
bcrp2arm<-subset(bcrp,cond<3)
head(bcrp2arm)

## ----echo=TRUE, include=TRUE---------------------------------------------
summary(bcrp2arm)

## ----echo=TRUE, include=TRUE---------------------------------------------
control1 <- quint.control(maxl = 5,B = 10)

## ----echo=TRUE, include=TRUE---------------------------------------------
formula1<- I(cesdt1-cesdt3)~cond | nationality+marital+wcht1+  age+trext+comorbid+disopt1+uncomt1+negsoct1

## ----echo=TRUE, include=TRUE---------------------------------------------
set.seed(2)
quint1<-quint(formula1, data= bcrp2arm, control=control1 )

## ----echo=TRUE, include=TRUE---------------------------------------------
quint1pr <- prune(quint1)

## ----echo=TRUE, include=TRUE---------------------------------------------
summary(quint1pr)

## ----echo=TRUE, include=TRUE, fig.width=7.5, fig.height=6----------------
plot(quint1pr)

