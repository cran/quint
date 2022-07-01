## ---- echo=FALSE,include=FALSE, results='hide'--------------------------------
library(quint)
data(bcrp)

## -----------------------------------------------------------------------------
bcrp[1:3, c(1:6,14)]
ex_data <- subset(bcrp, cond < 3)

## -----------------------------------------------------------------------------
formula1 <- I(cesdt1-cesdt3)~cond|cesdt1+negsoct1+uncomt1+disopt1+comorbid+age+wcht1+nationality+marital+trext

#Old formula2 <- I(physt3-physt3)~cond|cesdt1+negsoct1+uncomt1+disopt1+comorbid+age+wcht1+nationality+marital+trext
##New formula 2
formula2 <- I(physt3-physt1)~cond|physt1+negsoct1+uncomt1+disopt1+comorbid+age+wcht1+nationality+marital+trext


## ---- message = FALSE, warning = FALSE, results='hide'------------------------
set.seed(4717) 
quint1 <- quint(formula1, data = ex_data)

## -----------------------------------------------------------------------------
summary(quint1)
quint1$fi
quint1$si
quint1$li

## -----------------------------------------------------------------------------
quint1pr <- prune(quint1)
plot(quint1pr)

## ---- message = FALSE, results='hide'-----------------------------------------
set.seed(48) #note that this is again a different random number due to R version 4 and higher
quint2 <- quint(formula = formula2, data = ex_data)

## -----------------------------------------------------------------------------
quint2$fi
quint2pr <- prune(quint2) 
plot(quint2pr)

## ---- message = FALSE---------------------------------------------------------
control3 <- quint.control(crit = "dm")
set.seed(48)
quint3 <- quint(formula = formula2, data = ex_data, control = control3)

## ---- message = FALSE---------------------------------------------------------
control4 <- quint.control(maxl = 2)
set.seed(48)
quint4 <- quint(formula = formula1, data = ex_data, control = control4)
round(quint4$li, digits = 2)

## ---- message = FALSE, results='hide'-----------------------------------------
control5 <- quint.control(crit = "dm", dmin = 0.40)
set.seed(48)
quint5 <- quint(formula = formula2 , data = ex_data, control = control5)

## -----------------------------------------------------------------------------
quint5pr <- prune(quint5)
quint5pr$li

## ---- message = FALSE, results='hide'-----------------------------------------
set.seed(47) #this is the original seed of the first example in the paper
control6 <- quint.control(B = 50) #now we choose 50 bootstrap samples
quint6 <- quint(formula = formula1 , data = ex_data, control = control6)

## -----------------------------------------------------------------------------
quint6$fi
quint6pr <- prune(quint6)
plot(quint6pr)

