% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.quint.R
\name{predict.quint}
\alias{predict.quint}
\title{Predictions for new data with a QUINT object}
\usage{
\method{predict}{quint}(object, newdata, type = "class", ...)
}
\arguments{
\item{object}{an object of the class \dQuote{quint}.}

\item{newdata}{a data frame with data on new subjects for whom predictions should be made.
The data frame should contain at least the variables used in the splits of the fitted tree.
It is not necessary to include the treatment variable.}

\item{type}{character string denoting the type of predicted object to be returned. The default is
set to \code{type="class"}: a vector with predicted treatment subgroup classes per subject
is returned. If set to \code{"matrix"}, a matrix is returned with the leaf and
corresponding node of the tree to which a subject is assigned.}

\item{\dots}{optional additional arguments.}
}
\value{
One of the following objects is returned depending on output type specified
  in the function:

  If \code{type="class"}:
  vector of predicted treatment classes for every individual in the data set. Returns NA
  for subjects with missing values on one or more of the splitting variables.

  If \code{type="matrix"}:
  a matrix with predicted locations of subjects within the fitted tree. The leaf numbers are
  in the first column and the corresponding node numbers in the second column. Returns NA
  for subjects with missing values on one or more of the splitting variables.
}
\description{
Predicts for (new) subjects the treatment subgroups (P1, P2 or P3) based on a fitted
  \code{quint} object. The meaning of the subgroups are based on the two treatment categories
  used to fit the \code{quint} object.
}
\examples{
data(bcrp)
formula1<- I(cesdt1-cesdt3)~cond | nationality+marital+wcht1+age+
  trext+comorbid+disopt1+uncomt1+negsoct1

set.seed(10)
control1<-quint.control(maxl=5,B=2)
quint1<-quint(formula1, data= subset(bcrp,cond<3),control=control1) #Grow a QUINT tree

prquint1<-prune(quint1) #Prune QUINT tree to optimal size

#Predict for the same data set the treatment classes for patients individually:
predquint1<-predict(prquint1, newdata=subset(bcrp,cond<3), type='class')
predquint1

}
\seealso{
\code{\link{quint}}, \code{\link{prune.quint}}
}
