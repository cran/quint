\name{prune}
\alias{prune.quint}
\title{
Pruning of a Qualitative Interaction Tree
}
\description{
Determines the optimally pruned size of the tree by applying the one standard error rule to the results from the bias-corrected bootstrap procedure.
}
\usage{
\method{prune}{quint}(tree,pp=1,\dots)
}
\arguments{
  \item{tree}{
fitted tree of class \code{quint}.
}
  \item{pp}{
constant (\eqn{c}) to be used in the \eqn{c*}standard error rule. The default value is 1.}
  \item{\dots}{
generic function.
}
}
\details{
The pruning algorithm of \code{quint} is explained in Dusseldorp & Van Mechelen (2013), Appendix B of the online supplementary material. It is based on the bias-corrected bootstrap pruning procedure  (Le Blanc & Crowley, 1993) and the one standard error rule (Breiman, Friedman, Olshen, & Stone, 1984). The one standard error rule for \code{quint} uses the estimates of the bias-corrected criterion value (\eqn{C}) and its standard error for each value of \eqn{L} (= maximum number of leaves). The optimally pruned tree corresponds to the smallest tree with a bias-corrected \eqn{C} higher or equal to the maximum bias-corrected \eqn{C} minus its standard error.
}
\value{
Returns an object of class \code{quint}. The number of leaves of this object is equal to the optimally pruned size of the tree.
}
\references{
Breiman L., Friedman J.H., Olshen R.A. and Stone C.J. (1984). \emph{Classification and Regression Trees}. Chapman & Hall/CRC: Boca Raton.

Dusseldorp E. and Van Mechelen I. (2013). Qualitative interaction trees: a tool to identify qualitative treatment-subgroup interactions. \emph{Statistics in Medicine}, first online 6 Aug 2013. DOI: 10.1002/sim.5933.

LeBlanc M. and Crowley J. (1993). Survival trees by goodness of split. \emph{Journal of the American Statistical Association, 88,} 457-467.
}
\seealso{
\code{\link{quint.control}}, \code{\link{quint}}
}
\examples{
data(bcrp)
formula2<- I(cesdt1-cesdt3)~cond |age+trext++uncomt1+ disopt1+negsoct1
#Adjust the control parameters only to save computation time in the example;
#The default control parameters are preferred
control2<-quint.control(maxl=6,B=2)  
set.seed(2) #this enables you to repeat the results of the bootstrap procedure
quint2<-quint(formula2, data= subset(bcrp,cond<3),control=control2)
quint2pr<-prune(quint2)
summary(quint2pr)
}
\keyword{ tree }

