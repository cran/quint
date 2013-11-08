\name{quint-package}
\alias{quint-package}
\docType{package}
\title{
R-package for Qualitative Treatment-Subgroup Interactions
}
\description{
When two treatment alternatives (say A and B) are available  for some problem, one may be interested in qualitative 
treatment-subgroup interactions. Such interactions imply the existence of subgroups of persons (patients) which are 
such that in one subgroup Treatment A outperforms Treatment B, whereas the reverse holds in another subgroup. 
Obviously, this type of interactions is crucial for optimal treatment assignment of future patients. Given baseline 
characteristics and outcome data from a two-arm Randomized Controlled Trial (RCT), QUalitative INteraction Trees 
(QUINT) is a tool to identify subgroups that are involved in meaningful qualitative treatment-subgroup interactions. 
The result of QUINT is a tree that partitions the total group of participants (patients) on the basis of their baseline 
characteristics into three subgroups (i.e., partition classes): Subgroup 1: Those for whom Treatment A is better than 
Treatment B (P1), Subgroup 2: Those for whom Treatment B is better than Treatment A (P2), and Subgroup 3: 
Those for whom it does not make any difference (P3).

}
\details{
\tabular{ll}{
Package: \tab quint\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-11-08\cr
License: \tab GPL\cr
}
Version 1.0 is suitable for a continuous outcome variable. The baseline characteristics (i.e., possible splitting variables) may be continuous, ordinal or dichotomous.
The core function of the package is \code{\link{quint}}.
}
\author{
Maintainer: Elise Dusseldorp <elise.dusseldorp@tno.nl>
}
\references{
Dusseldorp E. and Van Mechelen I. (2013). Qualitative interaction trees: a tool to identify qualitative treatment-subgroup interactions. \emph{Statistics in Medicine}, first online 6 Aug 2013.  DOI: 10.1002/sim.5933.

Scheier M.F., Helgeson V.S., Schulz R., et al.(2007). Moderators of interventions designed to enhance physical and psychological functioning among younger women with early-stage breast cancer. \emph{Journal of Clinical Oncology, 25}, 5710-5714. DOI: 10.1200/JCO.2007.11.7093.
}
\keyword{ package }
\seealso{
\code{\link{quint}},\code{\link{summary.quint}},\code{\link{quint.control}},\code{\link{prune.quint}} 
}


