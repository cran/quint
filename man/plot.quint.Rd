\name{plot.quint}
\alias{plot.quint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualisation of a Qualitative Interaction Tree
}
\description{
Plot function for a \code{quint} object. The plot shows the result of \code{quint}: a binary tree with (a) splitting variable(s) and split point(s). The colors of the leaves of the tree correspond to the final subgroups: Subgroup 1 (P1), those patients for whom the mean treatment outcome (Y) is higher for Treatment A than B, is GREEN; Subgroup 2 (P2), those patients for whom the mean treatment outcome (Y) is higher for Treatment B than A, is RED, and Subgroup 3 (P3), those for whom the mean treatment outcome (Y) is about the same for both treatments. Wthin the leaves the effect size \emph{d} is displayed, with its 95 percent confidence interval. This effect size is the standardized mean difference between Treatment A and B. The plot function uses the plot method from the package \pkg{partykit} of Hothorn & Zeileis (2013).
}
\usage{
\method{plot}{quint}(x, digits=2,\dots)
}
\arguments{
  \item{x}{
fitted tree of class \code{quint}.
}
 \item{digits}{
specified number of decimal places of the splitpoints in the graph (default is 2).
}
   \item{\dots}{
additional arguments to be passed.  
}
}

\references{
Torsten Hothorn and Achim Zeileis (2013). partykit: A Toolkit for Recursive Partytioning. R package version 0.1-5.
  \url{http://CRAN.R-project.org/package=partykit}.
}
\author{
Cor Ninaber and Elise Dusseldorp
}
\seealso{
\code{\link{quint}},\code{\link{quint.control}},\code{\link{bcrp}} 
}

\keyword{ plot}
\keyword{ as.party}
