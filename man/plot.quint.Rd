% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.quint.R
\name{plot.quint}
\alias{plot.quint}
\title{Visualisation of a Qualitative Interaction Tree}
\usage{
\method{plot}{quint}(x, digits = 2, ...)
}
\arguments{
\item{x}{fitted tree of class \code{quint}.}

\item{digits}{specified number of decimal places of the splitpoints in the graph
(default is 2).}

\item{\dots}{additional arguments to be passed.}
}
\description{
Plot function for a \code{quint} object. The plot shows the result of \code{quint}:
a binary tree with (a) splitting variable(s) and split point(s). The colors of the
leaves of the tree correspond to the final subgroups: Subgroup 1 (P1), those
patients for whom the mean treatment outcome (Y) is higher for Treatment A than B,
is GREEN; Subgroup 2 (P2), those patients for whom the mean treatment outcome (Y)
is higher for Treatment B than A, is RED, and Subgroup 3 (P3), those for whom the
mean treatment outcome (Y) is about the same for both treatments, is GREY. Within the
leaves the effect size \emph{d} is displayed, with its 95 percent confidence interval.
This effect size is the standardized mean difference between Treatment A and B.
The plot function uses the plot method from the package \pkg{partykit} of Hothorn
& Zeileis (2013).
}
\details{
For categorical variables we recommend to use short names for levels to avoid overlapping
labels at split points.
}
\references{
Torsten Hothorn and Achim Zeileis (2013). partykit: A Toolkit for
  Recursive Partytioning. R package version 0.1-5.
}
\seealso{
\code{\link{quint}},\code{\link{quint.control}},\code{\link{bcrp}}
}
\author{
Cor Ninaber and Elise Dusseldorp
}
\keyword{as.party}
\keyword{plot}
