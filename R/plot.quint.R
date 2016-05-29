#' Visualisation of a Qualitative Interaction Tree
#'
#' Plot function for a \code{quint} object. The plot shows the result of \code{quint}:
#' a binary tree with (a) splitting variable(s) and split point(s). The colors of the
#' leaves of the tree correspond to the final subgroups: Subgroup 1 (P1), those
#' patients for whom the mean treatment outcome (Y) is higher for Treatment A than B,
#' is GREEN; Subgroup 2 (P2), those patients for whom the mean treatment outcome (Y)
#' is higher for Treatment B than A, is RED, and Subgroup 3 (P3), those for whom the
#' mean treatment outcome (Y) is about the same for both treatments, is GREY. Within the
#' leaves the effect size \emph{d} is displayed, with its 95 percent confidence interval.
#' This effect size is the standardized mean difference between Treatment A and B.
#' The plot function uses the plot method from the package \pkg{partykit} of Hothorn
#' & Zeileis (2013).
#'
#' For categorical variables we recommend to use short names for levels to avoid overlapping
#' labels at split points.
#'
#' @param x fitted tree of class \code{quint}.
#' @param digits specified number of decimal places of the splitpoints in the graph
#'   (default is 2).
#' @param \dots additional arguments to be passed.
#'
#' @references Torsten Hothorn and Achim Zeileis (2013). partykit: A Toolkit for
#'   Recursive Partytioning. R package version 0.1-5.
#'   \url{http://CRAN.R-project.org/package=partykit}.
#'
#' @author Cor Ninaber and Elise Dusseldorp
#' @seealso \code{\link{quint}},\code{\link{quint.control}},\code{\link{bcrp}}
#' @keywords plot
#' @keywords as.party
#'
#' @importFrom partykit as.party character_split id_node is.terminal kids_node node_party nodeids
#'   party partynode partysplit plot.party split_node
#' @importFrom graphics par plot
#' @importFrom grid gpar grid.layout grid.lines grid.points grid.polygon grid.rect grid.text
#'   grid.yaxis popViewport pushViewport unit upViewport viewport
#'
#' @export
plot.quint <- function(x, digits=2,...){
  #x is an object of class "quint"
  x$si[,4] <- round(x$si[,4],digits=digits)
  party.quint <- as.party(x)
  plot(party.quint, inner_panel= node_quint, terminal_panel=terminal_quint, ...)
  return(invisible(party.quint))
}

