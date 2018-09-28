# functions supporting main plot.quint() function

## function to make indices for node assignment of levels of a factor variable (used in to.party)
indexID <- function(nodeID, quint.out) {
  splitlevels <- unlist(strsplit(as.character(quint.out$si[quint.out$si[,1]==nodeID, 5]), split = ", "))
  everylevel <- sapply(quint.out$data[quint.out$si[quint.out$si[,1]==nodeID, 3]], FUN = function(p) sort(unique(p)))

  index1 <- numeric(length(splitlevels)) # index factor levels used for split
  for (i in 1:length(index1)) {
    index1[i] <- which(everylevel == splitlevels[i])
  }

  # indices of all levels not used in split
  index2 <- 1:length(everylevel)
  index2 <- index2[-index1]

  Ls <- integer(length(everylevel))
  Ls[index1] <- 1L; Ls[index2] <- 2L

  return(Ls)
}


## conversion function ##
to.party <- function(nodeID,quint.out, ...){
  if(class(nodeID)=="quint"){quint.out <- nodeID; nodeID <- 1L}
  if(is.null(quint.out$var.names)) quint.out$var.names <- colnames(quint.out$data)
  if(nodeID %in% quint.out$li[,1]) return(partynode(id=as.integer(nodeID),
                                                    info = quint.out$li[quint.out$li[,1]==nodeID,]
  ))

  if(nodeID %in% quint.out$si[,1]) {
    if(sapply(quint.out$dat[colnames(quint.out$data)==quint.out$si[quint.out$si[,1]==nodeID,3]], is.factor) == FALSE){
      return(partynode(id=as.integer(nodeID),
                       split =  partysplit(varid = as.integer(which(quint.out$var.names==quint.out$si[quint.out$si[,1]==nodeID,3])),
                                           breaks = quint.out$si[quint.out$si[,1]==nodeID,4]),
                       kids = lapply(c(nodeID*2,nodeID*2+1),to.party,quint.out)
      )
      )}

    if(sapply(quint.out$dat[colnames(quint.out$data)==quint.out$si[quint.out$si[,1]==nodeID,3]], is.factor) == TRUE){
      return(partynode(id=as.integer(nodeID),
                       split =  partysplit(varid = as.integer(which(quint.out$var.names==quint.out$si[quint.out$si[,1]==nodeID,3])),
                                           index = indexID(nodeID=nodeID, quint.out=quint.out)),
                       kids = lapply(c(nodeID*2,nodeID*2+1),to.party,quint.out))
      )}
  }
}



## conversion method
as.party.quint <- function(quint.out, nodeID=1L, ...){
  quintNodes <- to.party(nodeID,quint.out,...)
  # browser() ##########
  party.object <- party(quintNodes,quint.out$data
                        , fitted=data.frame(
                          "(fitted)"=colnames(quint.out$nind)[
                            apply(quint.out$nind==1,1,which)],
                          "(response)"=quint.out$data[,1],
                          check.names=FALSE)
                        , terms=terms(as.formula(paste(
                          colnames(quint.out$data)[1],"~",
                          paste(colnames(quint.out$data)[-1],collapse="+"))
                        )),
                        ...
  )
  party.object$ni <- quint.out$li
  class(party.object) <- c("constparty",class(party.object))
  return(party.object)
}


## quint inner node
node_quint <- function(obj, id = TRUE, abbreviate = FALSE, fill = "white", gp = gpar())
{
  meta <- obj$data
  nam <- names(obj)

  extract_label <- function(node) {
    if(is.terminal(node)) return(rep.int("", 2))

    varlab <- character_split(split_node(node), meta)$name
    if(abbreviate > 0) varlab <- abbreviate(varlab, as.numeric(abbreviate))

    plab <- ""
    return(c(varlab, plab))
  }

  maxstr <- function(node) {
    lab <- extract_label(node)
    klab <- if(is.terminal(node)) "" else unlist(lapply(kids_node(node), maxstr))
    lab <- c(lab, klab)
    lab <- unlist(lapply(lab, function(x) strsplit(x, "\n")))
    return(lab[which.max(nchar(lab))])
  }

  nstr <- maxstr(node_party(obj))
  if(nchar(nstr) < 6) nstr <- "aAAAAa"

  ### panel function for the inner nodes
  rval <- function(node) {
    node_vp <- viewport(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      width = unit(1, "strwidth", nstr) * 1.3,
      height = unit(3, "lines"),
      name = paste("node_inner", id_node(node), sep = ""),
      gp = gp
    )
    pushViewport(node_vp)

    xell <- c(seq(0, 0.2, by = 0.01),
              seq(0.2, 0.8, by = 0.05),
              seq(0.8, 1, by = 0.01))
    yell <- sqrt(xell * (1-xell))

    lab <- extract_label(node)
    fill <- rep(fill, length.out = 2)

    grid.polygon(x = unit(c(xell, rev(xell)), "npc"),
                 y = unit(c(yell, -yell)+0.5, "npc"),
                 gp = gpar(fill = fill[1]))

    grid.text(lab[1], y = unit(1.5 + 0.5 * FALSE, "lines"))

    if(id) {
      nodeIDvp <- viewport(x = unit(0.5, "npc"), y = unit(1, "npc"),
                           width = max(unit(1, "lines"), unit(1.3, "strwidth", nam[id_node(node)])),
                           height = max(unit(1, "lines"), unit(1.3, "strheight", nam[id_node(node)])))
      pushViewport(nodeIDvp)
      popViewport()
    }
    upViewport()
  }
  return(rval)
}
class(node_quint) <- "grapcon_generator"


## quint terminal node plot ##
terminal_quint <- function(obj,
                           col = "black",
                           fill = "lightgray",
                           width = 0.5,
                           yscale = NULL,
                           ylines = 3,
                           cex = 0.5,
                           id = TRUE,
                           gp = gpar())
{
  ni <- obj$ni
  ni.temp <- ni
  d.CI <- data.frame(
    lower = ni[,8] - 1.96 * ni[,9],
    mean = ni[,8],
    upper = ni[,8] + 1.96 * ni[,9]
  )
  yscale <- c(0-max(abs(d.CI)),max(abs(d.CI))) + c(-0.1, 0.1) * max(abs(d.CI))
  rval <- function(node) { # core plotting function
    nid <- id_node(node)
    top_vp <- viewport(layout = grid.layout(nrow = 3, ncol = 3, # define viewport
                                            widths = unit(c(ylines, 1, 1),
                                                          c("lines", "null", "lines")),
                                            heights = unit(c(1, 1, 2), c("lines","null","lines"))),
                       width = unit(1, "npc"),
                       height = unit(1, "npc") - unit(2, "lines"),
                       name = paste("node_quint", nid, sep = ""),
                       gp = gp)

    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = "transparent", col =0))

    ind2 <- nid == nodeids(obj,terminal=TRUE)

    ## main title
    bottom <- viewport(layout.pos.col=2, layout.pos.row=3)
    pushViewport(bottom)
    if(id){
      grid.text(sprintf("Leaf %s\nP%i",
                        which(nid==nodeids(obj,terminal=TRUE)), ni[ind2,10])
      )
    }else{grid.text("")}
    popViewport()

    plot <- viewport(layout.pos.col = 2, layout.pos.row = 2,
                     xscale = c(0, 1), yscale = yscale,
                     name = paste("node_quint", nid, "plot",
                                  sep = ""))
    pushViewport(plot)

    xl <- 0.5 - width/4
    xr <- 0.5 + width/4

    ## box & whiskers
    grid.rect(gp = gpar(fill = c("#92D050","#FF572F","#DDD8C2")[ni[ind2,10]]))
    ##refline
    grid.lines(unit(c(0, 1), "npc"),
               unit(0, "native"), gp = gpar(col = col,lwd=unit(width/4,"npc"),lty="dashed"))
    grid.lines(unit(c(xl, xr), "npc"),
               unit(d.CI$lower[ind2], "native"), gp = gpar(col = col,lwd=unit(width,"npc")))
    grid.lines(unit(0.5, "npc"),
               unit(c(d.CI$lower[ind2],d.CI$upper[ind2]), "native"), gp = gpar(col = col,lwd=unit(width/2,"npc")))
    meanline=FALSE ## if FALSE mean point
    if(meanline){
      grid.lines(unit(c(0.5 - width/2, 0.5+width/2), "npc"),
                 unit(d.CI$mean[ind2], "native"), gp = gpar(col = col, lwd = 2))
    }else{      grid.points(unit(0.5, "npc"),
                            unit(d.CI$mean[ind2], "native"), gp = gpar(col = col,lwd=1),size=unit(1,"lines"),pch=20)}
    grid.lines(unit(c(xl, xr), "npc"), unit(d.CI$upper[ind2], "native"),
               gp = gpar(col = col,lwd=unit(width,"npc")))

    grid.yaxis(label=TRUE) ## TO DO only TRUE for left terminal
    grid.rect(gp = gpar(fill = "transparent"))
    upViewport(2)
  }
  return(rval)
}
class(terminal_quint) <- "grapcon_generator"
