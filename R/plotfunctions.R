
innernodepositions <- function(hc) {
  # Returns the x coordinate of each interior node in an hclust plot.
  n <- length(hc$height) + 1
  x <- order(hc$order) # x[i] = x-coord of leaf i
  pos <- rep(0, n - 1)
  xx <- rep(0, 2)
  for(i in seq(n - 1)) {
    for(j in seq(2)) {
      if(hc$merge[i, j] < 0)
        xx[j] <- x[-hc$merge[i, j]]
      else
        xx[j] <- pos[hc$merge[i, j]]
    }
    pos[i] <- mean(xx)
  }
  return(pos)
}

plot.protoclust <- function(x, ...) {
  plotwithprototypes(x, ...)
}

plotwithprototypes <- function(hc, imerge=-seq(n), labels=NULL, bgcol="white", font=1, col=1, cex=1, ...) {
  # Uses plclust.  imerge controls which leaves and interior nodes to label.
  if (!inherits(hc, "protoclust"))
    stop("Must input object of class protoclust.")
  n <- length(hc$order)
  stopifnot(imerge < n & imerge >= -n)
  # set labels:
  if (is.null(labels)) {
    labels <- hc$labels
    if (is.null(labels))
      labels <- seq(n)
  }
  else if (length(labels) != n)
    stop("labels must have length equal to the number of objects clustered.")
  # draw dendrogram, labeling specified leaves:
  leaf <- -imerge[imerge < 0]
  leaflabels <- rep("", n)
  leaflabels[leaf] <- labels[leaf] # only draw leaves specified by imerge
  plclust(hc, labels=leaflabels, ...)
  # draw interior node labels:
  imerge <- imerge[imerge > 0]
  if (length(imerge) != 0) {
    x <- cbind(innernodepositions(hc)[imerge], hc$height[imerge])
    labs <- labels[hc$protos[imerge]]
    ww <- strwidth(labs) / 2 * cex
    hh <- strheight(labs) * cex
    rect(x[, 1] - ww, x[, 2], x[, 1] + ww, x[, 2] + hh, col=bgcol, border=FALSE)
    text(x[, 1], x[, 2] + hh / 2, labels=labs, cex=cex, col=col, font=font)
  }
}
