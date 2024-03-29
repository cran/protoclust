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

#' Plot Dendrogram
#' 
#' Calls \code{\link{plotwithprototypes}}, which allows one to add prototype
#' labels to the dendrogram.
#' 
#' @param x a protoclust object
#' @param ... additional arguments to be passed to 
#'        \code{\link{plotwithprototypes}}
#' @export
plot.protoclust <- function(x, ...) {
  plotwithprototypes(x, ...)
}



#' Plot Dendrogram with Prototype Labels Added
#' 
#' Makes a plot of the dendrogram (using \code{plot.hclust}) and adds labels of
#' prototypes on the interior nodes of a dendrogram.
#' 
#' This function lets one put prototype labels on a dendrogram.  The argument
#' \code{imerge} controls which interior nodes and leaves are labeled.  A
#' convenient choice for the argument \code{imerge} is the \code{imerge}-output
#' of \code{\link{protocut}}.  This allows one to label a dendrogram with the
#' prototypes of a particular cut.  See examples below.  This function is
#' called when one writes \code{plot(hc)}, where \code{hc} is an object of
#' class \code{protoclust}.
#' 
#' @param hc an object of class \code{protoclust} (as returned by the function
#' \code{protoclust})
#' @param imerge a vector of the nodes whose prototype labels should be added.
#' Interior nodes are numbered from 1 (lowest merge) to n - 1 (highest merge,
#' i.e. the root) and leaf-nodes are negative (so if element i is a prototype
#' for a singleton cluster, then -i is included in imerge).  Example:
#' \code{seq(1, n - 1)} means every interior node is labeled with a prototype.
#' For larger trees, showing only the prototypes at a given cut may be easier
#' (described more below). Default: \code{-seq(n)}, meaning all leaf labels and
#' no interior-node labels are shown.
#' @param labels an optional character vector of length n giving the labels of
#' the elements clustered.  If not provided, hc$labels is used (if not NULL) or
#' else labels are taken to be \code{seq(n)}.
#' @param bgcol background color for prototype labels
#' @param col,font color and font of prototype labels
#' @param cex size of prototype label
#' @param ... additional arguments to be passed to \code{plot.hclust}, such as
#' \code{hang}
#' @author Jacob Bien and Rob Tibshirani
#' @seealso \code{\link{protoclust}}, \code{\link{protocut}}
#' @references Bien, J., and Tibshirani, R. (2011), "Hierarchical Clustering
#' with Prototypes via Minimax Linkage," \emph{The Journal of the American 
#' Statistical Association}, 106(495), 1075-1084.
#' @keywords cluster
#' @examples
#' 
#' # generate some data:
#' set.seed(1)
#' n <- 100
#' p <- 2
#' x <- matrix(rnorm(n * p), n, p)
#' rownames(x) <- paste("A", 1:n, sep="")
#' d <- dist(x)
#' 
#' # perform minimax linkage clustering:
#' hc <- protoclust(d)
#' 
#' # cut the tree to yield a 10-cluster clustering:
#' k <- 10 # number of clusters
#' cut <- protocut(hc, k=k)
#' h <- hc$height[n - k]
#' 
#' # plot dendrogram (and show cut):
#' plotwithprototypes(hc, imerge=cut$imerge)
#' # or more simply: plot(hc, imerge=cut$imerge)
#' abline(h=h, lty=2)
#' 
#' # negative values of imerge specify which leaves to label
#' k2 <- 20 # more clusters... with two singletons
#' cut2 <- protocut(hc, k=k2)
#' h2 <- hc$height[n - k2]
#' plot(hc, hang=-1, imerge=cut2$imerge)
#' abline(h=h2, lty=2)
#' 
#' @export plotwithprototypes
plotwithprototypes <- function(hc, imerge=-seq(n), labels=NULL, bgcol="white", font=1, col=1, cex=1, ...) {
  # Uses plot.hclust  imerge controls which leaves and interior nodes to label.
  if (!inherits(hc, "protoclust"))
    stop("Must input object of class protoclust.")
  n <- length(hc$order)
  # set labels:
  if (is.null(labels)) {
    labels <- hc$labels
    if (is.null(labels))
      labels <- seq(n)
  }
  else if (length(labels) != n)
    stop("labels must have length equal to the number of objects clustered.")
  leaves <- imerge[imerge < 0]
  int_nodes <- imerge[imerge > 0]
  plotwithtext(hc, 
               imerge = c(leaves, int_nodes), 
               text = c(labels[-leaves], labels[hc$protos[int_nodes]]),
               bgcol = bgcol, font = font, col = col, cex = cex, ...)
}

# This is the old plotwithprototypes function:
# plotwithprototypes <- function(hc, imerge=-seq(n), labels=NULL, bgcol="white", font=1, col=1, cex=1, ...) {
#   # Uses plot.hclust  imerge controls which leaves and interior nodes to label.
#   if (!inherits(hc, "protoclust"))
#     stop("Must input object of class protoclust.")
#   additional_arguments <- list(...)
#   for (arg in c("imerge", "col", "font"))
#   if (!is.null(additional_arguments[[arg]])) {
#     assign(arg, additional_arguments[[arg]])
#     additional_arguments[arg] <- NULL
#   }
#   n <- length(hc$order)
#   stopifnot(imerge < n & imerge >= -n)
#   # set labels:
#   if (is.null(labels)) {
#     labels <- hc$labels
#     if (is.null(labels))
#       labels <- seq(n)
#   }
#   else if (length(labels) != n)
#     stop("labels must have length equal to the number of objects clustered.")
#   # draw dendrogram, labeling specified leaves:
#   leaf <- -imerge[imerge < 0]
#   leaflabels <- rep("", n)
#   leaflabels[leaf] <- labels[leaf] # only draw leaves specified by imerge
#   hhc <- hc
#   class(hhc) <- "hclust"
#   plot_args <- list(hhc, labels = leaflabels)
#   plot_args <- c(plot_args, additional_arguments)
#   do.call(plot, plot_args)
#   # draw interior node labels:
#   imerge <- imerge[imerge > 0]
#   if (length(imerge) != 0) {
#     x <- cbind(innernodepositions(hc)[imerge], hc$height[imerge])
#     labs <- labels[hc$protos[imerge]]
#     ww <- strwidth(labs) / 2 * cex
#     hh <- strheight(labs) * cex
#     rect(x[, 1] - ww, x[, 2], x[, 1] + ww, x[, 2] + hh, col=bgcol, border=FALSE)
#     text(x[, 1], x[, 2] + hh / 2, labels=labs, cex=cex, col=col, font=font)
#   }
# }

#' Plot Dendrogram with Interior Node Text Added
#' 
#' Makes a plot of the dendrogram (using \code{plot.hclust}) and adds interior
#' node text.
#' 
#' This function lets one put text on a dendrogram.  The argument
#' \code{imerge} controls which interior nodes and leaves are labeled with text.
#' 
#' @param hc an object of class \code{hclust}
#' @param imerge a vector of the nodes where text is desired.
#' Interior nodes are numbered from 1 (lowest merge) to n - 1 (highest merge,
#' i.e. the root) and leaf-nodes are negative.  Example:
#' \code{seq(1, n - 1)} means every interior node is labeled with text. 
#' Default: \code{-seq(n)}, meaning all leaf labels and no interior-node labels 
#' are shown.
#' @param text a character vector of length matching `imerge`.  The element
#' `text[i]` will label node `imerge[i]`.
#' @param bgcol background color for labels
#' @param col,font color and font of labels
#' @param cex size of label
#' @param ... additional arguments to be passed to \code{plot.hclust}, such as
#' \code{hang}
#' @author Jacob Bien and Rob Tibshirani
#' @references Bien, J., and Tibshirani, R. (2011), "Hierarchical Clustering
#' with Prototypes via Minimax Linkage," \emph{The Journal of the American 
#' Statistical Association}, 106(495), 1075-1084.
#' @keywords cluster
#' @examples
#' 
#' # generate some data:
#' set.seed(1)
#' n <- 100
#' p <- 2
#' x <- matrix(rnorm(n * p), n, p)
#' rownames(x) <- paste("A", 1:n, sep="")
#' d <- dist(x)
#' 
#' # perform minimax linkage clustering:
#' hc <- protoclust(d)
#' 
#' # cut the tree to yield a 10-cluster clustering:
#' k <- 10 # number of clusters
#' cut <- protocut(hc, k=k)
#' h <- hc$height[n - k]
#' 
#' # plot dendrogram (and show cut):
#' protoclust:::plotwithtext(hc, imerge=cut$imerge, text=paste("Cluster", 1:k))
#' abline(h=h, lty=2)
#' 
#' # negative values of imerge specify which leaves to label
#' protoclust:::plotwithtext(hc, imerge=c(-1, cut$imerge), text=c("1", paste("Cluster", 1:k)))
plotwithtext <- function(hc, imerge=-seq(n), text=as.character(1:n), bgcol="white", font=1, col=1, cex=1, ...) {
  # Uses plot.hclust  imerge controls which leaves and interior nodes to label.
  if (!inherits(hc, "hclust"))
    stop("Must input object of class hclust.")
  additional_arguments <- list(...)
  for (arg in c("imerge", "col", "font"))
    if (!is.null(additional_arguments[[arg]])) {
      assign(arg, additional_arguments[[arg]])
      additional_arguments[arg] <- NULL
    }
  n <- length(hc$order)
  stopifnot(imerge < n & imerge >= -n)
  stopifnot(length(text) == length(imerge))
  # draw dendrogram, labeling specified leaves:
  leaf <- -imerge[imerge < 0]
  leaflabels <- rep("", n)
  leaflabels[leaf] <- text[imerge < 0] # only draw leaves specified by imerge
  hhc <- hc
  class(hhc) <- "hclust"
  plot_args <- list(hhc, labels = leaflabels)
  plot_args <- c(plot_args, additional_arguments)
  do.call(plot, plot_args)
  # draw interior node labels:
  int_nodes <- which(imerge > 0)
  imerge <- imerge[int_nodes]
  if (length(imerge) != 0) {
    x <- cbind(innernodepositions(hc)[imerge], hc$height[imerge])
    labs <- text[int_nodes]
    ww <- strwidth(labs) / 2 * cex
    hh <- strheight(labs) * cex
    rect(x[, 1] - ww, x[, 2], x[, 1] + ww, x[, 2] + hh, col=bgcol, border=FALSE)
    text(x[, 1], x[, 2] + hh / 2, labels=labs, cex=cex, col=col, font=font)
  }
}

