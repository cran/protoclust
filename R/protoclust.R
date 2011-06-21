protoclust <- function(d, verb=FALSE) {
  if (is.matrix(d)) {
    if (nrow(d) == ncol(d)) {
      cat("converting to dist (note: ignores above diagonal)", fill=TRUE)
      d <- as.dist(d)
    }
  }
  if (class(d) != "dist")
    stop("d must be of class \"dist\" or be a square matrix.")
  nn <- length(d)
  n <- (1 + sqrt(1 + 8 * nn)) / 2
  
  out <- .C("hier",
            as.double(d),
            as.integer(n),
            as.integer(verb),
            as.integer(matrix(0, n - 1, 2)),
            as.double(rep(0, n - 1)),
            as.integer(rep(0, n)),
            as.integer(rep(0, n - 1)), PACKAGE="protoclust")[4:7]
    
  tree <- list(merge=matrix(out[[1]], n - 1, 2, byrow=TRUE),
               height=out[[2]],
               order=out[[3]],
               protos=out[[4]],
               labels=attr(d, "Labels"),
               method="minimax",
               call=match.call(),
               dist.method=attr(d, "method"))
  class(tree) <- c("protoclust", "hclust") # inherits from hclust
  tree
}
