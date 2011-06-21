protocut <- function(hc, k=NULL, h=NULL) {
  if (!inherits(hc, "protoclust"))
    stop("Must input an object of class protoclust.")
  n <- length(hc$height) + 1
  if(is.null(k)) {
    if(is.null(h))
      stop("Must input either k or h.")
    if (length(h) > 1) {
      warning("Ignoring all but first element of h.")
      h <- h[1]
    }
    k <- n - sum(hc$height <= h)
  }
  else if(!is.null(h))
    warning("Ignoring h since k is provided.")
  if (length(k) > 1) {
    warning("Ignoring all but first element of h.")
    k <- k[1]
  }
  if(round(k) != k) {
    k <- round(k)
    cat("Rounding k to", k, ".\n")
  }
  if(k > n / 2) {
    # go bottom up
    iprot <- rep(FALSE, n - 1)
    leafprot <- rep(TRUE, n)
    if(k < n) {
      for(i in seq(n - k)) {
        for (j in 1:2) {
          if(hc$merge[i, j] < 0) {
            leafprot[-hc$merge[i, j]] <- FALSE
          }
          if(hc$merge[i, j] > 0) {
            iprot[hc$merge[i, j]] <- FALSE
          }
        }
        iprot[i] <- TRUE
      }
    }
  }
  else {
    # for k < n/2, going from top will be faster.
    iprot <- rep(FALSE, n - 1) # indices of hc$protos that we will use.
    iprot[n - 1] <- TRUE
    leafprot <- rep(FALSE, n)
    if(k > 1) {
      for(i in seq(k - 1)) {
        iprot[n - i] <- FALSE
        m <- hc$merge[n - i, ]
        for(j in 1:2) {
          if(m[j] < 0)
            leafprot[-m[j]] <- TRUE
          else
            iprot[m[j]] <- TRUE
        }
      }
    }     
  }
  leafprot <- which(leafprot)
  imerge <- c(which(iprot != 0), -leafprot)
  protos <- c(hc$protos[iprot], leafprot)
  cl <- cutree(hc, k=k)
  o <- order(cl[protos])# reorder protos in order of class
  list(cl=cl, protos=protos[o], imerge=imerge[o])
}
