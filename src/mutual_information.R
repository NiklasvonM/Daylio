
# Mutual information (transinformation)
mutualInformation <- function(x, y) {
  n <- length(x)
  stopifnot(n == length(y))
  res <- 0
  for(xi in unique(x)) {
    nX <- length(which(x == xi)) / n
    if(nX == 0)
      next
    for(yi in unique(y)) {
      nBoth <- length(which(x == xi & y == yi)) / n
      nY <- length(which(y == yi)) / n
      if(nBoth == 0)
        next
      res <- res + nBoth * log(nBoth / (nX * nY)) # / (nX * nY)?
    }
  }
  res
}
entropy <- function(x) {
  res <- 0
  n <- length(x)
  for(xi in unique(x)) {
    nX <- length(which(x == xi)) / n
    res <- res - nX * log(nX)
  }
  res
}

mutualInformationNormed <- function(x, y) {
  mutualInformation(x, y) / sqrt(entropy(x) * entropy(y))
}

measureOfDependence <- 
  #mutualInformationNormed
  cor
