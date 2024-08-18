# Mutual information (transinformation)
mutual_information <- function(x, y) {
  n <- length(x)
  stopifnot(n == length(y))
  res <- 0
  for (xi in unique(x)) {
    nX <- length(which(x == xi)) / n
    if (nX == 0)
      next
      for (yi in unique(y)) {
        nBoth <- length(which(x == xi & y == yi)) / n
        nY <- length(which(y == yi)) / n
        if (nBoth == 0)
          next
          res <- res + nBoth * log(nBoth / (nX * nY)) # / (nX * nY)?
      }
  }
  res
}
entropy <- function(x) {
  res <- 0
  n <- length(x)
  for (xi in unique(x)) {
    nX <- length(which(x == xi)) / n
    res <- res - nX * log(nX)
  }
  res
}

mutual_information_normed <- function(x, y) {
  mutual_information(x, y) / sqrt(entropy(x) * entropy(y))
}

measure_of_dependence <-
#mutual_information_normed
cor
