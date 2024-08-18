# Count number of appearences (using fun as a counting function)
# in a vector v of the last n_lookback entries.
# The first n_lookback entries of the result are always NA.
# Alternative for fun = sum:
# function(x) sum(x > 0)
count_entries_lookback <- function(v, n_lookback = 14, fun = sum) {
  n <- length(v)
  if (n <= n_lookback)
    return(rep_len(NA, length.out = n))
  res <- c(rep_len(NA, length.out = n_lookback), vector("integer", length = n - n_lookback))
  for (i in (n_lookback + 1):n) {
    res[i] <- fun(v[(i - n_lookback):i], na.rm = TRUE)
  }
  return(res)
}
