  is_minimum <- function(x,v) {
  # stopifnot(!is.numeric(x), "Not numeric")
  if (sum(x)<v) return(FALSE)
  for (k in 1:length(x)) {
    xx <- x[-k]
    ifelse(sum(xx)<v, next, return(FALSE))
  }
  return(TRUE)
}