

district_core <- function(df, base_plan, plan, pop_col = NULL) {

  base <- df[[base_plan]]
  other <- df[[plan]]

  if (is.null(pop_col)) {
    w <- rep(1, nrow(df))
  } else {
    w <- df[[pop_col]]
  }

  districts <- sort(unique(base))

  sapply(districts, function(d) {
    idx <- base == d
    sum(w[idx & other == d], na.rm = TRUE) /
      sum(w[idx], na.rm = TRUE)
  })
}
