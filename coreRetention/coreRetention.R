
district_core <- function(df, base_plan, plan, pop_col = NULL) {

  base  <- df[[base_plan]]
  other <- df[[plan]]

  if (is.null(pop_col)) {
    w <- rep(1, nrow(df))
  } else {
    w <- df[[pop_col]]
  }

  base_districts  <- sort(unique(base))
  other_districts <- sort(unique(other))

  sapply(base_districts, function(d) {
    idx_base <- base == d
    base_pop <- sum(w[idx_base], na.rm = TRUE)

    overlaps <- sapply(other_districts, function(o) {
      sum(w[idx_base & other == o], na.rm = TRUE)
    })

    max(overlaps, na.rm = TRUE) / base_pop
  })
}


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
