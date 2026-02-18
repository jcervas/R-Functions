

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

core_retention <- function(df, base_plan, plans = NULL, pop_col = NULL) {

  if (is.null(plans)) {
    plans <- setdiff(names(df), c("GEOID20", pop_col))
  }

  base <- df[[base_plan]]

  # weights
  if (is.null(pop_col)) {
    w <- rep(1, nrow(df))
  } else {
    w <- df[[pop_col]]
  }

  total_w <- sum(w, na.rm = TRUE)

  retention <- sapply(plans, function(p) {
    same <- df[[p]] == base
    sum(w[same], na.rm = TRUE) / total_w
  })

  data.frame(
    plan = plans,
    core_retention = retention
  )
}
