  is_minimum <- function(x,v) {
  # stopifnot(!is.numeric(x), "Not numeric")
  if (sum(x)<v) return(FALSE)
  for (k in 1:length(x)) {
    xx <- x[-k]
    ifelse(sum(xx)<v, next, return(FALSE))
  }
  return(TRUE)
}

find_mwcs <- function(weights, quota) {
  all_sets <- all_combinations(weights)  # list of index vectors
  mwcs <- list()
  idx <- 1

  for (coal in all_sets) {
    weight_sum <- sum(weights[coal])
    if (weight_sum >= quota) {
      # Check minimality
      is_minimal <- TRUE
      for (j in coal) {
        if (sum(weights[setdiff(coal, j)]) >= quota) {
          is_minimal <- FALSE
          break
        }
      }
      if (is_minimal) {
        mwcs[[idx]] <- coal
        idx <- idx + 1
      }
    }
  }

  return(mwcs)
}

# # Example usage
# ec_weights <- c(3, 5, 8, 13, 21, 34, 55)
# quota <- 70
# mwcs <- find_mwcs(ec_weights, quota)

# # Optional: print results
# cat("Minimum Winning Coalitions:\n")
# for (coal in mwcs) {
#   cat("{", paste(coal, collapse = ", "), "} → weight:", sum(ec_weights[coal]), "\n")
# }
