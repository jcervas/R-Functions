  is_minimum <- function(x,v) {
  # stopifnot(!is.numeric(x), "Not numeric")
  if (sum(x)<v) return(FALSE)
  for (k in 1:length(x)) {
    xx <- x[-k]
    ifelse(sum(xx)<v, next, return(FALSE))
  }
  return(TRUE)
}

find_minimal_winning_coalitions <- function(weights, quota, print_results = TRUE) {
  n <- length(weights)
  coalitions <- list()
  index <- 1

  for (k in 1:n) {
    combs <- combn(n, k, simplify = FALSE)
    for (combo in combs) {
      w_sum <- sum(weights[combo])
      if (w_sum >= quota) {
        # Check for minimality
        is_minimal <- TRUE
        for (i in combo) {
          if (sum(weights[setdiff(combo, i)]) >= quota) {
            is_minimal <- FALSE
            break
          }
        }
        if (is_minimal) {
          coalitions[[index]] <- combo
          index <- index + 1
        }
      }
    }
  }

  if (print_results) {
    cat("Minimum winning coalitions (indices and weights):\n")
    for (coal in coalitions) {
      cat("{", paste(coal, collapse = ", "), "} -> weights: {", paste(weights[coal], collapse = ", "), "}\n")
    }
  }

  return(coalitions)
}

## Example:
# weights <- c(3, 5, 8, 13, 21, 34, 55)
# quota <- 70

# mwcs <- find_minimal_winning_coalitions(weights, quota)
