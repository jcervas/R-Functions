all_combinations <- function(weights) {
  n <- length(weights)
  index_combos <- list()
  
  for (i in 1:n) {
    index_combos[[i]] <- combn(n, i, simplify = FALSE)
  }
  
  unlist(index_combos, recursive = FALSE)
}

# # Example

# weights <- c(3, 5, 8, 13, 21, 34, 55)
# coalition_indices <- all_combinations(weights)

# # Inspect a few:
# coalition_indices[1:5]
# # Each element is a vector of indices, e.g., c(1, 3, 5)

# # Get the sum of weights for each coalition
# coalition_sums <- sapply(coalition_indices, function(idx) sum(weights[idx]))
