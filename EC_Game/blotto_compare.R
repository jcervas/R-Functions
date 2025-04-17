blotto_compare <- function(matrix_data, weights, sample = FALSE, n_opponents = 1000, tie_method = "coinflip") {
  n_players <- nrow(matrix_data)
  total_weight <- sum(weights)
  threshold <- total_weight / 2
  win_counts <- numeric(n_players)

  cat("Starting Blotto comparison...\n")

  for (i in 1:n_players) {
    p1 <- matrix_data[i, ]

    if (sample) {
      opponents_idx <- sample(setdiff(1:n_players, i), n_opponents)
    } else {
      opponents_idx <- setdiff(1:n_players, i)
    }

    opponents <- matrix_data[opponents_idx, , drop = FALSE]
    n_opps <- length(opponents_idx)

    p1_mat <- matrix(rep(p1, each = n_opps), nrow = n_opps)

    win_matrix <- (opponents < p1_mat)
    tie_matrix <- (opponents == p1_mat)

    win_points <- win_matrix %*% weights

    if (tie_method == "coinflip") {
      tie_flips <- matrix(rbinom(length(tie_matrix), 1, 0.5), nrow = n_opps)
      tie_points <- (tie_matrix * tie_flips) %*% weights
    } else if (tie_method == "p2wins") {
      tie_points <- matrix(0, nrow = n_opps, ncol = 1)
    } else {
      stop("Invalid tie_method")
    }

    total_points <- win_points + tie_points
    wins <- total_points > threshold
    win_counts[i] <- sum(wins)

    if (i %% 500 == 0 || i == n_players) {
      cat("→ Processed", i, "of", n_players, "players...\n")
    }
  }

  # Build results
  combinations_df <- as.data.frame(matrix_data)
  combinations_df$Win_Count <- win_counts

  total_possible_wins <- if (sample) n_opponents else (n_players - 1)
  combinations_df$Win_Percentage <- (win_counts / total_possible_wins) * 100
  combinations_df$Rank <- rank(-combinations_df$Win_Count, ties.method = "first")

  ranked_combinations <- combinations_df[order(combinations_df$Rank), ]

  # Print top and bottom results
  cat("\nTop 10 Combinations:\n")
  print(head(ranked_combinations, 10), row.names = FALSE)

  cat("\nBottom 10 Combinations:\n")
  print(tail(ranked_combinations, 10), row.names = FALSE)

  # Summary
  cat("\nSummary Statistics:\n")
  cat("→ Average Wins:", mean(win_counts), "\n")
  cat("→ Minimum Wins:", min(win_counts), "\n")
  cat("→ Maximum Wins:", max(win_counts), "\n")

  return(invisible(ranked_combinations))  # Return silently unless assigned
}



# Helper: Save progress as we go
save_progress <- function(result_vector, current_index) {
  write.csv(result_vector, file = sprintf("/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/win_counts_checkpoint_%04d.csv", current_index))
}


# # Example
blotto_compare(
  a, 
  ec_weights, 
  sample = F,
  n_opponents = 100,
  tie_method = "coinflip")
