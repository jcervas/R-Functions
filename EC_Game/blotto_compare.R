blotto_compare <- function(matrix_data, weights, sample = FALSE, n_opponents = 1000, tie_method = "coinflip") {
  n_players <- nrow(matrix_data)
  total_weight <- sum(weights)
  threshold <- total_weight / 2
  win_counts <- numeric(n_players)
  opp_counts <- numeric(n_players)  # To track actual number of opponents per player

  cat("Starting Blotto comparison...\n")

  for (i in 1:n_players) {
    p1 <- matrix_data[i, ]

    if (sample) {
      available_opps <- setdiff(1:n_players, i)
      if (length(available_opps) < n_opponents) {
        stop("Not enough players to sample the requested number of opponents. Reduce `n_opponents`.")
      }
      opponents_idx <- sample(available_opps, n_opponents)
    } else {
      opponents_idx <- setdiff(1:n_players, i)
    }

    opponents <- matrix_data[opponents_idx, , drop = FALSE]
    n_opps <- length(opponents_idx)
    opp_counts[i] <- n_opps  # Store for summary

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
      cat(sprintf("→ Processed %d of %d players...\n", i, n_players))
    }
  }

  # Build results
  combinations_df <- as.data.frame(matrix_data)
  combinations_df$Win_Count <- win_counts
  combinations_df$Opponents_Faced <- opp_counts
  combinations_df$Win_Percentage <- (win_counts / opp_counts) * 100
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

  return(invisible(ranked_combinations))
}
