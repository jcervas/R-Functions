blotto_compare <- function(strategy_matrix = NULL, weights, sample = FALSE, n_opponents = 1000,
                           tie_method = "coinflip", single_strategy = NULL,
                           strategy_set_A = NULL, strategy_set_B = NULL) {
  total_weight <- sum(weights)
  n_districts <- length(weights)

  compute_points <- function(p1, p2) {
    win_matrix <- p2 < p1
    tie_matrix <- p2 == p1
    lose_matrix <- p2 > p1

    if (tie_method %in% c("coinflip", "cointoss")) {
      tie_split <- rbinom(n_districts, 1, 0.5)
      p1_points <- sum((win_matrix + tie_matrix * tie_split) * weights)
      p2_points <- sum((lose_matrix + tie_matrix * (1 - tie_split)) * weights)
    } else if (tie_method == "p2wins") {
      p1_points <- sum(win_matrix * weights)
      p2_points <- sum((lose_matrix + tie_matrix) * weights)
    } else if (tie_method == "tie") {
      p1_points <- sum(win_matrix * weights)
      p2_points <- sum(lose_matrix * weights)
    } else if (tie_method == "winhalf") {
      p1_points <- sum((win_matrix + 0.5 * tie_matrix) * weights)
      p2_points <- sum((lose_matrix + 0.5 * tie_matrix) * weights)
    } else {
      stop("Invalid tie_method.")
    }

    margin <- abs(p1_points - p2_points)

    if (p1_points > p2_points) return(list(winner = 1, margin = margin))
    else if (p2_points > p1_points) return(list(winner = 0, margin = margin))
    else return(list(winner = 0.5, margin = 0))
  }

  if (!is.null(single_strategy)) {
    if (is.null(strategy_matrix)) stop("strategy_matrix must be provided for single_strategy comparison.")
    opponents <- strategy_matrix
    n_opps <- nrow(opponents)
    wins <- 0
    win_margins <- c()
    loss_margins <- c()

    for (i in 1:n_opps) {
      result <- compute_points(single_strategy, opponents[i, ])
      wins <- wins + result$winner
      if (result$winner == 1) {
        win_margins <- c(win_margins, result$margin)
      } else if (result$winner == 0) {
        loss_margins <- c(loss_margins, result$margin)
      }
    }

    return(list(
      Win_Count = wins,
      Opponents_Faced = n_opps,
      Win_Percentage = round(100 * wins / n_opps, 2),
      Avg_Win_Margin = if (length(win_margins) > 0) round(mean(win_margins), 2) else NA,
      Avg_Loss_Margin = if (length(loss_margins) > 0) round(mean(loss_margins), 2) else NA
    ))
  }

  if (!is.null(strategy_set_A) && !is.null(strategy_set_B)) {
    if (is.vector(strategy_set_A)) strategy_set_A <- matrix(strategy_set_A, nrow = 1)
    if (is.vector(strategy_set_B)) strategy_set_B <- matrix(strategy_set_B, nrow = 1)

    n_A <- nrow(strategy_set_A)
    n_B <- nrow(strategy_set_B)
    win_counts <- numeric(n_A)
    win_margins_list <- vector("list", n_A)
    loss_margins_list <- vector("list", n_A)

    for (i in 1:n_A) {
      for (j in 1:n_B) {
        result <- compute_points(strategy_set_A[i, ], strategy_set_B[j, ])
        win_counts[i] <- win_counts[i] + result$winner
        if (result$winner == 1) {
          win_margins_list[[i]] <- c(win_margins_list[[i]], result$margin)
        } else if (result$winner == 0) {
          loss_margins_list[[i]] <- c(loss_margins_list[[i]], result$margin)
        }
      }
    }

    return(data.frame(
      Strategy_ID = 1:n_A,
      Win_Count = win_counts,
      Opponents_Faced = n_B,
      Win_Percentage = round(100 * win_counts / n_B, 2),
      Avg_Win_Margin = sapply(win_margins_list, function(x) if (length(x)) round(mean(x), 2) else NA),
      Avg_Loss_Margin = sapply(loss_margins_list, function(x) if (length(x)) round(mean(x), 2) else NA)
    ))
  }

  if (is.null(strategy_matrix)) stop("strategy_matrix must be provided.")

  n_players <- nrow(strategy_matrix)
  win_counts <- numeric(n_players)
  win_margins <- vector("list", n_players)
  loss_margins <- vector("list", n_players)

  cat(sprintf("Starting round-robin comparison of %d strategies...\n", n_players))

  for (i in 1:(n_players - 1)) {
    for (j in (i + 1):n_players) {
      result <- compute_points(strategy_matrix[i, ], strategy_matrix[j, ])
      win_counts[i] <- win_counts[i] + result$winner
      win_counts[j] <- win_counts[j] + (1 - result$winner)

      if (result$winner == 1) {
        win_margins[[i]] <- c(win_margins[[i]], result$margin)
        loss_margins[[j]] <- c(loss_margins[[j]], result$margin)
      } else if (result$winner == 0) {
        win_margins[[j]] <- c(win_margins[[j]], result$margin)
        loss_margins[[i]] <- c(loss_margins[[i]], result$margin)
      }
    }
  }

  combinations_df <- as.data.frame(strategy_matrix)
  combinations_df$Win_Count <- win_counts
  combinations_df$Opponents_Faced <- n_players - 1
  combinations_df$Win_Percentage <- round(100 * win_counts / (n_players - 1), 2)
  combinations_df$Avg_Win_Margin <- sapply(win_margins, function(x) if (length(x)) round(mean(x), 2) else NA)
  combinations_df$Avg_Loss_Margin <- sapply(loss_margins, function(x) if (length(x)) round(mean(x), 2) else NA)
  combinations_df$Rank <- rank(-combinations_df$Win_Count, ties.method = "first")

  combinations_df <- combinations_df[order(as.numeric(rownames(combinations_df))), ]
  return(combinations_df)
}