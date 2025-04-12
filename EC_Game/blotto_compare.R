blotto_compare <- function(matrix_data, weights, sample = FALSE, n_opponents = 1000, tie_method = "coinflip") {
  n_players <- nrow(matrix_data)             # Total number of players (rows in the matrix)
  total_weight <- sum(weights)               # Sum of all weights, used to calculate majority threshold
  threshold <- total_weight / 2              # Threshold for a win (majority)
  win_counts <- numeric(n_players)           # Initialize win count for each player
  
  # Loop over each player
  for (i in 1:n_players) {
    p1 <- matrix_data[i, ]                   # Extract the ith player's data (vector)
    
    if (sample == TRUE) {
      # Randomly sample opponents (excluding the player themselves)
      opponents_idx <- sample(setdiff(1:n_players, i), n_opponents)
    } else {
      # Use all other players as opponents
      opponents_idx <- 1:nrow(matrix_data)
      n_opponents <- nrow(matrix_data)
    }
    
    opponents <- matrix_data[opponents_idx, ]  # Extract opponent rows
    
    # Replicate player i's data into a matrix with n_opponents rows for comparison
    p1_mat <- matrix(rep(p1, each = n_opponents), nrow = n_opponents)
    
    # Create boolean matrices for win and tie comparisons (element-wise)
    win_matrix <- (opponents < p1_mat)         # TRUE where p1 beats opponent
    tie_matrix <- (opponents == p1_mat)        # TRUE where p1 ties opponent
    
    # Multiply each TRUE in win_matrix by corresponding weight to get weighted wins
    win_points <- win_matrix %*% weights       # Each row gives total weight of positions where p1 wins
    
    # Handle ties based on specified method
    if (tie_method == "coinflip") {
      # Simulate coin flips (0 or 1) for each element in tie_matrix
      tie_flips <- matrix(rbinom(length(tie_matrix), 1, 0.5), nrow = n_opponents)
      tie_points <- (tie_matrix * tie_flips) %*% weights  # Only count tie-points where coin flip was "1"
    } else if (tie_method == "p2wins") {
      # Give no points to player 1 in case of a tie
      tie_points <- matrix(0, nrow = n_opponents, ncol = 1)
    } else {
      stop("Invalid tie_method")              # Error for unrecognized method
    }
    
    # Sum win points and tie points to get total score against each opponent
    total_points <- win_points + tie_points
    
    # Count number of wins where total points exceed the threshold (majority)
    wins <- total_points > threshold
    win_counts[i] <- sum(wins)
  }
  
  return(win_counts)  # Return vector of win counts for each player
}


## Depreciated (slow)
# ec_game <- function(p1, p2, weights, tie_method = "coinflip") {
#   # Determine outcomes
#   p1_wins <- p1 > p2
#   tie_indices <- p1 == p2
#   n_states <- length(weights)
#   quota <- sum(weights)/2
  
#   # Initialize points vector
#   p1_points_vec <- numeric(n_states)
  
#   # Assign wins
#   p1_points_vec[p1_wins] <- weights[p1_wins]

#    # Handle tie results
#      if (any(tie_indices == TRUE)) {
#         if (tie_method == "coinflip") {
#         # Method 1: Random coin flip (50% chance for p1)
#         p1_points_vec[tie_indices] <- rbinom(sum(tie_indices * 1), 1, 0.5) * weights[tie_indices]
#       } else if (tie_method == "p2wins") {
#         # Method 2: p2 always wins ties
#         p1_points_vec[tie_indices] <- rep(0, length(tie_indices[tie_indices == TRUE]))  # p1 gets 0 points
#       } else {
#         stop("Invalid tie_method. Use 'coinflip' or 'p2wins'")
#       }
#     }

#   # Scores
#   p1_score <- sum(p1_points_vec)
#   total_points <- sum(weights)
#   p1_percent <- p1_score / total_points
  
#   # Result: 1 if p1 wins, 0 if p2 wins, 0.5 if tie
#   result <- 0L
# if (p1_score > quota) result <- 1 

#   # Return all stats
#   return(list(
#     p1_result = result,
#     n_states = n_states,
#     p1_points = p1_score,
#     p1_percent = p1_percent
#   ))
# }