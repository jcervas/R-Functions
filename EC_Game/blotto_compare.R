blotto_compare <- function(matrix_data, weights, sample = FALSE, n_sample = 1000,
                           tie_method = "coinflip", batch_size = 100, save_fn = NULL) {
  
  n_players <- nrow(matrix_data)
  total_weight <- sum(weights)
  threshold <- total_weight / 2
  win_counts <- numeric(n_players)
  
  batches <- ceiling(n_players / batch_size)
  start_time <- Sys.time()
  
  for (b in 1:batches) {
    idx_start <- (b - 1) * batch_size + 1
    idx_end <- min(b * batch_size, n_players)
    batch_idx <- idx_start:idx_end
    
    for (i in batch_idx) {
      p1 <- matrix_data[i, ]
      
      if (sample) {
        opponents_idx <- sample(setdiff(1:n_players, i), n_sample)
      } else {
        opponents_idx <- setdiff(1:n_players, i)  # Exclude self
      }
      
      n_opps <- length(opponents_idx)
      opponents <- matrix_data[opponents_idx, ]
      
      # Vectorized comparison
      win_matrix <- t(t(opponents) < p1)
      tie_matrix <- t(t(opponents) == p1)
      
      win_points <- win_matrix %*% weights
      
      tie_points <- switch(tie_method,
                           "coinflip" = {
                             tie_flips <- matrix(rbinom(n_opps * ncol(matrix_data), 1, 0.5),
                                                 nrow = n_opps)
                             (tie_matrix * tie_flips) %*% weights
                           },
                           "p2wins" = matrix(0, nrow = n_opps, ncol = 1),
                           stop("Invalid tie_method")
      )
      
      total_points <- win_points + tie_points
      win_counts[i] <- sum(total_points > threshold)
    }
    
    # Progress
    if (b %% 1 == 0) {
      pct <- round(100 * idx_end / n_players, 1)
      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      cat(sprintf("Batch %d/%d — %d of %d players (%.1f%%) done | Time: %ss\n",
                  b, batches, idx_end, n_players, pct, elapsed))
    }
    
    # Optional intermediate save
    if (!is.null(save_fn)) {
      save_fn(win_counts, idx_end)
    }
  }
  
  return(win_counts)
}

# Helper: Save progress as we go
save_progress <- function(result_vector, current_index) {
  write.csv(result_vector, file = sprintf("/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/win_counts_checkpoint_%04d.csv", current_index))
}


# # Example
blotto_compare(
  a, 
  ec_weights, 
  sample = FALSE,
  n_sample = 1000,
  tie_method = "coinflip", 
  batch_size = 1000,
  save_fn = save_progress)

