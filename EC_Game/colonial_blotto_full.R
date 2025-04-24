
# -- colonial_blotto_full.R

# ========== Utility Functions ==========

all_combinations <- function(weights) {
  n <- length(weights)
  unlist(lapply(1:n, function(k) combn(n, k, simplify = FALSE)), recursive = FALSE)
}

is_minimum <- function(x, quota) {
  if (sum(x) < quota) return(FALSE)
  for (k in seq_along(x)) {
    if (sum(x[-k]) >= quota) return(FALSE)
  }
  return(TRUE)
}

# ========== Drop Irrational Allocations ==========

drop_irrational_allocations <- function(matrix_data, weights, quota = 70) {
  valid_rows <- apply(matrix_data, 1, function(row) sum(weights[row > 0]) >= quota)
  matrix_data[valid_rows, , drop = FALSE]
}

deduplicate_combinations <- function(mat) {
  unique_mat <- unique(mat)
  rownames(unique_mat) <- NULL
  return(unique_mat)
}

# ========== Allocation Utilities ==========

adjust_allocation <- function(raw_values, target_sum, bias_weights = NULL) {
  if (is.null(bias_weights)) bias_weights <- rep(1, length(raw_values))
  bias_weights <- bias_weights / sum(bias_weights)

  if (sum(raw_values) == 0) {
    alloc <- rep(0, length(raw_values))
    alloc[sample(length(raw_values), 1)] <- target_sum
    return(alloc)
  }

  alloc <- round(raw_values / sum(raw_values) * target_sum)
  diff <- target_sum - sum(alloc)
  tries <- 0

  while (diff != 0 && tries < 1000) {
    idx <- sample(seq_along(alloc), 1, prob = bias_weights)
    if (diff > 0) {
      alloc[idx] <- alloc[idx] + 1
      diff <- diff - 1
    } else if (alloc[idx] > 0) {
      alloc[idx] <- alloc[idx] - 1
      diff <- diff + 1
    }
    tries <- tries + 1
  }

  pmax(alloc, 0)
}

generate_random_allocations <- function(
  weights, target_sum = 100,
  method = c("uniform", "uniform_skewed", "normal", "poisson", "gamma", "beta", "exponential", "geometric", "custom"),
  sd_factor = 0.3, skew_power = 1, custom_generator = NULL, return_raw = FALSE
) {
  method <- match.arg(method)
  norm_weights <- weights / sum(weights)
  
  raw_values <- switch(method,
    "uniform" = runif(length(weights), 0, 1),
    "uniform_skewed" = runif(length(weights), 0, 1),
    "normal" = {
      means <- norm_weights * target_sum
      sds <- sd_factor * means
      pmax(rnorm(length(weights), means, sds), 0)
    },
    "poisson" = rpois(length(weights), lambda = norm_weights * target_sum),
    "gamma" = rgamma(length(weights), shape = norm_weights * 5, scale = target_sum / 5),
    "beta" = {
      alpha <- norm_weights * 2
      beta <- (1 - norm_weights) * 2
      alpha[alpha <= 0] <- 0.1
      beta[beta <= 0] <- 0.1
      rbeta(length(weights), alpha, beta)
    },
    "exponential" = rexp(length(weights), rate = 1 / (norm_weights * target_sum)),
    "geometric" = rgeom(length(weights), prob = 0.2),
    "custom" = {
      if (is.null(custom_generator)) stop("Custom generator must be provided.")
      custom_generator(length(weights), norm_weights, target_sum)
    },
    stop("Invalid method.")
  )

  alloc <- adjust_allocation(raw_values, target_sum, norm_weights)
  if (return_raw) return(list(alloc = alloc, raw = raw_values))
  alloc
}

# ========== Smart Combination Generator ==========

new_combinations <- function(
  weights, quota = 70, target_sum = 100, max_small_digits = 6, max_zeros = 5,
  method = c("uniform", "uniform_skewed", "normal", "poisson", "gamma", "beta", "exponential", "geometric", "custom"),
  small_mean = 0, small_sd = 2, min_small = 0, max_small = 100, custom_generator = NULL
) {
  method <- match.arg(method)
  vec_length <- length(weights)

  if (quota > 0 && sum(weights) <= 1.1) {
    quota <- 0  # Skip rationality check for normalized weights
  }

  repeat {
    combo <- numeric(vec_length)
    num_small_digits <- sample(0:max_small_digits, 1)
    num_zeros <- sample(0:min(num_small_digits, max_zeros), 1)

    zero_positions <- if (num_zeros > 0) sample(vec_length, num_zeros) else integer(0)
    remaining_positions <- setdiff(seq_len(vec_length), zero_positions)

    num_small <- num_small_digits - num_zeros
    small_positions <- if (num_small > 0) sample(remaining_positions, num_small) else integer(0)
    remaining_positions <- setdiff(remaining_positions, small_positions)

    # Assign small values
    if (num_small > 0) {
      repeat {
        small_values <- abs(round(rnorm(num_small, small_mean, small_sd)))
        small_values <- pmin(pmax(small_values, min_small), max_small)
        if (sum(small_values) > 0) break
      }
      combo[small_positions] <- small_values
    }

    remaining_sum <- target_sum - sum(combo)
    if (remaining_sum < 0 || length(remaining_positions) == 0) next

    # Generate fill for remaining positions
    raw <- generate_random_allocations(weights[remaining_positions], remaining_sum, method, custom_generator = custom_generator)
    if (sum(raw) == 0) next  # Retry if result is empty/invalid

    combo[remaining_positions] <- raw

    # Postprocess to prevent 0s in filled positions
    if (any(combo[remaining_positions] == 0)) {
      zero_idx <- which(combo[remaining_positions] == 0)
      nonzero_idx <- which(combo[remaining_positions] > 0)
      if (length(nonzero_idx) > 0) {
        for (z in zero_idx) {
          swap <- sample(nonzero_idx, 1)
          combo[remaining_positions[z]] <- 1
          combo[remaining_positions[swap]] <- max(combo[remaining_positions[swap]] - 1, 0)
        }
      }
    }

    if (quota == 0 || sum(weights[combo > 0]) >= quota) return(combo)
  }
}

# ========== Progress Reporter ==========

progress_reporter <- function(i, total, start_time, last_update, min_interval = 60) {
  now <- Sys.time()
  elapsed_secs <- as.numeric(difftime(now, last_update, units = "secs"))
  total_elapsed <- as.numeric(difftime(now, start_time, units = "mins"))
  if (elapsed_secs > min_interval || i == total) {
    avg_time_per_iter <- as.numeric(difftime(now, start_time, units = "secs")) / i
    remaining_mins <- avg_time_per_iter * (total - i) / 60
    cat(sprintf("→ Compared %d of %d strategies...\nElapsed time: %.1f minutes\nEstimated time remaining: %.1f minutes\n\n",
                i, total, total_elapsed, remaining_mins))
    return(now)
  }
  return(last_update)
}

# ========== Blotto Comparison ==========

blotto_compare <- function(matrix_data, weights, sample = FALSE, n_opponents = 1000, tie_method = "coinflip", single_strategy = NULL) {
  total_weight <- sum(weights)
  threshold <- total_weight / 2

  if (!is.null(single_strategy)) {
    opponents <- matrix_data
    n_opps <- nrow(opponents)
    p1_mat <- matrix(rep(single_strategy, each = n_opps), nrow = n_opps)

    win_matrix <- opponents < p1_mat
    tie_matrix <- opponents == p1_mat
    win_points <- win_matrix %*% weights
    tie_points <- switch(tie_method,
                         "coinflip" = (tie_matrix * matrix(rbinom(length(tie_matrix), 1, 0.5), nrow = n_opps)) %*% weights,
                         "p2wins" = matrix(0, nrow = n_opps, ncol = 1),
                         stop("Invalid tie_method"))
    total_points <- win_points + tie_points
    wins <- sum(total_points > threshold)
    return(list(
      Win_Count = wins,
      Opponents_Faced = n_opps,
      Win_Percentage = round((wins / n_opps) * 100, 2)
    ))
  }

  n_players <- nrow(matrix_data)
  win_counts <- numeric(n_players)
  opp_counts <- numeric(n_players)
  start_time <- Sys.time()
  last_update <- start_time
  cat(sprintf("Starting comparison of %d strategies...\n", n_players))
  cat("Estimated total time: calculating...\n")

  for (i in 1:n_players) {
    p1 <- matrix_data[i, ]
    opponents_idx <- setdiff(1:n_players, i)
    if (sample) {
      if (length(opponents_idx) < n_opponents) {
        stop("Not enough players to sample the requested number of opponents.")
      }
      opponents_idx <- sample(opponents_idx, n_opponents)
    }
    opponents <- matrix_data[opponents_idx, , drop = FALSE]
    n_opps <- length(opponents_idx)
    opp_counts[i] <- n_opps
    p1_matrix <- matrix(p1, nrow = n_opps, ncol = length(p1), byrow = TRUE)
    win_matrix <- opponents < p1_matrix
    tie_matrix <- opponents == p1_matrix
    win_points <- win_matrix %*% weights
    tie_points <- switch(tie_method,
                         "coinflip" = (tie_matrix * matrix(rbinom(length(tie_matrix), 1, 0.5), nrow = n_opps)) %*% weights,
                         "p2wins" = matrix(0, nrow = n_opps, ncol = 1),
                         stop("Invalid tie_method"))
    total_points <- win_points + tie_points
    win_counts[i] <- sum(total_points > threshold)
    last_update <- progress_reporter(i, n_players, start_time, last_update)
  }

  combinations_df <- as.data.frame(matrix_data)
  combinations_df$Win_Count <- win_counts
  combinations_df$Opponents_Faced <- opp_counts
  combinations_df$Win_Percentage <- round((win_counts / opp_counts) * 100, 2)
  combinations_df$Rank <- rank(-combinations_df$Win_Count, ties.method = "first")
  combinations_df[order(combinations_df$Rank), ]
}


# ========== MWC Sampling ==========

sample_distributions_fast <- function(
  mwcs, weights, target_sum = 100, samples_per_mwc = 100,
  quota = 70
) {
  n_players <- length(weights)
  result_list <- list()

  for (i in seq_along(mwcs)) {
    mwc <- mwcs[[i]]
    samples <- matrix(0, nrow = samples_per_mwc, ncol = n_players)
    for (j in 1:samples_per_mwc) {
      alloc <- runif(length(mwc), 1, 100)
      alloc <- adjust_allocation(alloc, target_sum, bias_weights = rep(1, length(mwc)))
      samples[j, mwc] <- alloc
    }
    result_list[[i]] <- samples
    cat(sprintf("✓ Sampled MWC %d of %d
", i, length(mwcs)))
  }

  names(result_list) <- paste0("MWC_", seq_along(result_list))
  result_list
}

# ========== Strategy Evaluation ==========

evaluate_strategies <- function(matrix_data, weights, quota = 70) {
  n <- nrow(matrix_data)
  m <- ncol(matrix_data)
  zero_counts <- colSums(matrix_data == 0)
  zero_percentage <- (zero_counts / n) * 100
  means <- colMeans(matrix_data)
  total_allocations <- rowSums(matrix_data)
  ec_covered <- apply(matrix_data > 0, 1, function(x) sum(weights[x]))
  rational <- ec_covered > quota
  percent_rational <- mean(rational) * 100
  summary_df <- data.frame(
    position = seq_len(m),
    zero_count = zero_counts,
    zero_percent = round(zero_percentage, 1),
    mean_allocation = round(means, 2)
  )
  cat("Strategy Evaluation Summary:
")
  cat("→ Total strategies:", n, "
")
  cat("→ Rational strategies (can reach quota):", sum(rational), "/", n,
      sprintf("(%.1f%%)
", percent_rational))
  cat("→ Mean EC coverage per strategy:", round(mean(ec_covered), 1), "
")
  return(list(
    position_summary = summary_df,
    rationality_rate = percent_rational,
    ec_coverage = ec_covered
  ))
}

# ========== Stacked Histogram Plot ==========

stacked_hist_plot <- function(
  random_vectors,
  overlay_vectors = NULL,
  state_names = rev(c("A", "B", "C", "D", "E", "F", "G")),
  main = "Distribution",
  show_density = FALSE,
  show_legend = FALSE, legend_labels = c('EC Weights', 'Banzhaf')
) {
  df <- as.data.frame(random_vectors)
  colnames(df) <- state_names
  num_states <- length(state_names)

  if (!is.null(overlay_vectors)) {
    df_overlay <- as.data.frame(overlay_vectors)
    colnames(df_overlay) <- state_names
  }

  par(mar = c(5, 6, 4, 2))
  plot(NULL, xlim = c(0, 100), ylim = c(0, num_states * 1.2 + 1.5),
       xlab = "Allocation", ylab = "", yaxt = "n", main = main)

  axis(2, at = seq(0.6, num_states * 1.2 - 0.6, by = 1.2),
       labels = state_names, las = 1)

  base_colors <- gray.colors(num_states, start = 0.2, end = 0.7)
  overlay_color <- gray(0.1, alpha = 0.4)

  for (i in 1:num_states) {
    position <- (num_states - i) * 1.2
    values <- df[[i]]
    h <- hist(values, plot = FALSE, breaks = seq(0, 100, by = 1))
    scaled <- if (max(h$counts) > 0) h$counts / max(h$counts) * 0.8 else rep(0, length(h$counts))

    rect(h$breaks[-length(h$breaks)], position,
         h$breaks[-1], position + scaled,
         col = adjustcolor(base_colors[i], alpha.f = 0.5), border = NA)

    if (!is.null(overlay_vectors)) {
      overlay_vals <- df_overlay[[i]]
      h_overlay <- hist(overlay_vals, plot = FALSE, breaks = seq(0, 100, by = 1))
      scaled_overlay <- if (max(h_overlay$counts) > 0) h_overlay$counts / max(h_overlay$counts) * 0.8 else rep(0, length(h_overlay$counts))

      rect(h_overlay$breaks[-length(h_overlay$breaks)], position,
           h_overlay$breaks[-1], position + scaled_overlay,
           col = overlay_color, border = NA)

      if (show_density && length(unique(overlay_vals)) > 1) {
        dens_overlay <- density(overlay_vals, from = 0, to = 100, bw = 0.5)
        scaled_dens_overlay <- dens_overlay$y / max(dens_overlay$y) * 0.8
        lines(dens_overlay$x, scaled_dens_overlay + position, col = "black", lwd = 1, lty = 2)
      }
    }

    lines(c(0, 100), c(position, position), col = "gray90")

    if (show_density && length(unique(values)) > 1) {
      dens <- density(values, from = 0, to = 100, bw = 0.5)
      scaled_dens <- dens$y / max(dens$y) * 0.8
      lines(dens$x, scaled_dens + position, col = "black", lwd = 1)
    }
  }

  if (!is.null(overlay_vectors) && show_legend) {
    legend("topright",
           legend = legend_labels,
           fill = c(adjustcolor("gray50", alpha.f = 0.5), overlay_color),
           border = c("gray50", "black"),
           lty = c(1, 2), lwd = 1,
           bty = "o")
  }
}



# ========== Banzhaf Power Index ==========

banzhaf <- function(member, weights, quota) {
  n <- length(weights)
  swing_counts <- numeric(n)

  for (k in 1:n) {
    coalitions <- combn(n, k, simplify = FALSE)
    for (coal in coalitions) {
      total <- sum(weights[coal])
      if (total >= quota) {
        for (i in coal) {
          reduced_total <- total - weights[i]
          if (reduced_total < quota) {
            swing_counts[i] <- swing_counts[i] + 1
          }
        }
      }
    }
  }

  total_swings <- sum(swing_counts)
  if (total_swings == 0) {
    warning("No swings found. Quota may be too high or too low.")
    normalized <- rep(0, n)
  } else {
    normalized <- swing_counts / total_swings
  }

  names(normalized) <- member
  return(normalized)
}



# ========== Find Minimum Winning Coalitions ==========

find_mwcs <- function(weights, quota) {
  all_sets <- all_combinations(weights)
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



# ========== Distribution Validation Plot ==========

validate_sampling_methods <- function(weights, methods, target_sum = 100) {
  par(mfrow = c(3, 3))
  for (m in methods) {
    sims <- t(replicate(1000, generate_random_allocations(weights, target_sum, method = m)))
    matplot(t(sims), type = "l", col = adjustcolor("gray50", alpha.f = 0.1), lty = 1,
            main = paste("Method:", m), xlab = "Position", ylab = "Allocation")
  }
  par(mfrow = c(1, 1))
}



# ========== MWC Sampling ==========

sample_mwc_distributions <- function(
  mwcs, weights, target_sum = 100, total_samples_goal = 10000,
  samples_per_mwc = NULL, verbose = TRUE
) {
  n_mwcs <- length(mwcs)
  if (is.null(samples_per_mwc)) {
    samples_per_mwc <- ceiling(total_samples_goal / n_mwcs)
  }

  result_list <- list()
  for (i in seq_along(mwcs)) {
    mwc <- mwcs[[i]]
    samples <- matrix(0, nrow = samples_per_mwc, ncol = length(weights))
    for (j in 1:samples_per_mwc) {
      alloc <- runif(length(mwc), 1, 100)
      alloc <- adjust_allocation(alloc, target_sum, bias_weights = rep(1, length(mwc)))
      samples[j, mwc] <- alloc
    }
    result_list[[i]] <- samples
    if (verbose) cat(sprintf("✓ Sampled MWC %d of %d\n", i, n_mwcs))
  }

  names(result_list) <- paste0("MWC_", seq_along(mwcs))
  return(result_list)
}



# ========== Verify MWC Support ==========

verify_mwc_support <- function(strategies, mwcs) {
  apply(strategies, 1, function(row) {
    any(sapply(mwcs, function(coal) all(row[coal] > 0)))
  })
}
