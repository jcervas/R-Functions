# -- colonial_blotto_full.R

# ========== Utility Functions ==========

# Function to compute Euclidean distance
euclidean <- function(x, y) sqrt(sum((x - y)^2))

all_combinations <- function(vector_weights) {
  n <- length(vector_weights)
  unlist(lapply(1:n, function(k) combn(n, k, simplify = FALSE)), recursive = FALSE)
}

all_winning_combinations <- function(members, vector_weights, threshold = 70) { 
# Collect results
results <- list()
index <- 1

for (k in 1:length(members)) {
  combos <- combn(members, k, simplify = FALSE)
  for (combo in combos) {
    weight_sum <- sum(vector_weights[combo])
    if (weight_sum > threshold) {
      weight_labels <- combo
      results[[index]] <- list(
        Members = paste(weight_labels, collapse = ", "),
        Total_Weight = weight_sum
      )
      index <- index + 1
    }
  }
}

# Convert to data frame
results_df <- do.call(rbind, lapply(results, function(x) {
  data.frame(
    Members = x$Members,
    Coalition_Size = length(unlist(strsplit(x$Members, ","))),
    Total_Weight = x$Total_Weight,
    stringsAsFactors = FALSE
  )
})) 

  return(results_df) # View the results
}

is_minimum <- function(x, quota) {
  if (sum(x) < quota) {
    return(FALSE)
  }
  for (k in seq_along(x)) {
    if (sum(x[-k]) >= quota) {
      return(FALSE)
    }
  }
  return(TRUE)
}

find_mwcs <- function(vector_weights, quota) {
  all_sets <- all_combinations(vector_weights)
  mwcs <- list()
  idx <- 1
  for (coal in all_sets) {
    weight_sum <- sum(vector_weights[coal])
    if (weight_sum >= quota) {
      is_minimal <- TRUE
      for (j in coal) {
        if (sum(vector_weights[setdiff(coal, j)]) >= quota) {
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

generate_expanded_mwcs <- function(mwcs, vector_weights, quota) {
  n <- length(vector_weights)
  expanded <- list()
  origins <- list()
  idx <- 1

  for (mwc in mwcs) {
    coal_weights <- vector_weights[mwc]
    remaining <- setdiff(1:n, mwc)
    for (extra in remaining) {
      extra_weight <- vector_weights[extra]
      if (extra_weight < min(coal_weights)) next
      replaceable_combinations <- combn(coal_weights, 2, simplify = TRUE)
      if (any(colSums(replaceable_combinations) <= extra_weight)) next
      new_coal <- sort(c(mwc, extra))
      if (sum(vector_weights[new_coal]) >= quota) {
        expanded[[idx]] <- new_coal
        origins[[idx]] <- mwc
        idx <- idx + 1
      }
    }
  }

  list(
    expanded = expanded,
    origins = origins
  )
}

label_coalition <- function(coalition, state_names) {
  paste(state_names[coalition], collapse = ",")
}

label_expanded_coalition <- function(expanded, mwc, state_names) {
  added <- setdiff(expanded, mwc)
  left <- paste(state_names[mwc], collapse = ",")
  right <- paste(state_names[added], collapse = ",")
  paste0(left, " + ", right)
}

# Test Function
# mwcs <- find_mwcs(blotto_weights, 70)
# res <- generate_expanded_mwcs(mwcs, blotto_weights, 70)
# length(res$expanded)  # should be > 0

# # Optional: label
# label_expanded_coalition <- function(expanded, mwc, state_names) {
#   added <- setdiff(expanded, mwc)
#   left <- paste(state_names[mwc], collapse = ",")
#   right <- paste(state_names[added], collapse = ",")
#   paste0(left, " + ", right)
# }

# expanded_labels <- mapply(
#   label_expanded_coalition,
#   res$expanded,
#   res$origins,
#   MoreArgs = list(state_names = state_names)
# )


drop_irrational_allocations <- function(strategy_matrix, vector_weights, quota = 70) {
  valid_rows <- apply(strategy_matrix, 1, function(row) sum(vector_weights[row > 0]) >= quota)
  strategy_matrix[valid_rows, , drop = FALSE]
}

deduplicate_combinations <- function(mat) {
  unique_mat <- unique(mat)
  rownames(unique_mat) <- NULL
  return(unique_mat)
}

normalize <- function(vector) {
  norm_vector <- vector / sum(vector)
  return(norm_vector)
}

assign_to_mwc <- function(strategies, mwcs) {
  apply(strategies, 1, function(strategy) {
    which_mwcs <- sapply(mwcs, function(mwc) all(strategy[mwc] > 0))
    if (any(which_mwcs)) {
      which.max(which_mwcs)
    } else {
      NA
    }
  })
}

assign_strict_mwc <- function(strategies, mwcs) {
  apply(strategies, 1, function(strategy) {
    matches <- sapply(mwcs, function(mwc) {
      all(strategy[-mwc] == 0) && all(strategy[mwc] > 0)
    })
    if (any(matches)) which.max(matches) else NA
  })
}

# ========== Allocation Functions ==========

#' Adjust a raw numeric vector to sum to a target using proportional rounding.
#'
#' Scales and rounds a numeric vector so its sum matches a specified target.
#' Optionally accepts bias weights to influence which elements get nudged.
#'
#' @param raw_values A numeric vector of raw values.
#' @param target_sum The desired total sum of the adjusted vector.
#' @param bias_weights Optional vector of weights used to bias rounding corrections.
#' @return An integer vector summing to `target_sum`.
#' @export
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

apportion_values <- function(values, target_sum, method = c("Hill-Huntington", "Jefferson", "Adams", "Webster"), initial_seats = 0) {
  method <- match.arg(method)
  n <- length(values)

  if (target_sum < n * initial_seats) {
    stop("Target sum must be at least the total number of initial seats.")
  }

  # Assign initial seats
  allocations <- rep(initial_seats, n)
  remaining <- target_sum - sum(allocations)

  # Define priority function based on method
  priority_function <- switch(method,
    "Jefferson" = function(pop, seats) pop / (seats + 1),
    "Adams" = function(pop, seats) pop / seats,
    "Webster" = function(pop, seats) pop / (seats + 0.5),
    "Hill-Huntington" = function(pop, seats) pop / sqrt(seats * (seats + 1))
  )

  for (i in seq_len(remaining)) {
    priorities <- priority_function(values, allocations)
    winner <- which.max(priorities)
    allocations[winner] <- allocations[winner] + 1
  }

  return(allocations)
}


#' Generate a random allocation of resources using various distributions.
#'
#' Supports distributions like normal, beta, gamma, etc., scaled to sum to a target.
#'
#' @param vector_weights A numeric vector to bias allocations.
#' @param target_sum Total amount to allocate.
#' @param method Sampling method (e.g., "normal", "poisson").
#' @param sd_factor Standard deviation multiplier for normal.
#' @param custom_generator Optional custom sampling function.
#' @param return_raw If TRUE, return both raw and adjusted values.
#' @return An integer vector summing to `target_sum`, or list if `return_raw = TRUE`.
#' @export
generate_random_allocations <- function(
    vector_weights, target_sum = 100,
    method = c("uniform", "uniform_skewed", "normal", "poisson", "gamma", "beta", "exponential", "geometric", "custom"),
    sd_factor = 0.3, custom_generator = NULL, return_raw = FALSE) {
  method <- match.arg(method)
  norm_weights <- vector_weights / sum(vector_weights)
  raw_values <- switch(method,
    "uniform" = runif(length(vector_weights), 0, 1),
    "uniform_skewed" = runif(length(vector_weights), 0, 1),
    "normal" = {
      means <- norm_weights * target_sum
      sds <- sd_factor * means
      pmax(rnorm(length(vector_weights), means, sds), 0)
    },
    "poisson" = rpois(length(vector_weights), lambda = norm_weights * target_sum),
    "gamma" = rgamma(length(vector_weights), shape = norm_weights * 5, scale = target_sum / 5),
    "beta" = {
      alpha <- norm_weights * 2
      beta <- (1 - norm_weights) * 2
      alpha[alpha <= 0] <- 0.1
      beta[beta <= 0] <- 0.1
      rbeta(length(vector_weights), alpha, beta)
    },
    "exponential" = rexp(length(vector_weights), rate = 1 / (norm_weights * target_sum)),
    "geometric" = rgeom(length(vector_weights), prob = 0.2),
    "custom" = {
      if (is.null(custom_generator)) stop("Custom generator must be provided.")
      custom_generator(length(vector_weights), norm_weights, target_sum)
    },
    stop("Invalid method.")
  )
  alloc <- apportion_values(raw_values, target_sum)
  if (return_raw) {
    return(list(alloc = alloc, raw = raw_values))
  }
  alloc
}

# ========== Smart Combination Generator ==========

#' Generate a strategic allocation vector for the Blotto game.
#'
#' Creates a vector of integer allocations summing to `target_sum`, using controlled sparsity,
#' random small values, and a final allocation adjustment to meet the target. Ensures that
#' the total covered weight meets or exceeds a given `quota`.
#'
#' @param vector_weights A numeric vector used to bias allocation probabilities.
#' @param quota Minimum quota the resulting allocation must cover.
#' @param target_sum Total amount to distribute across all positions.
#' @param max_small_digits Max number of entries allowed to be small (non-zero) values.
#' @param max_zeros Max number of entries that can be zero.
#' @param method Distribution method for raw value generation.
#' @param small_mean Mean for normally distributed small values.
#' @param small_sd Standard deviation for small values.
#' @param min_small Minimum allowed small value.
#' @param max_small Maximum allowed small value.
#' @param custom_generator Optional function for custom sampling.
#' @param check_weights Weights used for validating quota (can differ from vector_weights).
#'
#' @return Integer vector of allocations summing to `target_sum` and satisfying quota.
#' @export
#' Generate a strategic allocation vector for the Blotto game.
#'
#' Creates a vector of integer allocations summing to `target_sum`, using controlled sparsity,
#' random small values, and a final allocation adjustment to meet the target. Ensures that
#' the total covered weight meets or exceeds a given `quota`.
#'
#' @param vector_weights A numeric vector used to bias allocation probabilities.
#' @param quota Minimum quota the resulting allocation must cover.
#' @param target_sum Total amount to distribute across all positions.
#' @param max_small_digits Max number of entries allowed to be small (non-zero) values.
#' @param max_zeros Max number of entries that can be zero.
#' @param method Distribution method for raw value generation.
#' @param small_mean Mean for normally distributed small values.
#' @param small_sd Standard deviation for small values.
#' @param min_small Minimum allowed small value.
#' @param max_small Maximum allowed small value.
#' @param custom_generator Optional function for custom sampling.
#' @param check_weights Weights used for validating quota (can differ from vector_weights).
#'
#' @return Integer vector of allocations summing to `target_sum` and satisfying quota.
#' @export
new_combinations <- function(
    vector_weights, quota = 70, target_sum = 100, max_small_digits = 6, max_zeros = 5,
    method = c("uniform", "uniform_skewed", "normal", "poisson", "gamma", "beta", "exponential", "geometric", "custom"),
    small_mean = 0, small_sd = 5, min_small = 0, max_small = 100,
    custom_generator = NULL, check_weights = vector_weights) {
  method <- match.arg(method)
  vec_length <- length(vector_weights)
  max_attempts <- 1000
  attempts <- 0

  repeat {
    attempts <- attempts + 1
    if (attempts > max_attempts) stop("Failed to generate a valid combination after 1000 attempts")

    combo <- numeric(vec_length)
    num_zeros <- sample(0:max_zeros, 1)
    num_small_digits <- sample(num_zeros:max_small_digits, 1)

    zero_positions <- if (num_zeros > 0) sample(vec_length, num_zeros) else integer(0)
    if (sum(check_weights[zero_positions]) > (sum(check_weights) - quota)) next
    remaining_positions <- setdiff(seq_len(vec_length), zero_positions)

    num_small <- num_small_digits - num_zeros
    small_positions <- if (num_small > 0 && length(remaining_positions) >= num_small) sample(remaining_positions, num_small) else integer(0)
    remaining_positions <- setdiff(remaining_positions, small_positions)

    if (num_small > 0) {
      values <- 1:100
      small_values <- sample(values, size = num_small, replace = TRUE, prob = 1 / (values + 1))
      small_values <- pmin(pmax(small_values, min_small), max_small)
      combo[small_positions] <- small_values
    }

    remaining_sum <- target_sum - sum(combo)
    if (remaining_sum < 0 || length(remaining_positions) == 0) next

    raw <- generate_random_allocations(vector_weights[remaining_positions], remaining_sum, method, custom_generator = custom_generator)
    if (sum(raw) == 0) next
    combo[remaining_positions] <- raw

    if (quota == 0 || sum(check_weights[combo > 0]) >= quota) {
      return(combo)
    }
  }
}

# ========== Banzhaf Power Index ==========

#' Compute Banzhaf power index for a set of members.
#'
#' Calculates the proportion of swing votes attributed to each member
#' in all possible coalitions.
#'
#' @param member A character vector of member names.
#' @param vector_weights Numeric vector of weights.
#' @param quota Numeric threshold to form a winning coalition.
#' @return A named numeric vector of Banzhaf power scores.
#' @export
#' Compute Banzhaf power index for a set of members.
#'
#' Calculates the number of swing votes (raw or normalized) for each member
#' in all possible coalitions.
#'
#' @param member A character vector of member names.
#' @param vector_weights Numeric vector of weights.
#' @param quota Numeric threshold to form a winning coalition.
#' @param normalized Logical. If TRUE, return normalized Banzhaf scores.
#' @return A named numeric vector of Banzhaf power scores.
#' @export
banzhaf <- function(member, vector_weights, quota, normalized = TRUE) {
  n <- length(vector_weights)
  swing_counts <- numeric(n)
  for (k in 1:n) {
    coalitions <- combn(n, k, simplify = FALSE)
    for (coal in coalitions) {
      total <- sum(vector_weights[coal])
      if (total >= quota) {
        for (i in coal) {
          reduced_total <- total - vector_weights[i]
          if (reduced_total < quota) {
            swing_counts[i] <- swing_counts[i] + 1
          }
        }
      }
    }
  }
  total_swings <- sum(swing_counts)
  if (normalized) {
    if (total_swings == 0) {
      warning("No swings found. Quota may be too high or too low.")
      result <- rep(0, n)
    } else {
      result <- swing_counts / total_swings
    }
  } else {
    result <- swing_counts
  }
  names(result) <- member
  return(result)
}

# ========== Verify MWC Support ==========

verify_mwc_support <- function(strategies, mwcs) {
  apply(strategies, 1, function(row) {
    any(sapply(mwcs, function(coal) all(row[coal] > 0)))
  })
}




# ========== Blotto Progress Helper ==========

progress_reporter <- function(i, total, start_time, last_update, min_interval = 60) {
  now <- Sys.time()
  elapsed_secs <- as.numeric(difftime(now, last_update, units = "secs"))
  total_elapsed <- as.numeric(difftime(now, start_time, units = "mins"))
  if (elapsed_secs > min_interval || i == total) {
    avg_time_per_iter <- as.numeric(difftime(now, start_time, units = "secs")) / i
    remaining_mins <- avg_time_per_iter * (total - i) / 60
    cat(sprintf(
      "→ Compared %d of %d strategies...\nElapsed time: %.1f minutes\nEstimated time remaining: %.1f minutes\n\n",
      i, total, total_elapsed, remaining_mins
    ))
    return(now)
  }
  return(last_update)
}

# ========== Blotto Compare ==========

blotto_compare <- function(strategy_matrix = NULL, weights, sample = FALSE, n_opponents = 1000,
                           tie_method = "coinflip", single_strategy = NULL,
                           strategy_set_A = NULL, strategy_set_B = NULL) {
  total_weight <- sum(weights)
  n_districts <- length(weights)

  compute_points <- function(p1, p2) {
    win <- p2 < p1
    tie <- p2 == p1
    lose <- p2 > p1

    if (tie_method %in% c("coinflip", "cointoss")) {
      tie_split <- rbinom(n_districts, 1, 0.5)
      p1_pts <- sum((win + tie * tie_split) * weights)
      p2_pts <- sum((lose + tie * (1 - tie_split)) * weights)
    } else if (tie_method == "p2wins") {
      p1_pts <- sum(win * weights)
      p2_pts <- sum((lose + tie) * weights)
    } else if (tie_method == "tie") {
      p1_pts <- sum(win * weights)
      p2_pts <- sum(lose * weights)
    } else if (tie_method == "winhalf") {
      p1_pts <- sum((win + 0.5 * tie) * weights)
      p2_pts <- sum((lose + 0.5 * tie) * weights)
    } else {
      stop("Invalid tie_method.")
    }

    margin <- abs(p1_pts - p2_pts)
    winner <- if (p1_pts > p2_pts) 1 else if (p2_pts > p1_pts) 0 else 0.5
    list(winner = winner, margin = margin)
  }

  calc_stats <- function(wins, win_margins, loss_margins, total) {
    list(
      Win_Count = wins,
      Opponents_Faced = total,
      Win_Percentage = round(100 * wins / total, 2),
      Avg_Win_Margin = if (length(win_margins)) round(mean(win_margins), 2) else NA,
      Avg_Loss_Margin = if (length(loss_margins)) round(mean(loss_margins), 2) else NA
    )
  }

  if (!is.null(single_strategy)) {
    stopifnot(!is.null(strategy_matrix))
    n_opps <- nrow(strategy_matrix)
    win_margins <- numeric(n_opps)
    loss_margins <- numeric(n_opps)
    wins <- 0

    for (i in seq_len(n_opps)) {
      res <- compute_points(single_strategy, strategy_matrix[i, ])
      wins <- wins + res$winner
      if (res$winner == 1) {
        win_margins[i] <- res$margin
      } else if (res$winner == 0) loss_margins[i] <- res$margin
    }

    return(calc_stats(wins, win_margins[win_margins > 0], loss_margins[loss_margins > 0], n_opps))
  }

  if (!is.null(strategy_set_A) && !is.null(strategy_set_B)) {
    strategy_set_A <- if (is.vector(strategy_set_A)) matrix(strategy_set_A, nrow = 1) else strategy_set_A
    strategy_set_B <- if (is.vector(strategy_set_B)) matrix(strategy_set_B, nrow = 1) else strategy_set_B

    n_A <- nrow(strategy_set_A)
    n_B <- nrow(strategy_set_B)
    win_counts <- numeric(n_A)
    win_margins <- vector("list", n_A)
    loss_margins <- vector("list", n_A)

    for (i in seq_len(n_A)) {
      for (j in seq_len(n_B)) {
        res <- compute_points(strategy_set_A[i, ], strategy_set_B[j, ])
        win_counts[i] <- win_counts[i] + res$winner
        if (res$winner == 1) {
          win_margins[[i]] <- c(win_margins[[i]], res$margin)
        } else if (res$winner == 0) loss_margins[[i]] <- c(loss_margins[[i]], res$margin)
      }
    }

    return(data.frame(
      Strategy_ID = seq_len(n_A),
      Win_Count = win_counts,
      Opponents_Faced = n_B,
      Win_Percentage = round(100 * win_counts / n_B, 2),
      Avg_Win_Margin = sapply(win_margins, function(x) if (length(x)) round(mean(x), 2) else NA),
      Avg_Loss_Margin = sapply(loss_margins, function(x) if (length(x)) round(mean(x), 2) else NA)
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
      res <- compute_points(strategy_matrix[i, ], strategy_matrix[j, ])
      win_counts[i] <- win_counts[i] + res$winner
      win_counts[j] <- win_counts[j] + (1 - res$winner)

      if (res$winner == 1) {
        win_margins[[i]] <- c(win_margins[[i]], res$margin)
        loss_margins[[j]] <- c(loss_margins[[j]], res$margin)
      } else if (res$winner == 0) {
        win_margins[[j]] <- c(win_margins[[j]], res$margin)
        loss_margins[[i]] <- c(loss_margins[[i]], res$margin)
      }
    }
  }

  combinations_df <- as.data.frame(strategy_matrix)
  combinations_df$Win_Count <- win_counts
  combinations_df$Opponents_Faced <- n_players - 1
  combinations_df$Win_Percentage <- round(100 * win_counts / (n_players - 1), 2)
  combinations_df$Avg_Win_Margin <- sapply(win_margins, function(x) if (length(x)) round(mean(x), 2) else 0)
  combinations_df$Avg_Loss_Margin <- sapply(loss_margins, function(x) if (length(x)) round(mean(x), 2) else 0)
  combinations_df$Rank <- rank(-combinations_df$Win_Count, ties.method = "first")

  combinations_df <- combinations_df[order(as.numeric(rownames(combinations_df))), ]
  return(combinations_df)
}






# Monotonicity (test global monotonicity (i.e. x[1] ≤ x[2] ≤ x[3] ≤ ...)

monotonicity <- function(x, include_zeros = TRUE) {
  if (is.matrix(x)) {
    apply(x, 1, monotonicity, include_zeros = include_zeros)
  } else {
    if (!include_zeros) x <- x[x > 0]
    all(diff(x) >= 0)
  }
}



# monotonic_flags <- monotonicity(participant_matrix_adj, include_zeros = TRUE)



# ========== Sample MWC Distributions ==========

sample_mwc_distributions <- function(
    mwcs, vector_weights, target_sum = 100, total_samples_goal = 10000,
    samples_per_mwc = NULL, verbose = TRUE) {
  n_mwcs <- length(mwcs)
  if (is.null(samples_per_mwc)) {
    samples_per_mwc <- ceiling(total_samples_goal / n_mwcs)
  }
  result_list <- list()
  for (i in seq_along(mwcs)) {
    mwc <- mwcs[[i]]
    samples <- matrix(0, nrow = samples_per_mwc, ncol = length(vector_weights))
    for (j in 1:samples_per_mwc) {
      alloc <- runif(length(mwc), 1, 100)
      alloc <- apportion_values(alloc, target_sum)
      samples[j, mwc] <- alloc
    }
    result_list[[i]] <- samples
    if (verbose) cat(sprintf("✓ Sampled MWC %d of %d\n", i, n_mwcs))
  }
  names(result_list) <- paste0("MWC_", seq_along(result_list))
  return(result_list)
}



# ========== Distribution Validation Plot ==========

validate_sampling_methods <- function(vector_weights, methods, target_sum = 100) {
  par(mfrow = c(3, 3))
  for (m in methods) {
    sims <- t(replicate(1000, new_combinations(vector_weights, target_sum = target_sum, method = m)))
    matplot(t(sims),
      type = "l", col = adjustcolor("gray50", alpha.f = 0.1), lty = 1,
      main = paste("Method:", m), xlab = "Position", ylab = "Allocation"
    )
  }
  par(mfrow = c(1, 1))
}



# ========== Stacked Histogram Plot ==========

#' Plot a stacked histogram of allocation distributions.
#'
#' Visualizes distributions of simulated strategies across positions, with optional overlays
#' and density plots for comparison.
#'
#' @param random_vectors Matrix of strategies to be plotted.
#' @param overlay_vectors Optional second matrix to overlay for comparison.
#' @param state_names Optional vector of column names for display.
#' @param main Title of the plot.
#' @param show_density Whether to add density lines.
#' @param show_legend Whether to show a legend.
#' @param legend_labels Labels to use in the legend.
#'
#' @return A stacked histogram plot.
#' @export
stacked_hist_plot <- function(
    random_vectors,
    overlay_vectors = NULL,
    state_names = rev(c("A", "B", "C", "D", "E", "F", "G")),
    main = "Distribution",
    show_density = FALSE,
    show_legend = FALSE,
    legend_labels = c("EC vector_weights", "Banzhaf")) {
  df <- as.data.frame(random_vectors)
  colnames(df) <- state_names
  num_states <- length(state_names)

  if (!is.null(overlay_vectors)) {
    df_overlay <- as.data.frame(overlay_vectors)
    colnames(df_overlay) <- state_names
  }

  par(mar = c(5, 6, 4, 2))
  plot(NULL,
    xlim = c(0, 100), ylim = c(0, num_states * 1.2 + 1.5),
    xlab = "Allocation", ylab = "", yaxt = "n", main = main
  )

  axis(2,
    at = seq(0.6, num_states * 1.2 - 0.6, by = 1.2),
    labels = state_names, las = 1
  )

  base_colors <- gray.colors(num_states, start = 0.2, end = 0.7)
  overlay_color <- gray(0.1, alpha = 0.4)

  for (i in 1:num_states) {
    position <- (num_states - i) * 1.2
    values <- df[[i]]
    h <- hist(values, plot = FALSE, breaks = seq(0, 100, by = 1))
    scaled <- if (max(h$counts) > 0) h$counts / max(h$counts) * 0.8 else rep(0, length(h$counts))

    rect(h$breaks[-length(h$breaks)], position,
      h$breaks[-1], position + scaled,
      col = adjustcolor(base_colors[i], alpha.f = 0.5), border = NA
    )

    if (!is.null(overlay_vectors)) {
      overlay_vals <- df_overlay[[i]]
      h_overlay <- hist(overlay_vals, plot = FALSE, breaks = seq(0, 100, by = 1))
      scaled_overlay <- if (max(h_overlay$counts) > 0) h_overlay$counts / max(h_overlay$counts) * 0.8 else rep(0, length(h_overlay$counts))

      rect(h_overlay$breaks[-length(h_overlay$breaks)], position,
        h_overlay$breaks[-1], position + scaled_overlay,
        col = overlay_color, border = NA
      )

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
      bty = "o"
    )
  }
}



# ========== Evaluate Strategies ==========

#' Evaluate a matrix of strategies against quota and EC coverage.
#'
#' Calculates zero usage, average allocation, and the percentage of
#' strategies that meet the winning quota.
#'
#' @param strategy_matrix Strategy matrix (rows = strategies).
#' @param vector_weights Electoral weights.
#' @param quota Threshold for coalition success.
#' @return A summary list including position stats and rationality rate.
#' @export
evaluate_strategies <- function(strategy_matrix, vector_weights, quota = 70) {
  n <- nrow(strategy_matrix)
  m <- ncol(strategy_matrix)
  zero_counts <- colSums(strategy_matrix == 0)
  zero_percentage <- (zero_counts / n) * 100
  means <- colMeans(strategy_matrix)
  sd_unit <- apply(strategy_matrix, 2, sd) # Standard deviation
  medians <- apply(strategy_matrix, 2, median) # Median
  min_unit <- apply(strategy_matrix, 2, min) # Min
  max_unit <- apply(strategy_matrix, 2, max) # Max
  ec_covered <- apply(strategy_matrix > 0, 1, function(x) sum(vector_weights[x]))
  states_covered <- apply(strategy_matrix > 0, 1, function(x) sum(x))
  rational <- ec_covered >= quota
  percent_rational <- mean(rational) * 100

  summary_df <- data.frame(
    position = 1:m,
    zero_count = zero_counts,
    zero_percent = round(zero_percentage, 1),
    mean_allocation = round(means, 2),
    median_allocation = round(medians, 2),
    sd_allocation = round(sd_unit, 2),
    min_allocation = round(min_unit, 2),
    max_allocation = round(max_unit, 2)
  )

  cat("Strategy Evaluation Summary:\n")
  cat("→ Total strategies:", n, "\n")
  cat("→ Rational strategies (meet quota):", sum(rational), "/", n, sprintf("(%.1f%%)\n", percent_rational))
  cat("→ Mean # States with non-zero allocations:", round(mean(states_covered), 2), "\n")
  cat("→ Mean EC coverage per strategy:", round(mean(ec_covered), 1), "\n")

  list(
    position_summary = summary_df,
    rationality_rate = percent_rational,
    ec_coverage = ec_covered
  )
}


# Function to compute ENCM for a single strategy (Effective Number of Coalition Members (ENCM))
calc_encm <- function(allocation) {
  p <- allocation / sum(allocation)
  1 / sum(p^2)
}

# # Apply to all user strategies
# encm_values <- apply(user_matrix_adj, 1, calc_encm)

# # Summary
# summary(encm_values)
