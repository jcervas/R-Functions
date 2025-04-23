
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

drop_irrational_allocations <- function(alloc, weights, quota = 70) {
  if (sum(ifelse(alloc > 0, weights, 0)) > quota) alloc else NULL
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
  method = c("uniform", "uniform_skewed", "normal", "poisson", "gamma", "beta", "custom"),
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
  method = c("uniform", "uniform_skewed", "normal", "poisson", "gamma", "beta", "custom"),
  small_mean = 0, small_sd = 2, min_small = 0, max_small = 100, custom_generator = NULL
) {
  method <- match.arg(method)
  vec_length <- length(weights)

  # Auto-disable rationality if weights are normalized
  if (quota > 0 && sum(weights) <= 1.1) {
    warning("Detected normalized weights; skipping rationality check by setting quota = 0")
    quota <- 0
  }

  repeat {
    if (method == "uniform") {
      combo <- generate_random_allocations(weights, target_sum, method = "uniform")
    } else {
      combo <- numeric(vec_length)
      num_small_digits <- sample(0:max_small_digits, 1)
      num_zeros <- sample(0:min(num_small_digits, max_zeros), 1)
      zero_positions <- if (num_zeros > 0) sample(vec_length, num_zeros) else integer(0)
      remaining_positions <- setdiff(seq_len(vec_length), zero_positions)
      num_small <- num_small_digits - num_zeros
      small_positions <- if (num_small > 0) sample(remaining_positions, num_small) else integer(0)
      remaining_positions <- setdiff(remaining_positions, small_positions)
      small_values <- if (num_small > 0) {
        valid <- FALSE
        while (!valid) {
          small_values <- abs(round(rnorm(num_small, small_mean, small_sd)))
          small_values <- pmin(pmax(small_values, min_small), max_small)
          valid <- sum(small_values) > 0
        }
        small_values
      } else numeric(0)
      combo[small_positions] <- small_values
      remaining_sum <- target_sum - sum(combo)
      if (remaining_sum < 0 || length(remaining_positions) == 0) next
      filled_values <- generate_random_allocations(weights[remaining_positions], remaining_sum, method, small_sd / target_sum, 1, custom_generator)
      combo[remaining_positions] <- filled_values
    }

    if (quota == 0) return(combo)

    total_weight_allocated <- sum(weights[combo > 0])
    if (total_weight_allocated <= quota) next
    valid <- drop_irrational_allocations(combo, weights, quota)
    if (!is.null(valid)) return(combo)
  }
}
