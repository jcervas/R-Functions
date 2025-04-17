# Helper: Generate raw values based on method
get_raw_allocation_values <- function(weights, target_sum, method, sd_factor = 0.3, skew_power = 1, custom_generator = NULL) {
  n <- length(weights)
  weights <- weights / sum(weights)

  if (method == "nbinom") {
    mu_vals <- weights * target_sum
    raw_values <- rnbinom(n, size = mu_vals, prob = 0.5)
  } else if (method == "normal") {
    mu_vals <- weights * target_sum
    sd_vals <- sd_factor * mu_vals
    raw_values <- rnorm(n, mean = mu_vals, sd = sd_vals)
  } else if (method == "uniform") {
    raw_values <- runif(n, min = 0, max = target_sum * 2)
  } else if (method == "skewed") {
    skewed_weights <- weights^skew_power
    raw_values <- rnorm(n, mean = skewed_weights, sd = sd_factor * skewed_weights)
    raw_values <- raw_values + runif(n, 0, 1)
  } else if (method == "exponential") {
    raw_values <- rexp(n, rate = 1 / (weights * target_sum))
  } else if (method == "poisson") {
    lambda_vals <- weights * target_sum
    raw_values <- rpois(n, lambda = lambda_vals)
  } else if (method == "gamma") {
    shape_vals <- weights * 5
    scale_vals <- target_sum / sum(shape_vals)
    raw_values <- rgamma(n, shape = shape_vals, scale = scale_vals)
  } else if (method == "beta") {
    shape1 <- weights * 2
    shape2 <- (1 - weights) * 2
    shape1[shape1 <= 0] <- 0.1
    shape2[shape2 <= 0] <- 0.1
    raw_props <- rbeta(n, shape1 = shape1, shape2 = shape2)
    raw_values <- raw_props * target_sum
  } else if (method == "dirichlet") {
    stop("The 'dirichlet' method requires an external package and has been removed to avoid dependencies.")
  } else if (method == "custom") {
    if (is.null(custom_generator)) stop("Custom generator function must be provided for method='custom'")
    raw_values <- custom_generator(n, weights, target_sum)
  } else {
    stop("Unknown method.")
  }

  pmax(raw_values, 0)
}

# Helper: Round and adjust allocation to target sum
adjust_allocation <- function(raw_values, weights, target_sum) {
  alloc <- round(raw_values / sum(raw_values) * target_sum)
  diff <- target_sum - sum(alloc)
  tries <- 0
  n <- length(weights)

  while (diff != 0 && tries < 1000) {
    idx <- sample(1:n, 1, prob = weights)
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

# Helper: Choose positions for small and zero values
assign_small_and_zero_positions <- function(vec_length, max_small_digits, max_zeros, 
                                            small_mean, small_sd, min_small, max_small) {
  num_small_digits <- sample(0:max_small_digits, 1)
  max_zeros_actual <- min(num_small_digits, max_zeros)
  num_zeros <- sample(0:max_zeros_actual, 1)

  zero_positions <- if (num_zeros > 0) sample(1:vec_length, num_zeros) else integer(0)
  nonzero_positions <- setdiff(1:vec_length, zero_positions)

  small_count <- num_small_digits - num_zeros
  small_positions <- if (small_count > 0) sample(nonzero_positions, small_count) else integer(0)

  small_allocation <- rep(0, small_count)
  if (small_count > 0) {
    while (sum(small_allocation) == 0) {
      small_allocation <- abs(round(rnorm(small_count, mean = small_mean, sd = small_sd)))
      small_allocation <- pmin(pmax(small_allocation, min_small), max_small)
    }
  }

  list(
    zero_positions = zero_positions,
    small_positions = small_positions,
    small_allocation = small_allocation
  )
}

# Main: Generate random allocations
generate_random_allocations <- function(weights, 
                                        target_sum = 100, 
                                        method = c("normal", "nbinom", "uniform", "skewed", "exponential", "poisson", "gamma", "beta", "custom"),
                                        sd_factor = 0.3,
                                        skew_power = 1,
                                        custom_generator = NULL,
                                        return_raw = FALSE) {
  method <- match.arg(method)
  raw_values <- get_raw_allocation_values(weights, target_sum, method, sd_factor, skew_power, custom_generator)
  alloc <- adjust_allocation(raw_values, weights, target_sum)

  if (return_raw) return(list(alloc = alloc, raw = raw_values))
  alloc
}

# Main: Generate new combinations
new_combinations <- function(weights = c(3, 5, 8, 13, 21, 34, 55), 
                             max_small_digits = 6, 
                             max_zeros = 5, 
                             target_sum = 100,
                             method = c("normal", "nbinom", "uniform", "skewed", "exponential", "poisson", "gamma", "beta", "custom"),
                             small_mean = 0, 
                             small_sd = 2, 
                             min_small = 0, 
                             max_small = 100,
                             custom_generator = NULL) {

  method <- match.arg(method)
  vec_length <- length(weights)
  combo <- numeric(vec_length)

  positions <- assign_small_and_zero_positions(vec_length, max_small_digits, max_zeros, 
                                               small_mean, small_sd, min_small, max_small)
  combo[positions$small_positions] <- positions$small_allocation

  non_small_positions <- setdiff(1:vec_length, c(positions$zero_positions, positions$small_positions))
  reduced_weights <- weights[non_small_positions]
  reduced_weights <- reduced_weights / sum(reduced_weights)

  remaining_sum <- target_sum - sum(combo)
  if (length(reduced_weights) == 1) {
    nonzero_allocation <- remaining_sum
  } else {
    raw_values <- get_raw_allocation_values(reduced_weights, remaining_sum, method, custom_generator = custom_generator)
    nonzero_allocation <- adjust_allocation(raw_values, reduced_weights, remaining_sum)
  }
  combo[non_small_positions] <- nonzero_allocation

  combo
}


# # Generate Random MWC allocations

# Example MWC list
mwcs <- list(
  c(5, 7), c(6, 7), c(1, 4, 7), c(2, 4, 7), c(3, 4, 7),
  c(1, 2, 3, 7), c(1, 4, 5, 6), c(2, 4, 5, 6),
  c(3, 4, 5, 6), c(1, 2, 3, 5, 6)
)

sample_distributions_fast <- function(
  mwcs, 
  target_sum = 100, 
  samples_per_mwc = 1000
) {
  
  result_list <- list()
  
  for (indices in mwcs) {
    k <- length(indices)
    probs <- matrix(runif(samples_per_mwc * k), nrow = k)
    probs <- probs / colSums(probs)
    values <- apply(probs, 2, function(p) rmultinom(1, size = total, prob = p))
    values <- t(values)
    full <- matrix(0, nrow = samples_per_mwc, ncol = 7)
    full[, indices] <- values
    result_list[[length(result_list) + 1]] <- full
  }
  
  do.call(rbind, result_list)
}


sample_mwc_distributions <- function(
  mwcs,
  target_sum = 100,
  total_samples_goal = 1e6,
  batch_multiplier = 2,
  initial_seed = NULL,
  verbose = TRUE
) {
  if (!is.null(initial_seed)) set.seed(initial_seed)
  
  n_mwcs <- length(mwcs)
  samples_per_mwc <- ceiling((batch_multiplier * total_samples_goal) / n_mwcs)

  if (verbose) cat("Starting first big batch...\n")
  unique_samples <- unique(sample_distributions_fast(mwcs, target_sum, samples_per_mwc))
  if (verbose) cat(sprintf("Initial unique rows: %d\n", nrow(unique_samples)))

  while (nrow(unique_samples) < total_samples_goal) {
    remaining <- total_samples_goal - nrow(unique_samples)
    if (verbose) cat(sprintf("Need %d more samples. Generating next %dx batch...\n", remaining, batch_multiplier))

    new_batch <- unique(sample_distributions_fast(mwcs, target_sum, samples_per_mwc))
    before <- nrow(unique_samples)
    unique_samples <- unique(rbind(unique_samples, new_batch))
    added <- nrow(unique_samples) - before

    if (verbose) cat(sprintf("→ Added %d new samples. Total unique so far: %d / %d\n", added, nrow(unique_samples), total_samples_goal))
  }

  if (nrow(unique_samples) > total_samples_goal) {
    if (verbose) cat(sprintf("Clipping from %d to %d rows...\n", nrow(unique_samples), total_samples_goal))
    unique_samples <- unique_samples[sample(nrow(unique_samples), total_samples_goal), , drop = FALSE]
  }

  return(unique_samples)
}

# # Sample Code to Run

final_samples <- sample_mwc_distributions(
  mwcs,
  target_sum = 100,
  total_samples_goal = 1e6,
  batch_multiplier = 2,
  initial_seed = NULL
)
