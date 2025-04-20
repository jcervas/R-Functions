evaluate_strategies <- function(matrix_data, weights, quota = 70) {
  n <- nrow(matrix_data)
  m <- ncol(matrix_data)

  # Basic summary stats
  zero_counts <- colSums(matrix_data == 0)
  zero_percentage <- (zero_counts / n) * 100
  means <- colMeans(matrix_data)
  total_allocations <- rowSums(matrix_data)
  ec_covered <- apply(matrix_data > 0, 1, function(x) sum(weights[x]))
  rational <- ec_covered > quota

  # Rationality rate
  percent_rational <- mean(rational) * 100

  summary_df <- data.frame(
    position = seq_len(m),
    zero_count = zero_counts,
    zero_percent = round(zero_percentage, 1),
    mean_allocation = round(means, 2)
  )

  cat("Strategy Evaluation Summary:\n")
  cat("→ Total strategies:", n, "\n")
  cat("→ Rational strategies (can reach quota):", sum(rational), "/", n, 
      sprintf("(%.1f%%)\n", percent_rational))
  cat("→ Mean EC coverage per strategy:", round(mean(ec_covered), 1), "\n")

  return(list(
    position_summary = summary_df,
    rationality_rate = percent_rational,
    ec_coverage = ec_covered
  ))
}


# Function to validate the distribution of zeros
validate_distribution <- function(combinations) {
  # Count zeros in each position
  zero_counts <- colSums(combinations == 0)
  
  # Calculate percentage of zeros in each position
  zero_percentages <- (zero_counts / nrow(combinations)) * 100
  
  # Count overall zeros
  total_zeros <- sum(combinations == 0)
  total_elements <- prod(dim(combinations))
  overall_zero_percentage <- (total_zeros / total_elements) * 100
  
  # Mean, median for each position
  means <- colMeans(combinations)
  
  # Create summary data frame
  summary_df <- data.frame(
    position = 1:7,
    zero_count = zero_counts,
    zero_percentage = zero_percentages,
    mean_value = means
  )
  
  # Return results
  return(list(
    position_summary = summary_df,
    overall_zero_percentage = overall_zero_percentage
  ))
}

# stacked_hist_plot.R

#' Plot stacked histograms of allocation distributions with optional smoothing
#'
#' @param random_vectors A matrix of strategies (rows = strategies, columns = states)
#' @param state_names Optional character vector for state labels
#' @param main Title of the plot
#' @param show_density Logical; whether to overlay smoothed density lines
stacked_hist_plot <- function(random_vectors,
                               state_names = rev(c("A", "B", "C", "D", "E", "F", "G")),
                               main = "Distribution",
                               show_density = FALSE) {
  df <- as.data.frame(random_vectors)
  colnames(df) <- state_names
  num_states <- length(state_names)

  par(mar = c(5, 6, 4, 2))
  plot(NULL, xlim = c(0, 100), ylim = c(0, num_states * 1.2),
       xlab = "Allocation", ylab = "", yaxt = "n", main = main)

  axis(2, at = seq(0.6, num_states * 1.2 - 0.6, by = 1.2),
       labels = state_names, las = 1)

  colors <- gray.colors(num_states, start = 0.3, end = 0.8)

  for (i in 1:num_states) {
    position <- (num_states - i) * 1.2
    values <- df[[i]]

    h <- hist(values, plot = FALSE, breaks = seq(0, 100, by = 1))
    scaled <- h$counts / max(h$counts) * 0.8

    rect(h$breaks[-length(h$breaks)], position,
         h$breaks[-1], position + scaled,
         col = adjustcolor(colors[i], alpha.f = 0.7), border = NA)

    lines(c(0, 100), c(position, position), col = "gray90")

    if (show_density) {
      dens <- density(values, from = 0, to = 100)
      scaled_dens <- dens$y / max(dens$y) * 0.8
      lines(dens$x, scaled_dens + position, col = "black", lwd = 1.2)
    }
  }
}