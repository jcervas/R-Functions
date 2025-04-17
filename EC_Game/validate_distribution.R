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

stacked_hist_plot <- function(random_vectors, state_names=rev(c("A", "B", "C", "D", "E", "F", "G")), main = "Distribution") {
  # Convert the matrix to a data frame
  df <- as.data.frame(random_vectors)
  
  # Name the columns
  colnames(df) <- state_names
  
  # Number of states
  num_states <- length(state_names)
  
  # Set up the plotting area with appropriate margins
  par(mar = c(5, 6, 4, 2))  # Bottom, left, top, right margins
  
  # Create an empty plot
  plot(NULL, xlim = c(0, 100), 
       ylim = c(0, num_states * 1.2), 
       xlab = "Value", ylab = "", yaxt = "n",
       main = main)
  
  # Add y-axis labels
  axis(2, at = seq(0.6, num_states * 1.2 - 0.6, by = 1.2),
       labels = state_names, las = 1)
  
  # Generate colors
  colors <- rainbow(num_states)

  # Draw each histogram
  for (i in 1:num_states) {
    # Calculate vertical position for this histogram
    # We place the highest state at the top of the plot
    position <- (num_states - i) * 1.2
    
    # Get the histogram data
    h <- hist(df[, i], plot = FALSE, breaks = seq(0, 100, by = 5))
    
    # Scale the heights to a reasonable size
    scaled_heights <- h$counts / max(h$counts) * 0.8
    
    # Draw the bars
    rect(h$breaks[-length(h$breaks)], position,
         h$breaks[-1], position + scaled_heights,
         col = adjustcolor(colors[i], alpha.f = 0.7), border = NA)
    
    # Add a border line at the base of each histogram
    lines(c(0, 100), c(position, position), col = "gray")
  }
}