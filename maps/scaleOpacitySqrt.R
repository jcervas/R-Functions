scaleOpacitySqrt <- function(value, minOpacity = 0.25, maxOpacity = 0.75, return = "value") {
  # Ensure minDomain and maxDomain are provided
  if (is.na(minDomain) || is.na(maxDomain)) {
    stop("Need minDomain and maxDomain")
  }

  # Normalize values to be non-negative for square root transformation
  normalized_value <- value - minDomain
  normalized_maxDomain <- maxDomain - minDomain

  # Calculate the square root of the normalized value
  sqrt_value <- sqrt(normalized_value)

  # Scale the square root value to the range [0, 1]
  scaled_value <- sqrt_value / sqrt(normalized_maxDomain)

  # Scale the value to the desired opacity range [minOpacity, maxOpacity]
  scaled_opacity <- scaled_value * (maxOpacity - minOpacity) + minOpacity

  # Convert the opacity to a hexadecimal string if requested
  alpha_hex <- sprintf("%02X", round(scaled_opacity * 255))

  # Return either the scaled value or the hexadecimal representation
  if (return == "hex") {
    return(alpha_hex)
  } else {
    return(scaled_opacity)
  }
}

# # Example usage with the provided data frame
# data <- data.frame(per_point_diff = c(-0.1, -0.05, -0.01, 0, 0.1, 0.1))

# # Apply the function to the data frame
# data$opacity_value <- sapply(data$per_point_diff, scaleOpacitySqrt, minOpacity = 0.25, maxOpacity = 0.75, minDomain = minDomain, maxDomain = maxDomain)
# data$opacity_hex <- sapply(data$per_point_diff, scaleOpacitySqrt, minOpacity = 0.25, maxOpacity = 0.75, minDomain = minDomain, maxDomain = maxDomain, return = "hex")

# # Print the results
# print(data)
