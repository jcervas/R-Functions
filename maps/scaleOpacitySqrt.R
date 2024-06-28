scaleOpacitySqrt <- function(value, minOpacity = 0.25, maxOpacity = 0.75, maxDomain = NA, return = "value") {
  # Ensure maxDomain is provided
  if (is.na(maxDomain)) {
    stop("Need maxDomain")
  }

  # Calculate the absolute value and square root of the input value
  abs_sqrt_value <- sqrt(abs(value))

  # Scale the absolute square root value to the range [0, 1]
  scaled_value <- abs_sqrt_value / sqrt(maxDomain)

  # Scale the value to the desired opacity range [minOpacity, maxOpacity]
  scaled_opacity <- scaled_value * (maxOpacity - minOpacity) + minOpacity

  # Ensure the scaled opacity is within the specified range
  scaled_opacity <- pmax(minOpacity, pmin(maxOpacity, scaled_opacity))

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
# data$opacity_value <- sapply(data$per_point_diff, scaleOpacitySqrt, minOpacity = 0.25, maxOpacity = 0.75, maxDomain = 0.1)
# data$opacity_hex <- sapply(data$per_point_diff, scaleOpacitySqrt, minOpacity = 0.25, maxOpacity = 0.75, maxDomain = 0.1, return = "hex")

