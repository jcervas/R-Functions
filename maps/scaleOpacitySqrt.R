scaleOpacitySqrt <- function(value, minOpacity = 0, maxOpacity = 1, maxDomain = NA, return = "value") {
  # Ensure maxDomain is provided
  if (is.na(maxDomain)) {
    stop("Need maxDomain")
  }

  # Define the domain and range for scaling
  domain <- c(0, maxDomain)
  range <- c(minOpacity, maxOpacity)

  # Calculate the square root of the input value
  sqrt_value <- sqrt(value)

  # Scale the square root value to the range [0, 1]
  scaled_value <- (sqrt_value - sqrt(domain[1])) / (sqrt(domain[2]) - sqrt(domain[1]))

  # Scale the value to the desired opacity range [minOpacity, maxOpacity]
  scaled_opacity <- scaled_value * (range[2] - range[1]) + range[1]

  # Convert the opacity to a hexadecimal string if requested
  alpha_hex <- sprintf("%02X", round(scaled_opacity * 255))

  # Return either the scaled value or the hexadecimal representation
  if (return == "hex") {
    return(alpha_hex)
  } else {
    return(scaled_opacity)
  }
}


## Applying the scale function to percentage point difference for opacity scaling
# data <- data.frame(per_point_diff = c(-.1,-.05,-0.01,0,.1,.1))
# pop.opacity <- scaleOpacitySqrt(value= abs(data$per_point_diff), minOpacity=0.25, maxOpacity=0.75, maxDomain=max(abs(data$per_point_diff)), return="value")
#   print(pop.opacity)
