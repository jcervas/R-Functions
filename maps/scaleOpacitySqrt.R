# Function to calculate the square root scale for opacity (oScale)
scaleOpacitySqrt <- function(value, minOpacity = 0, maxOpacity = 1, maxDomain = NA) {
  if (is.na(maxDomain)) {
    stop("Need max Domain")
  }
  domain <- c(0, maxDomain)
  range <- c(minOpacity, maxOpacity)
  
  sqrt_value <- sqrt(value)
  scaled_value <- (sqrt_value - sqrt(domain[1])) / (sqrt(domain[2]) - sqrt(domain[1]))
  alpha_hex <- sprintf("%02X", round(scaled_value * 255))
  return(alpha_hex)
}

# Applying the scale function to percentage point difference for opacity scaling
pop.opacity <- scaleOpacitySqrt(abs(county.2020$per_point_diff), minOpacity=0.25, maxOpacity=0.75, maxDomain=max(abs(county.2020$per_point_diff)))