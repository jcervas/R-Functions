# Function to calculate the square root scale for opacity (oScale)
scaleOpacitySqrt <- function(value, minOpacity = 0, maxOpacity = 1, maxDomain = NA, return = "value") {
  if (is.na(maxDomain)) {
    stop("Need max Domain")
  }
  domain <- c(0, maxDomain)
  range <- c(minOpacity, maxOpacity)
  
  sqrt_value <- sqrt(value)
  scaled_value <- (sqrt_value - sqrt(domain[1])) / (sqrt(domain[2]) - sqrt(domain[1]))
  alpha_hex <- sprintf("%02X", round(scaled_value * 255))

  if (return == "hex") {
    return(alpha_hex)
    } else {
    return(scaled_value) }
}

# Applying the scale function to percentage point difference for opacity scaling

data <- data.frame(per_point_diff = c(-.1,-.05,-0.01,0,.1,.1))
pop.opacity <- scaleOpacitySqrt(value= abs(data$per_point_diff), minOpacity=0.25, maxOpacity=0.75, maxDomain=max(abs(data$per_point_diff)))
  print(pop.opacity)
