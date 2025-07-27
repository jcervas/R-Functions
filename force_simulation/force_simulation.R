relaxation_simulation <- function(nodes, radii, x0, y0, n_iter = 2000, attract_k = 5, repel_eps = 1e-6) {
  n <- length(nodes)
  x <- x0
  y <- y0

  for (iter in 1:n_iter) {
    # Attraction to original positions
    for (i in 1:n) {
      dx <- x0[i] - x[i]
      dy <- y0[i] - y[i]
      x[i] <- x[i] + dx * attract_k
      y[i] <- y[i] + dy * attract_k
    }

    # Overlap removal
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        dx <- x[j] - x[i]
        dy <- y[j] - y[i]
        dist_sq <- dx^2 + dy^2 + repel_eps
        dist <- sqrt(dist_sq)
        min_dist <- radii[i] + radii[j]

        if (dist < min_dist) {
          # Overlap detected
          overlap <- (min_dist - dist)
          shift_x <- dx / dist * overlap / 2
          shift_y <- dy / dist * overlap / 2

          # Apply shifts
          x[i] <- x[i] - shift_x
          y[i] <- y[i] - shift_y
          x[j] <- x[j] + shift_x
          y[j] <- y[j] + shift_y
        }
      }
    }
  }

  data.frame(region = nodes, x, y, radius = radii, x0, y0)
}
  