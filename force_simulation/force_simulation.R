force_simulation <- function(id,
                                   values,
                                   initial_x = NULL,
                                   initial_y = NULL,
                                   max_iter = 1000,
                                   learning_rate = 0.1,
                                   min_dist = 1e-6,
                                   seed = NULL,
                                   verbose = TRUE) {
  
  if (!is.null(seed)) set.seed(seed)
  n <- length(values)
  
  if (is.null(initial_x)) initial_x <- runif(n, 0, 100)
  if (is.null(initial_y)) initial_y <- runif(n, 0, 100)
  
  x <- initial_x
  y <- initial_y
  
  # Inline helper to compute radii
  get_radius_auto <- function(values, x, y,
                              scale_fn = sqrt,
                              min_frac = 0.01,
                              max_frac = 0.04) {
    dx <- diff(range(x, na.rm = TRUE))
    dy <- diff(range(y, na.rm = TRUE))
    diag_len <- sqrt(dx^2 + dy^2)
    
    r_min <- diag_len * min_frac
    r_max <- diag_len * max_frac
    
    scaled <- scale_fn(values)
    scaled <- (scaled - min(scaled, na.rm = TRUE)) /
              (max(scaled, na.rm = TRUE) - min(scaled, na.rm = TRUE))
    
    r_min + (r_max - r_min) * scaled
  }
  
  # Compute radii
  radii <- get_radius_auto(
    values = values,
    x = x,
    y = y,
    scale_fn = sqrt,
    min_frac = 0.01,
    max_frac = 0.04
  )
  
  total_moves <- 0
  
  for (iter in 1:max_iter) {
    moved <- FALSE
    move_count <- 0
    
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        dx <- x[j] - x[i]
        dy <- y[j] - y[i]
        dist <- sqrt(dx^2 + dy^2)
        r_sum <- radii[i] + radii[j]
        overlap <- r_sum - dist
        
        if (overlap > min_dist) {
          if (dist < min_dist) {
            angle <- runif(1, 0, 2 * pi)
            dx <- cos(angle)
            dy <- sin(angle)
            dist <- 1
          }
          
          ux <- dx / dist
          uy <- dy / dist
          shift <- overlap * learning_rate
          
          x[i] <- x[i] - ux * shift / 2
          y[i] <- y[i] - uy * shift / 2
          x[j] <- x[j] + ux * shift / 2
          y[j] <- y[j] + uy * shift / 2
          
          moved <- TRUE
          move_count <- move_count + 1

          if (verbose) {
            cat(sprintf("[Iter %3d] Moved %s ↔ %s | Overlap: %.2f | Dist: %.2f\n",
                        iter, id[i], id[j], overlap, dist))
          }
        }
      }
    }
    
    if (verbose) {
      cat(sprintf("[Iter %3d] Total pairs moved: %d\n", iter, move_count))
    }
    
    total_moves <- total_moves + move_count
    if (!moved) {
      if (verbose) cat(sprintf("Converged at iteration %d.\n", iter))
      break
    }
  }
  
  result <- data.frame(
    id = id,
    x_adj = x,
    y_adj = y,
    r = radii,
    values = values
  )
  
  attr(result, "iterations") <- iter
  attr(result, "moves") <- total_moves
  
  return(result)
}



add_alpha <- function(hex_colors, alpha = 1) {
  rgb_matrix <- col2rgb(hex_colors) / 255
  apply(rgb_matrix, 2, function(col) {
    rgb(col[1], col[2], col[3], alpha = alpha, maxColorValue = 1)
  })
}

