force_simulation <- function(values,
                             radius_fn = function(v) sqrt(v),
                             initial_x = NULL,
                             initial_y = NULL,
                             radius_scale = 1,
                             max_iter = 1000,
                             learning_rate = 0.1,
                             min_dist = 1e-6,
                             seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- length(values)
  radii <- radius_fn(values) / radius_scale
  
  if (is.null(initial_x)) initial_x <- runif(n, 0, 100)
  if (is.null(initial_y)) initial_y <- runif(n, 0, 100)
  
  x <- initial_x
  y <- initial_y
  
  for (iter in 1:max_iter) {
    moved <- FALSE
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        dx <- x[j] - x[i]
        dy <- y[j] - y[i]
        dist <- sqrt(dx^2 + dy^2)
        overlap <- radii[i] + radii[j] - dist
        if (overlap > min_dist) {
          if (dist < min_dist) {
            angle <- runif(1, 0, 2*pi)
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
        }
      }
    }
    if (!moved) break
  }
  
  return(list(x = x, y = y, radii = radii, values = values))
}

get_circle_specs <- function(sim_result,
                             fill = "skyblue",
                             border = "black",
                             lwd = 1) {
  n <- length(sim_result$x)
  
  data.frame(
    x = sim_result$x,
    y = sim_result$y,
    r = sim_result$radii,
    fill = rep_len(fill, n),
    border = rep_len(border, n),
    lwd = rep_len(lwd, n),
    stringsAsFactors = FALSE
  )
}

add_alpha <- function(hex_colors, alpha = 1) {
  rgb_matrix <- col2rgb(hex_colors) / 255
  apply(rgb_matrix, 2, function(col) {
    rgb(col[1], col[2], col[3], alpha = alpha, maxColorValue = 1)
  })
}



circle_layout_bbox <- function(sim_result, padding = 0.05) {
  x <- sim_result$x
  y <- sim_result$y
  r <- sim_result$radii
  
  x_range <- range(x - r, x + r)
  y_range <- range(y - r, y + r)
  
  x_pad <- diff(x_range) * padding
  y_pad <- diff(y_range) * padding
  
  list(xlim = x_range + c(-x_pad, x_pad),
       ylim = y_range + c(-y_pad, y_pad))
}
