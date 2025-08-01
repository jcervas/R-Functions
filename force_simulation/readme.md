## Read County Coordinates Data
`counties <- read.csv('/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/GitHub/R-Functions/force_simulation/counties.csv')`


## Helper Functions

## add_alpha
`add_alpha <- function(hex_colors, alpha = 1) {
  rgb_matrix <- col2rgb(hex_colors) / 255
  apply(rgb_matrix, 2, function(col) {
    rgb(col[1], col[2], col[3], alpha = alpha, maxColorValue = 1)
  })
}`


## Define domain thresholds
`domain <- c(1, 5, 10, 15, 20)`

## Define color range (mapped to domain + 1 levels)

## These are hex approximations of d3.interpolateGreys values

`range <- c("#E5E5E5",  # ~ d3.interpolateGreys(0.1)
           "#BFBFBF",  # ~ d3.interpolateGreys(0.35)
           "#999999",  # ~ d3.interpolateGreys(0.5)
           "#7A7A7A",  # ~ d3.interpolateGreys(0.65)
           "#595959",  # ~ d3.interpolateGreys(0.8)
           "#262626")  # ~ d3.interpolateGreys(0.95)`

`range <- add_alpha(range, alpha = 0.5)`



# Ensure `rate` is clean
`valid <- complete.cases(counties$rate, counties$S, counties$Y)`
`rate_vals <- counties$rate[valid]`

# Use findInterval to bin each value into one of the thresholds
`rate_bins <- findInterval(rate_vals, domain, rightmost.closed = TRUE, all.inside = TRUE)`

# Map to colors
`fill_colors <- range[rate_bins + 1]  # +1 because R is 1-based`


`sim_result <- force_simulation(values = counties$total[valid],
                               initial_x = counties$X[valid],
                               initial_y = counties$Y[valid],
                               radius_scale = 120,
                               max_iter = 20,
                               learning_rate = 0.6)`



`circles <- get_circle_specs(sim_result, fill = fill_colors, lwd = 0.4)`

# Flip the Y axis using range
`circles$y <- max(circles$y) - circles$y + min(circles$y)`


`svg("custom_circle_plot.svg", width = 8, height = 8*0.7)
plot(
     NA, 
     xlim = circle_layout_bbox(sim_result)$xlim,
     ylim = circle_layout_bbox(sim_result)$ylim,
     asp = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# Full control: call symbols() or draw polygons yourself
with(circles, {
  for (i in seq_along(x)) {
    symbols(x[i], y[i], circles = r[i], inches = FALSE, add = TRUE,
            bg = fill[i], 
            fg = border[i], 
            lwd = 0.1)
  }
})
dev.off()`