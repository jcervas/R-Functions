options(scipen = 999)


sv_plot <- function(...,
                              colors = NULL,
                              ltys = NULL,
                              labels = NULL,
                              label_pos = NULL,
                              label_cex = 0.6,
                              label_col = NULL,
                              legend = FALSE,
                              legend_pos = "bottomright",
                              legend_cex = 0.7,
                              legend_bg = "white",
                              legend_bty = "n",
                              main = "",
                              xlab = "% of Two-Party Votes",
                              ylab = "% of Two-Party Seats",
                              xaxis = TRUE,
                              yaxis = TRUE,
                              prop.line = TRUE,
                              box = TRUE,
                              xlim = c(0, 1),
                              ylim = c(0, 1),
                              theme = "light") {
  # Collect curve data
  curves <- list(...)
  n <- length(curves)
  if (n == 0) stop("You must supply at least one curve (e.g. sv.hyp output).")
  
  # --- Theme settings ---
  if (theme == "dark") {
    bg_col <- "black"
    fg_col <- "white"
    axis_col <- "gray80"
    grid_col <- "gray60"
    shade_col <- rgb(1, 1, 1, 0.4)
  } else {
    bg_col <- "white"
    fg_col <- "black"
    axis_col <- "gray50"
    grid_col <- "gray90"
    shade_col <- rgb(0.1, 0.1, 0.1, 0.5)
  }
  
  # Defaults
  if (is.null(colors)) colors <- rep_len(c("blue", "red", "darkgreen", "orange", "purple"), n)
  if (is.null(ltys))   ltys   <- rep_len(2, n)
  if (is.null(label_col)) label_col <- colors
  
  # --- Base plot setup ---
  par(pty = "s", mar = c(2.5, 2, 2, 1), mgp = c(0.5, 0.5, 0), bg = bg_col)
  plot(1,
       type = "n",
       ylim = ylim,
       xlim = xlim,
       xaxs = "i",
       yaxs = "i",
       ylab = "",
       xlab = "",
       main = main,
       bty = "n",
       axes = FALSE,
       col.main = fg_col)
  
  # --- Axes ---
  if (xaxis) {
    axis(side = 1, las = 2, at = seq(0, 1, 0.1), labels = FALSE, lwd.ticks = 0.4, col = axis_col)
    axis(side = 1, las = 2, at = seq(0, 1, 0.01), labels = FALSE, lwd.ticks = 0.2, tck = -0.01, col = axis_col)
    axis(side = 1, at = seq(0, 1, 0.1),
         labels = paste0(seq(0, 100, 10), "%"), cex.axis = 0.5, col.axis = axis_col)
  }
  
  if (yaxis) {
    axis(side = 2, las = 2, at = seq(0, 1, 0.1), labels = FALSE, lwd.ticks = 0.4, col = axis_col)
    axis(side = 2, las = 2, at = seq(0, 1, 0.01), labels = FALSE, lwd.ticks = 0.2, tck = -0.01, col = axis_col)
    axis(side = 2, las = 2, at = seq(0, 1, 0.1),
         labels = paste0(seq(0, 100, 10), "%"), cex.axis = 0.5, col.axis = axis_col)
  }
  
  mtext(xlab, side = 1, line = 1.5, col = fg_col)
  mtext(ylab, side = 2, line = 1.5, col = fg_col)
  
  # --- Optional proportional representation line ---
  if (prop.line) {
    abline(0, 1, lty = 2, col = grid_col)
    text(0.23, 0.25, "PROPORTIONAL REPRESENTATION",
         srt = 45, cex = 0.5, col = axis_col)
  }
  
  # --- Optional box & midlines ---
  if (box) {
    box(col = axis_col)
    abline(v = 0.5, lty = 3, col = axis_col)
    abline(h = 0.5, lty = 3, col = axis_col)
  }
  
  # --- Border shading ---
  rect(-.05, -.05, 0, 1.05, col = shade_col, density = 40, border = "transparent")
  rect(0, 0, 1.05, -.05, col = shade_col, density = 40, border = "transparent")
  rect(1, 0, 1.05, 1.05, col = shade_col, density = 40, border = "transparent")
  rect(0, 1, 1, 1.05, col = shade_col, density = 40, border = "transparent")
  
  # --- Plot curves ---
  for (i in seq_len(n)) {
    lines(curves[[i]], lwd = 1.5, col = colors[i], lty = ltys[i])
    
    if (!is.null(labels)) {
      if (is.null(label_pos)) {
        xlab <- tail(curves[[i]][, 1], 1)
        ylab <- tail(curves[[i]][, 2], 1)
      } else {
        xlab <- label_pos[[i]]$x
        ylab <- label_pos[[i]]$y
      }
      text(x = xlab, y = ylab, labels[i], col = label_col[i],
           cex = label_cex, pos = 4)
    }
  }
  
  # --- Optional legend ---
  if (legend && !is.null(labels)) {
    legend(legend_pos, legend = labels,
           col = colors, lty = ltys, lwd = 1.5,
           cex = legend_cex, bg = legend_bg, bty = legend_bty,
           text.col = fg_col)
  }
}



sv.hyp <- function(r, b, n = 1000){
  V  <- seq(from = 0.001,to = 0.999, length = n )
  LV <- log(V/(1-V))
  S  <- (1+exp(-b - r*LV))^-1

  dta <- cbind.data.frame(V,S)

  return(dta)
}


## Example
sv1 <- sv.hyp(r = 3, b = 0.5)
sv2 <- sv.hyp(r = 3, b = -0.5)
sv3 <- sv.hyp(r = 2.5, b = 0)

# Light theme (default)
plot_majoritarian(sv1, sv2, sv3,
                  colors = c("blue", "red", "darkgreen"),
                  labels = c("b = 0.5", "b = -0.5", "b = 0"),
                  legend = TRUE,
                  main = "Seat-Vote Curves — Light Theme")

# Dark theme for slides
plot_majoritarian(sv1, sv2, sv3,
                  colors = c("skyblue", "tomato", "limegreen"),
                  labels = c("b = 0.5", "b = -0.5", "b = 0"),
                  legend = TRUE,
                  theme = "dark",
                  main = "Seat-Vote Curves — Dark Theme")

## Example 2

# Generate the data
majoritarian.sv.dta <- sv.hyp(r = 3, b = 0)
proporional.sv.dta  <- sv.hyp(r = 1, b = 0)
negative.dv.dta     <- sv.hyp(r = 0.50, b = 0)
winner.sv.dta       <- sv.hyp(r = 10000, b = 0)

# Use the new plotting function
plot_majoritarian(
  majoritarian.sv.dta,
  negative.dv.dta,
  winner.sv.dta,
  proporional.sv.dta,
  colors = c("purple", "orange", "darkgreen", "gray50"),
  ltys   = rep(4, 4),
  labels = c("Majoritarian", "Negative Bonus", "Winner-take-all", "Proportional"),
  legend = FALSE,            # we’ll add direct text labels instead
  prop.line = FALSE,         # you’ll draw it manually below
  box = TRUE,
  main = "Seat-Vote Relationships"
)

# Add custom text annotations (same as your manual version)
text(x = 0.23, y = 0.25, "PROPORTIONAL REPRESENTATION", srt = 45, cex = 1, col = "gray50")
text(x = 0.35, y = 0.60, "Winner-take-all", srt = 0, cex = 1, col = "darkgreen")
text(x = 0.61, y = 0.96, "Majoritarian", srt = 0, cex = 1, col = "purple")
text(x = 0.76, y = 0.55, "Negative Bonus", srt = 0, cex = 1, col = "orange")



