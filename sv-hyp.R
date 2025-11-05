sv_plot_base <- function(main = "",
                         xlab = "% of Two-Party Votes",
                         ylab = "% of Two-Party Seats",
                         xaxis = TRUE,
                         yaxis = TRUE,
                         prop.line = TRUE,
                         box = TRUE,
                         xlim = c(0, 1),
                         ylim = c(0, 1),
                         theme = "light") {

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

  par(pty = "s", mar = c(2.5, 2, 2, 1), mgp = c(0.5, 0.5, 0), bg = bg_col)
  plot(1,
       type = "n",
       ylim = ylim, xlim = xlim,
       xaxs = "i", yaxs = "i",
       ylab = "", xlab = "",
       main = main, bty = "n",
       axes = FALSE, col.main = fg_col)

  if (xaxis) {
    axis(side = 1, las = 2, at = seq(0, 1, 0.1), labels = FALSE, lwd.ticks = 0.4, col = axis_col)
    axis(side = 1, las = 2, at = seq(0, 1, 0.01), labels = FALSE, lwd.ticks = 0.2, tck = -0.01, col = axis_col)
    axis(side = 1, at = seq(0, 1, 0.1),
         labels = paste0(seq(0, 100, 10), "%"), cex.axis = 0.5, col.axis = axis_col)
  }

  if (yaxis) {
    axis(side = 2, las = 2, at = seq(0, 1, 0.1), labels = FALSE, lwd.ticks = 0.4, col = axis_col)
    axis(side = 2, las = 2, at = seq(0, 1, 0.01), labels = FALSE, lwd.ticks = 0.2, tck = -0.01, col = axis_col)
    axis(side = 2, at = seq(0, 1, 0.1),
         labels = paste0(seq(0, 100, 10), "%"), cex.axis = 0.5, col.axis = axis_col)
  }

  mtext(xlab, side = 1, line = 1.5, col = fg_col)
  mtext(ylab, side = 2, line = 1.5, col = fg_col)

  if (prop.line) {
    abline(0, 1, lty = 2, col = grid_col)
    text(0.23, 0.25, "PROPORTIONAL REPRESENTATION",
         srt = 45, cex = 0.5, col = axis_col)
  }

  if (box) {
    box(col = axis_col)
    abline(v = 0.5, lty = 3, col = axis_col)
    abline(h = 0.5, lty = 3, col = axis_col)
  }

  rect(-.05, -.05, 0, 1.05, col = shade_col, density = 40, border = "transparent")
  rect(0, 0, 1.05, -.05, col = shade_col, density = 40, border = "transparent")
  rect(1, 0, 1.05, 1.05, col = shade_col, density = 40, border = "transparent")
  rect(0, 1, 1, 1.05, col = shade_col, density = 40, border = "transparent")
}


sv_add_curve <- function(curve,
                         color = "blue",
                         lty = 2,
                         label = NULL,
                         label_pos = NULL,
                         label_col = NULL,
                         label_cex = 0.6) {

  lines(curve, lwd = 1.5, col = color, lty = lty)

  if (!is.null(label)) {
    if (is.null(label_pos)) {
      xlab <- tail(curve[, 1], 1)
      ylab <- tail(curve[, 2], 1)
    } else {
      xlab <- label_pos$x
      ylab <- label_pos$y
    }
    if (is.null(label_col)) label_col <- color
    text(xlab, ylab, label, col = label_col, cex = label_cex, pos = 4)
  }
}

sv.hyp <- function(r, b, n = 1000){
  V  <- seq(from = 0.001,to = 0.999, length = n )
  LV <- log(V/(1-V))
  S  <- (1+exp(-b - r*LV))^-1

  dta <- cbind.data.frame(V,S)

  return(dta)
}

sv_curve <- function(s,v, lwd=2, col="gray40") {
  sv <- function(x) (x / (1 - x))
	reg <- summary(lm(log(sv(s)) ~ log(sv(v))))
	VOTES.tmp <- seq(0,1, by=.01)
	seatvotes <- reg$coefficients[2]*log(VOTES.tmp/(1 - VOTES.tmp)) + reg$coefficients[1]
	funct2 <- function (x) exp(seatvotes) / (1 + exp(seatvotes)) 
	plot(funct2, from=0.0, to=1, add=TRUE, lwd=lwd, col=col)	
}


# Example usage
# Generate data
sv1 <- sv.hyp(r = 3, b = 0.5)
sv2 <- sv.hyp(r = 3, b = -0.5)
sv3 <- sv.hyp(r = 2.5, b = 0)

# Draw base once
sv_plot_base(main = "Seat-Vote Curves — Modular Base")

# Add curves one by one
sv_add_curve(sv1, color = "blue", label = "b = 0.5")
sv_add_curve(sv2, color = "red", label = "b = -0.5")
sv_add_curve(sv3, color = "darkgreen", label = "b = 0")

# You can add additional annotations or lines freely
abline(v = 0.5, col = "gray40", lty = 3)
text(0.75, 0.5, "Custom annotation", col = "gray40", cex = 0.7)



# A wrapper for one-shot plotting
sv_plot <- function(..., main = "", theme = "light", labels = NULL, colors = NULL, ltys = NULL, ...) {
  curves <- list(...)
  n <- length(curves)
  sv_plot_base(main = main, theme = theme)
  if (is.null(colors)) colors <- rep_len(c("blue", "red", "green"), n)
  if (is.null(ltys)) ltys <- rep_len(2, n)
  for (i in seq_len(n)) {
    sv_add_curve(curves[[i]], color = colors[i],
                 lty = ltys[i],
                 label = if (!is.null(labels)) labels[i] else NULL)
  }
}
