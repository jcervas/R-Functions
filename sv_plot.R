# // Parallel Structure Seats-Votes Curve

sv_plot <- function(biasmeans,year) {
	bias.tmp <- biasmeans
	par(pty="s", mar=c(4,5,2,2))
	x <- seq(0.50,0.65,0.001)
	plot(x, bias.tmp$SeatShare[151:301],
		type="l", 
		xlab="Major Party Vote Share", 
		ylab="Expected Elector Share", 
		axes=F,
		xaxs="i", 
		yaxs="i",
		ylim=c(0.45,0.95), 
		xlim=c(0.5,0.65), 
		main=paste0(year, " Presidential Election"))
		# polygon(x=c(x, rev(x)), y= c(0, y[x1:x2], 0), col="gray70", density=40, border=NA)
		polygon(
			x= c(
				x,
				rev(x)), 
			y= c(
				bias.tmp$SeatShare[151:301]+bias.tmp$SeatSD[151:301], 
				rev(bias.tmp$SeatShare[151:301]-bias.tmp$SeatSD[151:301])), 
			col="#11111111", 
			border=NA)
		polygon(
			x= c(
				x,
				rev(x)), 
			y= c(
				rev(1-bias.tmp$SeatShare[1:151]+bias.tmp$SeatSD[1:151]), 
				1-bias.tmp$SeatShare[1:151]-bias.tmp$SeatSD[1:151]), 
			col="#11111111", 
			border=NA)
		lines(
			x, 
			rev(1-bias.tmp$SeatShare[1:151]), 
			lty=2)
		axis(
			side=1, 
			at=seq(0.50,0.65,0.01), 
			labels=paste0(seq(0.50,0.65,0.01)*100, "%"), 
			cex.axis=0.75)
		axis(
			side=1, 
			at=seq(0.50,0.65,0.005), 
			labels=F, 
			lwd.ticks=0.2, 
			tck=-0.01)
		axis(
			side=2, 
			las=2, 
			at=seq(0.450,0.95,0.05), 
			labels=paste0(seq(45,95,5), "%"), 
			cex.axis=0.75)
		axis(
			side=2, 
			las=2, 
			at=seq(0.450,0.95,0.01), 
			labels=F, 
			lwd.ticks=0.2, 
			tck=-0.01)
		abline(
			h=0.5, 
			lty=3)

			legend(
				"bottomright", 
				c(
					"Biden (D)", 
					"Trump (R)"), 
				lty=c(
					1,
					2)
				)
	}

