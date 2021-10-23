# // Parallel Structure Seats-Votes Curve
nom <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/Elections/Presidential/nominees.csv")
dodgerblue.30 <- rgb(30, 144, 255, 76.5, max =255)
indianred.30 <- rgb(205, 92, 92, 76.5, max =255)
indianred.75 <- rgb(205, 92, 92, 191, max =255)

#TEST 
seq(34,38,1)
i=39

	biasmeans <- gk_sv[[i]]$biasmeans
	votes <- gk_sv[[i]]$election_info$Votes
	seats <- gk_sv[[i]]$election_info$Seats
	year <- gk_sv[[i]]$election_info$year
	range.y=c(0.40,0.95)
	range.x=c(0.40,0.6)

sv_plot(
	biasmeans=bias.sims[[39]], 
	year=year, 
	range.x=c(0.45,0.65), 
	range.y=c(0.45,0.95), 
	votes=votes, 
	seats=seats
	)
# End Test


sv_plot <- function(
	biasmeans, 
	year, 
	range.x=c(0.45,0.65), 
	range.y=c(0.45,0.95), 
	votes, 
	seats,
	ci=F) {
		bias.tmp <- biasmeans
		nom.tmp <- nom[nom$year %in% year,]
		votes.actual <- paste0(round(ifelse(seats>0.5, votes, 1-votes), d=3)*100, "%")
		seats.actual <- paste0(round(ifelse(seats>0.5, seats, 1-seats), d=3)*100, "%")
		range.x.low <- range.x[1]
		range.x.high <- range.x[2]
		range.y.low <- range.y[1]
		range.y.high <- range.y[2]
		par(pty="s", mar=c(4,5,2,2))
			x <- seq(0.450,0.65,0.001)
			plot(NA,
				type="l", 
				xlab="Major Party Vote Share", 
				ylab="Expected Elector Share", 
				axes=F,
				xaxs="i", 
				yaxs="i",
				ylim=c(range.y.low,range.y.high), 
				xlim=c(range.x.low,range.x.high), 
				main=paste0(year, " Presidential Election"))
				if (ci==T) {
				polygon(
					x= c(
						x,
						rev(x)), 
					y= c(
						bias.tmp$SeatShare[101:301]+bias.tmp$SeatSD[101:301], 
						rev(bias.tmp$SeatShare[101:301]-bias.tmp$SeatSD[101:301])), 
					col=dodgerblue.30, 
					border=NA)
				polygon(
					x= c(
						seq(0.45,0.65,0.001),
						rev(seq(0.45,0.65,0.001))), 
					y= c(
						rev(1-bias.tmp$SeatShare[1:201]+bias.tmp$SeatSD[1:201]), 
						1-bias.tmp$SeatShare[1:201]-bias.tmp$SeatSD[1:201]), 
					col=indianred.30, 
					border=NA)
					}
				lines(
					seq(0.45,0.65,0.001), 
					(bias.tmp$SeatShare[101:301]), 
					lty=1)
				lines(
					seq(0.45,0.65,0.001), 
					rev(1-bias.tmp$SeatShare[1:201]), 
					lty=2)
				axis(
					side=1, 
					at=seq(range.x.low,range.x.high,0.05), 
					labels=paste0(seq(range.x.low,range.x.high,0.05)*100, "%"), 
					cex.axis=0.75)
				axis(
					side=1, 
					at=seq(range.x.low,range.x.high,0.01), 
					labels=F, 
					lwd.ticks=0.2, 
					tck=-0.01)
				axis(
					side=2, 
					las=2, 
					at=seq(range.y.low,range.y.high,0.05), 
					labels=paste0(seq(range.y.low*100,range.y.high*100,5), "%"), 
					cex.axis=0.75)
				axis(
					side=2, 
					las=2, 
					at=seq(range.y.low,range.y.high,0.01), 
					labels=F, 
					lwd.ticks=0.2, 
					tck=-0.01)


					points(votes,seats, pch=19, col="blue")
					points(1-votes,1-seats, pch=18, col="red")

				lines(
					x=c(range.x.low,range.x.high), 
					y=c(0.5,0.5),
					lty=1,
					col="gray70")
					
				lines(
					x=c(0.5,0.5), 
					y=c(range.y.low,range.y.high),
					lty=1,
					col="gray70")

				legend(
					0.53,0.90,
						c(
						paste0("Votes: ", votes.actual),
						paste0("Seats: ", seats.actual)),
					pch=c(NA, ifelse(seats>0.5, 19, 18)),
					col=c(NA, ifelse(seats>0.5, "blue", "red")),
					cex=0.8
					)
				legend(
					"bottomright", 
						c(
						paste0(nom.tmp$Democrat, " (D)"), 
						paste0(nom.tmp$Republican, " (R)")), 
					lty=c(
						1,
						2),
					pch=c(
						19,
						18),
					bty="n"
					)
		}

