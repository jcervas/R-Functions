			sv.hyp <- function(r, b, n = 1000){
			  V  <- seq(from = 0.001,to = 0.999, length = n )
			  LV <- log(V/(1-V))
			  S  <- (1+exp(-b - r*LV))^-1
			  
			  dta <- cbind.data.frame(V,S)
			  
			  return(dta)
			}
				majoritarian.sv.dta <- sv.hyp(r = 3, b = 0)
				proporional.sv.dta  <- sv.hyp(r = 1, b = 0)
				negative.dv.dta     <- sv.hyp(r = 0.50, b = 0)
				winner.sv.dta       <- sv.hyp(r = 10000, b = 0)

				'svplot' <- function (v,s, path=NULL) {
					if (!is.null(path)) {
						svg(path, width=10, height=6)
					}
					par(pty="s", mar=c(3.5,2.5,2,1))
					plot(v,s, ylim=c(0,1), xlim=c(0,1), type="p", pch=19, col="#FFFFFF", main="Symmetry", xlab="", ylab="", bty="n", axes=F)
						axis(side=1, las=2, at=seq(0,1,0.1), labels=F, lwd.ticks=0.4)
						axis(side=1, las=2, at=seq(0,1,0.01), labels=F, lwd.ticks=0.2, tck=-0.01)
						axis(side=1, at=seq(0, 1, 0.2), labels=c("0%", "20%", "40%", "60%", "80%", "100%"))

						axis(side=2, las=2, at=seq(0,1,0.1), labels=F, lwd.ticks=0.4)
						axis(side=2, las=2, at=seq(0,1,0.01), labels=F, lwd.ticks=0.2, tck=-0.01)
						axis(side=2, las=2, at=seq(0,1, 0.2), labels=c("0%", "20%", "40%", "60%", "80%", "100%"))

					abline(v=seq(0,1,0.2), lty=3, col="gray80")
					abline(h=seq(0,1,0.2), lty=3, col="gray80")
					abline(v=0.5, lty=2, col="gray40")
					abline(h=0.5, lty=2, col="gray40")
							rect(-.05,-.05,0,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #left
							rect(0,0,1.05,-.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #bottom
							rect(1,0,1.05,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #right
							rect(0,1,1,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #top


							lines(majoritarian.sv.dta, lwd=1.5, col="purple", lty=2)
							lines(proporional.sv.dta, lwd=1.5, col="gray50", lty=4)
							lines(winner.sv.dta, lwd=1.5, col="dark green", lty=5)
							lines(negative.dv.dta, lwd=1.5, col="orange", lty=3)

					text(x =  0.61, y = 0.96, "Majoritarian", srt=0, cex=1, col="purple")
					text(x =  0.23, y = 0.25, "Proportional Representation", srt=45, cex=1, col="gray50")
					text(x =  0.35, y = 0.60, "Winner-take-all", srt=0, cex=1, col="dark green")
					text(x =  0.76, y = 0.55, "Negative Bonus", srt=0, cex=1, col="orange")

						mtext(side=1, line=2, "% of Two-Party Votes", cex=1)
						mtext(side=2, line=3, "% of Two-Party Seats", cex=1)
						if (!is.null(path)) {
							dev.off() 
							}
				}

	
