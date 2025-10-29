options(scipen = 999)
'seatsvotes.plot' <- function(main="", ylim=c(0,1), xlim=c(0,1), xlab="Votes", ylab="Seats", xaxis=TRUE, yaxis=TRUE, prop.line=TRUE, box=TRUE) 
	{
		par(pty="s", mar = c(2.5, 2, 2, 1), mgp = c(0.5, 0.5, 0))
		plot(1, 
		     type = "n", 
		     ylim=ylim, 
		     xlim=xlim,
		     xaxs="i",
		     yaxs="i",
		     ylab="", 
		     xlab="" , 
		     main=main, 
		     bty="n", 
		     axes=F)
			
		if (xaxis==TRUE) {
			axis(side=1, las=2, at=seq(0,1,0.1), labels=F, lwd.ticks=0.4)
			axis(side=1, las=2, at=seq(0,1,0.01), labels=F, lwd.ticks=0.2, tck=-0.01)
			axis(side=1, at=seq(0,1,0.1), labels=paste0(seq(0,100,10), "%"), cex.axis=0.5, col.axis="gray50")
			}
		if (yaxis==TRUE) {
			axis(side=2, las=2, at=seq(0,1,0.1), labels=F, lwd.ticks=0.4)
			axis(side=2, las=2, at=seq(0,1,0.01), labels=F, lwd.ticks=0.2, tck=-0.01)
			axis(side=2, las=2, at=seq(0,1,0.1), labels=paste0(seq(0,100,10), "%"), cex.axis=0.5, col.axis="gray50")
			}
	mtext(xlab, side=1, line = 1.5)
	mtext(ylab, side=2, line = 1.5)
	

	if (prop.line==TRUE) {	
		abline(0,1, lty=2, col="gray90")
		text(.23,.25, "PROPORTIONAL REPRESENTATION", srt=45, cex=.5, col="gray50")
		}
	if (box==TRUE) {
		box()
		abline(v=0.5, lty=3, col="gray40")
		abline(h=0.5, lty=3, col="gray40")
		}
			# rect(-.05,-.05,0,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #left
			# rect(0,0,1.05,-.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #bottom
			# rect(1,0,1.05,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #right
			# rect(0,1,1,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #top
	}

seatsvotes.axis <- function(xmin=0, xmax=1, ymin=0, ymax=1) {
	axis(side=1, las=2, at=seq(xmin,xmax,0.1), labels=F, lwd.ticks=0.4)
	axis(side=1, las=2, at=seq(xmin,xmax,0.01), labels=F, lwd.ticks=0.2, tck=-0.01)
	axis(side=1, at=seq(xmin,xmax,0.1), labels=paste0(seq(xmin*100,xmax*100,10), "%"), cex.axis=0.5, col.axis="gray50")
			
	axis(side=2, las=2, at=seq(ymin,ymax,0.1), labels=F, lwd.ticks=0.4)
	axis(side=2, las=2, at=seq(ymin,ymax,0.01), labels=F, lwd.ticks=0.2, tck=-0.01)
	axis(side=2, las=2, at=seq(ymin,ymax,0.1), labels=paste0(seq(ymin*100,ymax*100,10), "%"), cex.axis=0.5, col.axis="gray50")
			
	}

sv.hyp <- function(r, b, n = 1000){
  V  <- seq(from = 0.001,to = 0.999, length = n )
  LV <- log(V/(1-V))
  S  <- (1+exp(-b - r*LV))^-1

  dta <- cbind.data.frame(V,S)

  return(dta)
}

# seatsvotes.plot()
# majoritarian.sv.dta <- sv.hyp(r = 3, b = 0)
# proporional.sv.dta  <- sv.hyp(r = 1, b = 0)
# negative.dv.dta     <- sv.hyp(r = 0.50, b = 0)
# winner.sv.dta       <- sv.hyp(r = 10000, b = 0)

#           lines(majoritarian.sv.dta, lwd=1.5, col="purple", lty=4)
#           lines(negative.dv.dta, lwd=1.5, col="orange", lty=4)
#           lines(winner.sv.dta, lwd=1.5, col="dark green", lty=4)
#           lines(proporional.sv.dta, lwd=1.5, col="gray50", lty=4)
#                text(x =  0.23, y = 0.25, "PROPORTIONAL REPRESENTATION", srt=45, cex=1, col="gray50")
#                text(x =  0.35, y = 0.60, "Winner-take-all", srt=0, cex=1, col="dark green")
#                text(x =  0.61, y = 0.96, "Majoritarian", srt=0, cex=1, col="purple")
#                text(x =  0.76, y = 0.55, "Negative Bonus", srt=0, cex=1, col="orange")


sv_curve <- function(s,v, lwd=2, col="gray40") {
	reg <- summary(lm(log(sv(s)) ~ log(sv(v))))
	VOTES.tmp <- seq(0,1, by=.01)
	seatvotes <- reg$coefficients[2]*log(VOTES.tmp/(1 - VOTES.tmp)) + reg$coefficients[1]
	funct2 <- function (x) exp(seatvotes) / (1 + exp(seatvotes)) 
	plot(funct2, from=0.0, to=1, add=TRUE, lwd=lwd, col=col)	
}

sv_bias <- function(s,v) {
	reg <- summary(lm(log(sv(s)) ~ log(sv(v))))
	VOTES.tmp <- seq(0,1, by=.01)
	bias <- reg$coefficients[2]*log(0.5/(1 - 0.5)) + reg$coefficients[1]
	funct2 <- function (x) exp(bias) / (1 + exp(bias)) 
	return(0.5 - funct2())
}

sv_responsiveness <- function(s,v) {
	reg <- summary(lm(log(sv(s)) ~ log(sv(v))))
	VOTES.tmp <- seq(0,1, by=.01)
	bias45 <- reg$coefficients[2]*log(0.45/(1 - 0.45)) + reg$coefficients[1]
	bias55 <- reg$coefficients[2]*log(0.55/(1 - 0.55)) + reg$coefficients[1]
	funct2 <- function (x) ((exp(bias55) / (1 + exp(bias55))) - (exp(bias45) / (1 + exp(bias45)))) / 0.1
	return(funct2())
}


sv_hyp_plot <- function(r, b, n = 1000){
			  V  <- seq(from = 0.001,to = 0.999, length = n )
			  LV <- log(V/(1-V))
			  S  <- (1+exp(-b - r*LV))^-1
			  
			  dta <- cbind.data.frame(V,S)
			  
			  return(dta)
			}
## Examples
# svg("/Users/user/Downloads/asymmetry.svg", width=10, height=6)
                  majoritarian.sv.dta <- sv.hyp(r = 3, b = 0.5)
                  majoritarian.sv.dta2 <- sv.hyp(r = 3, b = -0.5)
                         
                                            par(pty="s", mar=c(3.5,2.5,2,1))
                       plot(0,0, ylim=c(0,1), xlim=c(0,1), type="p", pch=19, col="#FFFFFF", main="", xlab="", ylab="", bty="n", axes=F)
                            axis(side=1, las=2, at=seq(0,1,0.1), labels=F, lwd.ticks=0.4, tck=-0.01)
                            axis(side=1, las=2, at=seq(0,1,0.01), labels=F, lwd.ticks=0.2, tck=-0.01)
                            axis(side=1, at=seq(0, 1, 0.25), labels=c("0%", "25%", "50%", "75%", "100%"), cex.axis=0.65)

                            axis(side=2, las=2, at=seq(0,1,0.1), labels=F, lwd.ticks=0.4, tck=-0.01)
                            axis(side=2, las=2, at=seq(0,1,0.01), labels=F, lwd.ticks=0.2, tck=-0.01)
                            axis(side=2, las=2, at=seq(0, 1, 0.25), labels=c("0%", "25%", "50%", "75%", "100%"), cex.axis=0.65)

                                 abline(v=seq(0, 1, 0.25), lty=3, col="gray80")
                                 abline(h=seq(0, 1, 0.25), lty=3, col="gray80")
                                 abline(v=0.5, lty=2, col="gray40")
                                 abline(h=0.5, lty=2, col="gray40")

                                 rect(-.05,-.05,0,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #left
                                 rect(0,0,1.05,-.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #bottom
                                 rect(1,0,1.05,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #right
                                 rect(0,1,1,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #top

                              mtext(side=1, line=2, "% of Two-Party Votes", cex=1)
                              mtext(side=2, line=2.5, "% of Two-Party Seats", cex=1)

                                 lines(majoritarian.sv.dta, lwd=1.5, col="blue", lty=2)
                                 lines(majoritarian.sv.dta2, lwd=1.5, col="red", lty=2)
                                 # text(x =  0.71, y = 0.61, "Asymmetric\nMajoritarian", srt=0, cex=0.65, col="purple")


# dev.off() 

	
