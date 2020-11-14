

`CI` <- 
	function(x, se, n=1000, level=0.95) {
		if (level==0.95) z <- qt(0.975, df=n-1)
		if (level==0.90) z <- qt(0.95, df=n-1)
		ci <- rbind(x, x - (z * se), x + (z * se))
		rownames(ci) <- c("beta", "lower", "upper")
		return(ci)
	}
	

round.ceiling <- function(x, d) ceiling(x/d)*d
round.floor <- function(x, d) floor(x/d)*d



plot.coef.bars <- 
	function (betas1, betas2, se1, se2, cat1=NULL, cat2=NULL, main=NULL, labels=NULL, xlab=NULL, ylab=NULL, ylim=NULL) {
			data1 <- CI(betas1, se1, level=0.95)
			data2 <- CI(betas2, se2, level=0.95)
			legend.placement <- ((data2[3,4]-data2[1,4])/2)+data2[1,4]
			xx <- seq(1,length(xlab),1)
			xx2 <- xx + 0.15
			yy <- data1[1,]
			yy2 <- data2[1,]

				values <- c(data1, data2)
				if(is.null(ylim)) ylim <- c(min(round.floor(values, 0.05))-0.05, max(round.ceiling(values, 0.05))+0.05)
				
		par(mar=c(3,5.5,4,2))
			plot(x = xx, y = yy, xlim = c(0.5, length(xlab)+0.5), ylim = ylim, main = main, type = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
			axis(side=2, las=2, labels= seq(ylim[1], ylim[2],0.05), at=seq(ylim[1], ylim[2],0.05), cex.axis=1, font=2)
			axis(side=1, at = seq(1.12,4.12,1), labels = xlab, cex.axis=1, lwd=0, lwd.ticks=0, font=2)
			mtext(side=2, line= 3, ylab, cex=1)
			abline(h=seq(ylim[1], ylim[2], 0.05), lty=2, lwd=0.5, col="gray80")
			abline(h=0, lwd=1.5, lty=2, col="gray20")


			arrows(y0=data1[2,], x0=xx, y1=data1[3,], x1=xx, code=3, angle = 90, length=0.1, lwd=1.5)
			points(xx, data1[1,], cex=1.5, pch=19)
			# points(xx, yy, cex=3, pch=ifelse(donate_F_low>0, ifelse(donate_F_high<0, 1, 19), 1))
			# points(xx, yy, pch=ifelse(donate_low<0, ifelse(donate_high>0, 1, 19), 1))

			arrows(y0=data2[2,], x0=xx2, y1=data2[3,], x1=xx2, code=3, angle=90, length=0.1, lty=1, col="gray40", lwd=1.5)
			points(xx2, data2[1,], cex=1.5, col="gray40", pch=19)
			# points(xx2, yy2, cex=3, col="gray40", pch=ifelse(donate_M_low>0, ifelse(donate_M_high<0, 1, 19), 1))
						text(xx, data1[2,] - 0.02, labels=cat1)
						text(xx2, data2[2,] - 0.02, labels=cat2)

# 	lines(xy.coords(c(4.18, 4.5), c(legend.placement, legend.placement)))
# arrows(4.5, legend.placement, 4.25, 0.39, length = 0.2, col='black')
# text(4, 0.39, labels="95% Confidence Interval", col='black', pos=3, cex=0.85)

# 	lines(xy.coords(c(3.7, 3.94), c(data1[1,4], data1[1,4])))
# arrows(3.7, data1[1,4], 3.75, 0.35, length = 0.2, col='black')
# text(3.75, 0.35, labels="Point Estimate", col='black', pos=3, cex=0.85)
		}


for (j in 1:length(title)) {
	pdf(paste0("/Users/cervas/Google Drive/Projects/Uhlaner/March2019_MPSA/Figures/pdf/", names[j] ,".pdf"), width = 8, height = 7)
		par(mfrow=c(1,1))

	main <- paste("Marginal Effect of Norm to", get(title[j]),"\nOLS Coefficient & 95% Confidence Intervals")
	ylab <- get(ylabel[j])
	xlab <- c("Non-Latinx \nWhite", "Latinx", "African \nAmerican", "Asian \nAmerican")
	ylim <- c(-0.05, 0.25)
	if(j==7) ylim <- c(-0.05,0.5)
	plot.coef.bars(betas1=get(beta1[j]), betas2=get(beta2[j]), se1=get(se1[j]), se2=get(se2[j]), cat1="F", cat2="M", main=main, xlab=xlab, ylab=ylab, ylim=ylim)
		dev.off()
	}

for (j in 1:length(title)) {
	# png(paste0("/Users/cervas/Google Drive/Projects/Uhlaner/March2019_MPSA/Figures/png/", names[j], ".png"), units="px", width=800,height=(600))
	png(paste0("/Users/cervas/Google Drive/Projects/Uhlaner/March2019_MPSA/Figures/png/", names[j], ".png"), units="in", res=500, width = 8, height = 7)
		par(mfrow=c(1,1))

	main <- paste("Marginal Effect of Norm to", get(title[j]),"\nOLS Coefficient & 95% Confidence Intervals")
	ylab <- get(ylabel[j])
	xlab <- c("Non-Latinx \nWhite", "Latinx", "African \nAmerican", "Asian \nAmerican")
	ylim <- c(-0.05, 0.25)
	if(j==7) ylim <- c(-0.05,0.5)
	plot.coef.bars(betas1=get(beta1[j]), betas2=get(beta2[j]), se1=get(se1[j]), se2=get(se2[j]), cat1="F", cat2="M", main=main, xlab=xlab, ylab=ylab, ylim=ylim)
		dev.off()
	}







