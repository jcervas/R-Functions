cat("Seats-Votes Function - v1.0")



bias <- 
	function(x, ...) {
		tmp <- SEATSvotes(VOTES = x)
			return(tmp["bias"])
	}

swing_ratio <- 
	function(x, ...) {
		tmp <- SEATSvotes(VOTES = x)
			return(tmp["swing_ratio"])
	}

centre <- function(x, w = NULL, type) {
	if(is.null(w)) w <- rep(1,length(x))
  switch(type,
         fifty = x - (mean.w(x, w) - 0.5),
         actual = x,
         average = x - (mean(x, na.rm=TRUE) - 0.5)
         )
	}

if.inv <- function(R,SeatShare) {1 * (R < 0.5 & SeatShare > 0.5) + 1 * (R > 0.5 & SeatShare < 0.5)}

'bias' <- function(x) as.numeric(seatsvotes(x)[2])

'seatsvotes' <- 
	function(
		VOTES = NULL,  
		DEMvotes = NULL, 
		REPvotes = NULL,
		SEATS = NULL,  
		POP = NULL,
		year = NULL, 
		center = "fifty", # center options ["fifty", "actual", "average"]
		iterations = 0.0015, 
		vBar.range = c(0.4, 0.6),
		output = "default",
		...
		) {
			if (is.null(VOTES)) 
			{	if (is.null(DEMvotes)) stop("No Election data found")
				if(length(DEMvotes)!=length(REPvotes)) stop("Parties don't have the same number of districts")
				VOTES <- two_party(DEMvotes, REPvotes)	}
			if (is.null(POP)) {
				POP <- rep(1, length(VOTES))
				}
				# cat(year, "\nPopulation Weighted Equally\n")}
			if (is.null(year)) {
				year <- "MISSING"
				cat("Year Missing\n")}

			sim <- seq(vBar.range[1], vBar.range[2], by= iterations)
			coefs <- array(NA, c(length(sim), 3))
			colnames(coefs) <- c("Seats", "Votes", "Sim")
			if (is.null(SEATS)){
				SEATS <- rep(1, length(VOTES))
			}
# =================================================================
# -- SIMULATE -- SIMULATE -- SIMULATE --  SIMULATE -- SIMULATE -- S 
# =================================================================

x.dem <- truncate(centre(x=VOTES, w=POP, type=center)) #default center x - (mean.w(x, w) - 0.5)
mean.w(x.dem,POP)
		for (i in 1:length(sim))
			{
				VOTES.tmp <- x.dem + (0.5 - sim[i])
				coefs[i,1] <- sum(find.winner(VOTES.tmp) * SEATS)/sum(SEATS)	#SEATS
				coefs[i,2] <- mean.w(VOTES.tmp, POP)		#Votes
				coefs[i,3] <- 0.5 - sim[i]
			}
				
		coefs2 <- coefs
		coefs2[,1] <- truncate(coefs2[,1])
		coefs2[,2] <- truncate(coefs2[,2]) 
		

		# x.dem <- truncate(centre(x=VOTES, w=POP, type=center)) #default center x - (mean.w(x, w) - 0.5)
		# 	times <- length(sim)
		# 	a <- t(do.call(rbind, lapply(x.dem, rep.int, times = times)))
		# 	a.baseline <- truncate(a + (sim-0.5))
		# 	a.sim <- split(a.baseline, seq(nrow(a.baseline)))
		# 	a.seats <- rowSums(find.winner(a.baseline)*SEATS)/sum(SEATS)
		# 	a.votes <- as.numeric(do.call(rbind, lapply(a.sim, function(x,p=POP) mean.w(x, p))))
		# y <- sv(a.seats)
		# x <- sv(a.votes)
		# logged <- cbind.data.frame(y=log(y),x=log(x))
		# summary(reg <- lm(y~x, data=logged))
# Toy Example
	# x.nb <- sv(seq(0.01,.99, 0.01))
	# y.nb <- sv(cube(seq(0.01,.99, 0.01)))

	# summary(toy <- lm (log(y.nb) ~ log(x.nb)))
			# 1 / (exp(toy$coefficients[1]) / toy$coefficients[2] + 1) - 0.5  # Bias from Grofman 1983 equation 16
			# (exp(log(1))/( 1 + exp(log(1)))) - .5  #No Bias
		# (exp(log(1))/( 1 + exp(log(1)))) - .5
			# mean(2*(coefs[,1][round(coefs[,2],2)==.50]-.5))
# =================================================================
# -- COLLECT DATA -- COLLECT DATA -- COLLECT DATA --  COLLECT DATA 
# =================================================================
		bias.out <- data.frame(year = year, 
			intercept=NA, 
			intercept_se=NA,
			intercept_Pr=NA, 
			swing_ratio=NA, 
			swing_ratio_se=NA,
			swing_ratio_Pr=NA,
			Log_Odds_SEATS=NA, 
			Linear_Regression_SEATS=NA, 
			Bias_low=NA,
			Bias_point=NA, 
			Bias_high=NA,
			ActualSEATS=NA,
			ActualVotes=NA,
			vote_bias=NA,
			seat_bias=NA
			)
		
	s <- coefs2[,1]
	v <- coefs2[,2]

summary(reglin <- lm(s ~ v))  #Linear Regression
summary(reg <- lm (log(sv(s)) ~ log(sv(v))))   #### Log-Odds Regression
# plot(coefs2[,2], coefs2[,1], main=years[i], xlab="Votes", ylab="Seats")
# abline(v=0.5, lty=3, col="gray70")
# abline(h=0.5, lty=3, col="gray70")
# abline(reglin, lwd=2)


				# cat("YEAR:", year, " \n")
				# cat("SEATS AT 50% VOTES = ", round(inv(((reg$coefficients[2] * log(sv(0.5))) + reg$coefficients[1])), digits=3) * 100, "%\n")
				# cat("BIAS % = ", round(inv(reg$coefficients[1]) - 0.5, digits=3) * 100, "%\n\n")
		bias.out$intercept <- round(reg$coefficients[1], 3)  #### Bias from Grofman 1983 equation 26
		bias.out$intercept_se <- round(summary(reg)$coef[1,2], 3)
		bias.out$intercept_Pr <- round(summary(reg)$coef[1,4], 5)
			# round(1 / (exp(reg$coefficients[1]) / reg$coefficients[2] + 1) - 0.5, 3)  # Bias from Grofman 1983 equation 16
		bias.out$swing_ratio <- round(reg$coefficients[2], 3)
		bias.out$swing_ratio_se <- round(summary(reg)$coef[2,2], 3)
		bias.out$swing_ratio_Pr <- round(summary(reg)$coef[2,4], 5)
		bias.out$Linear_Regression_SEATS <- paste0(round(100 * (reglin$coefficients[1]+(reglin$coefficients[2] * 0.5) ), digits=3), "%")	# Seat Predictions from Linear Regression
		bias.out$Log_Odds_SEATS <- round(100 * inv(reg$coefficients[1]), 3)	#Seat Prediction from Log-Odds Model
		bias.out$Bias_low <- round(inv(reg$coefficients[1] - (1.96 * summary(reg)$coef[1,2])) - 0.5, 3)
		bias.out$Bias_point <- round(inv(reg$coefficients[1]) - 0.5, 3)
		bias.out$Bias_high <- round(inv(reg$coefficients[1] + (1.96 * summary(reg)$coef[1,2]))- 0.5, 3) 
		bias.out$ActualSEATS <- round(sum(find.winner(VOTES) * SEATS)/sum(SEATS), 3)
		bias.out$ActualVotes <- round(mean.w(VOTES, POP), 3)

		bias.out$vote_bias <- 0.5-inv((-1*reg$coefficients[1])/reg$coefficients[2]) #set y to zero and solve
		bias.out$seat_bias <- inv(reg$coefficients[1])-0.5 #set y to zero and solve
		x <- bias.out 
		if (output == "regression") return(summary(reg))
		return(x)
	}




'biasplot' <- 
function(sv) {
	par(mar=c(3,3,0,0))
	plot(sv[,"year"], sv[,"Bias_point.(Intercept)"], ylim=c(.25,.75), xlim=c(1868,2016), type="n", yaxt="n", xaxt="n", xlab="", ylab="", bty="n")
	axis(side=2, las=2, labels=c("25%", "30%", "35%", "40%", "45%", "50%", "55%", "60%", "65%", "70%", "75%"), at=seq(0.25,0.75,0.05), cex.axis=.65)
	axis(side=1, at=seq(1872, 2016, 16), cex.axis=.65)
	mtext(side=2, line=2.25, "Expected Democratic Seat Share at 50% VOTES", cex=0.65)

	for (i in 1:length(sv[,1]))
	{
	xx <- c(sv[i,"year"], rev(sv[i,"year"]))
	yy <- c(sv[i,"Bias_high.(Intercept)"],rev(sv[i,"Bias_low.(Intercept)"]))

	lines(sv[i,"year"], sv[i,"Bias_low.(Intercept)"], lty=2, col= greycol)
	lines(sv[i,"year"], sv[i,"Bias_high.(Intercept)"], lty=2, col= greycol)
	polygon(xx,yy, col= "black", density=20)
	 }
	abline(h=.5005, lwd=2, lty=2, col="gray80")
	abline(h=.50, lwd=2, lty=2, col="gray20")
	points(sv[,"year"], sv[,"Bias_point.(Intercept)"], pch=ifelse(sv[,"Bias_low.(Intercept)"]>50, ifelse(sv[,"Bias_high.(Intercept)"]<50, 1, 19), 1))
	points(sv[,"year"], sv[,"Bias_point.(Intercept)"], pch=ifelse(sv[,"Bias_low.(Intercept)"]<50, ifelse(sv[,"Bias_high.(Intercept)"]>50, 1, 19), 1))
	text(1863, 60, "Pro-Democratic Bias", cex=0.65, srt=90, pos=1, col="gray30")
	text(1863, 40, "Pro-Republican Bias", cex=0.65, srt=90, pos=1, col="gray30")
	}



# -----------------------------------------

