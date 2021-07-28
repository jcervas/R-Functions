
`electSim` <- function(
	VOTES, 
	LAGVOTES, 
	TOTAL, 
	SEATS, 
	POP, 
	YEARS, 
	year=NULL, 
	vBar.range=c(0.35, 0.65), 
	n.sims=1000, 
	plot=F, 
	path=NULL,
	sigma=0.06514486, #Historic residual error
	seed=66
	) {
		set.seed(seed)
		sv <- list(
			# sv = list(), 
			# inversions = list(), 
			# sbar = list(), 
			# wins.total = list()
			)
				stopifnot(all.equal(length(VOTES),length(LAGVOTES)))
				# stopifnot(is.null(year))
			stopifnot(!is.null(year))
			asv <- npv <- ppv <- ewv <- rep(NA, length(unique(YEARS))) #create empty vector for avg. state VOTES, 1868-2016
			coefs <- array(NA, c(length(unique(YEARS)), 2)) #create empty matrix to store coefficients
			resid.errors <- rep(NA, length(unique(YEARS))) #empty vector to store residual errors

		for (i in 1:length(unique(YEARS))) {
			y.i <- YEARS %in% unique(YEARS)[i]
		    asv[i] <- mean.w(VOTES[y.i]) #get asv for each year, 1868-2016
		    npv[i] <- mean.w(VOTES[y.i], TOTAL[y.i]) #get npv for each year, 1868-2016
		    ppv[i] <- mean.w(VOTES[y.i], POP[y.i]) #get ppv for each year, 1868-2016
		    ewv[i] <- mean.w(VOTES[y.i], SEATS[y.i]) #get wev for each year, 1868-2016
				n = length(VOTES[y.i])
				LAGVOTES[y.i][is.na(LAGVOTES[y.i])] <- VOTES[y.i][is.na(LAGVOTES[y.i])]
		    fit <- lm(VOTES[y.i] ~ LAGVOTES[y.i])
		    coefs[i,] <- coef(fit)
		    resid.errors[i] <- sqrt(deviance(fit)/(n-2))
		        }
			resid.errors[resid.errors<0.01] <- sigma
		# Start looping through year
			# for (k in 1:length(year)) {
				cat(paste0("\n",year, "..."))

				y.k <- YEARS %in% year
				year.index <- match(year, unique(YEARS))

				start.year.indicator <- year.index-2 #two elections prior
					if (year.index == 1L) start.year.indicator <- k
					if (year.index == 2L) start.year.indicator <- (k-1)
				end.year.indicator <- year.index #current election

				rho <-  mean(coefs[start.year.indicator:end.year.indicator,2]) #get rho by taking mean coef from 3 year leading up to election year: 
				(sigma <- mean(resid.errors[start.year.indicator:end.year.indicator])) #get mean residual standard error from past 3 year

				dvote <- VOTES[y.k]
				VOTES.denominator <- TOTAL[y.k]
				ecvotes.k <- SEATS[y.k]
				dvote.imp <- default.unc(VOTES[y.k]) #imputed VOTES
				dvote.lag.k <- default.unc(LAGVOTES[y.k]) #use imputed lagged VOTES for prediction

				vbar.range <- vBar.range #create range from 35 to 65
				inv.j.d <- inv.j.r <- min.shift.j <- nca.j <- sbar.50.sd <- sbar.50 <- sbar.5 <- sbar.95 <- sbar.1 <- sbar.99 <- rep(NA, length(vbar.range)) #empty vectors for storing results
				s50 <- dvote.j <- sbar_full <- list()
			# Start looping over interavls of vbar_j
				for (j in 1:length(vbar.range)){
					cat(".")
					vbar <- vbar.range[j]
					inv.tmp.d <- inv.tmp.r <- sbar <- min.shift.i <- nca.i <- rep(NA, n.sims) 
					dvote.i  <- list()
				# Start looping through 1,000 simulations at vbar_i
					for (i in 1:n.sims){
						noisy <- rnorm(length(dvote.imp), dvote.imp, sigma) #add noise
						predict.d <- noisy + vbar - mean.w(noisy,VOTES.denominator)
						# winner <- find.winner(predict.d) #winner of each race (1 for Dems, 0 for GOP)
						sbar[i] <- sum(find.winner(predict.d) * ecvotes.k)/sum(ecvotes.k) #percent of seats
						dvote.i[[i]] <- predict.d
						min.shift.i[i] <- min.shift(predict.d, ecvotes.k)
						nca.i[i] <- nca(predict.d, ecvotes.k, VOTES.denominator)
						inv.tmp.d[i] <- 0 + (1 * (sbar[i] < 0.5 & vbar >= 0.5))
						inv.tmp.r[i] <- 0 + (1 * (sbar[i] > 0.5 & vbar <= 0.5))
					}
					dvote.j[[j]] <- dvote.i
					min.shift.j[j] <- mean(min.shift.i)
					nca.j[j] <- mean(nca.i)
					sbar.50[j] <- mean(sbar)
					sbar.50.sd[j] <- sd(sbar)
					# s50[[j]] <- NA
					s50[[j]] <- rep(vbar, length(sbar[round(sbar,d=2) == 0.5]))
					sbar.5[j] <- quantile(sbar, 0.05)
					sbar.95[j] <- quantile(sbar, 0.95)
					sbar.1[j] <- quantile(sbar, 0.01)
					sbar.99[j] <- quantile(sbar, 0.99)
					inv.j.d[j] <- sum(inv.tmp.d)
					inv.j.r[j] <- sum(inv.tmp.r)
					# if (vbar[j] == 0.5) {sbar_full[[j]] <- sbar}
				}
				if (!is.null(path)) {writeLines(jsonlite::toJSON(dvote.j, pretty=T, auto_unbox = T, na= "string"), paste0(path, "/sims_", year, ".json"))}
				# sbar_full.u <- unlist(sbar_full)
			sv$election_info <- data.frame(year=year, Votes=mean.w(dvote,VOTES.denominator), Seats=sum(find.winner(dvote) * ecvotes.k)/sum(ecvotes.k), total_EC=sum(ecvotes.k))
			sv$biasmeans <- cbind.data.frame(VoteShare=vbar.range, SeatShare=sbar.50, SeatSD=sbar.50.sd, One=sbar.1, Five=sbar.5, NintyFive=sbar.95, NintyNine=sbar.99, MinShift=min.shift.j, NCA=nca.j, Inversions_Dem=inv.j.d, Inversions_Rep=inv.j.r)
			# sv$sbar <- sbar_full
			sv$votebias <- mean(unlist(s50))
			sv$seatbias <- sbar.50[151]
			# sv$seats50 <- sbar_full
			# sv$dvote <- dvote.j
		}
			# rm(dvote.j,sbar_full,dvote.i)
		return(invisible(sv))
	}

