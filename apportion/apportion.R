
#### apportion(data, column with state names, data with state POP, number of  seats to be allocated, number of minimum seats per state, minimum number of votes to be awarded any seat)
##library(tidyverse)
options(scipen=999)
'standardizeText' <- function(m) substrLeft(deleteSpaces(tolower(substrPunct(m))),30)

'divisor' <- function(round="all") {
	if (round == "all") fq <- 1:1000
	if (round == "webster") fq <- seq(1, by = 2, len = 1000)
	if (round == "jefferson") fq <- seq(2, by = 2, len = 1000)
	if (round == "adams") fq <- seq(0, by = 2, len = 1000)
  ## min.autoseats <- fq < autoseats
  ## fq[min.autoseats] <- autoseats
	    return(fq)
			}

'firstquota' <- function(POP, divisor, round="down") {
	if (round == "down") fq <- floor(POP/divisor)
	if (round == "up") fq <- ceiling(POP/divisor)
	if (round == "nearest") fq <- round(POP/divisor)
  ## min.autoseats <- fq < autoseats
  ## fq[min.autoseats] <- autoseats
    return(fq)
	}

'appt' <- function(POP, STATES, nseats=435, autoseats=1, method="webster") {
		divs <- rep(divisor(method), length(POP))
		divisors <- divs[order(divs)]

		POP.matrix <- rep(POP,1000)/divisors
		POP.order <- order(POP.matrix, decreasing=T)
		
		st.matrix <- rep(STATES, 1000)
		appt.tmp <- st.matrix[POP.order]
		appt.first <- as.data.frame(table(appt.tmp[1:nseats]))
			names(appt.first) <- c("st", "appt")
		st.list <- as.data.frame(table(STATES))
			names(st.list) <- c("st", "x")
		appt.temp <- merge(appt.first, st.list, by="st", all=T)
		auto.st <- as.character(appt.temp[,1][is.na(appt.temp[,2])])
		auto.replace <- length(appt.temp[,2][is.na(appt.temp[,2])])
		appt.temp <- appt.tmp[1:(nseats-auto.replace)]
		appt.temp <- c(appt.temp, auto.st)
		appt.temp <- as.data.frame(table(appt.temp))
			names(appt.temp) <- c("st", "seats")
			appt.temp.2 <- merge(appt.temp, st.list, by="st", all=T)
			auto.st2 <- as.character(appt.temp.2[,1][is.na(appt.temp.2[,2])])
			auto.replace <- length(appt.temp.2[,2][is.na(appt.temp.2[,2])]) + auto.replace
			appt.temp.2 <- appt.tmp[1:(nseats-auto.replace)]
			appt.temp.2 <- c(appt.temp.2, auto.st, auto.st2)
			appt.temp.2 <- as.data.frame(table(appt.temp.2))
			names(appt.temp.2) <- c("state", "seats")
		return(appt.temp.2)
	}

'apportion' <- function(POP, STATES, nseats=435, autoseats=1, threshold=0, method="hill-huntington", state="all") {

	if (state != "all") {
			if (! standardizeText(state) %in% standardizeText(STATES)) {
				stop("Specify a State or choose \"all\" under option \"state\".")
			}
		}

	pop <- as.numeric(POP)
	st <- as.character(STATES[!is.na(pop)])
	POP.tmp <- pop[!is.na(pop)]
	POP <- POP.tmp[POP.tmp > threshold]
	st <- st[POP.tmp > threshold]
		nseats.tmp <- nseats-(length(POP) * autoseats)
		sdivisor <- sum(POP)/nseats

		if (length(unique(st)) != length(st)) {
	        stop("every party name must be unique")
	    }
	    if (nseats.tmp < 1 | floor(nseats.tmp) != ceiling(nseats.tmp)) {
	        stop("nseats must be an integer greater than 0")
	    	}

	    if (method == "hamilton") {
	    	fq <- firstquota(POP,sdivisor, "down")
	    	remainder <- POP/sdivisor - fq
	    		fq.zero <- (fq <= 0)
	    		fq[fq.zero] <- 1
	    		remainder[fq.zero] <- 0
	    	seats.remaining <- nseats - sum(fq)
	    	highest.remainder <- order(remainder, decreasing=T)
	    	st <- st[highest.remainder]
	    	fq <- fq[highest.remainder]
	    	POP <- POP[highest.remainder]
	    		remainder <- remainder[highest.remainder]
	    	fq[1:seats.remaining] <- fq[1:seats.remaining]+1
    	appt.tmp <- fq
    	apportionment <- data.frame(state=st, seats=appt.tmp)
	    apportionment <- apportionment[order(apportionment[,"state"]),]
	    }

			if (method == "hill-huntington") {
	       priority <- function(s) 1/(sqrt(s * (s - 1)))
	       priority.num <- list()
	       for (i in 2:60) {
	       priority.num[[i]] <- priority(i) * POP
	   			}
	   	pri <- data.frame(rep(st, 59), do.call(c, priority.num))
			appt.tmp <- pri[order(pri[,2], decreasing=T),][1:nseats.tmp,]
			appt.tmp <- aggregate(appt.tmp[,1], by=list(st=appt.tmp[,1]), FUN=length)
			appt.penultimate <- merge(data.frame(st=st), appt.tmp, all.x=T)
			appt.penultimate[is.na(appt.penultimate)] <- 0
			appt.penultimate[,2] <- appt.penultimate[,2] + autoseats
		    	st <- appt.penultimate[,1]
		    	appt.tmp <- appt.penultimate[,2]
		  apportionment <- data.frame(state=st, seats=appt.tmp)
	    apportionment <- apportionment[order(apportionment[,"state"]),]
	    }

	    if (!method %in% c("hill-huntington", "hamilton")) {
	    	apportionment <- appt(POP, STATES, nseats=435, method=method)
	    }

		if (state == "all") {
		return(apportionment)
			} else {
		return(apportionment$seats[standardizeText(apportionment$state) %in% standardizeText(state)])
	}
}



# █▀▀ ─▀─ █▀▀▄ █▀▀▄ 　 █▀▀ █▀▄▀█ █▀▀█ █── █── █▀▀ █▀▀ ▀▀█▀▀ 　 █▀▀ █──█ █▀▀█ █▀▀▄ █▀▀▀ █▀▀ █▀▀ 
# █▀▀ ▀█▀ █──█ █──█ 　 ▀▀█ █─▀─█ █▄▄█ █── █── █▀▀ ▀▀█ ──█── 　 █── █▀▀█ █▄▄█ █──█ █─▀█ █▀▀ ▀▀█ 
# ▀── ▀▀▀ ▀──▀ ▀▀▀─ 　 ▀▀▀ ▀───▀ ▀──▀ ▀▀▀ ▀▀▀ ▀▀▀ ▀▀▀ ──▀── 　 ▀▀▀ ▀──▀ ▀──▀ ▀──▀ ▀▀▀▀ ▀▀▀ ▀▀▀

'appt.gain' <- function(POP, STATES, method="hill-huntington") {
	appt.change <- data.frame(st=STATES, minPOP=NA)
		a <- apportion(POP,STATES,435, method=method)
		for (j in 1:length(STATES)) {
			POP.j <- POP
			appt.a <- a$seats[a$state %in% STATES[j]]			
			x.test <- rev(seq(0,1000000, by=1000))
				for (i in 1:length(x.test)) {
				POP.j[STATES %in% STATES[j]] <- POP[STATES %in% STATES[j]] + x.test[i]
					b <- apportion(POP.j,STATES,435, method=method)
						(appt.b <- b$seats[b$state %in% STATES[j]])
				if (appt.a == appt.b) {
					break
					}
				}
				if (i == 1) {next}
				cat(STATES[j], ":", x.test[i], "-", x.test[i-1], "\n")
				for (k in 1:1000) {
					cat(".")
					k.seq <- seq(x.test[i], x.test[i-1])
					POP.j[STATES %in% STATES[j]] <- POP[STATES %in% STATES[j]] + (k.seq[k])
					b <- apportion(POP.j,STATES,435, method=method)
						(appt.b <- b$seats[b$state %in% STATES[j]])
					if (appt.a != appt.b) {
					break
					}
				}

			appt.change[j,2] <- k.seq[k]
			cat("\n", STATES[j], ":", k.seq[k], "\n")
		}
		return(appt.change) 
	}


'appt.lose' <- function(POP, STATES, method="jefferson") {
	appt.change <- data.frame(st=STATES, minPOP=NA)
	a <- apportion(POP,STATES,435, method=method)
		for (j in 1:length(STATES)) {
			POP.j <- POP
			appt.a <- a$seats[a$state %in% STATES[j]]
			if (appt.a==1) next
			x.test <- rev(seq(0,1000000, by=1000))
				for (i in 1:length(x.test)) {
				POP.j[STATES %in% STATES[j]] <- POP[STATES %in% STATES[j]] - x.test[i]
					b <- apportion(POP.j,STATES,435, method=method)
						(appt.b <- b$seats[b$state %in% STATES[j]])
				if (appt.a == appt.b) {
					break
					}
				}
				if (i == 1) {next}
				cat(STATES[j], ":", x.test[i], "-", x.test[i-1], "\n")
				for (k in 1:1000) {
					cat(".")
					k.seq <- seq(x.test[i], x.test[i-1])
					POP.j[STATES %in% STATES[j]] <- POP[STATES %in% STATES[j]] - (k.seq[k])
					b <- apportion(POP.j,STATES,435, method=method)
						(appt.b <- b$seats[b$state %in% STATES[j]])
					if (appt.a != appt.b) {
					break
					}
				}

			appt.change[j,2] <- k.seq[k]
			cat("\n", STATES[j], ":", k.seq[k], "\n")
		}
		return(appt.change) 
	}
