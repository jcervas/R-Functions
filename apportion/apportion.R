
#### apportion(data, column with state names, data with state POP, number of  seats to be allocated, number of minimum seats per state, minimum number of votes to be awarded any seat)
library(tidyverse)
options(scipen=999)
apportion <- function(STATES, POP, n_seats=435, autoseats=1, threshold=0, method = "hill-huntington", state = "all") {
require(tidyverse)
	'firstquota' <- function(pop, divisor, round="down") {
		if (round == "down") fq <- floor(pop/divisor)
		if (round == "up") fq <- ceiling(pop/divisor)
		if (round == "nearest") fq <- round(pop/divisor)
		# min.autoseats <- fq < autoseats
  #   	fq[min.autoseats] <- autoseats
    return(fq)
	}
	'standardizeText' <- function(m) substrLeft(deleteSpaces(tolower(substrPunct(m))),30)

	appt <- function(pop, states, nseats=435, method="webster") {
			'divisor' <- function(round="hill-huntington") {
				if (round == "all") fq <- 1:1000
				if (round == "webster") fq <- seq(1, by = 2, len = 1000)
				if (round == "jefferson") fq <- seq(2, by = 2, len = 1000)
				if (round == "adams") fq <- seq(0, by = 2, len = 1000)
				# min.autoseats <- fq < autoseats
		  #   	fq[min.autoseats] <- autoseats
		    return(fq)
				}
		divisors <- rep(divisor(method), length(pop))
			divisors <- divisors[order(divisors)]

		pop.matrix <- rep(pop,1000)/divisors
			pop.order <- order(pop.matrix, decreasing=T)

		st.matrix <- rep(states, 1000)
		appt.tmp <- st.matrix[pop.order]
		appt.first <- as.data.frame(table(appt.tmp[1:nseats]))
			names(appt.first) <- c("st", "appt")
		st.list <- as.data.frame(table(states))
			names(st.list) <- c("st", "x")
		appt.temp <- full_join(appt.first, st.list, by="st")
		auto.st <- as.character(appt.temp[,1][is.na(appt.temp[,2])])
		auto.replace <- length(appt.temp[,2][is.na(appt.temp[,2])])
		appt.temp <- appt.tmp[1:(nseats-auto.replace)]
		appt.temp <- c(appt.temp, auto.st)
		appt.temp <- as.data.frame(table(appt.temp))
			names(appt.temp) <- c("state", "apportionment")
		return(appt.temp)
	}


	if (state != "all") {
			if (! standardizeText(state) %in% standardizeText(STATES)) {
				stop("Specify a State or choose \"all\" under option \"state\".")
			}
		}
	POP <- as.numeric(POP)
	st <- as.character(STATES[!is.na(POP)])
	pop.tmp <- POP[!is.na(POP)]
	pop <- pop.tmp[pop.tmp > threshold]
	st <- st[pop.tmp > threshold]
		n_seats.tmp <- n_seats-(length(pop) * autoseats)
		sdivisor <- sum(pop)/n_seats

		if (length(unique(st)) != length(st)) {
	        stop("every party name must be unique")
	    }
	    if (n_seats.tmp < 1 | floor(n_seats.tmp) != ceiling(n_seats.tmp)) {
	        stop("n_seats must be an integer greater than 0")
	    }

	    if (method == "hamilton") {
	    	fq <- firstquota(pop,sdivisor, "down")
	    	remainder <- pop/sdivisor - fq
	    		fq.zero <- (fq <= 0)
	    		fq[fq.zero] <- 1
	    		remainder[fq.zero] <- 0
	    	seats.remaining <- n_seats - sum(fq)
	    	highest.remainder <- order(remainder, decreasing=T)
	    	st <- st[highest.remainder]
	    	fq <- fq[highest.remainder]
	    	pop <- pop[highest.remainder]
	    		remainder <- remainder[highest.remainder]
	    	fq[1:seats.remaining] <- fq[1:seats.remaining]+1
    	appt <- fq


	    }

			if (method == "hill-huntington") {
	       priority <- function(s) 1/(sqrt(s * (s - 1)))
	       priority.num <- list()
	       for (i in 2:60) {
	       priority.num[[i]] <- priority(i) * pop
	   		}
	   		pri <- data.frame(rep(st, 59), do.call(c, priority.num))
			appt.tmp <- pri[order(pri[,2], decreasing=T),][1:n_seats.tmp,]
			appt.tmp <- aggregate(appt.tmp[,1], by=list(st=appt.tmp[,1]), FUN=length)
			appt.penultimate <- merge(data.frame(st=st), appt.tmp, all.x=T)
			appt.penultimate[is.na(appt.penultimate)] <- 0
			appt.penultimate[,2] <- appt.penultimate[,2] + autoseats
		    	st <- appt.penultimate[,1]
		    	appt.tmp <- appt.penultimate[,2]
	    }

	    if (method != c("hill-huntington", "hamilton")) {
	    	appt.tmp <- appt(pop, states, nseats, method=method)
	    }


	    apportionment <- data.frame(state=st, seats=appt.tmp)
	    apportionment <- apportionment[order(apportionment[,"state"]),]

		if (state == "all") {
		return(apportionment)
			} else {
		return(apportionment$seats[standardizeText(apportionment$state) %in% standardizeText(state)])
	}
}


rep.appt.gain <- function(pop, states, method="hill-huntington") {
	appt.change <- data.frame(st=states, minpop=NA)
		a <- appt(pop,states,435, method=method)
		for (j in 1:length(states)) {
			pop.j <- pop
			appt.a <- a$apportionment[a$state %in% states[j]]
			
			x.test <- rev(seq(0,1500000, by=5000))
				for (i in 1:length(x.test)) {
				pop.j[states %in% states[j]] <- pop[states %in% states[j]] + x.test[i]
					b <- appt(pop.j,states,435, method=method)
						(appt.b <- b$apportionment[b$state %in% states[j]])
				if (appt.a == appt.b) {
					break
					}
				}
				if (i == 1) {next}
				cat(states[j], ":", x.test[i], "-", x.test[i-1], "\n")
				for (k in 1:5000) {
					cat(".")
					k.seq <- seq(x.test[i], x.test[i-1])
					pop.j[states %in% states[j]] <- pop[states %in% states[j]] + (k.seq[k])
					b <- appt(pop.j,states,435, method=method)
						(appt.b <- b$apportionment[b$state %in% states[j]])
					if (appt.a != appt.b) {
					break
					}
				}

			appt.change[j,2] <- k.seq[k]
			cat("\n", states[j], ":", k.seq[k], "\n")
		}
		return(appt.change) 
	}


	rep.appt.lose <- function(pop, states, method="jefferson") {
	appt.change <- data.frame(st=states, minpop=NA)
	a <- appt(pop,states,435, method=method)
		for (j in 1:length(states)) {
			pop.j <- pop
			appt.a <- a$apportionment[a$state %in% states[j]]
			if (appt.a==1) next
			x.test <- rev(seq(0,1500000, by=5000))
				for (i in 1:length(x.test)) {
				pop.j[states %in% states[j]] <- pop[states %in% states[j]] - x.test[i]
					b <- appt(pop.j,states,435, method=method)
						(appt.b <- b$apportionment[b$state %in% states[j]])
				if (appt.a == appt.b) {
					break
					}
				}
				if (i == 1) {next}
				cat(states[j], ":", x.test[i], "-", x.test[i-1], "\n")
				for (k in 1:5000) {
					cat(".")
					k.seq <- seq(x.test[i], x.test[i-1])
					pop.j[states %in% states[j]] <- pop[states %in% states[j]] - (k.seq[k])
					b <- appt(pop.j,states,435, method=method)
						(appt.b <- b$apportionment[b$state %in% states[j]])
					if (appt.a != appt.b) {
					break
					}
				}

			appt.change[j,2] <- k.seq[k]
			cat("\n", states[j], ":", k.seq[k], "\n")
		}
		return(appt.change) 
	}