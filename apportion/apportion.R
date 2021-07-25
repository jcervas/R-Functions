
#### apportion(data, column with state names, data with state POP, number of  seats to be allocated, number of minimum seats per state, minimum number of votes to be awarded any seat)
library(tidyverse)
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

	    if (method == "jefferson") {
	    	fq.sum <- 0
	    	x <- (sum(pop)/n_seats)
	    for (i in 1:10000000)	{
	    	fq <- firstquota(pop, x, "down")
	    	pop[fq %in% 0] <- 0
	    	fq.adj <- firstquota(pop, x, "down")
	    	fq.adj[fq.adj %in% 0] <- 1
	    	(fq.sum <- sum(fq.adj))
	    	x <- x-.1
	    	if (fq.sum == n_seats) break
			}
	    	appt <- fq.adj
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
		    	appt <- appt.penultimate[,2]

	       #  while(sum(hill(pop,sdivisor))!=n_seats.tmp) {sdivisor <- sdivisor-1}
	       # appt <- hill(pop,sdivisor) + autoseats 
	    }

	   
	    apportionment <- data.frame(state=st, seats=appt)
	    apportionment <- apportionment[order(apportionment[,"state"]),]
	if (state == "all") {
		return(apportionment)
	} else {
		return(apportionment$seats[standardizeText(apportionment$state) %in% standardizeText(state)])
	}
}



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
	appt <- full_join(appt.first, st.list)
	auto.st <- as.character(appt[,1][is.na(appt[,2])])
	auto.replace <- length(appt[,2][is.na(appt[,2])])
	appt <- appt.tmp[1:(nseats-auto.replace)]
	appt <- c(appt, auto.st)
	appt <- as.data.frame(table(appt))
		names(appt) <- c("state", "apportionment")
	return(appt)
}