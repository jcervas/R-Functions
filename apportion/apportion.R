
#### apportion(data, column with state names, data with state POP, number of  seats to be allocated, number of minimum seats per state, minimum number of votes to be awarded any seat)

apportion <- function(STATES, POP, n_seats=1, autoseats=1, threshold=0, method = "hill-huntington", state = "all") {
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
	    	x <- floor(sum(pop)/n_seats)
	    for (i in 1:100000)	{
	    	fq <- firstquota(pop, x, "down")
	    	fq[fq %in% 0] <- 1
	    	fq.sum <- sum(fq)
	    	x <- x-1
	    	if (fq.sum == n_seats) break
			}
	    	appt <- fq
	    }




	    if (method == "adams") {
				fq.sum <- 0
	    	x <- floor(sum(pop)/n_seats)
	    for (i in 1:100000)	{
	    	fq <- firstquota(pop, x, "up")
	    	fq[fq %in% 0] <- 1
	    	fq.sum <- sum(fq)
	    	x <- x-1
	    	if (fq.sum == n_seats) break
			}
	    	appt <- fq
	    }






	    if (method == "webster") {
	    	while(n_seats.tmp - sum(firstquota(pop, sdivisor,"nearest")) != 0) {sdivisor <- sdivisor+1}
	    	appt <- firstquota(pop, sdivisor,"nearst") + autoseats 
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



