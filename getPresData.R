# source("https://raw.githubusercontent.com/jcervas/2020-Elections/main/NYT_json.R")
source("https://raw.githubusercontent.com/jcervas/R-Functions/main/GERRYfunctions.R")

getPres <- function(x) {

years <- seq(1868,2020, by=4) 	#Election Years
year.list <- seq(1790, 2010, by = 10) 	#Apportionment Years
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 170, maxColorValue = 255)
returns <- runif(100)

election.dta <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/Elections/Presidential/Presidential%20Elections_General_Full.csv")
election.dta <- election.dta[election.dta$year > 1867,]
	state.fips <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/fips.csv")
	state.fips <- cbind.data.frame(state=state.fips$state, fips=state.fips$fips)

# e.2020 <- getPresidential2020()
# election.dta[setdiff(names(e.2020), names(election.dta))] <- NA
# e.2020[setdiff(names(election.dta), names(e.2020))] <- NA
# election.dta <- rbind(election.dta, e.2020)

pres.data <- pres.data.lag <- pres.data.lag2 <- cbind.data.frame(
	year= election.dta$year, 
	state= election.dta$state,
	dem=two_party(as.numeric(election.dta$dem),as.numeric(election.dta$rep)), 
	total=as.numeric(election.dta$dem)+as.numeric(election.dta$rep),
	ecvotes=as.numeric(election.dta$ecvotes))

# Sort the data by State and Year
data <- data[order(data$State, data$Year), ]

# Create the lagged variable within each state
data$dlag <- ave(data$dem, data$state, FUN = function(x) c(NA, x[-length(x)]))
data$dlag2 <- ave(data$dlag, data$state, FUN = function(x) c(NA, x[-length(x)]))


pop <- jsonlite::fromJSON("https://raw.githubusercontent.com/jcervas/Data/master/Elections/hist_pop.json")
	a <- numeric()
		for (i in 1:length(pop)) {
			a <- cbind(a, pop[[i]]$census$population)
		}
		rownames(a) <- year.list
	
	popdata <- list()
		for (i in 1:39) {
			yr.tmp <- pres.data$year == years[i]
				pres.tmp <- cbind.data.frame(
					year= rep(years[i], 51),
					state=election.dta$state[election.dta$year == 2016]
					)
			if (substrRight(years[i],1) %in% c("2", "4")) {
				pres.tmp$pop <- a[which(year.list == round(years[i],-1)),]
					} 
					else {
						pres.tmp$pop <- a[which(year.list == round(years[i],-1)-10),]
					}

					popdata[[i]] <- pres.tmp
				}


elect.dta <- do.call(rbind,popdata)
pres <- merge(elect.dta, pres.data, by=c("year", "state"), all=T)
miss.pop <- !is.na(pres$dem) & is.na(pres$pop)

	pres$pop[miss.pop] <- round(pres$total[miss.pop] * median((pres$pop-pres$total)/pres$total, na.rm=T))


	electiondata <- list()

	for (j in 1:length(years)) {
		year <- years[j]
		pres_tmp <- pres[pres$year %in% year,]
		pres_lag <- pres[pres$year %in% (year-4),]
		pres_tmp$year <- year
		pres_tmp$house <- pres_tmp$ecvotes-2
		pres_tmp$house[pres_tmp$state %in% "D. C."] <- 0
			pres_new <- pres_tmp
		electiondata[[j]] <- pres_new
	}
electiondata <- do.call(rbind, electiondata)
return(electiondata)
}
