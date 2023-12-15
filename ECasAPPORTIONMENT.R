

ec_apportionment <- function (data, seats="ecvotes", autoseats=0, thirdparties=0) 
	{

thirdparties <-  0 	#Include other parties, 0-no 1-yes
#autoseats <- 0 #Number of guarenteed seats

cand <- data.frame(state=NA, pop=NA)
state <- matrix(NA, ncol=5, nrow=length(data$state))

for (i in 1:length(data$state)) {
	num <- data[, seats][i] + autoseats  #Number of Seats
	cand[1,1] <- "Democrat"
	cand[2,1] <- "Republican"
	cand[3,1] <- "Libertarian"
	cand[4,1] <- "Other"
	cand[1,2] <- data$dem[i]
	cand[2,2] <- data$rep[i]
	cand[3,2] <- ifelse("lib" %in% colnames(data), data$lib[i],0)
	cand[3,2] <- ifelse(thirdparties==0,0,cand[3,2])
	cand[4,2] <- ifelse("other" %in% colnames(data), data$other[i],0)
	cand[4,2] <- ifelse(thirdparties==0,0,cand[4,2])

# cand <- cand[cand[,2] != 0,]

priority <- seq(1, num + 5, by=1)

dta <- data.frame(party=rep(cand[,1],length(priority)), votes=as.numeric(rep(cand[,2]),length(priority)), row.names=NULL)
#dta2 <- data.frame(party=rep(cand[,1],autoseats),votes=NA, row.names=NULL)
#dta2[,2] <- 99999999
n <- 1
l <- 0
for (j in 1: length(priority)) {
k <- l+1
l <- k+nrow(cand)-1
multiplier <- 1/(sqrt(n*(n+1)))

dta[k:l,3] <- dta$votes[k:l] * multiplier

n <- n + 1



#dta <- dta[(is.finite(dta[,2])),]
#dta <- rbind(dta,dta2)
dta <- dta[order(-dta[,3]),]
final <- dta
final <- final[1:num,]
final[,3] <- rep(1,num, by=1)
state[i,2:5] <-  as.numeric(unname(table(final[,1])))
#state[i,4] <- state[i,2]+state[i,3]+state[i,4]+state[i,5]
#state[i,5] <- data$ecvotes[i]
#state[,1] <- as.character(data$state)



}


}
return(list(democrat=sum(state[,2]), republican=sum(state[,5]), libertarian=sum(state[,3]), other=sum(state[,4])))

	}
