winningness <- function(data, votes, demvotes, repvotes, high, low) {
quota <- numeric(0)
output <-list()
noncompet <- data[(data[,demvotes]>high | data[, demvotes]<low) & !is.na(data[, demvotes]),]
compet <- data[data[, demvotes]>low & data[, demvotes]<high & !is.na(data[, demvotes]),]
n <- data.frame(state= noncompet$state, demvotes=NA, repvotes=NA, ecvotes=NA, s=1)
c <- data.frame(state=compet$state, demvotes=NA, repvotes=NA, ecvotes=NA, s=1)
n[1:length(noncompet$state),"demvotes"] <- noncompet[, demvotes]
n[1:length(noncompet$state),"repvotes"] <- noncompet[, repvotes]
n[1:length(noncompet$state),"ecvotes"] <- noncompet[, votes]

ec.win <- sum(data[,votes], na.rm=TRUE)/2
rep.ec.safe <- sum(n$ecvotes[n$repvotes > 0.5])
dem.ec.safe <- sum(n$ecvotes[n$demvotes > 0.5])
safe.win <- ifelse(dem.ec.safe<=ec.win, ifelse(rep.ec.safe<=ec.win, safe.win <- 0, safe.win <- 1),  safe.win <- 1)
c[1:length(compet$state),"demvotes"] <- compet[, demvotes]
c[1:length(compet$state),"repvotes"] <- compet[, repvotes]
c[1:length(compet$state),"ecvotes"] <- compet[, votes]

quota <- c(ec.win-rep.ec.safe,ec.win-dem.ec.safe)
for (i in 1:2){
total.wins <- numeric(0)
total.wins <- list()
total.frag <- numeric(0)
vuln <-list()
for (sets in 1:nrow(c)) {
t <- combn(c[, votes], sets) 
total.wins[[sets]] <- colSums(t)

	total.vul <- numeric(0)
for (vul in 1:sets){
	v <- array(t[(-1*vul),] , dim=c(nrow(t)-1,ncol(t)))
	if(sets==1){v <- t}
	v.totals <- colSums(v)
	total.vul <- rbind(total.vul, v.totals)
}
	vul.totals <- ifelse(total.vul<=quota[[i]], vul.totals <- 1, vul.totals <- 0)
	  vuln[[sets]] <- colSums(vul.totals)
}
vuln <- unlist(vuln)
total.wins <- unlist(total.wins)
frag.matrix <- rbind(total.wins, vuln)
fragility <- mean(frag.matrix["vuln",][frag.matrix["total.wins",]>=quota[[i]]])
ec <- ifelse(total.wins!=quota[[i]], ifelse(total.wins > quota[[i]], ec <- 1, ec <- 0), ec <- 0.5)
ec <- c(ec, ifelse(quota[[i]]<=0, 1, 0))
vuln <- c(vuln, ifelse(quota[[i]]<=0, 0, 1))

vulnerability <- length(vuln[vuln==0])

output[[i]] <- data.frame(year= data[1,"year"], wins =sum(ec[ec==1]), ties= sum(ec[ec==.5])*2, vulnerable =vulnerability, totalcoalitions=(length(ec)), winningness =sum(ec) / (length(ec)),  vulnerability=(sum(ec[ec==1])-vulnerability) / sum(ec[ec==1]), fragility= fragility) 
}
return(cbind(output[[1]],output[[2]])) 
}

