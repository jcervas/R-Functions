####################################################################
# Set working directory.  You'll want to add folders called "2012", "2016", and "2016full".  The 2016full folder will include third party.  The other folders will contain just two party information.  Each state will have its own xlsx file.  The combinded national file will be in the your working directory.
####################################################################

setwd("/Users/cervas/Desktop/ElectionsEXCEL")


library(reshape2) 
library(rjson)
library(xlsx)


st <-  c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", 'MT', "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
for (k in 1:length(st)) {
	state <- st[k]
url <- sprintf("http://data.cnn.com/jsonp/ELECTION/2012/%s/county/P.json",state)
json <- sub("callbackWrapper\\((.*)\\)","\\1",
paste(readLines(url,warn=FALSE), collapse=""),
perl=T)
json_data <- fromJSON(json)[['counties']]
dta <- data.frame(state=rep(state,1000),
countyfips=rep(NA,1000),
cty=rep(NA,1000),
name=rep(NA,1000),
votes=rep(NA,1000))
 i <- 1
for (cty in json_data) {
for (cand in cty$race$candidates) {
dta[i,1] <- state
dta[i,2] <- cty$co_id
dta[i,3] <- cty$name
dta[i,4] <- cand$lname
dta[i,5] <- cand$votes
i <- i + 1
}
}
dta <- dta[1:(i-1),]
dta2 <- dcast(dta, formula=state+cty+countyfips ~ name, value.var="votes")
dta2$pctObama <- dta2$Obama/(dta2$Obama + dta2$Romney)
dta2$diff2012 <- dta2$Obama - dta2$Romney

write.xlsx(dta2,  paste0("states/2012/", state, ".xlsx"), row.names=FALSE)

}

filenames <- list.files(path = "states/2012")
dat2012 <- do.call("rbind", lapply(paste0("states/2012/",filenames), read.xlsx, header = TRUE))
write.xlsx(dat2012,  "countylevelpres2012.xlsx", row.names=FALSE)



for (k in 1:length(st)) {
	state <- st[k]
url <- sprintf("http://data.cnn.com/ELECTION/2016/%s/county/P_county.json",state)
json_data <- fromJSON(paste(readLines(url,warn=FALSE), collapse=""))
dta <- data.frame(state=rep(state,1000),
countyfips=rep(NA,1000),
cty=rep(NA,1000),
name=rep(NA,1000),

votes=rep(NA,1000))
i <- 1
for (cty in json_data$counties) {
rpt <- cty$race$pctsrep
for (cand in cty$race$candidates) {
dta[i,2] <- cty$co_id
dta[i,3] <- cty$name
dta[i,4] <- cand$lname
dta[i,5] <- cand$votes
i <- i + 1
}
}
dta <- dta[1:(i-1),]
dta2 <- dcast(dta, formula=state+countyfips+cty ~ name, value.var="votes")
dta2$pctClinton <- dta2$Clinton/(dta2$Clinton + dta2$Trump)
dta2$diff2016 <- dta2$Clinton - dta2$Trump
write.xlsx(dta2,  paste0("states/2016full/", state, ".xlsx"))
dta2 <- dta2[, c("state","countyfips", "cty", "Clinton","Trump", "pctClinton", "diff2016")]
write.xlsx(dta2,  paste0("states/2016/", state, ".xlsx"), row.names=FALSE)

}

filenames <- list.files(path = "states/2016")
dat2016 <- do.call("rbind", lapply(paste0("states/2016/",filenames), read.xlsx, header = TRUE))
write.xlsx(dat2016,  "countylevelpres2016.xlsx", row.names=FALSE)