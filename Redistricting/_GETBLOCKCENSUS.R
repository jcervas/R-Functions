# Remove all objects just to be safe.
			rm(list=ls(all=TRUE))

	attachLibraries <- c("geojsonio", "rgdal", "censusapi", "tigris", "tidyverse", "cartogram", "spdep", "rmapshaper", "xlsx", "compactness", "jsonlite", "RColorBrewer", "rmapshaper", "polylabelr")
	lapply(attachLibraries, library, character.only = TRUE)
options(stringsAsFactors=F)
options(scipen=999)

key <- "7865f31139b09e17c5865a59c240bdf07f9f44fd"
setwd("/Users/cervas/Google Drive/Data/US Census/2010/SF1/equiv")
setwd("C:/Users/jcerv/Google Drive/Data/US Census/2010/SF1/equiv")
# cnty <- getCensus(name = "2010/dec/sf1", vars = c("NAME"), region = "county:*", regionin = "state:*", key=key)
# cnty <- cnty[order(cnty$state),]
# write.csv(cnty, "/Users/cervas/Google Drive/Data/US Census/2010/SF1/stcnty.cvs",row.names=F)
cnty <- read.csv("/Users/cervas/Google Drive/Data/US Census/2010/SF1/stcnty.cvs")
cnty$state <- sprintf("%02s", cnty$state)
cnty$county <- sprintf("%03s", cnty$county)
for (j in 1:length(unique(cnty[,"state"]))) {
	st <- cnty[cnty[,"state"] %in% unique(cnty[,"state"])[j],]
	st.tmp <- unique(cnty[,"state"])[j]
cat("State ", st.tmp, "\n")
	tmp.st <- numeric()
	for (i in 1:length(st[,"county"])) {
		cnty.tmp <- st[i,"county"] 
cat("County ", cnty.tmp, "\n")
tryCatch({
	write.csv(getBlocks(st.tmp,cnty.tmp),
			paste0(st.tmp, cnty.tmp, ".csv"), row.names=F)
	}, 
	 error=function(e){
	 	cat("ERROR :",conditionMessage(e), "\n", unique(cnty[,"state"])[j], st[i,"county"], "\n")
	 	
	 })
Sys.sleep(runif(1,4,10))
}
}


getBlocks <- function(s,c) {		
	jsonlite::fromJSON(
		paste0("https://api.census.gov/data/2010/dec/sf1?get=NAME,PLACE,BLKGRP,P001001,P003001,P003002,P003003,P010001,P010002,P010003,P010004,P005001,P005002,P005003,P005004,P006001,P006002,P006003,P007001,P007002,P007003,P007004,P042002,P042003,P042004&for=block:*&in=state:", 
			s, 
			"%20county:",
			c,"&key=7865f31139b09e17c5865a59c240bdf07f9f44fd"))
	}





substrLeft <- function(x, n) substr(as.character(x), 1, n)


files.ext <- list.files("/Users/cervas/Google Drive/Data/US Census/2010/SF1/equiv")
files <- substrLeft(files.ext, 5)
stcnty <- paste0(cnty$state,cnty$county)
stcnty[is.na(match(stcnty,files))]

for (j in 1:length(unique(cnty[,"state"]))) {
st.tmp <- cnty[cnty$state %in% unique(cnty[,"state"])[j],]
st.new.tmp <- numeric()
for (i in 1:length(st.tmp$county)) {
tmp <- read.csv(paste0("/Users/cervas/Google Drive/Data/US Census/2010/SF1/equiv/", unique(cnty[,"state"])[j], st.tmp$county[i], ".csv"), skip=1)
st.new.tmp <- rbind(st.new.tmp, tmp)
}
write.csv(st.new.tmp, paste0("/Users/cervas/Google Drive/Data/US Census/2010/SF1/st_blocks/", unique(cnty[,"state"])[j], ".csv"), row.names=F)
}



