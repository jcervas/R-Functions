# Remove all objects just to be safe.
			rm(list=ls(all=TRUE))

	attachLibraries <- c("jsonlite", "geojsonio", "rgdal", "censusapi", "tigris", "tidyverse", "cartogram", "spdep", "rmapshaper", "xlsx", "compactness", "jsonlite", "RColorBrewer", "rmapshaper", "polylabelr")
	lapply(attachLibraries, library, character.only = TRUE)
options(stringsAsFactors=F)
options(scipen=999)

key <- "7865f31139b09e17c5865a59c240bdf07f9f44fd"
# setwd("/Users/user/Google Drive/Data/US Census/2010/SF1/equiv")
cnty <- censusapi::getCensus(name = "2010/dec/sf1", vars = c("NAME"), region = "county:*", regionin = "state:*", key=key)
cnty <- cnty[order(cnty$state),]

for (j in 1:length(unique(cnty[,"state"]))) {
	st <- cnty[cnty[,"state"] %in% unique(cnty[,"state"])[j],]
cat("State ", unique(cnty[,"state"])[j], "\n")
	tmp.st <- list()
	for (i in 1:length(st[,"county"])) {
cat("County ", st[i,"county"], "\n")
tmp.st[[i]] <- jsonlite::fromJSON(paste0("https://api.census.gov/data/2010/dec/sf1?get=P001001,P003001,P003002,P003003,P010001,P010002,P010003,P010004,P005001,P005002,P005003,P005004,P006001,P006002,P006003,P007001,P007002,P007003,P007004,P042002,P042003,P042004,NAME,PLACE,BLKGRP&for=block:*&in=state:", unique(cnty[,"state"])[j], "%20county:",st[i,"county"]))
	tmp.st[[i]] <- tmp.st[[i]][-1,]
	write.csv(tmp.st[[i]], paste0(unique(cnty[,"state"])[j], st[i,"county"], ".csv"), row.names=F)
}}


	blocks <- do.call(rbind, tmp.st)