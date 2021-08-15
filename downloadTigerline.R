library("geojsonio")
library("rgdal")

source("https://raw.githubusercontent.com/jcervas/R-Functions/main/GERRYfunctions.R")
# fips <- read.csv("/Users/user/Google Drive/Data/fips.csv")
# cntyfips <- read.csv("/Users/user/Google Drive/Data/countyfips.csv")
fips <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/fips.csv")
cntyfips <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/countyfips.csv")

cntyfips$fips <- leadingZeroes(cntyfips$FIPS,5)
# Zip codes

zips.shp <- geojsonio::topojson_read("/Users/user/Google Drive/GitHub/Data/GIS/Tigerline/2010/tl_2020_us_zcta510.json")
head(fips)
fips$fips <- leadingZeroes(fips$fips,2)
st.download <- fips$fips[!fips$fips %in% c("31", "60", "66", "69", "72", "78")]
us.files <- c(st.download, "31")

for (i in st.download) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/SLDL/tl_2020_", i,"_sldl.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/SLDL/",i))
	unlink(temp)
}

# Nebraska Unicameral
	temp <- tempfile()
	download.file("https://www2.census.gov/geo/tiger/TIGER2020/SLDU/tl_2020_31_sldu.zip", destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/SLDL/31"))
	unlink(temp)

# Upper Chambers
for (i in st.download) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/SLDU/tl_2020_", i,"_sldu.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/SLDU/",i))
	unlink(temp)
}


sldl <- list()
sldl.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	sldl[[i]] <-  rgdal::readOGR(paste0("/Users/user/Google Drive/GitHub/Data/GIS/Tigerline/2020/SLDL/",us.files[i],"/tl_2020_",us.files[i],"_sldl.shp"))
	sldl.length[i] <- length(sldl[[i]]$GEOID)
}
	sum(sldl.length)

a <- lapply(sldl, function(x) a  <- x@data[,c(1,3,4)])

for (i in 1:length(sldl.length)) {
	sldl[[i]]@data <- a[[i]]
}


tracts <- list()
tracts.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_", us.files[i],"_tract.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/tracts/",us.files[i]))
	unlink(temp)
}



water <- list()
water.length <- rep(NA, length(water.list))
n <- 1
for (i in n:length(cntyfips$fips)) {
		n <- n+1
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/AREAWATER/tl_2020_", cntyfips$fips[i], "_areawater.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/water/", cntyfips$State[i], "/", cntyfips$fips[i]))
	unlink(temp)
}

# Primary and Secondary Roads
n <- 1
for (i in n:length(us.files)) {
		n <- n+1
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/PRISECROADS/tl_2020_", us.files[i], "_prisecroads.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/primary and secondary roads/", us.files[i]))
	unlink(temp)
}

blockgroups <- list()
blockgroups.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/BG/tl_2020_", us.files[i], "_bg.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/blockgroups/", us.files[i]))
	unlink(temp)
}


places <- list()
places.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/PLACE/tl_2020_", us.files[i], "_place.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/places/", us.files[i]))
	unlink(temp)
}


cntysub <- list()
cntysub.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/COUSUB/tl_2020_", us.files[i], "_cousub.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/cousub/", us.files[i]))
	unlink(temp)
}




options(timeout=200)
blocks <- list()
# blocks.length <- rep(NA, length(us.files))
n <- 1
for (i in n:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_", us.files[i], "_tabblock20.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/blocks/", us.files[i]))
	unlink(temp)
	n <- n+1
}

# 2010 Blocks
for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/2010/tl_2010_", us.files[i], "_tabblock10.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/blocks/", us.files[i],"/"))
	unlink(temp)
}
a

for (i in 1:length(cntyfips$fips)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2020/ROADS/tl_2020_", cntyfips$fips[i], "_roads.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/user/Downloads/roads/", cntyfips$State[i], "/", cntyfips$fips[i]))
	unlink(temp)
}

# Roads





### Zip Code API Call
zipcodes.sf1 <- jsonlite::fromJSON("https://api.census.gov/data/2010/dec/sf1?get=NAME,group(P1)&for=zip%20code%20tabulation%20area:*")

tracts.sf1 <- list()
for (i in 1:length(us.files)) {
	tracts.sf1[[i]] <- jsonlite::fromJSON(paste0("https://api.census.gov/data/2010/dec/sf1?get=NAME,group(P3)&for=tract:*&in=state:",us.files[i]))
	colnames(tracts.sf1[[i]]) <- tracts.sf1[[i]][1,]
	tracts.sf1[[i]] <- tracts.sf1[[i]][-1,]
}
b <- do.call(rbind, tracts.sf1)





