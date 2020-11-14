library("geojsonio")
library("rgdal")
fips <- read.csv("/Users/cervas/Google Drive/Data/fips.csv")
cntyfips <- read.csv("/Users/cervas/Google Drive/Data/countyfips.csv")
cntyfips$fips <- leadingZeroes(cntyfips$FIPS,5)
# Zip codes

zips.shp <- geojsonio::topojson_read("/Users/cervas/Google Drive/GitHub/Data/GIS/Tigerline/2010/tl_2019_us_zcta510.json")
head(fips)
fips$fips <- leadingZeroes(fips$fips,2)
st.download <- fips$fips[!fips$fips %in% c("31", "60", "66", "69", "72", "78")]
us.files <- c(st.download, "31")

for (i in st.download) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2019/SLDL/tl_2019_", i,"_sldl.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/SLDL/",i))
	unlink(temp)
}

# Nebraska Unicameral
	temp <- tempfile()
	download.file("https://www2.census.gov/geo/tiger/TIGER2019/SLDU/tl_2019_31_sldu.zip", destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/SLDL/31"))
	unlink(temp)

# Upper Chambers
for (i in st.download) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2019/SLDU/tl_2019_", i,"_sldu.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/SLDU/",i))
	unlink(temp)
}


sldl <- list()
sldl.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	sldl[[i]] <-  rgdal::readOGR(paste0("/Users/cervas/Google Drive/GitHub/Data/GIS/Tigerline/2019/SLDL/",us.files[i],"/tl_2019_",us.files[i],"_sldl.shp"))
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
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2019/TRACT/tl_2019_", us.files[i],"_tract.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/tracts/",us.files[i]))
	unlink(temp)
}



water <- list()
water.length <- rep(NA, length(water.list))
for (i in 2540:length(water.list)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2019/AREAWATER/", water.list[i]), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/water/", substrLeft(water.list[i],13)))
	unlink(temp)
}


blockgroups <- list()
blockgroups.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2019/BG/tl_2019_", us.files[i], "_bg.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/blockgroups/", us.files[i]))
	unlink(temp)
}


places <- list()
places.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2019/PLACE/tl_2019_", us.files[i], "_place.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/places/", us.files[i]))
	unlink(temp)
}


blocks <- list()
blocks.length <- rep(NA, length(us.files))
for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2019/TABBLOCK/tl_2019_", us.files[i], "_tabblock10.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/blocks/", us.files[i]))
	unlink(temp)
}


for (i in 1:length(us.files)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/2010/tl_2010_", us.files[i], "_tabblock10.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/blocks/", us.files[i],"/"))
	unlink(temp)
}
a

for (i in 1651:length(cntyfips$fips)) {
	temp <- tempfile()
	download.file(paste0("https://www2.census.gov/geo/tiger/TIGER2019/ROADS/tl_2019_", cntyfips$fips[i], "_roads.zip"), destfile= temp)
	a <- unzip(temp, exdir=paste0("/Users/cervas/Downloads/roads/", cntyfips$fips[i]))
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





