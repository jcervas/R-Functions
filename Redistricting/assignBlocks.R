# Takes a district plan file and assigns lower level geography to create a relationship file. 
library(rmapshaper)
library(raster)

assignPolys <- function(block_shp=NA, district_shp=NA, districtID=NA, blockID=NA, district_label=NA) {
	library(rgdal)
	if (class(district_shp) %in% "SpatialPolygonsDataFrame") {
			district.shp <- district_shp
		}  else if (class(district_shp) == "character") {
			district.shp <- rgdal::readOGR(district_shp)
		} else {
			stop("Error: District file not acceptable")
		}
	 if (class(block_shp) == "sp") {
			block.shp <- block_shp
		} else if (class(block_shp) == "character") {
			block.shp <- rgdal::readOGR(block_shp)
		} else {
			stop("Error: Block file not acceptable")
		}

		master <- list()
		ID <- unique(district.shp@data[,districtID])
		for (j in 1:length(ID)) {
			poly.tmp <- district.shp[district.shp@data[,districtID] %in% ID[j],]
			# blocks.subset <- block.shp[poly.tmp,] #subset in base R
			# blocks.subset <- raster(block.shp)[poly.tmp,] #subset in base R, add buffer from raster
			# blocks.subset <- rmapshaper::ms_clip(block.shp, poly.tmp) #subset using rmapshaper
			# x <- raster::intersect(block.shp, poly.tmp) # subset using raster
			blocks.subset <- rgeos::gIntersection(block.shp, poly.tmp) # subset using rgeos
			cat(ID[j], "\n")
				master[[j]] <- data.frame(ID=blocks.subset@data[,blockID], District=ID[j])

		}

		return(do.call(rbind, master))
	}

# Example - Minnesota 116th Congress, Census Blocks
	us_cd <- rgdal::readOGR("/Users/user/Google Drive/GitHub/Data Files/GIS/Tigerline/2019/cd/tl_2019_us_cd116.shp") 
	district_shp <- us_cd[us_cd@data$STATEFP %in% 27,]
block_shp <- "/Users/user/Library/Mobile Documents/com~apple~CloudDocs/Downloads/tl_2019_27_tabblock10/tl_2019_27_tabblock10.shp"
block_shp <- "/Users/user/Google Drive/GitHub/Data Files/GIS/Tigerline/2019/blocks/27/tl_2019_27_tabblock10.shp"

a <- assignPolys(block_shp=block_shp, district_shp=district_shp, districtID="GEOID", blockID="GEOID10")
head(a)

mn_blocks <- rgdal::readOGR(block_shp)

mn_blocks@data <- dplyr::full_join(mn_blocks@data, a, by= c("GEOID10"="ID"))


mycolours <- c("red","yellow","purple","blue","green","pink","orange")
mybreaks <- unique(mn_blocks$District)
mybreaks

mycolourscheme <- mycolours[findInterval(mn_blocks@data$District, vec = mybreaks)]

plot(mn_blocks)
plot(mn_blocks[is.na(mn_blocks@data$District),])