# Takes a district plan file and assigns lower level geography to create a relationship file. 



assignBlock <- function(block_shp=NA, district_shp=NA, districtID=NA, blockID=NA, district_label=NA) {
	library(rgdal)
	if (class(district_shp) == "sp") {
			district.shp <- district_shp
		}  else if (class(district_shp) == "character") {
			district.shp <- rgdal::readOGR(district_shp)
		}
	 if (class(block_shp) == "sp") {
			block.shp <- block_shp
		} else if (class(block_shp) == "character") {
			block.shp <- rgdal::readOGR(block_shp)
		}

		master <- list()
		ID <- unique(district.shp@data[,districtID])
		for (j in 1:length(ID)) {
			poly.tmp <- district.shp[district.shp@data[,districtID] %in% ID[j],]
			blocks.subset <- block.shp[poly.tmp,]

				master[[j]] <- data.frame(ID=blocks.subset@data[,blockID], District=ID[j])

		}

		return(do.call(rbind, master))
	}

# Example - Minnesota 116th Congress, Census Blocks
	us_cd <- rgdal::readOGR("/Users/user/Google Drive/GitHub/Data Files/GIS/Tigerline/2019/cd/tl_2019_us_cd116.shp") 
	district_shp <- us_cd[us_cd@data$STATEFP %in% 27,]
block_shp <- "/Users/user/Library/Mobile Documents/com~apple~CloudDocs/Downloads/tl_2019_27_tabblock10/tl_2019_27_tabblock10.shp"


a <- assignBlock(block_shp=block_shp, district_shp=district_shp, districtID="GEOID", blockID="GEOID10")
head(a)
