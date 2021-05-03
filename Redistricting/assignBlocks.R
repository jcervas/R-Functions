# Takes a district plan file and assigns lower level geography to create a relationship file. 
library(rmapshaper)
library(raster)

assignPolys <- function(block_point=NA, district_shp=NA, districtID=NA, blockID=NA, district_label=NA) {
	library(rgdal)
	if (class(district_shp) %in% "SpatialPolygonsDataFrame") {
			district.shp <- district_shp
		}  else if (class(district_shp) == "character") {
			district.shp <- rgdal::readOGR(district_shp)
		} else {
			stop("Error: District file not acceptable")
		}
	 if (class(block_point) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")) {
			block.shp <- block_point
		} else if (class(block_point) == "character") {
			block.shp <- rgdal::readOGR(block_point)
		} else {
			stop("Error: Block file not acceptable")
		}

		block.shp <- sp::spTransform(block.shp, raster::crs(district.shp))

				# block.shp.buffer <- raster::buffer(block.shp)
		master <- list()
		ID <- unique(district.shp@data[,districtID])
		for (j in 1:length(ID)) {
			poly.tmp <- district.shp[district.shp@data[,districtID] %in% ID[j],]
			blocks.subset <- block.shp[poly.tmp,] #subset in base R
			# blocks.subset <- block.shp.buffer[poly.tmp,] #subset in base R, add buffer from raster
			# blocks.subset <- rmapshaper::ms_clip(block.shp, poly.tmp) #subset using rmapshaper
			# x <- raster::intersect(block.shp, poly.tmp) # subset using raster
			# blocks.subset <- rgeos::gIntersection(block.shp, poly.tmp) # subset using rgeos
			cat(ID[j], "\n")
				master[[j]] <- data.frame(ID=blocks.subset@data[,blockID], District=ID[j])

		}

		return(do.call(rbind, master))
	}


