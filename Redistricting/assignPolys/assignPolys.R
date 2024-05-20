# Takes a district plan file and assigns lower level geography to create a relationship file. 
library(rmapshaper)
library(raster)
# library(rgdal)

assignPolys <- function(district_shp=NA, block_point=NA, districtID=NA, blockID=NA) {
	
	if (class(district_shp)[1] %in% c("sf")) {
			district.shp <- district_shp
		}  else if (class(district_shp) == "character") {
			district.shp <- as(sf::st_read(district_shp), "Spatial")
		} else {
			stop("Error: District file not acceptable")
		}
	 if (class(block_point)[1] %in% c("sf")) {
			block.shp <- block_point
		} else if (class(block_point) == "character") {
			block.shp <- as(sf::st_read(block_point), "Spatial")
		} else {
			stop("Error: Block file not acceptable")
		}

		block.shp <- sf::st_transform(block.shp, crs = sf::st_crs(district.shp))

		master <- list()
		# Extract the NAME20 column as a character vector

		ID <- unique(district.shp[[districtID]])
		
		for (j in 1:length(ID)) {
			poly.tmp <- district.shp[district.shp[[districtID]] %in% ID[j],]
			blocks.subset <- block.shp[poly.tmp,] #subset in base R
			
			cat(ID[j], "\n")

			if (length(blocks.subset) == 0) {
				master[[j]] <- data.frame(ID=NA, District=ID[j])
				} else {
				master[[j]] <- data.frame(ID=blocks.subset[,blockID], District=ID[j])
				}

		}

		return(do.call(rbind, master))
	}


