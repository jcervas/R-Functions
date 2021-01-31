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
	 if (class(block_shp) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")) {
			block.shp <- block_shp
		} else if (class(block_shp) == "character") {
			block.shp <- rgdal::readOGR(block_shp)
		} else {
			stop("Error: Block file not acceptable")
		}
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

# To set up, go to mapshaper.org and put in the Census block polygon. In the Console, enter the command "-points inner". Export to shapefile, and use this new point-based file for "block_shp"
# Example - Minnesota 116th Congress, Census Blocks
	district_shp <- rgdal::readOGR("/Users/user/Library/Mobile Documents/com~apple~CloudDocs/Downloads/tl_2019/tl_2019_us_cd116.shp") 
	block_shp <- rgdal::readOGR("/Users/user/Library/Mobile Documents/com~apple~CloudDocs/Downloads/mn_points_inner/mn_points_inner.shp")
block_poly <- rgdal::readOGR("/Users/user/Google Drive/GitHub/Data Files/GIS/Tigerline/2019/blocks/27/tl_2019_27_tabblock10.shp")

a <- assignPolys(block_shp=block_shp, district_shp=district_shp, districtID="GEOID", blockID="GEOID10")
head(a)

# mn_blocks <- rgdal::readOGR(block_shp)
mn_blocks <- block_poly
mn_blocks@data <- dplyr::full_join(mn_blocks@data, a, by= c("GEOID10"="ID"))
 	mn_blocks@data$District[is.na(mn_blocks@data$District)] <- 0

		cols <- cbind.data.frame(
			District=unique(mn_blocks$District),
			cols=sample(c("#A6CEE3", "#1F78B4", "#FFFF99", "#FF7F00", "#ADDD8E", "#B2DF8A", "#2b51a1", "#f9f934", "#f75167", "#80B1D3", "#FB8090", "#8dd3c7", "#ADDD8E", "#fdb462", "#FDDBC7", "#00B0F0", "#70AD47", "#305496", "#bc80bd", "#fb8072", "#e31a1c", "#3182BD", "#fdbf6f", "#ffff99", "#FF7F00", "#a6cee3"), length(unique(mn_blocks$District)))
			)
mn_blocks@data <- dplyr::left_join(mn_blocks@data, cols, by="District")
mn_blocks@data$cols[is.na(mn_blocks@data$cols)] <- "#000000"



png("/Users/user/Library/Mobile Documents/com~apple~CloudDocs/Downloads/map.png", height = 1600*10, width = 900*10, units = "px", pointsize = 12)
plot(district_shp, border="gray50", lwd=3)
	plot(mn_blocks, border = "#FFFFFF", col = mn_blocks@data$cols, lty=1, lwd = 1.5, add=T)
dev.off()

# x <- block_poly
# tmp.data <-  numeric()
# # p <- polylabelr::poi(fortify(tmp.shp)[,1:2], precision = 0.01)
# for (k in x@data[,blockID]) {
		# tmp.shp <- x[x@data[,blockID]==k,]
# 		p <- polylabelr::poi(coordinates(tmp.shp), precision = 0.01)
# 		tmp.data <- rbind(tmp.data, data.frame(DISTRICT=k, xtext=p$x, ytext=p$y))
# 			}

