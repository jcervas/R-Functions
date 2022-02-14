# New Jersey

source("https://raw.githubusercontent.com/jcervas/R-Functions/main/Redistricting/assignBlocks.R")

- Download from "https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2021&layergroup=County+Subdivisions"
district_shp <- rgdal::readOGR("/Users/user/Google Drive/GitHub/Data Files/Census/NJ/tl_2021_34_cousub/tl_2021_34_cousub.shp")

- Download from "https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2021&layergroup=Blocks+%282020%29" and transform in mapshaper.org
block_point <- rgdal::readOGR("/Users/user/Google Drive/GitHub/Data Files/Census/NJ/NJ_blocks_centroids/NJ_blocks_centroids.shp")
block_poly <- rgdal::readOGR("/Users/user/Google Drive/GitHub/Data Files/Census/NJ/tl_2021_34_tabblock20/tl_2021_34_tabblock20.shp")

a <- assignPolys(point_shp=block_point, district_shp=district_shp, districtID="NAMELSAD", blockID="GEOID20")
head(a)
write.csv(a, "/Users/user/Google Drive/GitHub/Data Files/Census/NJ/NJ_muni_block_equiv.csv", row.names=F)



lower_poly <- block_poly
lower_poly@data <- dplyr::full_join(lower_poly@data, a, by= c("GEOID20"="ID"))
 	lower_poly@data$District[is.na(lower_poly@data$District)] <- 0

		cols.wheel <- cbind.data.frame(
			District=unique(lower_poly$District),
			cols=sample(rep(c("#A6CEE3", "#1F78B4", "#FFFF99", "#FF7F00", "#ADDD8E", "#B2DF8A", "#2b51a1", "#f9f934", "#f75167", "#80B1D3", "#FB8090", "#8dd3c7", "#ADDD8E", "#fdb462", "#FDDBC7", "#00B0F0", "#70AD47", "#305496", "#bc80bd", "#fb8072", "#e31a1c", "#3182BD", "#fdbf6f", "#ffff99", "#FF7F00", "#a6cee3"), length(unique(lower_poly$District))/20), length(unique(lower_poly$District)))
			)
lower_poly@data <- dplyr::left_join(lower_poly@data, cols.wheel, by="District")
lower_poly@data$cols[is.na(lower_poly@data$cols)] <- "#000000"

png("/Users/user/Google Drive/GitHub/Data Files/Census/NJ/map.png", height = 1200*5, width = 900*5, units = "px", pointsize = 12)
	plot(lower_poly, border = "#FFFFFF", col = lower_poly@data$cols, lty=1, lwd = 0.5)
	plot(district_shp, border="gray50", lwd=3, add=T)
dev.off()
