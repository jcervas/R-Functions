# This repository is for Redistricting based functions.


### Assign Blocks
- This is a generic function written in R that takes a shapefile and creates an equivalency file to assign all the polygons from some lower geography to the higher geography. For instance, in redistricting, we might want to know which blocks are in which districts. From that, we can calculate county splits, demographic information, or partisanship.

###### To set up, go to mapshaper.org and put in the Census block polygon. In the Console, enter the command "-points inner". Export to shapefile, and use this new point-based file for "block_point". We can use the block_poly to read in the original polygon shapefile for mapping.
- Example - Minnesota 116th Congress, Census Blocks (Function uses package "sp" to read in shapefiles)
```
source("https://raw.githubusercontent.com/jcervas/R-Functions/main/Redistricting/assignBlocks.R")
district_shp <- rgdal::readOGR("tl_2019_us_cd116.shp")
block_point <- rgdal::readOGR("tl_2019_27_tabblock10.shp")
block_poly <- rgdal::readOGR("tl_2019_27_tabblock10.shp")
```
- This is the function command
```
a <- assignPolys(block_point=block_point, district_shp=district_shp, districtID="GEOID", blockID="GEOID10")
head(a)
write.csv(a, "block_equiv.csv", row.names=F)
```

|Row|ID|District|
|---|---|---|
|1|270731802001335|2707|
|2|270731802001330|2707|
|3|270731802001385|2707|
|4|270731802001386|2707|
|5|270731802001422|2707|
|6|270731802001376|2707|

- Now that we have the new assignments, let's map the data to make sure it looks right
```
mn_blocks <- rgdal::readOGR(block_poly)
mn_blocks@data <- dplyr::full_join(mn_blocks@data, a, by= c("GEOID10"="ID"))
 	mn_blocks@data$District[is.na(mn_blocks@data$District)] <- 0

		cols <- cbind.data.frame(
			District=unique(mn_blocks$District),
			cols=sample(c("#A6CEE3", "#1F78B4", "#FFFF99", "#FF7F00", "#ADDD8E", "#B2DF8A", "#2b51a1", "#f9f934", "#f75167", "#80B1D3", "#FB8090", "#8dd3c7", "#ADDD8E", "#fdb462", "#FDDBC7", "#00B0F0", "#70AD47", "#305496", "#bc80bd", "#fb8072", "#e31a1c", "#3182BD", "#fdbf6f", "#ffff99", "#FF7F00", "#a6cee3"), length(unique(mn_blocks$District)))
			)
mn_blocks@data <- dplyr::left_join(mn_blocks@data, cols, by="District")
mn_blocks@data$cols[is.na(mn_blocks@data$cols)] <- "#000000"

png("map.png", height = 1200*5, width = 900*5, units = "px", pointsize = 12)
	plot(mn_blocks, border = "#FFFFFF", col = mn_blocks@data$cols, lty=1, lwd = 0.5)
	plot(district_shp, border="gray50", lwd=3, add=T)
dev.off()
```
###### Map of Minneapolis, with Census blocks assigned. A small number of blocks were not correctly assigned, because the district shapefile has a different boundary than the block shapefile. For most purposes, this is irrelevent, but we need to make note of this and may need to manually add them to the block equivalancy file.
![alt text](https://github.com/jcervas/R-Functions/blob/main/Redistricting/images/map.png)
