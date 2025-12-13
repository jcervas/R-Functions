# This repository is for Redistricting based functions.


### Assign Blocks
- This is a generic function written in R that takes a shapefile and creates an equivalency file to assign all the polygons from some lower geography to the higher geography. For instance, in redistricting, we might want to know which blocks are in which districts. From that, we can calculate county splits, demographic information, or partisanship.

###### To set up, go to mapshaper.org and put in the Census block polygon. In the Console, enter the command 
`-points inner` 
###### Export to shapefile, and use this new point-based file for "block_point". We can use the block_poly to read in the original polygon shapefile for mapping.
- Example - Minnesota 116th Congress, Census Blocks (Function uses package "sp" to read in shapefiles)
```
source('https://raw.githubusercontent.com/jcervas/R-Functions/refs/heads/main/assignPolys/assignPolys.R')
district <- "/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@uci.edu/My Drive/Projects/Redistricting/2022/PA/data/Plans/2022 LRC Senate Final.geojson"

blocks <- "/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@uci.edu/My Drive/GitHub/Data Files/Census/PA2020.pl/GIS/blocks_simplified/WP_Blocks.json"
```

```
district_shp <- as(sf::st_read(district), "Spatial")
block_point <- as(sf::st_read(blocks), "Spatial")

head(district_shp)
head(block_point)
```

- This is the function command
```
a <- assignPolys(block_point=block_point, district_shp=district_shp, districtID="GEOID", blockID="GEOID20")
head(a)
```

```
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
lower_poly <- block_shp
lower_poly@data <- dplyr::full_join(lower_poly@data, a, by= c("GEOID20"="ID"))
 	lower_poly@data$District[is.na(lower_poly@data$District)] <- 0

          cols <- cbind.data.frame(
               District=unique(lower_poly$District),
               cols=sample(rep(c("#A6CEE3", "#1F78B4", "#FFFF99", "#FF7F00", "#ADDD8E", "#B2DF8A", "#2b51a1", "#f9f934", "#f75167", "#80B1D3", "#FB8090", "#8dd3c7", "#ADDD8E", "#fdb462", "#FDDBC7", "#00B0F0", "#70AD47", "#305496", "#bc80bd", "#fb8072", "#e31a1c", "#3182BD", "#fdbf6f", "#ffff99", "#FF7F00", "#a6cee3"), length(unique(lower_poly$District))), length(unique(lower_poly$District)))
               )
lower_poly@data <- dplyr::left_join(lower_poly@data, cols, by="District")
lower_poly@data$cols[is.na(lower_poly@data$cols)] <- "#000000"

png("map.png", height = 1200*5, width = 900*5, units = "px", pointsize = 12)
	plot(lower_poly, border = "#FFFFFF", col = lower_poly@data$cols, lty=1, lwd = 0.5)
	plot(district_shp, border="gray50", lwd=3, add=T)
dev.off()

# rgdal::writeOGR(lower_poly, "/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads", layer="lower_poly", driver="ESRI Shapefile", overwrite_layer=T)
```
###### Map of Minneapolis, with Census blocks assigned. A small number of blocks were not correctly assigned, because the district shapefile has a different boundary than the block shapefile. For most purposes, this is irrelevent, but we need to make note of this and may need to manually add them to the block equivalancy file.
![alt text](https://github.com/jcervas/R-Functions/blob/main/Redistricting/images/map.png)



```
# depreciated
block_point <- rgdal::readOGR("tl_2019_27_tabblock10.shp")
block_point <- ms_points(block_shp, location = "inner")
```
