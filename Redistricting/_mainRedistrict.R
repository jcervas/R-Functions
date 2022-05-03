
# Remove all objects just to be safe.
			rm(list=ls(all=TRUE))
library(maptools)
# library(spdep)
library(tidyverse)
library(censusapi)
library(redist)
library(rgdal)
library(sp)
library(rgeos)
library(tigris)
library(cartogram)
tigris_cache_dir("/Users/user/Downloads")
# Restart R
options(tigris_use_cache = TRUE)
options(stringsAsFactors = FALSE)
options(scipen = 999) # Turn off Scientific Notation

funct.dir <- "https://raw.githubusercontent.com/jcervas/R-Functions/main/"

source(paste0(funct.dir, "getCensusApi.R"))
key <- "7865f31139b09e17c5865a59c240bdf07f9f44fd"
setwd("/Users/user/Downloads")

make.map <- function (shapefile, dataframe, Population, by="GEOID") {
	baseshape <- shapefile
			baseshape$shp@data <- full_join(baseshape$shp@data, dataframe, by=by)
			baseshape <- gUnaryUnion(baseshape, id = baseshape$shp@data$district)
			baseshape <- spChFIDs(baseshape, row.names(baseshape))
			row.names(baseshape) <- as.character(1:length(baseshape))
	new.data <- aggregate.data.frame(list(Population=as.numeric(dataframe[,Population])), by=list(district=dataframe$district), FUN=sum)
	colnames(new.data) <- c("district", Population)
baseshape <- SpatialPolygonsDataFrame(baseshape, new.data)
return(baseshape)
}

# # First we must download the data we will use.
# # We begin with the geography files from the US Census Tigerline
# tract.shp <- "tl_2010_51_tract10"
# cty.shp <- "tl_2010_51_county10" 

# tmp <- tempfile()
# download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2010/COUNTY/2010/tl_2010_51_county10.zip", destfile = tmp)
# unzip(tmp, exdir = ".")

# shape.1 <- readOGR("/Users/user/Downloads/tl_2010_51_county10.shp")
# # # Use this code to let county label positions in the most ideal part for visualization
# # 		tmp.data <-  numeric()
# # 	for (k in shape.1@data[,1]) {
# # 	tmp.shp <- shape.1[shape.1@data$GEOID==k,]
# # 	p <- polylabelr::poi(fortify(tmp.shp)[,1:2], precision = 0.01)
# # 	tmp.data <- rbind(tmp.data, data.frame(GEOID=k, xtext=p$x, ytext=p$y))
# # 		}
# # 		shape.1@data <- left_join(shape.1@data, tmp.data)
# # va.coor <- coordinates(shape.1)
# # rownames(va.coor) <- paste(1:length(shape.1@data$GEOID),shape.1@data$GEOID)




redist.setup <- function(shp, uniqueid, population, vars, num.dist, ...) 
	{
		redist.out <- NULL
		redist.out$shp <- shp
		redist.out$adj <- sort.adj(adjlist(shp))
		redist.out$GEOID <- as.character(uniqueid)
		redist.out$newID <- 1:length(redist.out$adj)
		redist.out$id <- 1:length(redist.out$adj)
		for (i in 1:length(vars)) 
			{
				redist.out$data[[as.character(vars[i])]] <- shp@data[,vars[i]]
			}
		redist.out$pop <- redist.out$data[[as.character(population)]]
		redist.out$results <- as.list(1:length(redist.out$adj))
		
		
	# We now want to do our best to keep the maximum number of shape.1 whole, so we must discover how many districts can 
	# fit inside each of the shape.1 wholly, so we can first set those shape.1. 
	# Set ideal value to the zero devation population number for each district, ie, [total population] ÷ [# of districts]
		redist.out$meta$num.dist <- num.dist
		redist.out$meta$geoName <- "Unknown"
		redist.out$meta$date <- date()
		redist.out$ideal <- round(sum(as.numeric(redist.out$pop), na.rm=T) / redist.out$meta$num.dist, digits=0)
		return(redist.out)
	}

# We now create a contiguity list using the poly2nb function with queen set to FALSE (this means point contiguity is not enough)
adjlist <- function (shp.file, queen=F)
	{
			shp.adj <- spdep::poly2nb(shp.file, queen = F)
            for(i in 1:length(shp.adj))
            	{
                shp.adj[[i]] <- shp.adj[[i]]
           		}
            class(shp.adj) <- "list"
            return(shp.adj)
	}

read.shp <- function(x, proj) {
	shp <- readOGR(x)
	shp <- spTransform(shp, CRS(proj))
	return(shp)
}

 # a <- list(a$adj[[1453]], a$adj[[1454]])

merge.adj <- function(redist.out, x, y) 
	{
		cat("\nmerge", x, " and ", y, "\n\n")
		if (x[!is.na(x)] == y[!is.na(y)]) return(redist.out) 
		redist.out$adj <- rapply(redist.out$adj, function(w) ifelse(w==y,x,w), how="replace")
		redist.out$adj <- lapply(redist.out$adj, function(w) w[!w %in% y])		
		redist.out$adj[[x]] <- c(redist.out$adj[[x]], redist.out$adj[[y]])
		redist.out$adj[[y]] <- NA
		redist.out$adj[[x]] <- unique(redist.out$adj[[x]])

		redist.out$pop[[x]] <- redist.out$pop[[x]] + redist.out$pop[[y]]
		redist.out$pop[[y]] <- NA
				for (i in 1:length(redist.out$data)) 
			{
				redist.out$data[[i]][[x]] <- redist.out$data[[i]][[x]] + redist.out$data[[i]][[y]] 
				redist.out$data[[i]][[y]] <- NA
			}

		redist.out$results[[x]] <- c(redist.out$results[[x]], redist.out$results[[y]])
		redist.out$results[[y]] <- NA
		redist.out$newID[[y]] <- x
			cat("Number of Polygons left to combine:", length(redist.out$pop[!is.na(redist.out$pop)]) - redist.out$meta$num.dist, "\n")
		redist.out <- single.adj(redist.out)
	return(redist.out) 
	}

sample.adj <- function(x, n=1)
	{
		if (length(x)>1)
		{
			smp <- sample(x,n)
			return(smp)
		} else {
			return(x[1])
		}
	}

single.adj <- function(redist.out) {
		sgl.adj <- redist.out$id[sapply(redist.out$adj, function(x) (length(x[!is.na(x)]) == 1))]
		while (length(sgl.adj[!is.na(sgl.adj)]) > 0)	 
			{
				w <- sample.adj(sgl.adj)
				m <- sample.adj(redist.out$adj[[w]])
				redist.out <- merge.adj(redist.out, m, w) # Send to merge.adj function
				sgl.adj <- redist.out$id[sapply(redist.out$adj, function(x) (length(x[!is.na(x)]) == 1))]
		# cat("Single Adjacencies units left: \n", sgl.adj)
			}
				return(redist.out)	
	}


zero.pop <- function(redist.out, x, y) 
	{
		pop.adj <- which(redist.out$pop == 0)
		while (length(pop.adj[!is.na(pop.adj)]) > 0)
			{
				w <- sample.adj(pop.adj, 1)
				m <- sample.adj(redist.out$adj[[w]])
				redist.out <- merge.adj(redist.out, m, w) # Send to merge.adj function
		pop.adj <- which(redist.out$pop == 0)
		# cat("Zero-Pop units left: \n", pop.adj)
			}
		return(redist.out)
	}

# check.cont <- function (redist.out, x, y)
# 	{
# 	for (i in 1:length(redist.out$adj))
# 		{
# 			c.tmp <- redist.out$adj[[i]][!redist.out$adj[[i]] %in% y]
# 			if (length(c.tmp)==0) redist.out <- merge.adj(redist.out, x, c.tmp)
# 		}
# 	return(redist.out)
# 	}

sort.adj <- function(x) lapply(x, function(x) sort(unique(x[!is.na(x)])))

find.match <- function(redist.out)
	{
		merger <- mergee <- NA	
		n.units <- length(redist.out$adj)
		new.find <- function(n) sample(1:n.units, 1)
		while (is.na(merger))
			{
				merger <- new.find(n)
			}
				mergee <- sample.adj(redist.out$adj[[merger]])	
		return(c(merger, mergee))
	}



poly.label <- function (x) 
	{
		tmp.data <-  numeric()
		for (k in x@data[,"DISTRICT"]) {
			tmp.shp <- x[x@data[,"DISTRICT"]==k,]
			tmp.data <- rbind(tmp.data, data.frame(DISTRICT=k, xtext=p$x, ytext=p$y))
			}
			x@data <- left_join(x@data, tmp.data, by="DISTRICT")
		return(invisible(x))
	}
				
# new.check <- function(redist.out)
# 	{
# 		for (w in 1:length(redist.out$adj))
# 			{
# 				if (length(redist.out$adj[[w]][!is.na(redist.out$adj[[w]])]) == 0) next
# 				if (length(!is.na(redist.out$adj[[w]])) == 1) 
# 					{
# 						s <- redist.out$adj[[w]][1]
						
# 						cat("Polygon", w, "and", s, "\n New Polygon Adjacencies:", redist.out$adj[[w]], "\n")
# 					redist.out <- merge.adj(redist.out, w, s)
# 					return(redist.out)
# 		# Checks to see if any polygons have just one adjacency.
# 					}
# 			}
# 	}


shape.1 <- readOGR("/Users/user/Google Drive/GitHub/Data Files/GIS/Tigerline/2019/counties/tl_2019_us_county.shp")
shape.1 <- shape.1[shape.1@data$STATEFP %in% 13,]
xxnb <- spdep::poly2nb(shape.1, queen=F)
# We can map the contiguous boundaries to help get a sense of how places will be combined
# Queen == FALSE
res <- 4
svg("GA_county_contiquity.svg", width=8, height=9)
par(mar=c(0,0,0,0))
	plot(shape.1, lwd=0.5, border="#11111111")
		points(coordinates(shape.1)[,1], coordinates(shape.1)[,2], col="blue", cex=1, pch=16)
		text(coordinates(shape.1)[,1], coordinates(shape.1)[,2], labels = shape.1@data$NAME, col="#33333333", cex=0.6)
	plot(xxnb, cbind(coordinates(shape.1)[,1], coordinates(shape.1)[,2]), add=TRUE, col="blue", lwd=1)
dev.off()


#################################################################

# For any polygons that have only one adjacency but not enough population but by necessity first be combined with it's adjacency
district <- function(redist.out) 
	{
		redist.out <- single.adj(redist.out)
		redist.out <- zero.pop(redist.out)	
		
	while (length(redist.out$pop[!is.na(redist.out$pop)]) != redist.out$meta$num.dist) 
		{
		redist.out <- single.adj(redist.out)
# Now we want to randomize which geographies get combined. 
# We then merge the closest k geography, which attempts to increase compactness. 
	b <- find.match(redist.out)
	while (is.na(b[2])) b <- find.match(redist.out)
			redist.out <- merge.adj(redist.out, b[1], b[2]) # Send to merge.adj function

		}
		return(redist.out)
	}

agg.new <- function(redist.out)
	{
		baseshape <- redist.out$shp
		pop <- redist.out$pop[!is.na(redist.out$pop)]
		P001001 <- redist.out$shp@data$P001001[!is.na(redist.out$pop)]
		P003003 <- redist.out$shp@data$P003003[!is.na(redist.out$pop)]
		redist.out$shp$DISTRICT <- rownames(redist.out$shp@data)
	
	for (d in 1:length(redist.out$results[!is.na(redist.out$results)]) )
		{
			dis <- redist.out$results[!is.na(redist.out$results)][[d]][1]
			to.merge.adj <- redist.out$results[!is.na(redist.out$results)][[d]]
			redist.out$shp@data$DISTRICT[redist.out$shp@data$DISTRICT %in% to.merge.adj] <- dis
				
		}
			agg.data <- data.frame(DISTRICT = unique(redist.out$shp@data$DISTRICT), pop, P001001, P003003)
				row.names(agg.data) <- as.character(1:length(agg.data[,1]))
				redist.out$shp <- gUnaryUnion(redist.out$shp, id = redist.out$shp@data$DISTRICT)
				redist.out$shp <- spChFIDs(redist.out$shp, row.names(redist.out$shp))
				row.names(redist.out$shp) <- as.character(1:length(redist.out$shp))
				redist.out$shp <- SpatialPolygonsDataFrame(redist.out$shp, agg.data)
				redist.out$shp@data$DISTRICT <- 1:length(redist.out$shp@data$DISTRICT)
			redist.out$data <- lapply(redist.out$data, function(w) w[!is.na(w)])
			return(redist.out)
	}

redist.out <- redist.setup(blocks, blocks@data$GEOID10, pop = "P001001", vars = c("P001001", "P003003"), num.dist=7)
a <- district(redist.out) # This feeds into part two
# a <- redist.out
b <- agg.new(a) # Do this to plot currently updated map
plot(b$shp, col=cols)

# sumter_base <- spTransform(redist.out$shp[redist.out$shp@data$combindedpop > 0,], 
#                     CRS(proj))
sumter_carto <- cartogram_cont(b$shp, "pop", prepare = "adjust", threshold = 0.05, itermax=10)
sumter_cartodorling <- cartogram_dorling(sumter_base, "combindedpop")
sumter_nc <- cartogram_ncont(sumter_base, "combindedpop")

cols <- c("white", paste0("gray", 100:1), "black")
pop.brks <- c(-1, seq(0,1,0.01), 2)
cols <- c("gray100", "gray90", "gray70", "gray50", "gray30", "gray10")
pop.brks <- brks <- c(-1, 0, 0.10, 0.20, 0.30, 0.40, 0.50, 1)
gs.pop.blocks <- cols[findInterval(sumter_carto@data$black_pct, vec = pop.brks)]

cols <- c("#4B8EB3", "#FF626C", "#85D4FF", "#CCC956", "#B3B054", "#9EDDFF", "#122399")

###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
# Check for biggest imbalance between blobs.
	# From those two, randomly select a unit which on the border of the blob with a higher population and switch to blob with lower pop.
	# If pop is still too low, select a unit adjacent to that, perferable in the same county, which equalizes the pop.  If none do, then take the one that gets to the closest.
# 	# Repeat until it's within x%.  

# shape.1 <- a
# population <- cbind.data.frame(ID=shape.1@data$DISTRICT, Population=as.numeric(shape.1@data$Population), Ideal=as.numeric(shape.1@data$Population)/ideal)
# results <- as.list(1:length(adj)) 

# a <- adjlist(baseshape, queen=F)

# merger <- mergee <- numeric()
# merger <- which.max(abs(redist.out$pop - ideal))
# mergee <- redist.out$adj[[merger]][1]


# 		# if (is.na(redist.out$adj[[mergee]][1])) next
# 		# if (is.na(redist.out$pop[mergee,2])) next
# 				n <- 1
# 						while (merger==mergee)
# 					{
# 						n <- n+1
# 						if (is.na(redist.out$adj[[merger]][n+1])) break
# 						mergee <- redist.out$adj[[merger]][[n]]
# 					}
# 				if (merger==mergee) next
# 					cat(merger, mergee, "\n", redist.out$adj[[merger]], "and", redist.out$adj[[mergee]], "\n")
# 				a <- merge.adj(merger,mergee) # Send to merge.adj function
# 					for (w in 1:length(a))
# 						{
# 							if (length(redist.out$adj[[w]][!is.na(redist.out$adj[[w]])]) == 0) next
							
# 							if (length(!is.na(redist.out$adj[[w]]))!=1) next
# 								 cat(w, redist.out$adj[[w]][1], "\n", redist.out$adj[[w]], "and" , redist.out$adj[[redist.out$adj[[w]][1]]], "\n")
# 								 a <- merge.adj(redist.out$adj[[w]][1], w) 
# 						}


# plot(baseshape)

###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################



# res <- 4
# png("VA_New_Map.png", units="px", width=800*res, height=500*res)
# 			plot(shape.1, lty=3, border="gray70", lwd=res/2)
# 			plot(baseshape, lwd=res*2, add=T)
# 			# text(poly.label(baseshape), labels= paste0("Population: ", baseshape$shp@data$Population, "\n Ideal: ", round(baseshape$shp@data$Ideal, 2)), cex=1)
# dev.off()















