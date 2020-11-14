# Redistricting Algorithm v2.1

### Data Collection

###### Tigerline Shapefile
First we must download the data we will use.
We begin with the geography files from the US Census Tigerline:

```
	shapeFile <- "tl_2018_us_county"

	# Only Run this if the file isn't already downloaded
	# tmp <- tempfile()
	# download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2018/COUNTY/tl_2018_us_county.zip", destfile = tmp)
	# unzip(tmp, exdir = ".")


	counties <- readOGR("." , shapeFile)
	va.counties <- counties[counties@data$STATEFP=="51",c("STATEFP", "COUNTYFP", "GEOID", "NAMELSAD", "NAME")]
	rownames(va.counties@data) <- paste(1:length(va.counties@data$NAMELSAD),va.counties@data$NAMELSAD)
	va.counties <- va.counties[map,]
```

###### Census Population
We then need to add in the most recent population data that will be used for districting:

```
va.pop <- getCensus(name="sf1", vintage=2010, vars=c("NAME","P0010001"), region="county:*", regionin="state:51", key=key)
va.pop$GEOID <- as.character(paste0(va.pop$state,va.pop$county))
colnames(va.pop)[colnames(va.pop)=="NAME"] <- "NAMELSAD"
va.counties@data <- left_join(va.counties@data, va.pop)
```  

### Data Preparation
We now create a contiguity list using the poly2nb function with queen set to FALSE (this means point contiguity is not enough):
```  
adjlist <- poly2nb(va.counties, queen = F, row.names=va.counties@data$NAMELSAD)
            for(i in 1:length(adjlist)){
                adjlist[[i]] <- adjlist[[i]]
            }
            class(adjlist) <- "list"
```  

###### Map Contiguities


We can map the contiguous boundaries to help get a sense of how places will be combined:
Queen == FALSE
```  
res <- 4
png("VA_county_contiquity.png", units="px", width=800*res, height=500*res)
	xxnb <- poly2nb(va.counties, queen=F)
	plot(va.counties, lwd=4)
		points(cbind(va.counties@data$xtext, va.counties@data$ytext), col=paste0("#000000", opacity[120]), cex=res, pch=16)
		text(va.counties@data$xtext, va.counties@data$ytext, labels = va.counties@data$NAME, col=paste0("#000000", opacity[30]), cex=2.5)
	plot(xxnb, cbind(va.counties@data$xtext, va.counties@data$ytext), add=TRUE, col="blue", lwd=4)
dev.off()
```  

<img src= "https://github.com/jcervas/districting/blob/master/VA_county_contiquity.png" width="800" />


### Algorithm

###### Population
This keeps track of the combined population
```
pop.aggregate <- function (x,y) {
	population[x,2] <- population[x,2] + population[y,2]
	population[y,2] <- NA
	population[,3] <- population[,2]/ideal
	return(population)
	}
```
###### Final assignments
This creates a list of which polygons belong together, which can later be combined to create new shapefiles or other data.
```
assignments <- function (x,y) {
	results[[x]] <- c(results[[x]],results[[y]])
	results[[y]] <- NA
	return(results)
	}
```
##### Combining polygons
This is the powerhouse of the Algorithm that combines polygons

```
	merge <- function (x,y) {
		a[[x]] <- c(a[[x]],a[[y]])
		a <- lapply(a, function(x) x[x!=y])
		lapply(a, function(x) length(a)==)
		a[[x]] <- unique(a[[x]], y)
		a[[x]] <- a[[x]][a[[x]] != x]
		a[[y]] <- NA
			population <<- pop.aggregate(x, y)
			results <<- assignments(x, y)
	return(a)
		}
```

# Running the Algorithm
First set the population ideal (to be used in future versions)
```
ideal <- 80010
```

We now want to start combining polygons until all of the smaller population polygons are combined with it's adjacencies. All combined polygons need to have a population to fill at least one seat (ideal):


|        County        | Seats Per County |   |         County        | Seats Per County |
|:--------------------:|:----------------:|:-:|:---------------------:|:----------------:|
|     Poquoson city    |       0.15       |   |   Greensville County  |       0.15       |
|   James City County  |       0.84       |   |    Dinwiddie County   |       0.35       |
|     Surry County     |       0.09       |   |      Norfolk city     |       3.03       |
|    Hanover County    |       1.25       |   |     Amelia County     |       0.16       |
|     Emporia city     |       0.07       |   |   Williamsburg city   |       0.18       |
|     Richmond city    |       2.55       |   |    Nottoway County    |        0.2       |
|   Goochland County   |       0.27       |   |    Petersburg city    |       0.41       |
|   Lunenburg County   |       0.16       |   |   Southampton County  |       0.23       |
|    Chesapeake city   |       2.78       |   |      Suffolk city     |       1.06       |
| Isle of Wight County |       0.44       |   |      York County      |       0.82       |
|    Henrico County    |       3.84       |   |   Northampton County  |       0.15       |
|     Franklin city    |       0.11       |   |  Chesterfield County  |       3.95       |
|    New Kent County   |       0.23       |   |    Brunswick County   |       0.22       |
|  Charles City County |       0.09       |   |  Prince George County |       0.45       |
|   Newport News city  |       2.26       |   |      Hampton city     |       1.72       |
|     Sussex County    |       0.15       |   | Colonial Heights city |       0.22       |
|     Hopewell city    |       0.28       |   |  Virginia Beach city  |       5.47       |
|    Portsmouth city   |       1.19       |   |   Mecklenburg County  |       0.41       |

###### Adjacency issues
Some polygons, such as Emporia in this example, do not have any adjacencies but one, since it is completely incapsulated by another polygon. These types of polygons, by necessity since they don't have enough population to create a district, must be combined before moving forward. This process is looped until there are no single adjacency polygons left, and will start again after every merged polygon action.

```
	for (i in 1:length(a))
		{
			if (length(a[[i]][!is.na(a[[i]])]) >= 2) next # Multiple adjacencies means no need to take action on this polygon

			if (length(a[[i]][!is.na(a[[i]])]) == 0) next # if this polygon has already been merged with another go to next

				n <- 1
			m <- a [[i]][n]

				while (m==i)
				{
					if (is.na(a[[i]][n+1])) break
					n <- n+1
					m <- a[[i]][[n]]
				}
					if (m==i) next
			a <- merge(i,m) # Send to merge function
			print(c(i, m))
		}
if (any(lapply(a[!is.na(a)], function (x) length(x))==1)) next # Checks to see if any polygons have just one adjacency.
```

###### Randomly selected polygons to merged
Now we want to randomize which geographies get combined. We then merge the k-closest geography, which attempts to maximize compactness.
```
merger <- sample(which(population[,3] <= 1), size=1, replace=F)
	if (is.na(population[merger,3]) >= 1) break
	if (is.na(population[merger,2])) next

mergee <- a[[merger]][1]

	if (is.na(mergee)) next
	if (is.na(population[mergee,2])) next
	print(c(merger, mergee))
	a <- merge(merger, mergee)
```

###### Resulting Combinations
We are left with a select few districts after multiple iterations.  Core counties are the left-over polygons which were only the merger and never the mergee. The table with the population can be matched by the first polygon on the list of combined polygons:

|      Core County     | Population | Seats Per District |   |     Core County     | Population | Seats Per District |   |      Core County      | Population | Seats Per District |
|:--------------------:|:----------:|:-----------------:|:-:|:-------------------:|:----------:|:-----------------:|:-:|:---------------------:|:----------:|:-----------------:|
|     Poquoson city    |     NA     |         NA        |   |   New Kent County   |     NA     |         NA        |   |    Petersburg city    |     NA     |         NA        |
|   James City County  |     NA     |         NA        |   | Charles City County |     NA     |         NA        |   |   Southampton County  |     NA     |         NA        |
|     Surry County     |     NA     |         NA        |   |  Newport News city  |     NA     |         NA        |   |      Suffolk city     |     NA     |         NA        |
|    Hanover County    |     NA     |         NA        |   |    Sussex County    |     NA     |         NA        |   |      York County      |     NA     |         NA        |
|     Emporia city     |   81,245   |        1.02       |   |    Hopewell city    |     NA     |         NA        |   |   Northampton County  |     NA     |         NA        |
|     Richmond city    |  1,005,285 |       12.56       |   |   Portsmouth city   |   95,535   |        1.19       |   |  Chesterfield County  |     NA     |         NA        |
|   Goochland County   |     NA     |         NA        |   |  Greensville County |     NA     |         NA        |   |    Brunswick County   |     NA     |         NA        |
|   Lunenburg County   |     NA     |         NA        |   |   Dinwiddie County  |     NA     |         NA        |   |  Prince George County |     NA     |         NA        |
|    Chesapeake city   |   222,209  |        2.78       |   |     Norfolk city    |   242,803  |        3.03       |   |      Hampton city     |     NA     |         NA        |
| Isle of Wight County |     NA     |         NA        |   |    Amelia County    |   56,544   |        0.71       |   | Colonial Heights city |   424,383  |        5.30       |
|    Henrico County    |     NA     |         NA        |   |  Williamsburg city  |   308,516  |        3.86       |   |  Virginia Beach city  |   437,994  |        5.47       |
|     Franklin city    |     NA     |         NA        |   |   Nottoway County   |     NA     |         NA        |   |   Mecklenburg County  |     NA     |         NA        |

###### Combined Polygons
| District 	|                       	|                      	|                      	|
|----------	|-----------------------	|----------------------	|----------------------	|
|        1 	|      Emporia city     	|  Greensville County  	|   Brunswick County   	|
|          	|   Mecklenburg County  	|   Lunenburg County   	|                      	|
|          	|                       	|                      	|                      	|
|        2 	|     Richmond city     	|    Henrico County    	|   Goochland County   	|
|          	|     Hanover County    	|    New Kent County   	|  Charles City County 	|
|          	|      Surry County     	|     Sussex County    	|  Southampton County  	|
|          	|     Franklin city     	| Isle of Wight County 	|   Newport News city  	|
|          	|      Suffolk city     	|                      	|                      	|
|          	|                       	|                      	|                      	|
|        3 	|    Chesapeake city    	|                      	|                      	|
|          	|                       	|                      	|                      	|
|        4 	|    Portsmouth city    	|                      	|                      	|
|          	|                       	|                      	|                      	|
|        5 	|      Norfolk city     	|                      	|                      	|
|          	|                       	|                      	|                      	|
|        6 	|     Amelia County     	|   Dinwiddie County   	|    Nottoway County   	|
|          	|                       	|                      	|                      	|
|        7 	|   Williamsburg city   	|      York County     	|     Poquoson city    	|
|          	|   James City County   	|  Northampton County  	|     Hampton city     	|
|          	|                       	|                      	|                      	|
|        8 	| Colonial Heights city 	|    Petersburg city   	| Prince George County 	|
|          	|     Hopewell city     	|  Chesterfield County 	|                      	|
|          	|                       	|                      	|                      	|
|        9 	|  Virginia Beach city  	|                      	|                      	|

### Final Map (so far)
<img src= "https://github.com/jcervas/districting/blob/master/VA_New_Map.png" width="800" />


## Next Steps

Pivot populations from one module to an adjacent one such to create integer ideal numbers of districts, starting with ___________ and then proceeding by _________.
