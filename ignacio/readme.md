# Replication code to geolocate IP addresses

First, you need a code to get from the internet the coordinates from inputted IP addresses. We will use `ipgeolocation.io`.

The function `freegeoip()` takes an IP address as the input and will return a list of  
```{r}
require(rjson)
## Function for fetching and converting IP address geo-coordinates

freegeoip <- function(ip, format = ifelse(length(ip)==1,'dataframe','dataframe'))
{
    if (1 == length(ip))
    {
        # a single IP address
        require(rjson)
        url <- paste(c("https://api.ipgeolocation.io/ipgeo?apiKey=d46e7c1556fb42e3806ad46491fade5d&ip=", ip), collapse='')
        ret <- fromJSON(readLines(url, warn=FALSE))
        if (format == 'dataframe')
            ret <- data.frame(t(unlist(ret)))
        return(ret)
    } else {
        ret <- data.frame()
        for (i in 1:length(ip))
        {
            r <- freegeoip(ip[i], format="dataframe")
            ret <- rbind(ret, r)
        }
        return(ret)
    }
}   
```


Transform IP addresses to Long/Lat

```{r}
## Read in IP addresses
ip <- read.csv("https://raw.githubusercontent.com/jcervas/R-Functions/main/ignacio/data/ip.csv")

## Loop through each IP address and return geo-coordinates. If there is an error, move to next.
for (i in 1:nrow(ip)) {
       skip_to_next <- FALSE

  tryCatch(geo[i,] <- freegeoip(ip[i,]), error = function(e) {skip_to_next <<- TRUE})

  if(skip_to_next) { next } 
     
}
```

Write Data to file for import to Mapshaper

```{r}
head(geo)
write.csv(geo, "/Users/cervas/My Drive/GitHub/R-Functions/ignacio/data/geocode-ip.csv", row.names=F)
```


Open in your browser `mapshaper.org`.

Locate and drag file `data/geocode-ip.csv` into browser window. Then, using the console, create point shapefile using `-points` command.

`-points x=longitude y=latitude`

Set style so that radius of circle equal 1
`-each r=1`

Now drag file `gis/world_countries_2020.json` into mapshaper, creating a new layer. Using the arrow on the right, "select" Antarctica and delete it.

Style the map in console

`-style fill=#eeeeee stroke=#ffffff stroke-width=0.25`

Style the points

`-style fill=#eeeeee fill-opacity=0.25 stroke=#333333 stroke-width=1 stroke-opacity=0.5`


Export map (top right) as SVG.


Alternative version (circle sized to count):

```{r}
# Function to calculate the rScale
scaleSqrt <- function(value, maxRadius=20, maxDomain=NA) {
if (is.na(maxDomain)) {stop("Need max Domain")}
# Input domain values
     domain <- c(0, maxDomain)  # Example domain values
# Output range values
     range <- c(0, maxRadius)  # Example range values

  # Calculate the square root of the value
  sqrt_value <- sqrt(value)
  
  # Map the square root value to the output range
  scaled_value <- (sqrt_value - sqrt(domain[1])) / (sqrt(domain[2]) - sqrt(domain[1]))
  scaled_value <- scaled_value * (range[2] - range[1]) + range[1]
  
  return(scaled_value)
}
```

```{r}
geo <- read.csv("https://raw.githubusercontent.com/jcervas/R-Functions/main/ignacio/data/geocode-ip.csv")
    head(geo)
    
NewEngland <- c("Maine", "Vermont", "Massachusetts", "New Hampshire", "Connecticut", "Rhode Island")
NewYorkJerseyPennDelawareDC <- c("New York", "New Jersey", "Pennsylvania", "Delaware", "District of Columbia")
CentralAmerica <- c("Guatemala City", "San Salvador", "Tegucigalpa", "Managua", "Cartago", "San José", "San Jose", "Panamá")


geo$id <- geo$country_name
geo$id[geo$id %in% "United States"] <- geo$state_prov[geo$id %in% "United States"]

geo$id[geo$id %in% NewEngland] <- "_NewEngland"
geo$id[geo$id %in% NewYorkJerseyPennDelawareDC] <- "_NewYorkJerseyPennDelawareDC"
geo$id[geo$id %in% CentralAmerica] <- "_CentralAmerica"

geo_counts <- table(geo$id)
geo_split <- split(geo, geo$id)
geo_first_obs <- lapply(geo_split, head, n = 1)
geo_first_obs_df <- do.call(rbind, geo_first_obs)
geo_counts_df <- data.frame(geo_counts)


names(geo_counts_df) <- c("id", "count")
geo_counts_first_obs_df <- merge(geo_counts_df, geo_first_obs_df, by = "id")
geo_counts_first_obs_df$count_scaled <- 
    scaleSqrt(
        geo_counts_first_obs_df$count, 
        maxRadius=5, 
        maxDomain=max(geo_counts_first_obs_df$count))

geo_counts_first_obs_df$r <- scaleSqrt(
        geo_counts_first_obs_df$count, 
        maxRadius=4, 
        maxDomain=max(geo_counts_first_obs_df$count))
write.csv(geo_counts_first_obs_df, "/Users/cervas/My Drive/GitHub/R-Functions/ignacio/data/counts-city.csv")
```


In `mapshaper.org`, follow above instructions, but instead size circle by number of observations.

`-each r=count_scaled`


Plot in R
```{r}

world <- rgdal::readOGR("https://raw.githubusercontent.com/jcervas/R-Functions/main/ignacio/gis/world_countries_2020.json")

df <- geo_counts_first_obs_df

svglite::svglite(paste0(dir.download, "/ignacio.svg"), width=15, height=10)
sp::plot(world,
    col="#eeeeee",
    border="#ffffff",
    lwd=0.5,
    main = "Ignacio's Interviews")
points(
    df$longitude,
    df$latitude,
    cex=scaleSqrt(
        df$count, 
        maxRadius=4, 
        maxDomain=max(df$count)),
    col="#00000033",
    bg="#77777733",
    pch=16)

dev.off()
```

### In Mapshaper.org
```
mapshaper -i '/Users/cervas/My Drive/GitHub/R-Functions/ignacio/gis/world_countries_2020.json' name=gis \
-innerlines + name=lines \
-dissolve target=gis \
-i '/Users/cervas/My Drive/GitHub/R-Functions/ignacio/data/counts-city.csv' name=data \
-points target=data x=longitude y=latitude + name=points \
-proj target=gis,points webmercator \
-style target=gis fill=#eeeeee \
-style target=lines stroke=#ffffff stroke-width=0.5 \
-style target=points fill=#000000 stroke=#777777 opacity=0.33 \
-o target=gis,lines,points '/Users/cervas/My Drive/GitHub/R-Functions/ignacio/map-mapshaper.svg'
```