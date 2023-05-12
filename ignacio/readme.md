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

Export map (top right) as SVG.


Alternative version:

```{r}
geo$city[geo$city %in% ""] <- geo$state_prov[geo$city %in% ""]
geo_counts <- table(geo$city)
geo_split <- split(geo, geo$city)
geo_first_obs <- lapply(geo_split, head, n = 1)
geo_first_obs_df <- do.call(rbind, geo_first_obs)
geo_counts_df <- data.frame(geo_counts)
names(geo_counts_df) <- c("city", "count")
geo_counts_first_obs_df <- merge(geo_counts_df, geo_first_obs_df, by = "city")

write.csv(geo_counts_first_obs_df, "/Users/cervas/My Drive/GitHub/R-Functions/ignacio/data/counts-city.csv")
```

In `mapshaper.org`, follow above instructions, but instead size circle by number of observations.

`-each r=Math.sqrt(count)`