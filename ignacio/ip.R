freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
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




ip <- read.csv("/Users/cervas/Downloads/IP addresses. Ignacio's survey.csv")

geo <- as.data.frame(freegeoip(ip[1,]))
for (i in 2:nrow(ip)) {
       skip_to_next <- FALSE

  # Note that print(b) fails since b doesn't exist

  tryCatch(geo[i,] <- as.data.frame(freegeoip(ip[i,])), error = function(e) {skip_to_next <<- TRUE})

  if(skip_to_next) { next } 
     
}

