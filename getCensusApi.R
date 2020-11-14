getCensusApi <- function(data_url,key, get, region, numeric=TRUE){
  if(length(get)>1) get <- paste(get, collapse=',', sep='')
  api_call <- paste(data_url, 
                    'key=', key, 
                    '&get=', get,
                    '&', region,
                    sep='')

  dat_raw <- try(readLines(api_call, warn="F"))
  if(class(dat_raw)=='try-error') {
    print(api_call)
    return}
  dat_df <- data.frame()


  #split the datastream into a list with each row as an element
  # Thanks to roodmichael on github
  tmp <- strsplit(gsub("[^[:alnum:], _]", '', dat_raw), "\\,")
  #dat_df <- rbind(dat_df, t(sapply(tmp, '[')))
  #names(dat_df) <- sapply(dat_df[1,], as.character)
  #dat_df <- dat_df[-1,]
  dat_df <- as.data.frame(do.call(rbind, tmp[-1]), stringsAsFactors=FALSE)
  names(dat_df) <- tmp[[1]]
  # convert to numeric
  # The fips should stay as character... so how to distinguish fips from data?
  # I think all of the data have numbers in the names, the fips do not
  #  Example: field names of B01001_001E vs state
  if(numeric==TRUE){
    value_cols <- grep("[0-9]", names(dat_df), value=TRUE)
    for(col in value_cols) dat_df[,col] <- as.numeric(as.character(dat_df[,col]))
  }
  return(dat_df)
}

vecToChunk <- function(x, max=50){
  s <- seq_along(x)
  x1 <- split(x, ceiling(s/max))
  return(x1)
}

# getCensusApi("https://api.census.gov/data/2010/dec/sf1?", key=key, get=c("P010001","NAME"), region = "for=county:*&in=state:51")