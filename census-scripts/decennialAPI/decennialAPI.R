
decennialAPI <- function(state=NULL, geo="tract", county="*", table="P1", year = "2020", variables=NULL) {

library(httr)
library(jsonlite)

  if (missing(state) || is.null(state) || state == "") {
    stop("State is not defined. Please provide a valid state or 'all' for all states.")
  }

# State abbreviations and FIPS codes
  lookup_fips <- function(state) {
  state <- tolower(state)
  fips <- state_fips$fips[tolower(state_fips$state) == state]
  
  if (length(fips) == 0) {
    stop("Invalid state name. Please provide a valid state.")
  }
  
  return(fips)
}

  get_county_fips <- read.csv("https://raw.githubusercontent.com/jcervas/R-Functions/main/census-scripts/decennialAPI/county_fips.csv")
  fips <- data.frame(state=sprintf("%02s", get_county_fips$state), county=sprintf("%03s", get_county_fips$county))

  state_fips <- data.frame(
    state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
              "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
              "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
              "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
              "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
    fips = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19",
             "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
             "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53",
             "54", "55", "56")
  )

# Handle "all states" case
if (tolower(state) == "all") {
  # Define the base URL and API key
  base_url <- paste0("https://api.census.gov/data/", year, "/dec/pl")
  api_key <- "7865f31139b09e17c5865a59c240bdf07f9f44fd"
  
  # Define the variables to request
  if (is.null(variables)) {
    get_vars <- paste0("group(", table, ")")
  } else {
    get_vars <- variables
  }
  
  # Use ucgid parameter for all states data
  if (geo == "state") {
    ucgid_param <- "pseudo(0100000US$0400000)"  # All states
  } else if (geo == "county") {
    ucgid_param <- "pseudo(0100000US$0500000)"  # All counties
  } else {
    # For other geographies, fall back to individual state calls
    all_data <- list()
    for (st in state_fips$state) {
      cat("Processing state:", st, "\n")
      state_data <- decennialAPI(state = st, geo = geo, county = county, table = table, year = year, variables = variables)
      all_data[[st]] <- state_data
    }
    combined_data <- do.call(rbind, all_data)
    return(combined_data)
  }
  
  # Chunk the variables into groups of 50
  variable_chunks <- split(get_vars, ceiling(seq_along(get_vars) / 50))
  
  # Create an empty list to store the results
  data_list <- list()
  
  # Iterate over the variable chunks
  for (chunk in variable_chunks) {
    get_params <- paste0("get=", paste(chunk, collapse = ","), "&ucgid=", ucgid_param)
    
    # Construct the full API URL
    api_url <- paste0(base_url, "?", get_params)
    print(api_url)
    
    # Make the GET request
    response <- httr::GET(api_url, httr::add_headers(Authorization = paste0("Bearer ", api_key)))
    
    # Extract the content from the response
    content <- httr::content(response, as = "text")
    
    # Convert the JSON content into a data frame
    chunk_data <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE, flatten = TRUE)
    
    colnames(chunk_data) <- chunk_data[1,]
    chunk_data <- chunk_data[-1,]
    
    # Convert to data frame
    chunk_data <- as.data.frame(chunk_data)
    
    # Delete "NA columns
    chunk_data <- chunk_data[, !grepl("NA$", names(chunk_data))]
    
    # Make data numeric
    for (col in colnames(chunk_data)[colnames(chunk_data) %in% grep("^P", colnames(chunk_data), value = TRUE)]) {
      chunk_data[, col] <- as.numeric(chunk_data[, col])
    }
    
    # Store the chunk data
    data_list[[length(data_list) + 1]] <- chunk_data
  }
  
  # Combine all chunks by merging on geographic identifiers
  if (length(data_list) > 1) {
    # Get geographic columns (typically the last few columns)
    geo_cols <- names(data_list[[1]])[grep("^(state|county|tract|block|NAME|ucgid)$", names(data_list[[1]]))]
    
    # Start with first chunk
    combined_data <- data_list[[1]]
    
    # Merge additional chunks
    for (i in 2:length(data_list)) {
      combined_data <- merge(combined_data, data_list[[i]], by = geo_cols, all = TRUE)
    }
    
    return(combined_data)
  } else {
    return(data_list[[1]])
  }
}


# Define the base URL and API key
base_url <- paste0("https://api.census.gov/data/", year, "/dec/pl")
api_key <- "7865f31139b09e17c5865a59c240bdf07f9f44fd"

# Define the variables to request
# vars <- c("001N", "002N", "003N", "004N", "005N", "006N", "007N", "008N", "009N", "010N", "011N", "012N", "013N", "014N", "015N", "016N", "017N", "018N", "019N", "020N", "021N", "022N", "023N", "024N", "025N", "026N", "027N", "028N", "029N", "030N", "031N", "032N", "033N", "034N", "035N", "036N", "037N", "038N", "039N", "040N", "041N", "042N", "043N", "044N", "045N", "046N", "047N", "048N", "049N", "050N", "051N", "052N", "053N", "054N", "055N", "056N", "057N", "058N", "059N", "060N", "061N", "062N", "063N", "064N", "065N", "066N", "067N", "068N", "069N", "070N", "071N")
# get_vars <- paste0(table, "_", vars)
  if (is.null(variables)) {
  get_vars <- paste0("group(", table, ")")
    } else {
    get_vars <- variables
    }

# https://api.census.gov/data/2020/dec/pl?get=P1_051N,P1_052N,P1_053N,P1_054N,P1_055N,P1_056N,P1_057N,P1_058N,P1_059N,P1_060N,P1_061N,P1_062N,P1_063N,P1_064N,P1_065N,P1_066N,P1_067N,P1_068N,P1_069N,P1_070N,P1_071N&for=block:*&in=state:06%20county:*


# Chunk the variables into groups of 50
variable_chunks <- split(get_vars, ceiling(seq_along(get_vars) / 50))

# Create an empty list to store the results
data_list <- list()

# Iterate over the variable chunks
for (chunk in variable_chunks) {
  # Construct the parameters for the request
  if (geo == "block") {
    in_param <- paste0(sprintf("%02s", lookup_fips(state)), "%20county:",county)
  } else {
    in_param <- sprintf("%02s", lookup_fips(state))
  }
  get_params <- paste0("get=", paste(chunk, collapse = ","), "&for=", geo, ":*&in=state:", in_param)

  # Construct the full API URL
  api_url <- paste0(base_url, "?", get_params)
  print(api_url)
        
  # Make the GET request
  response <- httr::GET(api_url, httr::add_headers(Authorization = paste0("Bearer ", api_key)))

  # Extract the content from the response
  content <- httr::content(response, as = "text")

  # Convert the JSON content into a data frame
  chunk_data <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE, flatten = TRUE)

  colnames(chunk_data) <- chunk_data[1,]
  chunk_data <- chunk_data[-1,]

  # Convert to data frame
  chunk_data <- as.data.frame(chunk_data)

  # Delete "NA columns
  chunk_data <- chunk_data[, !grepl("NA$", names(chunk_data))]

  # Make data numeric
  for (col in colnames(chunk_data)[colnames(chunk_data) %in% grep("^P", colnames(chunk_data), value = TRUE)]) {
    chunk_data[, col] <- as.numeric(chunk_data[, col])
  }
  
  # Store the chunk data
  data_list[[length(data_list) + 1]] <- chunk_data
}

# Combine all chunks by merging on geographic identifiers
if (length(data_list) > 1) {
  # Get geographic columns (typically the last few columns)
  geo_cols <- names(data_list[[1]])[grep("^(state|county|tract|block|NAME)$", names(data_list[[1]]))]
  
  # Start with first chunk
  combined_data <- data_list[[1]]
  
  # Merge additional chunks
  for (i in 2:length(data_list)) {
    combined_data <- merge(combined_data, data_list[[i]], by = geo_cols, all = TRUE)
  }
  
  data <- combined_data
} else {
  data <- data_list[[1]]
}
# Display the resulting data
# head(data)
# str(data)


return(data)
}

# Allow for either "decennialAPI" or "censusAPI" to call function
censusAPI <- decennialAPI
