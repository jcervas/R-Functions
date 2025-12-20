censusAPI <- decennialAPI <- function(
  state = NULL,
  geo = "tract",
  county = "*",
  table = NULL,
  year = "2020",
  variables = NULL,
  dataset = "pl"
) {

  library(httr)
  library(jsonlite)

  if (missing(state) || is.null(state) || state == "") {
    stop("State is not defined. Please provide a valid state or 'all'.")
  }

  state_fips <- data.frame(
    state = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
              "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
              "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
              "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
              "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
    fips = c("01","02","04","05","06","08","09","10","11","12",
             "13","15","16","17","18","19","20","21","22","23",
             "24","25","26","27","28","29","30","31","32","33",
             "34","35","36","37","38","39","40","41","42","44",
             "45","46","47","48","49","50","51","53","54","55","56")
  )

  lookup_fips <- function(st) {
    st <- toupper(st)
    f <- state_fips$fips[state_fips$state == st]
    if (length(f) == 0) stop("Invalid state abbreviation.")
    f
  }

  base_url <- paste0("https://api.census.gov/data/", year, "/dec/", dataset)
  api_key  <- "7865f31139b09e17c5865a59c240bdf07f9f44fd"

  if (!is.null(variables)) {
    get_vars <- variables
  } else if (!is.null(table)) {
    get_vars <- paste0("group(", table, ")")
  } else {
    stop("Supply `variables` or `table`.")
  }

  variable_chunks <- split(get_vars, ceiling(seq_along(get_vars) / 50))
  data_list <- list()

  for (chunk in variable_chunks) {

    ## ---- GEOGRAPHY LOGIC (PL-CORRECT) ----
    if (geo == "county") {

      for_clause <- if (county != "*") {
        paste0("county:", county)
      } else {
        "county:*"
      }

      in_clause <- paste0("state:", lookup_fips(state))

    } else if (geo == "tract") {

      if (county == "*") stop("Tract requests require a specific county FIPS.")

      for_clause <- "tract:*"
      in_clause  <- paste0(
        "state:", lookup_fips(state),
        "+county:", county
      )

    } else if (geo == "block") {

      for_clause <- "block:*"

      if (county == "*") {
        ## PL allows all counties for blocks
        in_clause <- paste0("state:", lookup_fips(state))
        extra_in  <- "&in=county:*"
      } else {
        in_clause <- paste0(
          "state:", lookup_fips(state),
          "+county:", county
        )
        extra_in <- ""
      }

    } else if (geo == "state") {

      for_clause <- "state:*"
      in_clause  <- NULL
      extra_in  <- ""

    } else {
      stop("Unsupported geography.")
    }

    api_url <- paste0(
      base_url,
      "?get=", paste(chunk, collapse = ","),
      "&for=", for_clause,
      if (!is.null(in_clause)) paste0("&in=", in_clause) else "",
      if (exists("extra_in")) extra_in else ""
    )

    response <- httr::GET(
      api_url,
      httr::add_headers(Authorization = paste0("Bearer ", api_key))
    )

    content <- httr::content(response, as = "text")
    chunk_data <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE)

    colnames(chunk_data) <- chunk_data[1, ]
    chunk_data <- chunk_data[-1, , drop = FALSE]
    chunk_data <- as.data.frame(chunk_data, stringsAsFactors = FALSE)
    rownames(chunk_data) <- NULL

    ## drop annotation variables
    chunk_data <- chunk_data[, !grepl("NA$", names(chunk_data)), drop = FALSE]

    for (col in grep("^P", names(chunk_data), value = TRUE)) {
      chunk_data[[col]] <- as.numeric(chunk_data[[col]])
    }

    data_list[[length(data_list) + 1]] <- chunk_data
  }

  if (length(data_list) > 1) {
    geo_cols <- names(data_list[[1]])[
      grep("^(state|county|tract|block|NAME|GEO_ID)$", names(data_list[[1]]))
    ]
    combined <- data_list[[1]]
    for (i in 2:length(data_list)) {
      combined <- merge(combined, data_list[[i]], by = geo_cols, all = TRUE)
    }
    combined
  } else {
    data_list[[1]]
  }
}
