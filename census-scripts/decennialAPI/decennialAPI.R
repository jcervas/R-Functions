decennialAPI <- censusAPI <- function(
  state = NULL,
  geo = "tract",
  county = "*",
  table = NULL,
  year = "2020",
  variables = NULL,
  dataset = "pl",
  ucgid = NULL,
  api_key = Sys.getenv("CENSUS_API_KEY")
) {
  library(httr)
  library(jsonlite)

  if (missing(state) || is.null(state) || state == "") {
    stop("State is not defined. Please provide a valid state.")
  }

  if (is.null(api_key) || api_key == "") {
    stop("No Census API key found. Set CENSUS_API_KEY in your .Renviron or pass api_key= directly.")
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

  if (!is.null(variables)) {
    get_vars <- variables
  } else if (!is.null(table)) {
    get_vars <- paste0("group(", table, ")")
  } else {
    stop("Supply `variables` or `table`.")
  }

  # Block-level: Census API requires a specific county — loop over all counties and return early
  if (geo == "block" && is.null(ucgid) && county == "*") {
    counties_url <- paste0(
      base_url, "?get=NAME&for=county:*&in=state:", lookup_fips(state),
      "&key=", api_key
    )
    resp   <- httr::GET(counties_url)
    raw    <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyDataFrame = TRUE)
    raw_df <- as.data.frame(raw, stringsAsFactors = FALSE)
    colnames(raw_df) <- raw[1, ]
    raw_df <- raw_df[-1, ]
    all_counties <- raw_df[, "county"]

    return(do.call(rbind, lapply(all_counties, function(co) {
      decennialAPI(
        state = state, geo = "block", table = table,
        variables = variables, county = co,
        year = year, dataset = dataset, api_key = api_key
      )
    })))
  }

  variable_chunks <- split(get_vars, ceiling(seq_along(get_vars) / 50))
  data_list <- list()

  for (chunk in variable_chunks) {

    use_ucgid <- !is.null(ucgid)

    # ---- GEOGRAPHY LOGIC ----
    if (use_ucgid) {
      for_clause <- NULL
      in_clause  <- paste0("ucgid=", ucgid)

    } else if (geo == "state") {
      for_clause <- "state:*"
      in_clause  <- NULL

    } else if (geo == "county") {
      for_clause <- if (county != "*") paste0("county:", county) else "county:*"
      in_clause  <- paste0("state:", lookup_fips(state))

    } else if (geo == "tract") {
      for_clause <- "tract:*"
      in_clause  <- if (county == "*") {
        paste0("state:", lookup_fips(state))
      } else {
        paste0("state:", lookup_fips(state), "+county:", county)
      }

    } else if (geo == "block") {
      for_clause <- "block:*"
      in_clause  <- paste0("state:", lookup_fips(state), "+county:", county)

    } else {
      stop("Unsupported geography. Use: 'state', 'county', 'tract', or 'block'.")
    }

    api_url <- paste0(
      base_url,
      "?get=", paste(chunk, collapse = ","),
      if (!is.null(for_clause)) paste0("&for=", for_clause) else "",
      if (!is.null(in_clause))  paste0("&in=",  in_clause)  else "",
      "&key=", api_key
    )
    cat(api_url, "\n")

    response <- httr::GET(api_url)
    content  <- httr::content(response, as = "text")

    if (grepl("^\\s*<", content)) {
      stop("API returned HTML instead of JSON. URL was:\n", api_url,
           "\nCheck your API key and query parameters.")
    }

    chunk_data <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE)

    colnames(chunk_data) <- chunk_data[1, ]
    chunk_data <- chunk_data[-1, , drop = FALSE]
    chunk_data <- as.data.frame(chunk_data, stringsAsFactors = FALSE)
    rownames(chunk_data) <- NULL

    # Drop Census annotation columns (end in "NA")
    chunk_data <- chunk_data[, !grepl("NA$", names(chunk_data)), drop = FALSE]

    # Convert P-table columns to numeric
    for (col in grep("^P", names(chunk_data), value = TRUE)) {
      chunk_data[[col]] <- as.numeric(chunk_data[[col]])
    }

    data_list[[length(data_list) + 1]] <- chunk_data
  }

  if (length(data_list) > 1) {
    geo_cols <- names(data_list[[1]])[
      grep("^(state|county|tract|block|NAME|GEO_ID)$", names(data_list[[1]]))
    ]
    out <- data_list[[1]]
    for (i in 2:length(data_list)) {
      out <- merge(out, data_list[[i]], by = geo_cols, all = TRUE)
    }
    out
  } else {
    data_list[[1]]
  }
}
