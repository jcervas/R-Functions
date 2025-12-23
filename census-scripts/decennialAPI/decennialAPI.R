  any_part_black_p1 <- c("P1_004N","P1_011N","P1_016N","P1_017N","P1_018N","P1_019N","P1_027N","P1_028N","P1_029N","P1_030N","P1_037N","P1_038N","P1_039N","P1_040N","P1_041N","P1_042N","P1_048N","P1_049N","P1_050N","P1_051N","P1_052N","P1_053N","P1_058N","P1_059N","P1_060N","P1_061N","P1_064N","P1_065N","P1_066N","P1_067N","P1_069N","P1_071N")
  any_part_black_p2 <- c("P2_006N","P2_013N","P2_018N","P2_019N","P2_020N","P2_021N","P2_029N","P2_030N","P2_031N","P2_032N","P2_039N","P2_040N","P2_041N","P2_042N","P2_043N","P2_044N","P2_050N","P2_051N","P2_052N","P2_053N","P2_054N","P2_055N","P2_060N","P2_061N","P2_062N","P2_063N","P2_066N","P2_067N","P2_068N","P2_069N","P2_071N","P2_073N")

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
    stop("State is not defined. Please provide a valid state.")
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

  variable_chunks <- split(get_vars, ceiling(seq_along(get_vars) / 50))
  data_list <- list()

  for (chunk in variable_chunks) {

    ## ---- GEOGRAPHY LOGIC (2020 PL–CORRECT) ----
    if (geo == "county") {

      for_clause <- if (county != "*") paste0("county:", county) else "county:*"
      in_clause  <- paste0("state:", lookup_fips(state))
} else if (geo == "tract") {

  for_clause <- "tract:*"

  in_clause <- if (county == "*") {
    paste0("state:", lookup_fips(state))
  } else {
    paste0("state:", lookup_fips(state), "+county:", county)
  }
} else if (geo == "block") {

      for_clause <- "block:*"

      if (county == "*") {
        ## IMPORTANT: county wildcard must be in SAME in= clause
        in_clause <- paste0("state:", lookup_fips(state), "+county:*")
      } else {
        in_clause <- paste0("state:", lookup_fips(state), "+county:", county)
      }

    } else if (geo == "state") {

      for_clause <- "state:*"
      in_clause  <- NULL

    } else {
      stop("Unsupported geography.")
    }

    api_url <- paste0(
      base_url,
      "?get=", paste(chunk, collapse = ","),
      "&for=", for_clause,
      if (!is.null(in_clause)) paste0("&in=", in_clause) else ""
    )

    response <- httr::GET(api_url)
    content  <- httr::content(response, as = "text")
    chunk_data <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE)

    colnames(chunk_data) <- chunk_data[1, ]
    chunk_data <- chunk_data[-1, , drop = FALSE]
    chunk_data <- as.data.frame(chunk_data, stringsAsFactors = FALSE)
    rownames(chunk_data) <- NULL

    ## drop Census annotation variables
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
    out <- data_list[[1]]
    for (i in 2:length(data_list)) {
      out <- merge(out, data_list[[i]], by = geo_cols, all = TRUE)
    }
    out
  } else {
    data_list[[1]]
  }
}
