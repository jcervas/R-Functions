geo_prefix_root <- function(state_fips) sprintf("pseudo(0400000US%02d", as.integer(state_fips))

build_dataset_map <- function(acs_year = 5) {
  # Validate acs_year input
  if (!acs_year %in% c(1, 3, 5)) {
    stop("acs_year must be 1, 3, or 5")
  }
  
  acs_suffix <- paste0("acs", acs_year)
  c(
    DP = paste0("acs/", acs_suffix, "/profile"),
    S = paste0("acs/", acs_suffix, "/subject"), 
    default = paste0("acs/", acs_suffix),
    pums = paste0("acs/", acs_suffix, "/pums")
  )
}

infer_dataset_from_table <- function(table, acs_year = 5) {
  dataset_map <- build_dataset_map(acs_year)
  if (is.null(table) || nchar(table) < 1) return(dataset_map["default"])
  tbl <- toupper(table)
  if (tbl == "PUMS") {
    return(dataset_map["pums"])
  }
  # for profile tables starting with DP, use the two-letter key
  if (nzchar(tbl) && substr(tbl,1,2) %in% names(dataset_map)) {
    return(dataset_map[substr(tbl,1,2)])
  }
  # otherwise fall back on single-letter prefix
  prefix <- substr(tbl, 1, 1)
  if (prefix %in% names(dataset_map)) return(dataset_map[prefix])
  dataset_map["default"]
}

append_key <- function(url, key = Sys.getenv("CENSUS_API_KEY", "")) {
  if (is.null(key) || !nzchar(key)) return(url)
  paste0(url, "&key=", utils::URLencode(key, reserved = TRUE))
}

construct_group_url <- function(table, geography, geo_filter, state_fips, year, dataset, key = Sys.getenv("CENSUS_API_KEY", "")) {
  # (we'll only use geo_filter/state_fips for county/place/etc.;
  #  for AIANNH, we ignore state_fips entirely)
  
  if (geography == "state") {
    # State‐level: if state_fips is NULL, get all states; otherwise get specific state
    if (is.null(state_fips)) {
      ucgid     <- "pseudo(0100000US$0400000)"  # All states
      geo_clause<- paste0("&ucgid=", ucgid)
    } else {
      ucgid     <- paste0("pseudo(0400000US", state_fips, ")")
      geo_clause<- paste0("&ucgid=", ucgid)
    }
    
  } else if (geography == "aian") {
    # National‐level AIANNH: summary level 251, root = 0100000US
    # → "pseudo(0100000US$2500000)"
    ucgid     <- "pseudo(0100000US$2500000)"
    # no URLencode needed (no extra characters besides "(" ")" and "$")
    geo_clause<- paste0("&ucgid=", ucgid)
    
  } else if (geography == "cd") {
    # National‐level Congressional Districts: summary level 500, root = 0100000US
    # → "pseudo(0100000US$5000000)"
    ucgid     <- "pseudo(0100000US$5000000)"
    # no URLencode needed (no extra characters besides "(" ")" and "$")
    geo_clause<- paste0("&ucgid=", ucgid)
    
  } else {
    # county/place/county subdivision/puma: same as before, but leave AIANNH out
    root   <- geo_prefix_root(state_fips)  # e.g. "pseudo(0400000US36" if state_fips = "36"
    suffix <- switch(
      geography,
      county               = "$0500000)",
      place                = "$1600000)",
      "county subdivision" = "$0600000)",
      puma                 = "$7950000)",
      stop("Unsupported geography for group URL.")
    )
    ucgid     <- paste0(root, suffix)
    geo_clause<- paste0("&ucgid=", URLencode(ucgid, reserved = TRUE))
  }
  
  vars_clause <- paste0("group(", table, ")")
  url <- paste0(
    "https://api.census.gov/data/", year, "/", dataset,
    "?get=",     vars_clause,
    geo_clause
  )
  append_key(url, key)
}


construct_standard_url <- function(variables, geography, state_fips, year, dataset, key = Sys.getenv("CENSUS_API_KEY", "")) {
  vars_clause <- paste(variables, collapse = ",")
  
  if (grepl("/pums$", dataset)) {
    geo_clause <- paste0("&for=state:", state_fips)
  } else {
    geo_clause <- switch(
      geography,
      
      state   = if (is.null(state_fips)) "&for=state:*" else paste0("&for=state:", state_fips),
      county  = paste0("&for=county:*&in=state:", state_fips),
      place   = paste0("&for=place:*&in=state:",  state_fips),
      "county subdivision" = paste0("&for=county%20subdivision:*&in=state:", state_fips),
      
      aiannh  = "&for=american%20indian%20area/alaska%20native%20area/hawaiian%20home%20land:*",   # NO "&in=state:" because ACS does not support it
      cd      = "&for=congressional%20district:*",   # Congressional Districts - national level
      
      stop("Unsupported geography")
    )
  }
  
  url <- paste0(
    "https://api.census.gov/data/", year, "/", dataset,
    "?get=",     vars_clause,
    geo_clause
  )
  append_key(url, key)
}


normalize_inputs <- function(table, variables, custom_states, state_fips, dataset, acs_year = 5) {
  table <- if (!is.null(table)) toupper(table) else NULL
  if (!is.null(variables)) variables <- toupper(variables)
  if (!is.null(custom_states) && length(custom_states) == 1) {
    state_fips <- custom_states[[1]]
    custom_states <- NULL
  }
  if (is.null(dataset)) dataset <- infer_dataset_from_table(table, acs_year)
  list(
    table = table,
    variables = variables,
    custom_states = custom_states,
    state_fips = state_fips,
    dataset = dataset
  )
}

get_acs_data <- function(url, timeout = 300) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("Please install 'httr'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'.")
  
  resp <- httr::GET(url, httr::timeout(timeout))
  status <- httr::status_code(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  ctype <- httr::headers(resp)[["content-type"]]
  
  if (grepl("key must be included", txt, ignore.case = TRUE)) {
    stop("Census API key required. Set CENSUS_API_KEY or pass a key argument.")
  }
  
  if (grepl("invalid key", txt, ignore.case = TRUE) || grepl("key provided is not valid", txt, ignore.case = TRUE)) {
    stop("Census API key appears invalid. Response starts: ", substr(txt, 1, 400))
  }
  
  if (status >= 400) {
    stop(
      "Census API HTTP error ", status,
      if (!is.null(ctype)) paste0(" (content-type: ", ctype, ")") else "",
      ". Response starts: ",
      substr(txt, 1, 400),
      "\nURL: ", url
    )
  }
  
  if (grepl("^\\s*<html", txt, ignore.case = TRUE)) {
    stop(
      "Census API returned HTML instead of JSON",
      if (!is.null(ctype)) paste0(" (content-type: ", ctype, ")") else "",
      ". Response starts: ",
      substr(txt, 1, 400),
      "\nURL: ", url
    )
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(txt),
    error = function(e) stop(
      "Invalid JSON from Census API: ", e$message,
      "\nResponse starts: ", substr(txt, 1, 400),
      "\nURL: ", url
    )
  )
  
  if (!(is.matrix(parsed) || is.list(parsed))) {
    stop("Unexpected API response format")
  }
  
  df <- as.data.frame(parsed[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- unlist(parsed[1, ])
  df
}


compute_geoid <- function(df) {
  if ("GEO_ID" %in% names(df)) {
    df$GEOID <- sub(".*US", "", df$GEO_ID)
    df$GEO_ID <- NULL
  } else if ("ucgid" %in% names(df)) {
    df$GEOID <- sub(".*US", "", df$ucgid)
    df$ucgid <- NULL
  } else if (all(c("state", "county") %in% names(df))) {
    df$GEOID <- paste0(df$state, df$county)
  } else if (all(c("state", "place") %in% names(df))) {
    df$GEOID <- paste0(df$state, df$place)
  } else if (all(c("state", "county", "county subdivision") %in% names(df))) {
    df$GEOID <- paste0(df$state, df$county, df[["county subdivision"]])
  } else if ("state" %in% names(df) && !("county" %in% names(df)) && !("place" %in% names(df))) {
    df$GEOID <- df$state
  } else if ("congressional district" %in% names(df)) {
    df$GEOID <- df[["congressional district"]]
  } else if ("american indian area/alaska native area/hawaiian home land" %in% names(df)) {
    df$GEOID <- df[["american indian area/alaska native area/hawaiian home land"]]
  } else {
    stop("Cannot compute GEOID: response lacks GEO_ID/ucgid and no known geographic columns were found.")
  }
  df
}

make_numeric <- function(df) {
  keep <- c("NAME", "GEOID")
  nums <- setdiff(names(df), keep)
  df[nums] <- lapply(df[nums], function(x) suppressWarnings(as.numeric(x)))
  df
}

handle_group_logic <- function(use_group, table, variables, geo_filter, geography, state_fips, year, dataset, key = Sys.getenv("CENSUS_API_KEY", "")) {
  if (!is.null(table) && is.null(variables)) use_group <- TRUE
  if (use_group && is.null(geo_filter)) {
    if (geography == "state") {
      geo_filter <- if (is.null(state_fips)) list("dummy") else list(state_fips)
    } else if (geography == "county") {
      url <- append_key(
        paste0("https://api.census.gov/data/", year, "/", dataset,
               "?get=NAME&for=county:*&in=state:", state_fips),
        key
      )
      raw <- tryCatch(readLines(url, warn = FALSE), error = function(e) NULL)
      parsed <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
      if (is.null(parsed) || length(parsed) < 2) stop("Failed to retrieve FIPS codes")
      counties <- as.data.frame(parsed[-1, , drop = FALSE], stringsAsFactors = FALSE)
      colnames(counties) <- parsed[1, ]
      geo_filter <- unique(sprintf("%03d", as.integer(counties[, "county"])))
    } else {
      geo_filter <- list("dummy")
    }
  }
  list(use_group = use_group, geo_filter = geo_filter)
}

apply_tidy <- function(df, tidy) {
  if (tidy) {
    df <- reshape(df,
      varying = list(names(df)[!(names(df) %in% c("NAME", "GEOID"))]),
      v.names = "value",
      timevar = "variable",
      times = names(df)[!(names(df) %in% c("NAME", "GEOID"))],
      new.row.names = 1:(nrow(df) * (ncol(df) - 2)),
      direction = "long"
    )
    rownames(df) <- NULL
  }
  df
}

apply_labels <- local({
  cache <- NULL
  function(df, label, acs_year = 5, year = 2023) {
    if (!label) return(df)
    vars <- setdiff(names(df), c("NAME", "GEOID"))
    acs_suffix <- paste0("acs", acs_year)
    labels_url <- paste0("https://api.census.gov/data/", year, "/acs/", acs_suffix, "/variables.json")
    if (is.null(cache)) cache <<- tryCatch(jsonlite::fromJSON(labels_url)$variables, error = function(e) NULL)
    if (!is.null(cache)) {
      lbls <- sapply(vars, function(v) if (!is.null(cache[[v]]$label)) cache[[v]]$label else v, USE.NAMES = FALSE)
      names(df)[match(vars, names(df))] <- lbls
    }
    df
  }
})

maybe_save_to_file <- function(df, save_to) {
  if (!is.null(save_to)) write.csv(df, save_to, row.names = FALSE)
}

# Main function to retrieve ACS data
# Parameters:
#   state_fips: FIPS code for the state (required for most geographies except 'state', 'aian', 'cd')
#               For 'state' geography, if NULL, returns data for all states
#   acs_year: ACS dataset year (1, 3, or 5 for 1-year, 3-year, or 5-year estimates)
#             Default is 5 (5-year estimates)
acsAPI <- function(table = NULL,
                    state_fips = NULL,
                    geography = "county",
                    geo_filter = NULL,
                    custom_states = NULL,
                    year = 2023,
                    dataset = NULL,
                    variables = NULL,
                    tidy = FALSE,
                    save_to = NULL,
                    label = FALSE,
                    var_types = c("E"),
                    use_group = FALSE,
                    acs_year = 5,
                    key = Sys.getenv("CENSUS_API_KEY", "")) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'.")
  
  # Validate state_fips for geographies that require it (except "state" which can get all states)
  if (is.null(state_fips) && !geography %in% c("state", "aian", "cd")) {
    stop("state_fips is required for geography '", geography, "'. Please provide a state FIPS code (e.g., '36' for New York).")
  }
  
  norm <- normalize_inputs(table, variables, custom_states, state_fips, dataset, acs_year)
  table <- norm$table; variables <- norm$variables; state_fips <- norm$state_fips; dataset <- norm$dataset
  gl <- handle_group_logic(use_group, table, variables, geo_filter, geography, state_fips, year, dataset, key)
  use_group <- gl$use_group; geo_filter <- gl$geo_filter
  if (use_group) {
    url <- construct_group_url(table, geography, geo_filter, state_fips, year, dataset, key)
    message("Group API URL: ", url)
    df <- get_acs_data(url)
    base_names <- sub("\\.\\d+$", "", names(df))
    df <- df[, !duplicated(base_names)]
    df <- compute_geoid(df)
    df <- make_numeric(df)
    core   <- c("NAME","GEOID")
    type_pattern <- paste0("_\\d+(", paste(var_types, collapse="|"), ")$")
    keep   <- setdiff(grep(type_pattern, names(df), value=TRUE), core)
    df      <- df[c(core, keep)]
    df <- apply_tidy(df, tidy)
    df <- apply_labels(df, label, acs_year, year)
    maybe_save_to_file(df, save_to)
    return(df)
  } else {
    if (is.null(variables)) stop("Must specify variables or set use_group = TRUE.")
    url <- construct_standard_url(variables, geography, state_fips, year, dataset, key)
    message("Standard API URL: ", url)
    df <- get_acs_data(url)
    df <- compute_geoid(df)
    df <- make_numeric(df)
    df <- apply_tidy(df, tidy)
    df <- apply_labels(df, label, acs_year, year)
    maybe_save_to_file(df, save_to)
    return(df)
  }
}

# Wrapper: Retrieve PUMS (Public Use Microdata Sample) data
# Parameters:
#   state_fips: FIPS code for the state (required for PUMS data)
#   acs_year: ACS dataset year (1, 3, or 5 for 1-year, 3-year, or 5-year estimates)
#             Default is 5 (5-year estimates)
get_pums <- function(variables,
                     state_fips = NULL,
                     year       = 2023,
                     tidy       = FALSE,
                     save_to    = NULL,
                     label      = FALSE,
                     var_types  = NULL,
                     acs_year   = 5,
                     key        = Sys.getenv("CENSUS_API_KEY", "")) {
  if (is.null(variables) || length(variables) == 0) {
    stop("Must specify at least one variable for PUMS.")
  }
  
  if (is.null(state_fips)) {
    stop("state_fips is required for PUMS data. Please provide a state FIPS code (e.g., '36' for New York).")
  }
  
  # 1) Always use both join‐keys, even if the user only wants housing‐unit variables.
  join_keys <- c("SERIALNO", "SPORDER")
  
  # 2) Uppercase the user's requested variables
  vars_upper <- toupper(variables)
  
  # 3) If either join_key is missing, prepend both in order
  missing_keys <- setdiff(join_keys, vars_upper)
  if (length(missing_keys) > 0) {
    vars_upper <- unique(c(join_keys, vars_upper))
  }
  
  # 4) Split into ≤50‐variable chunks.
  #    Each chunk must contain both SERIALNO and SPORDER, leaving 48 "other" slots.
  dataset_map <- build_dataset_map(acs_year)
  dataset    <- dataset_map["pums"]
  max_vars   <- 50
  all_vars   <- unique(vars_upper)
  other_vars <- setdiff(all_vars, join_keys)
  
  chunk_size  <- max_vars - length(join_keys)  # 50 - 2 = 48
  chunk_parts <- split(
    other_vars,
    ceiling(seq_along(other_vars) / chunk_size)
  )
  chunks      <- lapply(chunk_parts, function(x) c(join_keys, x))
  
  # 5) Fetch each chunk and coerce join_keys to character
  df_list <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    this_vars <- chunks[[i]]
    url <- construct_standard_url(
      variables  = this_vars,
      geography   = "state",
      state_fips  = state_fips,
      year        = year,
      dataset     = dataset,
      key         = key
    )
    message(sprintf("PUMS API URL (chunk %d/%d): %s",
                    i, length(chunks), url))
    
    df_chunk <- get_acs_data(url)
    # Coerce both join‐keys to character so merge works perfectly
    for (key_name in join_keys) {
      if (!(key_name %in% names(df_chunk))) {
        stop("Join‐key '", key_name, "' not found in chunk ", i)
      }
      df_chunk[[key_name]] <- as.character(df_chunk[[key_name]])
    }
    df_list[[i]] <- df_chunk
  }
  
  # 6) Merge every chunk on c("SERIALNO","SPORDER")
  df_merged <- df_list[[1]]
  if (length(df_list) > 1) {
    for (i in 2:length(df_list)) {
      df_merged <- merge(
        df_merged, 
        df_list[[i]],
        by   = join_keys,
        all  = TRUE,
        sort = FALSE
      )
    }
  }
  
  # 7) Post‐processing: numeric conversion, tidy, label, optional save
  df_out <- make_numeric(df_merged)
  df_out <- apply_tidy(df_out, tidy)
  df_out <- apply_labels(df_out, label, acs_year, year)
  maybe_save_to_file(df_out, save_to)
  
  df_out
}



annotate_geography <- function(df, state_fips, state_abbrev) {
  # ensure GEOID is character
  df$GEOID <- as.character(df$GEOID)

  # helper to read every field as character
  safe_read <- function(url) {
    read.table(
      url,
      sep           = "|",
      header        = TRUE,
      stringsAsFactors = FALSE,
      quote         = "",
      fill          = TRUE,
      comment.char  = "",
      colClasses    = "character"
    )
  }

  # 1) load lookups
  base     <- "https://www2.census.gov/geo/docs/reference/codes2020"
  places   <- safe_read(sprintf("%s/place_by_cou/st%s_%s_place_by_county2020.txt",
                                base, state_fips, tolower(state_abbrev)))
  cousubs  <- safe_read(sprintf("%s/cousub/st%s_%s_cousub2020.txt",
                                base, state_fips, tolower(state_abbrev)))
  counties <- safe_read(sprintf("%s/cou/st%s_%s_cou2020.txt",
                                base, state_fips, tolower(state_abbrev)))
  aiannh   <- safe_read(sprintf("%s/national_aiannh2020.txt", base))
  aiannh   <- aiannh[grepl(toupper(state_abbrev), aiannh$STATES), ]

  # 2) build GEOID keys
  places$geoid   <- paste0(places$STATEFP,   places$PLACEFP)                     # 7-digit
  cousubs$geoid  <- paste0(cousubs$STATEFP,  cousubs$COUNTYFP,  cousubs$COUSUBFP) # 10-digit
  counties$geoid <- paste0(counties$STATEFP, counties$COUNTYFP)                  # 5-digit
  aiannh$geoid   <- paste0(state_fips,        aiannh$AIANNHCE)                   # 6-digit

  # 3) prepare output columns
  df$County <- NA_character_
  df$Place  <- NA_character_

  # 4a) counties → County
  m_co    <- match(df$GEOID, counties$geoid)
  have_co <- !is.na(m_co)
  df$County[have_co] <- counties$COUNTYNAME[m_co[have_co]]

  # 4b) places → Place + prefix County
  m_pl    <- match(df$GEOID, places$geoid)
  have_pl <- !is.na(m_pl)
  df$Place[have_pl]  <- places$PLACENAME[m_pl[have_pl]]
  df$County[have_pl] <- paste0(places$COUNTYNAME[m_pl[have_pl]])

  # 4c) subdivisions → Place
  m_cs    <- match(df$GEOID, cousubs$geoid)
  have_cs <- !is.na(m_cs)
  df$Place[have_cs]  <- cousubs$COUSUBNAME[m_cs[have_cs]]
  df$County[have_cs] <- cousubs$COUNTYNAME[m_cs[have_cs]]

  # 4d) AI/AN & NH areas → Place
  m_ai    <- match(df$GEOID, aiannh$geoid)
  have_ai <- !is.na(m_ai)
  df$Place[have_ai]  <- aiannh$AIANNHNAME[m_ai[have_ai]]

  # 5) pure counties (5-digit) get "***" in Place
  is_county     <- nchar(df$GEOID) == 5
  df$Place[is_county] <- "***"

  # 6) drop NAME and reorder: County, Place, GEOID, then everything else
  df$NAME <- NULL
  new_order <- c("County", "Place", "GEOID",
                 setdiff(names(df), c("County","Place","GEOID")))
  df <- df[ , new_order]

  df
}
