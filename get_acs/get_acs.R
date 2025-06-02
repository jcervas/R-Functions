geo_prefix_root <- function(state_fips) sprintf("pseudo(0400000US%02d", as.integer(state_fips))

dataset_map <- c(DP = "acs/acs5/profile", S = "acs/acs5/subject", default = "acs/acs5", pums = "acs/acs5/pums")

infer_dataset_from_table <- function(table) {
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

construct_group_url <- function(table, geography, geo_filter, state_fips, year, dataset) {
  # (we’ll only use geo_filter/state_fips for county/place/etc.;
  #  for AIANNH, we ignore state_fips entirely)
  
  if (geography == "state") {
    # State‐level “pseudo(0400000US<STATEFP>)” is unchanged:
    ucgid     <- paste0("0400000US", state_fips)
    geo_clause<- paste0("&ucgid=", ucgid)
    
  } else if (geography == "aian") {
    # National‐level AIANNH: summary level 251, root = 0100000US
    # → “pseudo(0100000US$2510000)”
    ucgid     <- "pseudo(0100000US$2500000)"
    # no URLencode needed (no extra characters besides “(” “)” and “$”)
    geo_clause<- paste0("&ucgid=", ucgid)
    
  } else {
    # county/place/county subdivision/puma: same as before, but leave AIANNH out
    root   <- geo_prefix_root(state_fips)  # e.g. “pseudo(0400000US36” if state_fips = "36"
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
  paste0(
    "https://api.census.gov/data/", year, "/", dataset,
    "?get=",     vars_clause,
    geo_clause
  )
}


construct_standard_url <- function(variables, geography, state_fips, year, dataset) {
  vars_clause <- paste(variables, collapse = ",")
  
  if (grepl("/pums$", dataset)) {
    geo_clause <- paste0("&for=state:", state_fips)
  } else {
    geo_clause <- switch(
      geography,
      
      state   = paste0("&for=state:", state_fips),
      county  = paste0("&for=county:*&in=state:", state_fips),
      place   = paste0("&for=place:*&in=state:",  state_fips),
      "county subdivision" = paste0("&for=county%20subdivision:*&in=state:", state_fips),
      
      aiannh  = "for=american%20indian%20area/alaska%20native%20area/hawaiian%20home%20land:*",   # NO “&in=state:” because ACS does not support it
      
      stop("Unsupported geography")
    )
  }
  
  paste0(
    "https://api.census.gov/data/", year, "/", dataset,
    "?get=",     vars_clause,
    geo_clause
  )
}


normalize_inputs <- function(table, variables, custom_states, state_fips, dataset) {
  table <- if (!is.null(table)) toupper(table) else NULL
  if (!is.null(variables)) variables <- toupper(variables)
  if (!is.null(custom_states) && length(custom_states) == 1) {
    state_fips <- custom_states[[1]]
    custom_states <- NULL
  }
  if (is.null(dataset)) dataset <- infer_dataset_from_table(table)
  list(
    table = table,
    variables = variables,
    custom_states = custom_states,
    state_fips = state_fips,
    dataset = dataset
  )
}

# get_acs_data <- function(url) {
#   if (!requireNamespace("httr", quietly = TRUE)) stop("Please install 'httr'.")
#   resp <- httr::GET(url, httr::timeout(30))
#   if (httr::http_error(resp)) stop("HTTP error ", httr::status_code(resp))
#   raw <- httr::content(resp, as = "text", encoding = "UTF-8")
#   parsed <- tryCatch(jsonlite::fromJSON(raw), error = function(e) stop("Invalid JSON"))
#   if (!is.list(parsed) && !is.matrix(parsed)) stop("Unexpected API response")
#   df <- as.data.frame(parsed[-1, , drop = FALSE], stringsAsFactors = FALSE)
#   names(df) <- unlist(parsed[1, ])
#   df
# }

get_acs_data <- function(url, timeout = getOption("timeout")) {
  # temporarily bump R’s download timeout
  old_to <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_to), add = TRUE)
  
  # open a libcurl connection and read all lines
  con <- url(url, open = "rb", method = "libcurl")
  on.exit(close(con), add = TRUE)
  
  raw <- tryCatch(
    readLines(con, warn = FALSE),
    error = function(e) stop("Download error: ", e$message)
  )
  
  # parse JSON
  parsed <- tryCatch(
    jsonlite::fromJSON(paste(raw, collapse = "")),
    error = function(e) stop("Invalid JSON: ", e$message)
  )
  
  # sanity check
  if (!(is.matrix(parsed) || is.list(parsed))) {
    stop("Unexpected API response format")
  }
  
  # turn into a data.frame, using first row as header
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
  } else {
    stop("Cannot compute GEOID")
  }
  df
}

make_numeric <- function(df) {
  keep <- c("NAME", "GEOID")
  nums <- setdiff(names(df), keep)
  df[nums] <- lapply(df[nums], function(x) suppressWarnings(as.numeric(x)))
  df
}

handle_group_logic <- function(use_group, table, variables, geo_filter, geography, state_fips, year, dataset) {
  if (!is.null(table) && is.null(variables)) use_group <- TRUE
  if (use_group && is.null(geo_filter)) {
    if (geography == "state") {
      geo_filter <- list(state_fips)
    } else if (geography == "county") {
      url <- paste0("https://api.census.gov/data/", year, "/", dataset,
                    "?get=NAME&for=county:*&in=state:", state_fips)
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
  function(df, label) {
    if (!label) return(df)
    vars <- setdiff(names(df), c("NAME", "GEOID"))
    if (is.null(cache)) cache <<- tryCatch(jsonlite::fromJSON("https://api.census.gov/data/2023/acs/acs5/variables.json")$variables, error = function(e) NULL)
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

get_acs <- function(table = NULL,
                    state_fips = "36",
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
                    use_group = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'.")
  norm <- normalize_inputs(table, variables, custom_states, state_fips, dataset)
  table <- norm$table; variables <- norm$variables; state_fips <- norm$state_fips; dataset <- norm$dataset
  gl <- handle_group_logic(use_group, table, variables, geo_filter, geography, state_fips, year, dataset)
  use_group <- gl$use_group; geo_filter <- gl$geo_filter
  if (use_group) {
    url <- construct_group_url(table, geography, geo_filter, state_fips, year, dataset)
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
    df <- apply_labels(df, label)
    maybe_save_to_file(df, save_to)
    return(df)
  } else {
    if (is.null(variables)) stop("Must specify variables or set use_group = TRUE.")
    url <- construct_standard_url(variables, geography, state_fips, year, dataset)
    message("Standard API URL: ", url)
    df <- get_acs_data(url)
    df <- compute_geoid(df)
    df <- make_numeric(df)
    df <- apply_tidy(df, tidy)
    df <- apply_labels(df, label)
    maybe_save_to_file(df, save_to)
    return(df)
  }
}

# Wrapper: Retrieve PUMS (Public Use Microdata Sample) data
get_pums <- function(variables,
                     state_fips = "36",
                     year = 2023,
                     tidy = FALSE,
                     save_to = NULL,
                     label = FALSE,
                     var_types = NULL) {
  if (is.null(variables)) stop("Must specify variables for PUMS.")
  dataset <- "acs/acs5/pums"
  vars <- toupper(variables)
  url <- construct_standard_url(vars, "state", state_fips, year, dataset)
  message("PUMS API URL: ", url)
  df <- get_acs_data(url)
  df <- make_numeric(df)
  df <- apply_tidy(df, tidy)
  df <- apply_labels(df, label)
  maybe_save_to_file(df, save_to)
  df
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

