# --------------------------------------------------------------
# batch_geocode()
#
# Sends address records to the Census Batch Geocoding API.
# - Splits into ≤ 7,500-row chunks
# - Retries failed chunks
# - Pads chunk outputs to same width
# - Returns combined dataframe of all results
# --------------------------------------------------------------

batch_geocode <- function(input_csv,
                          output_csv = "geocoderesult.csv",
                          returntype = "geographies",
                          benchmark = "4",
                          vintage = "4",
                          outdir = "geocode_results",
                          tag = "run1") {
  
  # Ensure output directory exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # Validate required fields
  df <- input_csv
  names(df) <- tolower(names(df))
  required <- c("id","street","city","state","zip")
  missing <- setdiff(required, names(df))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse=", "))
  
  df_clean <- df[, required]

  # ---------- helper that handles a single chunk ----------
  process_chunk <- function(chunk, chunk_id, max_attempts=3, wait_seconds=5) {

    tmp_in  <- tempfile(fileext = ".csv")
    tmp_out <- file.path(outdir, paste0(tag, "_chunk_", chunk_id, "_out.csv"))
    fail_out <- file.path(outdir, "failed_chunks")
    
    if (!dir.exists(fail_out)) dir.create(fail_out)

    fail_file <- file.path(fail_out, paste0(tag, "_chunk_", chunk_id, "_failed.csv"))

    # Write chunk to temp file
    write.table(chunk, tmp_in, sep = ",", row.names = FALSE,
                col.names = FALSE, quote = TRUE)

    # Choose correct API endpoint
    if (returntype == "geographies") {
      url <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
      args <- c("--form", paste0('addressFile=@"', tmp_in, '"'),
                "--form", paste0("benchmark=", benchmark),
                "--form", paste0("vintage=", vintage),
                url, "--output", tmp_out)
    } else {
      url <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"
      args <- c("--form", paste0('addressFile=@"', tmp_in, '"'),
                "--form", paste0("benchmark=", benchmark),
                url, "--output", tmp_out)
    }

    # Retry loop
    for (attempt in seq_len(max_attempts)) {
      message("Attempt ", attempt, " for chunk ", chunk_id)
      status <- system2("curl", args = args)
      ok <- status == 0L && file.exists(tmp_out) && file.info(tmp_out)$size > 0
      
      if (ok) return(read.csv(tmp_out, header = FALSE, stringsAsFactors = FALSE, fill = TRUE))
      Sys.sleep(wait_seconds)
    }

    # Final fallback if all retries fail
    warning("Chunk ", chunk_id, " failed; saved to failed_chunks/")
    write.table(chunk, fail_file, sep = ",", row.names = FALSE, col.names = FALSE, quote = TRUE)
    return(NULL)
  }

  # ---------- Split into ≤7500-row chunks ----------
  n <- nrow(df_clean)
  idx <- split(seq_len(n), ceiling(seq_len(n)/7500))

  # Run each chunk
  dfs <- lapply(seq_along(idx), function(i) {
    message("Processing chunk ", i, "/", length(idx))
    process_chunk(df_clean[idx[[i]], ], i)
  })

  # Drop failed chunks
  dfs <- Filter(Negate(is.null), dfs)
  if (!length(dfs)) stop("All chunks failed.")

  # ---------- Ensure all chunk dataframes have same number of columns ----------
  max_cols <- max(vapply(dfs, ncol, integer(1)))

  pad_to <- function(x, k) {
    if (ncol(x) == k) return(x)
    x[, (ncol(x)+1):k] <- NA
    names(x) <- paste0("V", seq_len(k))
    x
  }

  dfs <- lapply(dfs, pad_to, k = max_cols)

  # ---------- Combine all chunks ----------
  all_results <- do.call(rbind, dfs)

  final_out <- file.path(outdir, output_csv)
  write.table(all_results, final_out, sep = ",", row.names = FALSE,
              col.names = FALSE, quote = TRUE)

  message("Completed batch geocoding → ", final_out)
  invisible(all_results)
}


# --------------------------------------------------------------
# resolve_tie_first_match_fast()
#
# Takes a single address string and queries the Census 
# "onelineaddress" endpoint. Pulls out:
#   - matched address
#   - lon/lat
#   - state, county, tract, block
# --------------------------------------------------------------

resolve_tie_first_match_fast <- function(address,
                                         benchmark = 4,
                                         vintage = 4) {

  address <- gsub('^"|"$', "", address)
  addr_enc <- URLencode(address, reserved = TRUE)

  # Census onelineaddress endpoint
  url <- paste0(
    "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress?",
    "address=", addr_enc,
    "&benchmark=", benchmark,
    "&vintage=", vintage,
    "&format=json"
  )

  json <- tryCatch(readChar(url, nchars = 300000), error = function(e) NULL)
  if (is.null(json)) return(NULL)

  if (!grepl('"matchedAddress"', json, fixed = TRUE))
    return(NULL)

  # Extract using [[:space:]] (safe in base R)
  extract <- function(pattern) sub(pattern, "\\1", json)

  matched <- extract('.*"matchedAddress"[[:space:]]*:[[:space:]]*"([^"]+)".*')
  lon     <- extract('.*"x"[[:space:]]*:[[:space:]]*([-0-9\\.]+).*')
  lat     <- extract('.*"y"[[:space:]]*:[[:space:]]*([-0-9\\.]+).*')
  state   <- extract('.*"STATE"[[:space:]]*:[[:space:]]*"([^"]+)".*')
  county  <- extract('.*"COUNTY"[[:space:]]*:[[:space:]]*"([^"]+)".*')
  tract   <- extract('.*"TRACT"[[:space:]]*:[[:space:]]*"([^"]+)".*')
  block   <- extract('.*"BLOCK"[[:space:]]*:[[:space:]]*"([^"]+)".*')

  list(
    matched_address = matched,
    lon   = as.numeric(lon),
    lat   = as.numeric(lat),
    state = state,
    county = county,
    tract = tract,
    block = block
  )
}


# --------------------------------------------------------------
# resolve_all_ties_fast()
#
# Extracts all rows marked "Tie" from batch output,
# resolves each using resolve_tie_first_match_fast(),
# returns a dataframe of fixed entries.
# --------------------------------------------------------------

resolve_all_ties_fast <- function(batch_results, mc.cores = 4) {

  tie_idx <- which(batch_results[,3] == "Tie")
  if (!length(tie_idx)) {
    message("No ties to resolve.")
    return(NULL)
  }

  message("Resolving ", length(tie_idx), " ties via onelineaddress...")

  addresses <- batch_results[tie_idx, 2]
  ids       <- batch_results[tie_idx, 1]

  # Parallelize where available
  if (.Platform$OS.type == "unix" && mc.cores > 1) {
    fixes <- parallel::mclapply(addresses, resolve_tie_first_match_fast,
                                mc.cores = mc.cores)
  } else {
    fixes <- lapply(addresses, resolve_tie_first_match_fast)
  }

  # Assemble cleaned results
  out <- do.call(rbind, lapply(seq_along(fixes), function(i) {
    fx <- fixes[[i]]
    if (is.null(fx)) return(NULL)
    cbind(id = ids[i], as.data.frame(fx, stringsAsFactors = FALSE))
  }))

  out
}

# --------------------------------------------------------------
# merge_tie_fixes()
#
# Replaces original tie rows in batch output with corrected
# rows produced by resolve_all_ties_fast().
# --------------------------------------------------------------

merge_tie_fixes <- function(batch_results, tie_fixes_df) {

  if (is.null(tie_fixes_df) || nrow(tie_fixes_df) == 0) {
    message("No fixes to merge.")
    return(batch_results)
  }

  # Drop old Tie rows
  cleaned <- batch_results[!(batch_results[,1] %in% tie_fixes_df$id), ]

  # Insert corrected rows
  fixed_rows <- cbind(
    tie_fixes_df$id,
    tie_fixes_df$matched_address,
    "Match",
    tie_fixes_df$lon,
    tie_fixes_df$lat,
    NA,       # TIGERLINE placeholder
    NA,       # Side placeholder
    tie_fixes_df$state,
    tie_fixes_df$county,
    tie_fixes_df$tract,
    tie_fixes_df$block
  )

  fixed_rows <- as.data.frame(fixed_rows, stringsAsFactors = FALSE)

  # merge into final dataset
  final <- rbind(cleaned, fixed_rows)

  final
}
