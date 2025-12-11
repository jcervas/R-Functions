## Batch Geocoding Function

# This function sends the cleaned addresses to the Census batch geocoding API in chunks (≤ 7,500 records each), retries on errors, and combines all outputs into a single result.

# Batch Geocoding Script in Base R (using curl system call)
# Input: CSV file with required format (Unique ID, Street address, City, State, ZIP)
# Output: A CSV file with Census geographies (state, county, tract, block)

batch_geocode <- function(input_csv,
                          output_csv = "geocoderesult.csv",
                          returntype = "geographies",
                          benchmark = "4",
                          vintage = "4",
                          outdir = "geocode_results",
                          tag = "run1") {
  
  # if (!file.exists(input_csv)) stop("Input file not found: ", input_csv)
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # Read input with headers
  df <- input_csv
  names(df) <- tolower(names(df))
  required <- c("id","street","city","state","zip")
  missing <- setdiff(required, names(df))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse=", "))
  df_clean <- df[, required]
  
  # ----------------------
  # Process one chunk with retry
  process_chunk <- function(chunk, chunk_id, tag, max_attempts=3, wait_seconds=5) {
    tmp_in  <- tempfile(fileext = ".csv")
    tmp_out <- file.path(outdir, paste0(tag, "_chunk_", chunk_id, "_out.csv"))
    fail_out <- file.path(outdir, "failed_chunks",
                          paste0(tag, "_chunk_", chunk_id, "_failed.csv"))
    
    write.table(chunk, tmp_in, sep = ",", row.names = FALSE,
                col.names = FALSE, quote = TRUE)
    
    if (!dir.exists(file.path(outdir, "failed_chunks"))) {
      dir.create(file.path(outdir, "failed_chunks"))
    }
    
    if (returntype == "geographies") {
      url <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
      args <- c("--form", paste0('addressFile=@"', normalizePath(tmp_in), '"'),
                "--form", paste0("benchmark=", benchmark),
                "--form", paste0("vintage=", vintage),
                url, "--output", tmp_out)
    } else {
      url <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"
      args <- c("--form", paste0('addressFile=@"', normalizePath(tmp_in), '"'),
                "--form", paste0("benchmark=", benchmark),
                url, "--output", tmp_out)
    }
    
    # Retry loop
    status <- 1L
    for (attempt in 1:max_attempts) {
      message("  Attempt ", attempt, " for chunk ", chunk_id)
      status <- system2("curl", args = args)
      good <- (status == 0L &&
               file.exists(tmp_out) &&
               file.info(tmp_out)$size > 0)
      if (good) break
      Sys.sleep(wait_seconds)
    }
    
    if (status != 0L || !file.exists(tmp_out) || file.info(tmp_out)$size == 0) {
      warning("Chunk ", chunk_id, " failed after ", max_attempts, " attempts. Writing to failed_chunks.")
      write.table(chunk, fail_out, sep = ",", row.names = FALSE,
                  col.names = FALSE, quote = TRUE)
      return(NULL)
    }
    
    read.csv(tmp_out, header = FALSE, stringsAsFactors = FALSE, fill = TRUE)
  }
  
  # ----------------------
  # Run all chunks
  n <- nrow(df_clean)
  idx <- split(seq_len(n), ceiling(seq_len(n)/7500))  # ≤7500 rows each
  dfs <- lapply(seq_along(idx), function(i) {
    message("Processing ", tag, " chunk ", i, "/", length(idx),
            " (", length(idx[[i]]), " rows)")
    process_chunk(df_clean[idx[[i]], ], i, tag)
  })
  
  # Remove NULLs from failed chunks
  dfs <- Filter(Negate(is.null), dfs)
  
  if (!length(dfs)) stop("All chunks failed — check input file or service availability.")
  
  # Pad to max columns before combine
  ncols <- vapply(dfs, ncol, integer(1))
  maxc  <- max(ncols)
  pad_to <- function(d, k) {
    if (ncol(d) == k) return(d)
    for (j in seq_len(k - ncol(d))) d[[ncol(d)+1]] <- NA_character_
    names(d) <- paste0("V", seq_len(k)); d
  }
  dfs <- lapply(dfs, pad_to, k=maxc)
  
  all_results <- do.call(rbind, dfs)
  
  # Save combined
  final_out <- file.path(outdir, output_csv)
  write.table(all_results, file = final_out,
              sep = ",", row.names = FALSE, col.names = FALSE, quote = TRUE)
  message("Done. Combined results in ", final_out)
  
  invisible(all_results)
}



## Tie Resolution

# These functions handle "Tie" results by calling the onelineaddress geocoding endpoint and taking the first returned match for each tied address. 

resolve_tie_first_match_fast <- function(address,
benchmark = 4,
vintage = 4) {

# Clean batch CSV quotes

address <- gsub('^"|"$', "", address)
addr_enc <- URLencode(address, reserved = TRUE)

url <- paste0(
"[https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress](https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress)?",
"address=", addr_enc,
"&benchmark=", benchmark,
"&vintage=", vintage,
"&format=json"
)

# Read raw JSON as a single string

json <- tryCatch(readChar(url, nchars = 300000), error = function(e) NULL)
if (is.null(json)) return(NULL)

# If no matches exist

if (!grepl('"matchedAddress"', json, fixed = TRUE))
return(NULL)

# Extract first match components

matched <- sub('.*"matchedAddress"\s*:\s*"([^"]+)".*','\1',json)
lon     <- sub('.*"x"\s*:\s*([-0-9\.]+).*', '\1', json)
lat     <- sub('.*"y"\s*:\s*([-0-9\.]+).*', '\1', json)

tract  <- sub('.*"TRACT"\s*:\s*"([^"]+)".*',  '\1', json)
block  <- sub('.*"BLOCK"\s*:\s*"([^"]+)".*',  '\1', json)
county <- sub('.*"COUNTY"\s*:\s*"([^"]+)".*', '\1', json)
state  <- sub('.*"STATE"\s*:\s*"([^"]+)".*',  '\1', json)

# Clean extraction failures

tract[tract == json]   <- NA
block[block == json]   <- NA
county[county == json] <- NA
state[state == json]   <- NA

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

resolve_all_ties_fast <- function(batch_results, mc.cores = 4) {

# Column 3 of batch output = match status

tie_idx <- which(batch_results[,3] == "Tie")
if (!length(tie_idx)) {
message("No Ties found.")
return(NULL)
}

message("Found ", length(tie_idx), " ties — resolving via onelineaddress...")

addresses <- batch_results[tie_idx, 2]
ids       <- batch_results[tie_idx, 1]

# Parallel where available (Mac/Linux)

if (.Platform$OS.type == "unix" && mc.cores > 1) {
fixes <- parallel::mclapply(addresses, resolve_tie_first_match_fast,
mc.cores = mc.cores)
} else {
# Windows fallback: sequential
fixes <- lapply(addresses, resolve_tie_first_match_fast)
}

# Build a clean data frame

out <- do.call(rbind, lapply(seq_along(fixes), function(i) {
fx <- fixes[[i]]
if (is.null(fx)) return(NULL)
cbind(id = ids[i], as.data.frame(fx, stringsAsFactors = FALSE))
}))

out
}

merge_tie_fixes <- function(batch_results, tie_fixes_df) {
if (is.null(tie_fixes_df) || nrow(tie_fixes_df) == 0) {
message("No fixes to merge.")
return(batch_results)
}

# Remove original Tie rows

cleaned <- batch_results[!(batch_results[,1] %in% tie_fixes_df$id), ]

# Start with empty matrix

fixed_rows <- matrix(NA, nrow = nrow(tie_fixes_df), ncol = ncol(batch_results))

# ID

fixed_rows[,1] <- tie_fixes_df$id

# Address (use matched address from fix)

fixed_rows[,2] <- tie_fixes_df$matched_address

# Match status

fixed_rows[,3] <- "Match"

# Coordinates (Census format: lon, lat after fields 1–3)

fixed_rows[,4] <- tie_fixes_df$lon
fixed_rows[,5] <- tie_fixes_df$lat

# Tigerline placeholder

fixed_rows[,6] <- NA

# Side placeholder

fixed_rows[,7] <- NA

# Geography

fixed_rows[,8]  <- tie_fixes_df$state
fixed_rows[,9]  <- tie_fixes_df$county
fixed_rows[,10] <- tie_fixes_df$tract
fixed_rows[,11] <- tie_fixes_df$block

# Convert to data.frame

fixed_rows <- as.data.frame(fixed_rows, stringsAsFactors = FALSE)

# Combine with cleaned original rows

final <- rbind(cleaned, fixed_rows)

final
}