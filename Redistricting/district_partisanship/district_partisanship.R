district_partisanship <- function(state_abr,
                            path,
                            save = FALSE,
                            save_path = NULL) {
  
  # Read files
  old <- read.csv(
    file.path(path, paste0(state_abr, "_map_old.csv")),
    check.names = FALSE
  )
  
  new <- read.csv(
    file.path(path, paste0(state_abr, "_map_new.csv")),
    check.names = FALSE
  )
  
  # Add map labels
  old$map <- "old"
  new$map <- "new"
  
  # Columns to reshape
  value_cols <- names(old)[!names(old) %in% c("ID", "map")]
  
  # Wide -> long
  old_long <- reshape(
    old,
    varying = value_cols,
    v.names = "value",
    timevar = "election",
    times = value_cols,
    direction = "long"
  )
  
  new_long <- reshape(
    new,
    varying = value_cols,
    v.names = "value",
    timevar = "election",
    times = value_cols,
    direction = "long"
  )
  
  # Keep needed columns
  old_long <- old_long[, c("ID", "map", "election", "value")]
  new_long <- new_long[, c("ID", "map", "election", "value")]
  
  # Rename district column
  names(old_long)[1] <- "district"
  names(new_long)[1] <- "district"
  
  # Merge side-by-side
  df <- merge(
    old_long[, c("district", "election", "value")],
    new_long[, c("district", "election", "value")],
    by = c("district", "election"),
    suffixes = c("_old", "_new")
  )
  
  # Final output
  out <- data.frame(
    state = state_abr,
    map = "comparison",
    election = df$election,
    district = df$district,
    old = df$value_old,
    new = df$value_new
  )
  
  # Sort
  out <- out[order(out$election, out$district), ]
  
  rownames(out) <- NULL
  
  # Optional save
  if (save) {
    
    if (is.null(save_path)) {
      save_path <- file.path(path, paste0(state_abr, "_comparison.csv"))
    }
    
    write.csv(out, save_path, row.names = FALSE)
  }
  
  return(out)
}