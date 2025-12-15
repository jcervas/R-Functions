countSplits <- function(plan = NULL,
                        census_blocks = NULL,
                        geo = "COUNTY",
                        custom_geo = NULL,
                        plan_id = "GEOID20",
                        block_id = "GEOID20",
                        custom_geo_id = "GEOID20",
                        pop_var = "pop",
                        min_split_pop = 0,
                        min_fragment_pop = 0,
                        save = NULL,
                        save_pop = NULL) {

  #-----------------------------
  # Helper: read CSV as character
  #-----------------------------
  read.equiv <- function(x) {
    read.csv(x, colClasses = "character")
  }

  #-----------------------------
  # Read inputs
  #-----------------------------
  if (!inherits(plan, "data.frame")) {
    plan.read <- read.equiv(plan)
  } else {
    plan.read <- plan
  }

  if (!inherits(census_blocks, "data.frame")) {
    census_blocks.read <- read.equiv(census_blocks)
  } else {
    census_blocks.read <- census_blocks
  }

  # Ensure District column exists
  if (!("District" %in% colnames(plan.read))) {
    colnames(plan.read)[2] <- "District"
  }

  #-----------------------------
  # Merge plan with blocks
  #-----------------------------
  plan_tmp <- merge(
    plan.read,
    census_blocks.read,
    by.x = plan_id,
    by.y = block_id
  )

  # Ensure population column exists
  if (!(pop_var %in% colnames(plan_tmp))) {
    stop(paste("Population column", pop_var, "not found in census_blocks"))
  }
  plan_tmp[[pop_var]] <- as.numeric(plan_tmp[[pop_var]])

  #-----------------------------
  # Handle geography
  #-----------------------------
  if (!is.null(custom_geo)) {
    custom_geo.read <- read.equiv(custom_geo)
    plan_tmp <- merge(
      plan_tmp,
      custom_geo.read,
      by.x = plan_id,
      by.y = custom_geo_id
    )
    colnames(plan_tmp)[ncol(plan_tmp)] <- "geo"
  } else {
    colnames(plan_tmp)[colnames(plan_tmp) == geo] <- "geo"
  }

  #-----------------------------
  # Split by geography
  #-----------------------------
  geolist <- split(plan_tmp, plan_tmp$geo)

  cntysplits <- 0
  totalsplits <- integer(0)

  list_splits <- data.frame(
    Split = character(),
    Districts = character(),
    Total_Splits = numeric(),
    stringsAsFactors = FALSE
  )

  pop_splits <- data.frame(
    geo = character(),
    District = character(),
    Population = numeric(),
    Share = numeric(),
    stringsAsFactors = FALSE
  )

  #-----------------------------
  # Main loop (correct ordering)
  #-----------------------------
  for (i in seq_along(geolist)) {

    g <- geolist[[i]]

    # Aggregate population by (geo x district)
    tmp <- aggregate(
      g[[pop_var]],
      by = list(
        geo = g$geo,
        District = g$District
      ),
      FUN = sum,
      na.rm = TRUE
    )

    colnames(tmp)[3] <- "Population"

    # Drop low-pop fragments
    tmp <- tmp[tmp$Population > min_fragment_pop, , drop = FALSE]

    # Must have at least two populated districts
    if (nrow(tmp) < 2) next

    # Total population threshold (applied after fragment filter)
    geo_pop <- sum(tmp$Population)
    if (geo_pop < min_split_pop) next

    # NOW count the split
    cntysplits <- cntysplits + 1
    totalsplits <- c(totalsplits, nrow(tmp))

    districts_string <- paste0("[", paste(tmp$District, collapse = ", "), "]")

    list_splits <- rbind(
      list_splits,
      data.frame(
        Split = tmp$geo[1],
        Districts = districts_string,
        Total_Splits = nrow(tmp) - 1,
        stringsAsFactors = FALSE
      )
    )

    # Shares
    tmp$Share <- tmp$Population / geo_pop
    pop_splits <- rbind(pop_splits, tmp)
  }

  #-----------------------------
  # Handle no splits
  #-----------------------------
  if (nrow(list_splits) == 0) {
    list_splits <- data.frame(Split = NA)
    cntysplits <- 0
    totalsplits <- 0
    message("There are no splits of this type in the plan after population filtering.")
  }

  #-----------------------------
  # Summary table
  #-----------------------------
  splits.table <- rbind(
    cntysplits,
    sum(totalsplits) - length(totalsplits)
  )

  if (splits.table[2, ] < 0) splits.table[2, ] <- 0

  row.names(splits.table) <- c(
    "Geos Splits",
    "Total Splits"
  )

  #-----------------------------
  # Save outputs
  #-----------------------------
  if (!is.null(save)) {
    write.csv(list_splits, save, row.names = FALSE)
  }

  if (!is.null(save_pop)) {
    write.csv(pop_splits, save_pop, row.names = FALSE)
  }

  #-----------------------------
  # Return
  #-----------------------------
  return(list(
    summary = splits.table,
    population = pop_splits,
    splits = list_splits
  ))
}
