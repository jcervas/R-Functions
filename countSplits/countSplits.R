countSplits <- function(plan = NULL,
                        census_blocks = NULL,
                        geo = "COUNTY",
                        custom_geo = NULL,
                        plan_id = "GEOID20",
                        block_id = "GEOID20",
                        custom_geo_id = "GEOID20",
                        pop_var = "pop",
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
  # (population already lives here)
  #-----------------------------
  plan_tmp <- merge(
    plan.read,
    census_blocks.read,
    by.x = plan_id,
    by.y = block_id
  )

  # Coerce population to numeric
  if (pop_var %in% colnames(plan_tmp)) {
    plan_tmp[[pop_var]] <- as.numeric(plan_tmp[[pop_var]])
  } else {
    stop(paste("Population column", pop_var, "not found in census_blocks"))
  }

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
  # Prep for split counting
  #-----------------------------
  a <- split(plan_tmp, plan_tmp$geo)

  cntysplits <- 0
  totalsplits <- c()

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
  # Main loop
  #-----------------------------
  for (i in seq_along(a)) {

    districts <- unique(a[[i]]$District)

    if (length(districts) > 1) {

      # total population in this geography
      geo_pop <- sum(a[[i]][[pop_var]], na.rm = TRUE)

      cntysplits <- cntysplits + 1
      totalsplits <- c(totalsplits, length(districts))

      districts_string <- paste0("[", paste(districts, collapse = ", "), "]")

      list_splits <- rbind(
        list_splits,
        data.frame(
          Split = a[[i]]$geo[1],
          Districts = districts_string,
          Total_Splits = length(districts) - 1,
          stringsAsFactors = FALSE
        )
      )

      #-------------------------
      # Population by split part
      #-------------------------
      tmp <- aggregate(
        a[[i]][[pop_var]],
        by = list(
          geo = a[[i]]$geo,
          District = a[[i]]$District
        ),
        FUN = sum,
        na.rm = TRUE
      )

      colnames(tmp)[3] <- "Population"
      tmp$Share <- tmp$Population / sum(tmp$Population)

      pop_splits <- rbind(pop_splits, tmp)
    }
  }

  #-----------------------------
  # Handle no splits
  #-----------------------------
  if (nrow(list_splits) == 0) {
    list_splits <- data.frame(Split = NA)
    cntysplits <- 0
    totalsplits <- 0
    message("There are no splits of this type in the plan.")
  }

  #-----------------------------
  # Summary table
  #-----------------------------
  splits.table <- rbind(
    cntysplits,
    sum(totalsplits) - length(totalsplits)
  )

  if (splits.table[2, ] == -1) splits.table[2, ] <- 0

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
    population = pop_splits
  ))
}
