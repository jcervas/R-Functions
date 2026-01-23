countSplits <- function(plan = NULL,
                        census_blocks = NULL,
                        geo = "COUNTY",
                        custom_geo = NULL,
                        plan_id = "GEOID20",
                        block_id = "GEOID20",
                        custom_geo_id = "GEOID20",
                        pop_var = "pop",
                        save = NULL) {

  read.equiv <- function(x) {
    read.csv(x, colClasses = "character")
  }

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

  if (!("District" %in% colnames(plan.read))) {
    colnames(plan.read)[2] <- "District"
  }

  plan_tmp <- merge(
    plan.read,
    census_blocks.read,
    by.x = plan_id,
    by.y = block_id
  )

  if (!(pop_var %in% colnames(plan_tmp))) {
    stop(paste("Population column", pop_var, "not found"))
  }
  plan_tmp[[pop_var]] <- as.numeric(plan_tmp[[pop_var]])

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

  a <- split(plan_tmp, plan_tmp$geo)

  cntysplits <- 0
  totalsplits <- c()

  pop_splits <- data.frame(
    geo = character(),
    District = character(),
    Population = numeric(),
    Share = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(a)) {

    districts <- unique(a[[i]]$District)

    if (length(districts) > 1) {

      cntysplits <- cntysplits + 1
      totalsplits <- c(totalsplits, length(districts))

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

  if (nrow(pop_splits) == 0) {
    message("There are no splits of this type in the plan.")
  }

  # -----------------------------
  # Population-aware split counts
  # -----------------------------

  # geographies that are trivially unsplit (Share == 1)
  unsplit_geos <- unique(pop_splits$geo[pop_splits$Share == 1])

  pop_nonzero <- pop_splits[!(pop_splits$geo %in% unsplit_geos), ]

  geo_nonzero_splits <- length(unique(pop_nonzero$geo))

  total_nonzero_splits <- if (nrow(pop_nonzero) == 0) {
    0
  } else {
    sum(
      tapply(pop_nonzero$District, pop_nonzero$geo, function(x) {
        length(unique(x)) - 1
      })
    )
  }

  # -----------------------------
  # Summary table
  # -----------------------------

splits.table <- cbind(
  Raw = c(
    cntysplits,
    sum(totalsplits) - length(totalsplits)
  ),
  `Pop > 0` = c(
    geo_nonzero_splits,
    total_nonzero_splits
  )
)

row.names(splits.table) <- c(
  "Geos Split",
  "Total Splits"
)

  # -----------------------------
  # Save outputs
  # -----------------------------

  return(list(
    summary = splits.table,
    population = pop_splits
  ))
}
