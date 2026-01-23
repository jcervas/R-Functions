countSplits <- function(plan = NULL,
                        census_blocks = NULL,
                        geo = "COUNTY",
                        custom_geo = NULL,
                        plan_id = "GEOID20",
                        block_id = "GEOID20",
                        custom_geo_id = "GEOID20",
                        pop_var = "pop",
                        drop_zero_pop = TRUE) {

  # -----------------------------
  # Helpers
  # -----------------------------

  read.equiv <- function(x) read.csv(x, colClasses = "character")

  load_df <- function(x) {
    if (inherits(x, "data.frame")) x else read.equiv(x)
  }

  # -----------------------------
  # Read inputs
  # -----------------------------

  plan.read <- load_df(plan)
  census_blocks.read <- load_df(census_blocks)

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
    custom_geo.read <- load_df(custom_geo)
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

  # -----------------------------
  # Aggregate population by geo × district
  # -----------------------------

  pop_by_geo_dist <- aggregate(
    plan_tmp[[pop_var]],
    by = list(
      geo = plan_tmp$geo,
      District = plan_tmp$District
    ),
    FUN = sum,
    na.rm = TRUE
  )

  colnames(pop_by_geo_dist)[3] <- "Population"

  if (drop_zero_pop) {
    pop_by_geo_dist <- pop_by_geo_dist[pop_by_geo_dist$Population > 0, ]
  }

  # -----------------------------
  # Structural (raw) splits
  # -----------------------------

  districts_by_geo <- tapply(
    plan_tmp$District,
    plan_tmp$geo,
    function(x) length(unique(x))
  )

  # number of split geographies (aka your old cntysplits)
  raw_geos_split <- sum(districts_by_geo > 1)

  # total splits = sum over split geos of (districts_in_geo - 1)
  # (aka your old sum(totalsplits) - length(totalsplits))
  raw_total_splits <- sum(districts_by_geo[districts_by_geo > 1] - 1)

  raw_splits_by_geo <- districts_by_geo - 1
  raw_splits_by_geo[raw_splits_by_geo < 0] <- 0

  # -----------------------------
  # Population-aware splits
  # -----------------------------

  pop_districts_by_geo <- if (nrow(pop_by_geo_dist) == 0) {
    integer(0)
  } else {
    tapply(
      pop_by_geo_dist$District,
      pop_by_geo_dist$geo,
      function(x) length(unique(x))
    )
  }

  pop_geos_split <- if (length(pop_districts_by_geo) == 0) 0 else sum(pop_districts_by_geo > 1)
  pop_total_splits <- if (length(pop_districts_by_geo) == 0) 0 else sum(pop_districts_by_geo[pop_districts_by_geo > 1] - 1)

  pop_splits_by_geo <- pop_districts_by_geo - 1
  pop_splits_by_geo[pop_splits_by_geo < 0] <- 0

  # -----------------------------
  # Detailed population split table (shares)
  # -----------------------------

  if (nrow(pop_by_geo_dist) > 0) {
    pop_by_geo_dist$Share <- ave(
      pop_by_geo_dist$Population,
      pop_by_geo_dist$geo,
      FUN = function(x) x / sum(x)
    )
  } else {
    pop_by_geo_dist$Share <- numeric(0)
  }

  # -----------------------------
  # Clearer summary output
  # -----------------------------

  summary <- data.frame(
    Metric = c("Geographies split",
               "Total splits"),
    Raw = c(raw_geos_split,
            raw_total_splits),
    PopulationAware = c(pop_geos_split,
                        pop_total_splits),
    stringsAsFactors = FALSE
  )

  attr(summary, "drop_zero_pop") <- drop_zero_pop

  # -----------------------------
  # Optional per-geo clarity table
  # -----------------------------

  geos_all <- names(districts_by_geo)

  # align population-aware counts to the same geo universe
  pop_districts_aligned <- setNames(rep(0L, length(geos_all)), geos_all)
  if (length(pop_districts_by_geo) > 0) {
    pop_districts_aligned[names(pop_districts_by_geo)] <- as.integer(pop_districts_by_geo)
  }

  per_geo <- data.frame(
    geo = geos_all,
    districts_raw = as.integer(districts_by_geo[geos_all]),
    splits_raw = as.integer(raw_splits_by_geo[geos_all]),
    districts_pop = as.integer(pop_districts_aligned[geos_all]),
    splits_pop = as.integer(pmax(pop_districts_aligned[geos_all] - 1L, 0L)),
    stringsAsFactors = FALSE
  )

    # -----------------------------
  # Meta-data
  # -----------------------------

  meta <- list(
    total_geos = length(unique(plan_tmp$geo)),
    total_districts = length(unique(plan_tmp$District)),
    geos_with_positive_population = if (nrow(pop_by_geo_dist) == 0) 0 else length(unique(pop_by_geo_dist$geo)),
    drop_zero_pop = drop_zero_pop
  )

  return(list(
    summary = summary,
    per_geo = per_geo,
    population = pop_by_geo_dist,
    meta = meta
  ))
}
