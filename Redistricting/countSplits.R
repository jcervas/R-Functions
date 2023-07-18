countSplits <- function(plan = NULL, census_blocks = NULL, geo = "COUNTY", custom_geo = NULL) {
  # Function to read a CSV file and set all columns as character
  read.equiv <- function(x) {
    read.csv(x, colClasses = c("character"))
  }
  
  # Read plan and census blocks files using read.equiv function
  plan.read <- read.equiv(plan)
  census_blocks.read <- read.equiv(census_blocks)
  
  # Merge plan and census_blocks based on common column names
  plan_tmp <- merge(plan.read, census_blocks.read, by.x = "GEOID20", by.y = "GEOID20")
  
  # Calculate district population by summing the 'TOTAL' column for each district
  dist_pop <- aggregate(list(TOTAL = as.numeric(plan_tmp$TOTAL)), by = list(District = plan_tmp$District), FUN = sum)
  
  if (!is.null(custom_geo)) {
    # Read custom_geo file if provided and merge with plan_tmp
    custom_geo.read <- read.equiv(custom_geo)
    plan_tmp <- merge(plan_tmp, custom_geo.read, by.x = colnames(plan_tmp)[1], by.y = "GEOID20")
  } else {
    # Rename the "COUNTY" column to "GEO" in plan_tmp
    colnames(plan_tmp)[colnames(plan_tmp) == geo] <- "geo"
  }
  
  # Create a unique identifier for district and geo by combining District and geo columns
  plan_tmp$uniq <- paste0(plan_tmp$District, "_", plan_tmp$geo)
  
  # Calculate aggregated district-geo sums by summing the 'TOTAL' column for each unique district-geo combination
  uni_dist_geo <- aggregate(as.numeric(plan_tmp$TOTAL), by = list(plan_tmp$uniq), FUN = sum)
  plan_tmp <- merge(plan_tmp, uni_dist_geo, by.x = "uniq", by.y = "Group.1")
  
  # Split plan_tmp by geo into a list of data frames, where each data frame corresponds to a unique geo
  a <- split(plan_tmp, plan_tmp$geo)
  
  b <- list()
  bpop <- list()
  
  # Get unique districts for each geo and store them in a list
  for (i in 1:length(a)) {
    b[[i]] <- unique(a[[i]]$District)
  }
  
  cntysplits <- c()
  totalsplits <- c()
  n <- 0

  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
    # Count the number of geo splits (more than one unique district)
      cntysplits <- n <- n+1
    # Count the number of total splits (more than one unique district)
      totalsplits <- c(totalsplits, length(b[[i]]))
    }
  }
  
  # TN County Splits
  
  tnsplits <- list()
  data_new <- list()
  
  # Filter out duplicated districts within each geo and store the non-duplicated data in data_new
  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
      data_new[[i]] <- a[[i]][!duplicated(a[[i]]$District), ]
    }
  }
  tnsplits_tmp <- do.call(rbind, data_new)
  tnsplits <- tnsplits_tmp[(tnsplits_tmp$x < ideal_minus_5), ]
  tnsplits_results <- length(aggregate(tnsplits$x, by = list(tnsplits$COUNTY), FUN = sum)[, 1])

  # Calculate ideal population as the average population across all districts
  ideal <- sum(dist_pop$TOTAL) / length(dist_pop$TOTAL)
  ideal_minus_5 <- ideal - (ideal * 0.05)  # 5% below ideal population
  ideal_plus_5 <- ideal + (ideal * 0.05)  # 5% above ideal population
  
  # Create splits table by calculating various statistics
  splits.table <- rbind(
    sum(dist_pop$TOTAL),
    cntysplits,
    sum(totalsplits) - length(totalsplits),
    tnsplits_results,
    min(dist_pop$TOTAL),
    round(100 * ((min(dist_pop$TOTAL) - ideal) / ideal), 2),
    max(dist_pop$TOTAL),
    round(100 * ((max(dist_pop$TOTAL) - ideal) / ideal), 2),
    round(100 * ((max(dist_pop$TOTAL) - ideal) / ideal + abs((min(dist_pop$TOTAL) - ideal) / ideal)), 2),
    round(100 * (mean(abs(dist_pop$TOTAL - ideal) / ideal)), 2)
  )
  row.names(splits.table) <- c(
    "Total Population",
    "Geos Splits",
    "Total Splits",
    "TN Splits",
    "Smallest District",
    "Smallest Percentage",
    "Largest District",
    "Largest Percentage",
    "Overall Deviation",
    "Average Deviation"
  )
  
  return(splits.table)  # Return the splits table
}

# Define the paths to the required files
# custom_geo <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/villages-block-equiv.csv"
# plan <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/Plans/nassau-county-adopted-2023.csv"
# census_blocks <- "/Users/cervas/My Drive/GitHub/Data Files/Census/NY2020.pl/clean data/blocks.csv"

# Call the countSplits function with the specified arguments
# countSplits(plan = plan, census_blocks = census_blocks, custom_geo = custom_geo)
