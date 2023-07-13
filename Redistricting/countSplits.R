# TEST

# Path to the custom geographical data file
custom_geo <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/villages-block-equiv.csv"

# Path to the plan file
plan <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/Plans/nassau-county-adopted-2023.csv"

# Path to the census blocks file
census_blocks <- "/Users/cervas/My Drive/GitHub/Data Files/Census/NY2020.pl/clean data/blocks.csv"

# Call the countSplits function with the specified arguments
countSplits(plan = plan, census_blocks = census_blocks, custom_geo = custom_geo)

# Function definition for countSplits
countSplits <- function(plan = NULL, census_blocks = NULL, geo = "COUNTY", custom_geo = NULL) {

  # Internal function to read a CSV file with character data types
  read.equiv <- function(x) {
    read.csv(x, colClasses = c("character"))
  }

  # Read the plan file and census blocks file
  plan <- read.equiv(plan)
  census_blocks <- read.equiv(census_blocks)

  # Merge the plan and census_blocks data frames based on matching columns
  plan_tmp <- merge(plan, census_blocks, by.x = colnames(plan)[1], by.y = colnames(census_blocks)[3])

  # Aggregate the total population by district
  dist_pop <- aggregate(list(TOTAL = as.numeric(plan_tmp$TOTAL)), by = list(District = plan_tmp$District), FUN = sum)

  # Calculate the ideal population for each district
  ideal <- sum(dist_pop$TOTAL) / length(dist_pop$TOTAL)
  ideal_minus_5 <- ideal - (ideal * 0.05)
  ideal_plus_5 <- ideal + (ideal * 0.05)

  # If custom_geo is provided, merge the plan_tmp data frame with the custom geographical data
  if (!is.null(custom_geo)) {
    custom_geo <- read.equiv(custom_geo)
    plan_tmp <- merge(plan_tmp, custom_geo, by.x = colnames(plan_tmp)[1], by.y = colnames(custom_geo)[1])
  } else {
    # Rename the "COUNTY" column to "GEO"
    colnames(plan_tmp)[colnames(plan_tmp) == geo] <- "geo"
  }

  # Create a unique identifier for each district-geography combination
  plan_tmp$uniq <- paste0(plan_tmp$District, "_", plan_tmp$geo)

  # Aggregate the total population by unique district-geography combinations
  uni_dist_geo <- aggregate(as.numeric(plan_tmp$TOTAL), by = list(plan_tmp$uniq), FUN = sum)

  # Merge the plan_tmp data frame with the aggregated unique district-geography data
  plan_tmp <- merge(plan_tmp, uni_dist_geo, by.x = "uniq", by.y = "Group.1")

  # Split the plan_tmp data frame by the "geo" column
  a <- split(plan_tmp, plan_tmp$geo)

  # Create empty lists for storing district information and district population
  b <- list()
  bpop <- list()

  # Loop over each element in the 'a' list
  for (i in 1:length(a)) {
    # Extract unique district values for each geography
    b[[i]] <- unique(a[[i]]$District)
  }

  # Calculate the number of county splits
  cntysplits <- c()
  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
      cntysplits <- c(cntysplits, 1)
    }
  }
  print(sum(cntysplits))

  # Calculate the total number of splits
  totalsplits <- c()
  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
      totalsplits <- c(totalsplits, length(b[[i]]))
    }
  }
  print(sum(totalsplits) - length(totalsplits))

  # Create empty lists for storing district information and new data
  tnsplits <- list()
  data_new <- list()

  # Loop over each element in the 'a' list
  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
      # Remove duplicated districts for each geography
      data_new[[i]] <- a[[i]][!duplicated(a[[i]]$District),]
    }
  }

  # Combine the data in the 'data_new' list into a single data frame
  tnsplits_tmp <- do.call(rbind, data_new)

  # Filter the districts with population less than ideal_minus_5
  tnsplits <- tnsplits_tmp[(tnsplits_tmp$x < ideal_minus_5),]
  print(length(aggregate(tnsplits$x, by = list(tnsplits$geo), FUN = sum)[,1]))

  # Create a table to store the calculated splits and deviations
  splits.table <- rbind(
    sum(cntysplits),
    sum(totalsplits) - length(totalsplits),
    length(aggregate(tnsplits$x, by = list(tnsplits$COUNTY), FUN = sum)[,1]),
    min(dist_pop$TOTAL),
    round(100 * ((min(dist_pop$TOTAL) - ideal) / ideal), 2),
    max(dist_pop$TOTAL),
    round(100 * ((max(dist_pop$TOTAL) - ideal) / ideal), 2),
    round(100 * ((max(dist_pop$TOTAL) - ideal) / ideal + abs((min(dist_pop$TOTAL) - ideal) / ideal)), 2),
    round(100 * (mean(abs(dist_pop$TOTAL - ideal) / ideal)), 2)
  )

  # Set row names for the splits.table
  row.names(splits.table) <- c(
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

  # Return the splits.table
  return(splits.table)
}
