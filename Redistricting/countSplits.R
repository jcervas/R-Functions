# Function to read a CSV file with character data types
readEquiv <- function(file) {
  read.csv(file, colClasses = c("character"))
}

# Function to calculate county splits
calculateCountySplits <- function(districts) {
  numSplits <- sum(lengths(districts) > 1)
  return(numSplits)
}

# Function to calculate total splits
calculateTotalSplits <- function(districts) {
  numSplits <- sum(lengths(districts))
  numUniqueSplits <- length(districts)
  totalSplits <- numSplits - numUniqueSplits
  return(totalSplits)
}

# Function to remove duplicated districts
removeDuplicatedDistricts <- function(data) {
  uniqueData <- data[!duplicated(data$District), ]
  return(uniqueData)
}

# Function to calculate district splits below a threshold
calculateThresholdSplits <- function(data, threshold) {
  thresholdSplits <- data[data$x < threshold, ]
  numSplits <- length(aggregate(thresholdSplits$x, by = list(thresholdSplits$geo), FUN = sum)[, 1])
  return(numSplits)
}

# Function to calculate various splits and deviations
countSplits <- function(plan = NULL, census_blocks = NULL, geo = "COUNTY", custom_geo = NULL) {
  # Read the plan and census blocks data
  planData <- readEquiv(plan)
  censusData <- readEquiv(census_blocks)
  
  # Merge plan and census blocks data
  mergedData <- merge(planData, censusData, by.x = colnames(planData)[1], by.y = colnames(censusData)[3])
  
  # Aggregate total population by district
  distPop <- aggregate(list(TOTAL = as.numeric(mergedData$TOTAL)), by = list(District = mergedData$District), FUN = sum)
  
  # Calculate ideal population
  ideal <- sum(distPop$TOTAL) / length(distPop$TOTAL)
  ideal_minus_5 <- ideal - (ideal * 0.05)
  ideal_plus_5 <- ideal + (ideal * 0.05)
  
  # Merge with custom geographical data if provided
  if (!is.null(custom_geo)) {
    customData <- readEquiv(custom_geo)
    mergedData <- merge(mergedData, customData, by.x = colnames(mergedData)[1], by.y = colnames(customData)[1])
  } else {
    # Rename the "COUNTY" column to "GEO"
    colnames(mergedData)[colnames(mergedData) == geo] <- "geo"
  }
  
  # Create a unique identifier for each district-geography combination
  mergedData$uniq <- paste0(mergedData$District, "_", mergedData$geo)
  
  # Aggregate total population by unique district-geography combinations
  uniqueDistGeo <- aggregate(as.numeric(mergedData$TOTAL), by = list(mergedData$uniq), FUN = sum)
  
  # Merge with aggregated unique district-geography data
  mergedData <- merge(mergedData, uniqueDistGeo, by.x = "uniq", by.y = "Group.1")
  
  # Split the data by the "geo" column
  splitData <- split(mergedData, mergedData$geo)
  
  # Calculate county splits
  countySplits <- calculateCountySplits(lapply(splitData, function(x) unique(x$District)))
  print(countySplits)
  
  # Calculate total splits
  totalSplits <- calculateTotalSplits(lapply(splitData, function(x) unique(x$District)))
  print(totalSplits)
  
  # Calculate threshold splits
  thresholdSplits <- calculateThresholdSplits(mergedData, ideal_minus_5)
  print(thresholdSplits)
  
  # Calculate other splits and deviations
  smallestDistrict <- min(distPop$TOTAL)
  smallestPercentage <- round(100 * ((smallestDistrict - ideal) / ideal), 2)
  largestDistrict <- max(distPop$TOTAL)
  largestPercentage <- round(100 * ((largestDistrict - ideal) / ideal), 2)
  overallDeviation <- round(100 * ((largestDistrict - ideal) / ideal + abs((smallestDistrict - ideal) / ideal)), 2)
  averageDeviation <- round(100 * (mean(abs(distPop$TOTAL - ideal) / ideal)), 2)
  
  # Create a table to store the calculated splits and deviations
  splitsTable <- matrix(c(
    countySplits,
    totalSplits,
    thresholdSplits,
    smallestDistrict,
    smallestPercentage,
    largestDistrict,
    largestPercentage,
    overallDeviation,
    averageDeviation
  ), nrow = 9, ncol = 1)
  
  # Set row names for the splits table
  row.names(splitsTable) <- c(
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
  
  # Return the splits table
  return(splitsTable)
}

# Usage example:
custom_geo <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/villages-block-equiv.csv"
plan <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/Plans/nassau-county-adopted-2023.csv"
census_blocks <- "/Users/cervas/My Drive/GitHub/Data Files/Census/NY2020.pl/clean data/blocks.csv"
countSplits(plan = plan, census_blocks = census_blocks, custom_geo = custom_geo)
