countSplits <- function(plan = NULL, census_blocks = NULL, geo = "COUNTY", custom_geo = NULL, plan_id="GEOID20", block_id="GEOID20", custom_geo_id="GEOID20", save=NULL) {
  # Function to read a CSV file and set all columns as character
  read.equiv <- function(x) {
    read.csv(x, colClasses = c("character"))
  }

# Check if 'plan' is a data frame
if (!inherits(plan, "data.frame")) {
  plan.read <- read.equiv(plan)
} else {
  plan.read <- plan
}

# Check if 'census_blocks' is a data frame
if (!inherits(census_blocks, "data.frame")) {
  census_blocks.read <- read.equiv(census_blocks)
} else {
  census_blocks.read <- census_blocks
}

if (!("District" %in% colnames(plan.read))) {
  colnames(plan.read)[2] <- "District"
}

  # Merge plan and census_blocks based on common column names
  plan_tmp <- merge(plan.read, census_blocks.read, by.x = plan_id, by.y = block_id)
  
  if (!is.null(custom_geo)) {
    # Read custom_geo file if provided and merge with plan_tmp
    custom_geo.read <- read.equiv(custom_geo)
    plan_tmp <- merge(plan_tmp, custom_geo.read, by.x = plan_id, by.y = custom_geo_id)
    colnames(plan_tmp)[ncol(plan_tmp)] <- "geo"
  } else {
    # Rename the "COUNTY" column to "GEO" in plan_tmp
    colnames(plan_tmp)[colnames(plan_tmp) == geo] <- "geo"
  }
  
  # Create a unique identifier for district and geo by combining District and geo columns
  plan_tmp$uniq <- paste0(plan_tmp$District, "_", plan_tmp$geo)
  
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
  list_splits <- data.frame(Split=character(), Districts=character(), Total_Splits = numeric())

  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
    # Count the number of geo splits (more than one unique district)
      cntysplits <- n <- n+1
      # print(a[[i]]$geo[1])
      unique_districts <- unique(a[[i]]$District)
      # Convert the unique elements to a single string with brackets
      districts_string <- paste0("[", paste(unique_districts, collapse = ", "), "]")

      list_splits <- rbind(
        list_splits, 
        data.frame(
          Split = a[[i]]$geo[1], 
          Districts = districts_string, 
          Total_Splits = (length(unique_districts)-1)))

    # Count the number of total splits (more than one unique district)
      totalsplits <- c(totalsplits, length(b[[i]]))
    }
  }

  # # TN County Splits
  # tnsplits <- list()
  # data_new <- list()
  
  # # Filter out duplicated districts within each geo and store the non-duplicated data in data_new
  # for (i in 1:length(a)) {
  #   if (length(b[[i]]) > 1) {
  #     data_new[[i]] <- a[[i]][!duplicated(a[[i]]$District), ]
  #   }
  # }
  # tnsplits_tmp <- do.call(rbind, data_new)

  # if (exists("tnsplits_tmp$x")) {
  # tnsplits <- tnsplits_tmp[(tnsplits_tmp$x < ideal_minus_5), ]
  #     tnsplits_results <- length(aggregate(tnsplits$x, by = list(tnsplits$geo), FUN = sum)[, 1])
  #   } else {
  #   tnsplits <- tnsplits_tmp
  #   dim(tnsplits)[1]
  #   }
  
      if(nrow(list_splits) == 0) {
        list_splits <- data.frame(Split=NA)
        print("There are no splits of this type in the plan.")
        cntysplits <- 0
        totalsplits <- 0
          }
  # Create splits table by calculating various statistics
  splits.table <- rbind(
      cntysplits,
      sum(totalsplits) - length(totalsplits)
    )
    if (splits.table[2,] == -1) {splits.table[2,] <- 0}  
    row.names(splits.table) <- c(
      "Geos Splits",
      "Total Splits"
    )
      if (!is.null(save)) {
          write.csv(list_splits, save, row.names = FALSE)
        }
return(splits.table)  # Return the splits table
  
}

# Define the paths to the required files
# custom_geo <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/villages-block-equiv.csv"
# plan <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/Plans/nassau-county-adopted-2023.csv"
# census_blocks <- "/Users/cervas/My Drive/GitHub/Data Files/Census/NY2020.pl/clean data/blocks.csv"

# Call the countSplits function with the specified arguments
# countSplits(plan = plan, census_blocks = census_blocks, geo = "COUNTY", custom_geo = NULL, plan_id="GEOID20", block_id="GEOID20", custom_geo_id=NULL)
