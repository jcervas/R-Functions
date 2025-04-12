# source("https://raw.githubusercontent.com/jcervas/2020-Elections/main/NYT_json.R")
source("https://raw.githubusercontent.com/jcervas/R-Functions/main/GERRYfunctions.R")

getPres <- function() {
  # Define election years and apportionment years
  election_years <- seq(1868, 2020, by = 4)
  apportionment_years <- seq(1790, 2010, by = 10)
  grey_col <- rgb(190, 190, 190, 170, maxColorValue = 255)

  # Load election data
  election_data <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/Elections/Presidential/Presidential%20Elections_General_Full.csv")
  election_data <- election_data[election_data$year > 1867, ]
  state_fips <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/fips.csv")
  state_fips <- data.frame(state = state_fips$state, fips = state_fips$fips)

  # Create presidential data frame
  pres_data <- data.frame(
    year = election_data$year,
    state = election_data$state,
    dem = two_party(as.numeric(election_data$dem), as.numeric(election_data$rep)),
    total = as.numeric(election_data$dem) + as.numeric(election_data$rep),
    ecvotes = as.numeric(election_data$ecvotes)
  )

  # Sort the data by State and Year
  pres_data <- pres_data[order(pres_data$state, pres_data$year), ]

  # Create lagged variables within each state
  pres_data$dlag <- ave(pres_data$dem, pres_data$state, FUN = function(x) c(NA, x[-length(x)]))
  pres_data$dlag2 <- ave(pres_data$dlag, pres_data$state, FUN = function(x) c(NA, x[-length(x)]))
  pres_data$swing <- pres_data$dem - pres_data$dlag

  # Load population data
  pop <- jsonlite::fromJSON("https://raw.githubusercontent.com/jcervas/Data/master/Elections/hist_pop.json")
  pop_matrix <- sapply(pop, function(p) p$census$population)
  rownames(pop_matrix) <- apportionment_years

  # Create population data frame
  pop_data <- lapply(1:length(election_years), function(i) {
    year <- election_years[i]
    if (substrRight(year, 1) %in% c("2", "4")) {
      pop_year <- round(year, -1)
    } else {
      pop_year <- round(year, -1) - 10
    }
    data.frame(
      year = rep(year, 51),
      state = election_data$state[election_data$year == 2016],
      pop = pop_matrix[pop_year == apportionment_years, ]
    )
  })

  # Merge election and population data
  elect_data <- do.call(rbind, pop_data)
  pres <- merge(elect_data, pres_data, by = c("year", "state"), all = TRUE)
  missing_pop <- !is.na(pres$dem) & is.na(pres$pop)
  pres$pop[missing_pop] <- round(pres$total[missing_pop] * median((pres$pop - pres$total) / pres$total, na.rm = TRUE))

  # Prepare final election data
  election_data_list <- lapply(1:length(election_years), function(j) {
    year <- election_years[j]
    pres_tmp <- pres[pres$year == year, ]
    pres_tmp$year <- year
    pres_tmp$house <- pres_tmp$ecvotes - 2
    pres_tmp$house[pres_tmp$state == "D. C."] <- 0
    pres_tmp
  })

  # Combine all election data
  election_data <- do.call(rbind, election_data_list)
  return(election_data)
}

# Utility function to get the last character(s) of a string
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
