# Function to replace missing election swings with moving average
missing_swing <- function(year, state, data, decay_rate=0.5) {
  state_data <- data[data$state == state,]
  replace_years <- seq(year - 12, year + 12, 4)
  replace_years <- replace_years[replace_years %in% state_data$year]
  
  if(length(replace_years) == 0) return(NA)
  
  swing_y <- state_data$swing[state_data$year %in% replace_years]
  weights <- exp(-decay_rate * abs(replace_years - year) / 4)
  weights[replace_years == year] <- 0  # Do not use the NA value itself
  
  # Normalize weights
  weights <- weights / sum(weights, na.rm = TRUE)
  
  # Calculate weighted moving average
  wma <- sum(swing_y * weights, na.rm = TRUE)
  
  return(wma)
}

# Function to calculate weighted moving average with noise
predict_vote <- function(data, state, year, prev_elections = NULL, num_simulations = 10, decay_rate = 0.1) {
  # Filter data for the specified state
  state_data <- data[data$state %in% state & data$year <= year,]

  # If prev_elections is specified, limit the data to the last prev_elections
  if (!is.null(prev_elections)) {
    state_data <- tail(state_data, prev_elections)
  }
  
  # If no data is left after filtering, return NA
  if (nrow(state_data) == 0) {
    return(rep(NA, num_simulations))
  }
  
  # Calculate the election differences
  year_diff <- abs(state_data$year - year)/4

  # Assign weights based on exponential decay using year differences
  weights <- exp(-decay_rate * year_diff)
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Calculate weighted moving average
  wma <- sum(state_data$swing * weights)
  
  # Calculate weighted sum of squares
  weighted_sum_squares <- sum(weights * (state_data$swing - wma)^2)
  
  # Calculate weighted standard deviation
  wsd <- sqrt(weighted_sum_squares / sum(weights))


  if (nrow(state_data) == 1) {
    wsd <- sd(data$swing[data$year == year])
  }
  
  # Generate predictions with added noise
  noise <- rnorm(num_simulations, mean = 0, sd = wsd)
  
  return(noise)
}



`electSim` <- function(
  data=NULL,
  year=NULL, 
  vBar.range=c(0.35, 0.65), 
  n.sims=1000, 
  plot=FALSE, 
  path=NULL,
  sigma=0.06514486, # Historic residual error
  decay_rate = 0.5,
  seed=66
) {
  # Set seed for reproducibility
  set.seed(seed)
  sv <- list() # output list

  VOTES <- data$dem
  LAGVOTES <- data$dlag
  AVESWING <- data$weighted_avg_swing
  TOTAL <- data$total
  SEATS <- data$ecvotes 
  POP <- data$pop
  STATES <- data$state
  YEARS <- data$year
  
  # Error checking for input vectors
  stopifnot(is.numeric(VOTES), is.numeric(LAGVOTES), is.numeric(TOTAL), is.numeric(SEATS), is.numeric(POP), is.numeric(YEARS))
  stopifnot(length(VOTES) == length(LAGVOTES), length(VOTES) == length(TOTAL), length(VOTES) == length(SEATS), length(VOTES) == length(POP), length(VOTES) == length(YEARS))

  # Check if number of simulations is provided
  stopifnot(!is.na(n.sims))

  # Check year is provided and within the range of YEARS
  # stopifnot(!is.null(year), year %in% unique(YEARS))
  
  # Check vBar.range is numeric and within the range [0, 1]
  stopifnot(is.numeric(vBar.range), all(vBar.range >= 0 & vBar.range <= 1))
  
  # Initialize vectors and matrices for storing results
  unique_years <- unique(YEARS)
  n_years <- length(unique_years)
  
  asv <- numeric(n_years)
  npv <- numeric(n_years)
  ppv <- numeric(n_years)
  ewv <- numeric(n_years)
  # coefs <- matrix(NA, nrow=n_years, ncol=2)
  # resid.errors <- numeric(n_years)


  for (y in seq_len(n_years)) {
    year <- unique_years[y]
    y.y <- YEARS == year
    # Extract unique states and years
    states <- unique(STATES[y.y])

    
    weights <- TOTAL[y.y]
    asv[i] <- mean(VOTES[y.y])
    npv[i] <- weighted.mean(VOTES[y.y], weights) # turnout weighted
    ppv[i] <- weighted.mean(VOTES[y.y], POP[y.y]) # population weighted
    ewv[i] <- weighted.mean(VOTES[y.y], SEATS[y.y]) # ec weighted
  
  cat(paste0("\n", year, "..."))

  # Create variables with just the current year
  dvote <- VOTES[y.y]
  dvote.lag <- LAGVOTES[y.y]
  VOTES.denominator <- TOTAL[y.y]
  ecvotes <- SEATS[y.y]
  states <- STATES[y.y]
  swing <- dvote -  dvote.lag
  ave_swing <- AVESWING[y.y]
  dvote.imp <- default.unc(dvote) # Replace small or large vote shares with 25 or 75%
  dvote.lag.imp <- default.unc(LAGVOTES[y.y]) # Replace small or large vote shares with 25 or 75%
  
  # Set up empty return objects
  n_vbar <- length(vBar.range) # Number of simulated Vote Shares
  inv.v.d <- numeric(n_vbar)
  inv.v.r <- numeric(n_vbar)
  nca.v <- numeric(n_vbar)
  sbar.50.sd <- numeric(n_vbar)
  sbar.50 <- numeric(n_vbar)
  sbar.5 <- numeric(n_vbar)
  sbar.95 <- numeric(n_vbar)
  sbar.1 <- numeric(n_vbar)
  sbar.99 <- numeric(n_vbar)
  min.shift.v <- vector("list", n_vbar)
  s50 <- vector("list", n_vbar)
  predict.dem.vote <- vector("list", n_vbar)
  sbar <- vector("list", n_vbar)
  dvote.v <- vector("list", n_vbar)
  pivotal.v <- vector("list", n_vbar)
    
    min.shift.s <- numeric(n.sims)
    nca.s <- numeric(n.sims)
    inv.tmp.d <- logical(n.sims)
    inv.tmp.r <- logical(n.sims)
    dvote.s <- vector("list", n.sims)
    pivotal.s <- vector("character", n.sims) 


  # Loop over intervals of vBar.range
  for (v in seq_len(n_vbar)) {
    cat(".",v)
    vbar <- vBar.range[v] # Vote Share Simulation

    # Run predictions
    # Create a function to run predictions for each state and year
    noise <- list()
  
    for (state in states) {
    
      # Extract the last known vote percentage for the state and year
      last_vote <- tail(data[data$state == state & data$year == year, "dem"], 1)
      
      # If there's no vote data for the state-year, skip
      if (length(last_vote) == 0) {
        next
      }
      
      # Apply the predict_vote function
      predictions <- predict_vote(data, state, year, num_simulations = n.sims, decay_rate = decay_rate)
      
      # Store the results
      noise[paste(state, year, sep = "_")] <- list(
        # state = state,
        # year = year,
        predictions = predictions
      )
    }

    noisy_predictions <- lapply(seq_along(noise), function(s) {
      noise[[s]] + dvote.imp[s]
    })
    names(noisy_predictions) <- states


    # Initialize a list to store the transposed data
    noisy <- vector("list", n.sims)

    # Loop over each value position
    for (i in 1:n.sims) {
      # Extract the i-th element from each vector and store it in the transposed list
      noisy[[i]] <- sapply(noisy_predictions, function(x) x[i])
    }

    predicted.vote <- lapply(noisy, vbar=vbar, function(x, vbar) x + vbar - weighted.mean(x, VOTES.denominator))
    sbar[[v]] <-unlist(lapply(predicted.vote, function(x) sum((x > 0.5) * ecvotes) / sum(ecvotes)))
    predict.dem.vote[[v]] <- predicted.vote

    # noisy <- rnorm(length(dvote.imp), dvote.imp, sigma) # Add random noise to states
    # predict.dem.vote <- noisy + vbar - weighted.mean(noisy, VOTES.denominator) # Vote share equal to vbar
    # sbar[s] <- sum(find.winner(predict.dem.vote) * ecvotes) / sum(ecvotes) # Seat share at vbar
    # dvote.s[[s]] <- predict.dem.vote

    # # Minimum shift needed
    # min.shift.s <- lapply(predict.dem.vote, function(x) min.shift(x, ecvotes))

    # # Non-competitive Advantage
    # nca.s <- lapply(predict.dem.vote, function(x) nca(x, ecvotes, VOTES.denominator))

    # Determine if simulation results in an inversion
    # inv.tmp.d <- lapply(sbar, function(x) x < 0.5 & vbar > 0.5) # Democrat favoring
    # inv.tmp.r <- lapply(sbar, function(x) x > 0.5 & vbar > 0.5) # Republican favoring

    # inv.tmp.d[s] <- sbar[s] < 0.5 & vbar >= 0.5 # Democrat favoring
    # inv.tmp.r[s] <- sbar[s] > 0.5 & vbar <= 0.5 # Republican favoring

    # Calculate each Sim's pivotal state
    # dem.state.order <- lapply(predicted.vote, function(x) order(x, decreasing=T))
    # state.order <- lapply(dem.state.order, function(x) states[x])
    # state.piv <- lapply(dem.state.order, function(x) which(cumsum(ecvotes[x]) > sum(ecvotes)/2)[1])
    # state.piv <- unlist(state.piv)
    # pivotal.state <- lapply(seq_along(state.order), function(s) {
    #   state.order[[s]][state.piv[s]]
    # })
    # unlist(pivotal.state)
    # state.piv <- states[order(predict.dem.vote, decreasing=T)]
    # pivot.state <- state.piv[which(cumsum(ecvotes[order(predict.dem.vote, decreasing=T)]) > sum(ecvotes)/2)[1]]
    # pivotal.s[[s]] <- pivot.state # Pivotal State in Simulation
  
  # dvote.j[[j]] <- dvote.i
  # pivotal.j[[j]] <- pivotal.i
  # min.shift.j[[j]] <- mean(min.shift.i)
  # nca.j[j] <- mean(nca.i)
  # sbar.50[j] <- mean(sbar)
  # sbar.50.sd[j] <- sd(sbar)
  # s50[[j]] <- rep(vbar, sum(round(sbar, 2) == 0.5))
  # sbar.5[j] <- quantile(sbar, 0.05)
  # sbar.95[j] <- quantile(sbar, 0.95)
  # sbar.1[j] <- quantile(sbar, 0.01)
  # sbar.99[j] <- quantile(sbar, 0.99)
  # inv.j.d[j] <- sum(inv.tmp.d)
  # inv.j.r[j] <- sum(inv.tmp.r)
}

names(predict.dem.vote) <- vBar.range
names(sbar) <- vBar.range

	pivotal <- lapply(pivotal.j, table)

if (!is.null(path)) {
  writeLines(jsonlite::toJSON(dvote.j, pretty=TRUE, auto_unbox=TRUE, na="string"), paste0(path, "/sims_", year, ".json"))
}
  
  sv$election_info <- data.frame(year=year, Votes=weighted.mean(dvote, VOTES.denominator), Seats=sum(find.winner(dvote) * ecvotes) / sum(ecvotes), total_EC=sum(ecvotes))
  sv$biasmeans <- data.frame(VoteShare=vBar.range, SeatShare=sbar.50, SeatSD=sbar.50.sd, One=sbar.1, Five=sbar.5, NinetyFive=sbar.95, NinetyNine=sbar.99, MinShift=min.shift.j[n.sims/2], NCA=nca.j, Inversions_Dem=inv.j.d, Inversions_Rep=inv.j.r)
  sv$votebias <- mean(unlist(s50))
  sv$seatbias <- sbar.50[which(vBar.range == 0.5)] # Share of seats at 50% vote
  sv$pivotal <- pivotal
  sv$prob.pivotal <- r(prop.table(table(unlist(pivotal.j)))*100)

  return(invisible(sv))
}
