`electSim` <- function(
  VOTES, 
  LAGVOTES, 
  TOTAL, 
  SEATS, 
  POP, 
  STATES,
  YEARS, 
  year=NULL, 
  vBar.range=c(0.35, 0.65), 
  n.sims=1000, 
  plot=FALSE, 
  path=NULL,
  sigma=0.06514486, # Historic residual error
  seed=66
) {
  # Set seed for reproducibility
  set.seed(seed)
  sv <- list()
  
  # Error checking for input vectors
  stopifnot(is.numeric(VOTES), is.numeric(LAGVOTES), is.numeric(TOTAL), is.numeric(SEATS), is.numeric(POP), is.numeric(YEARS))
  stopifnot(length(VOTES) == length(LAGVOTES), length(VOTES) == length(TOTAL), length(VOTES) == length(SEATS), length(VOTES) == length(POP), length(VOTES) == length(YEARS))
  
  # Check year is provided and within the range of YEARS
  stopifnot(!is.null(year), year %in% unique(YEARS))
  
  # Check vBar.range is numeric and within the range [0, 1]
  stopifnot(is.numeric(vBar.range), all(vBar.range >= 0 & vBar.range <= 1))
  
  # Initialize vectors and matrices for storing results
  unique_years <- unique(YEARS)
  n_years <- length(unique_years)
  
  asv <- numeric(n_years)
  npv <- numeric(n_years)
  ppv <- numeric(n_years)
  ewv <- numeric(n_years)
  coefs <- matrix(NA, nrow=n_years, ncol=2)
  resid.errors <- numeric(n_years)
  
  # Vectorize the loop through each unique year
  for (i in seq_len(n_years)) {
    y.i <- YEARS == unique_years[i]
    weights <- TOTAL[y.i]
    asv[i] <- mean(VOTES[y.i])
    npv[i] <- weighted.mean(VOTES[y.i], weights)
    ppv[i] <- weighted.mean(VOTES[y.i], POP[y.i])
    ewv[i] <- weighted.mean(VOTES[y.i], SEATS[y.i])
    
    n <- sum(y.i)
    lag_votes <- LAGVOTES[y.i]
    lag_votes[is.na(lag_votes)] <- VOTES[y.i][is.na(lag_votes)]
    
    fit <- lm(VOTES[y.i] ~ lag_votes)
    coefs[i, ] <- coef(fit)
    resid.errors[i] <- sqrt(deviance(fit) / (n - 2))
  }
  
  # Replace very small residual errors with a default sigma value
  resid.errors[resid.errors < 0.01] <- sigma
  
  cat(paste0("\n", year, "..."))
  
  y.k <- YEARS == year
  year.index <- match(year, unique_years)
  
  start.year.indicator <- max(1, year.index - 2)
  end.year.indicator <- year.index
  
  rho <- mean(coefs[start.year.indicator:end.year.indicator, 2])
  sigma <- mean(resid.errors[start.year.indicator:end.year.indicator])
  
  dvote <- VOTES[y.k]
  VOTES.denominator <- TOTAL[y.k]
  weighted.mean(dvote, VOTES.denominator)
  ecvotes.k <- SEATS[y.k]
  states.k <- STATES[y.k]
  dvote.imp <- default.unc(dvote) # Replace small or large vote shares with 25 or 75%
  dvote.lag.k <- default.unc(LAGVOTES[y.k]) # Replace small or large vote shares with 25 or 75%
  
  n_vbar <- length(vBar.range)
  inv.j.d <- numeric(n_vbar)
  inv.j.r <- numeric(n_vbar)
  nca.j <- numeric(n_vbar)
  sbar.50.sd <- numeric(n_vbar)
  sbar.50 <- numeric(n_vbar)
  sbar.5 <- numeric(n_vbar)
  sbar.95 <- numeric(n_vbar)
  sbar.1 <- numeric(n_vbar)
  sbar.99 <- numeric(n_vbar)
  min.shift.j <- vector("list", n_vbar)
  s50 <- vector("list", n_vbar)
  dvote.j <- vector("list", n_vbar)
  pivotal.j <- vector("list", n_vbar)
  
  # Loop over intervals of vBar.range
  for (j in seq_len(n_vbar)) {
    cat(".")
    vbar <- vBar.range[j] # Vote Share Simulation
    sbar <- numeric(n.sims)
    min.shift.i <- numeric(n.sims)
    nca.i <- numeric(n.sims)
    inv.tmp.d <- logical(n.sims)
    inv.tmp.r <- logical(n.sims)
    dvote.i <- vector("list", n.sims)
    pivotal.i <- vector("character", n.sims)
    
    # Loop through simulations
    for (i in seq_len(n.sims)) {
      noisy <- rnorm(length(dvote.imp), dvote.imp, sigma) # Add random noise to states
      predict.d <- noisy + vbar - weighted.mean(noisy, VOTES.denominator) # Vote share equal to vbar
      sbar[i] <- sum(find.winner(predict.d) * ecvotes.k) / sum(ecvotes.k) # Seat share at vbar
      dvote.i[[i]] <- predict.d

      # Minimum shift needed
      min.shift.i[i] <- min.shift(predict.d, ecvotes.k)

      # Non-competitive Advantage
      nca.i[i] <- nca(predict.d, ecvotes.k, VOTES.denominator)

      # Determine if simulation results in an inversion
      inv.tmp.d[i] <- sbar[i] < 0.5 & vbar >= 0.5 # Democrat favoring
      inv.tmp.r[i] <- sbar[i] > 0.5 & vbar <= 0.5 # Republican favoring

      # Calculate each Sim's pivotal state
      state.piv <- states.k[order(predict.d, decreasing=T)]
      pivot.state <- state.piv[which(cumsum(ecvotes.k[order(predict.d, decreasing=T)]) > sum(ecvotes.k)/2)[1]]
      pivotal.i[[i]] <- pivot.state # Pivotal State in Simulation
    }
    
    dvote.j[[j]] <- dvote.i
    pivotal.j[[j]] <- pivotal.i
    min.shift.j[[j]] <- mean(min.shift.i)
    nca.j[j] <- mean(nca.i)
    sbar.50[j] <- mean(sbar)
    sbar.50.sd[j] <- sd(sbar)
    s50[[j]] <- rep(vbar, sum(round(sbar, 2) == 0.5))
    sbar.5[j] <- quantile(sbar, 0.05)
    sbar.95[j] <- quantile(sbar, 0.95)
    sbar.1[j] <- quantile(sbar, 0.01)
    sbar.99[j] <- quantile(sbar, 0.99)
    inv.j.d[j] <- sum(inv.tmp.d)
    inv.j.r[j] <- sum(inv.tmp.r)
  }

  	pivotal <- lapply(pivotal.j, table)
  
  if (!is.null(path)) {
    writeLines(jsonlite::toJSON(dvote.j, pretty=TRUE, auto_unbox=TRUE, na="string"), paste0(path, "/sims_", year, ".json"))
  }
  
  sv$election_info <- data.frame(year=year, Votes=weighted.mean(dvote, VOTES.denominator), Seats=sum(find.winner(dvote) * ecvotes.k) / sum(ecvotes.k), total_EC=sum(ecvotes.k))
  sv$biasmeans <- data.frame(VoteShare=vBar.range, SeatShare=sbar.50, SeatSD=sbar.50.sd, One=sbar.1, Five=sbar.5, NinetyFive=sbar.95, NinetyNine=sbar.99, MinShift=min.shift.j[n.sims/2], NCA=nca.j, Inversions_Dem=inv.j.d, Inversions_Rep=inv.j.r)
  sv$votebias <- mean(unlist(s50))
  sv$seatbias <- sbar.50[which(vBar.range == 0.5)] # Share of seats at 50% vote
  sv$pivotal <- pivotal
  sv$prob.pivotal <- r(prop.table(table(unlist(pivotal.j)))*100)

  return(invisible(sv))
}
