# ==============================================================================
# PROJECTION FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# Two-Point Projection Methods
# ------------------------------------------------------------------------------

#' Linear growth projection using two data points
project_linear <- function(start, end, start_year, end_year, target_year) {
  annual_growth <- (end - start) / (end_year - start_year)
  years_to_target <- target_year - end_year
  as.integer(end + annual_growth * years_to_target)
}

#' Exponential growth projection using two data points
project_exponential <- function(start, end, start_year, end_year, target_year) {
  growth_rate <- (end / start)^(1 / (end_year - start_year)) - 1
  years_to_target <- target_year - end_year
  as.integer(end * (1 + growth_rate)^years_to_target)
}

#' Average annual percent change projection using two data points
project_aapc <- function(start, end, start_year, end_year, target_year) {
  aapc <- (end / start)^(1 / (end_year - start_year)) - 1
  years_to_target <- target_year - end_year
  as.integer(end * (1 + aapc)^years_to_target)
}

# ------------------------------------------------------------------------------
# Multi-Point Projection Methods
# ------------------------------------------------------------------------------

#' Linear regression projection using multiple data points
project_linear_regression <- function(values, years, target_year) {
  if (length(values) != length(years) || length(values) < 2) {
    stop("Values and years must have the same length and at least 2 points")
  }
  
  lm_model <- lm(values ~ years)
  predicted <- predict(lm_model, newdata = data.frame(years = target_year))
  
  # Apply demographic realism constraints
  last_value <- values[length(values)]
  years_to_project <- target_year - max(years)
  
  # Calculate implied annual growth rate
  if (last_value > 0) {
    implied_annual_growth <- (predicted / last_value)^(1/years_to_project) - 1
    
    # Cap annual growth rates at realistic demographic limits
    # Most US states don't sustain >2% annual population growth long-term
    max_annual_growth <- 0.02  # 2% per year
    min_annual_growth <- -0.015  # -1.5% per year (decline states)
    
    if (implied_annual_growth > max_annual_growth) {
      # Use capped growth rate
      predicted <- last_value * (1 + max_annual_growth)^years_to_project
      cat("Capping linear regression growth for demographic realism\n")
    } else if (implied_annual_growth < min_annual_growth) {
      # Use capped decline rate
      predicted <- last_value * (1 + min_annual_growth)^years_to_project
      cat("Capping linear regression decline for demographic realism\n")
    }
  }
  
  as.integer(max(0, predicted))  # Ensure non-negative
}

#' Polynomial regression projection using multiple data points
project_polynomial <- function(values, years, target_year, degree = 2) {
  if (length(values) != length(years) || length(values) < (degree + 1)) {
    # Fall back to linear regression if not enough points
    return(project_linear_regression(values, years, target_year))
  }
  
  # Limit degree to prevent overfitting and extreme extrapolation
  max_degree <- min(degree, 2, length(values) - 1)
  
  tryCatch({
    poly_model <- lm(values ~ poly(years, max_degree, raw = TRUE))
    predicted <- predict(poly_model, newdata = data.frame(years = target_year))
    
    # Check for reasonable bounds - if projection is more than 50% different from
    # linear trend, use a constrained approach
    linear_pred <- project_linear_regression(values, years, target_year)
    if (abs(predicted - linear_pred) / linear_pred > 0.5) {
      # Use weighted combination of polynomial and linear
      predicted <- 0.7 * linear_pred + 0.3 * predicted
    }
    
    as.integer(max(0, predicted))  # Ensure non-negative
  }, error = function(e) {
    # Fall back to linear regression on error
    return(project_linear_regression(values, years, target_year))
  })
}

#' Weighted trend projection (gives more weight to recent data)
project_weighted_trend <- function(values, years, target_year, weight_factor = 3) {
  if (length(values) != length(years) || length(values) < 2) {
    stop("Values and years must have the same length and at least 2 points")
  }
  
  # Create weights that increase exponentially for more recent years
  # Use higher weight factor for stronger recency bias
  weights <- weight_factor^(0:(length(years)-1))
  
  # Also try a more recent-focused approach: use only last 3 points if we have 5
  if (length(values) >= 4) {
    # Focus on most recent 3-4 years with very high weights
    recent_values <- tail(values, 3)
    recent_years <- tail(years, 3)
    recent_weights <- c(1, 3, 9)  # Much higher weight on most recent
    
    # Weighted linear regression on recent data
    recent_model <- lm(recent_values ~ recent_years, weights = recent_weights)
    predicted <- predict(recent_model, newdata = data.frame(recent_years = target_year))
  } else {
    # Use all data with exponential weighting
    lm_model <- lm(values ~ years, weights = weights)
    predicted <- predict(lm_model, newdata = data.frame(years = target_year))
  }
  
  as.integer(max(0, predicted))  # Ensure non-negative
}

#' Moving average of growth rates projection
project_moving_average <- function(values, years, target_year, window = 2) {
  if (length(values) != length(years) || length(values) < 2) {
    stop("Values and years must have the same length and at least 2 points")
  }
  
  # Calculate year-over-year growth rates
  growth_rates <- numeric(length(values) - 1)
  for (i in 2:length(values)) {
    growth_rates[i-1] <- (values[i] / values[i-1]) - 1
  }
  
  # Use moving average of recent growth rates
  recent_rates <- tail(growth_rates, window)
  avg_growth_rate <- mean(recent_rates)
  
  # Project from the last known value
  years_to_project <- target_year - max(years)
  last_value <- values[length(values)]
  
  projected <- last_value * (1 + avg_growth_rate)^years_to_project
  as.integer(max(0, projected))
}

#' Recent trend projection (emphasizes last 2-3 data points)
project_recent_trend <- function(values, years, target_year) {
  if (length(values) != length(years) || length(values) < 3) {
    # Fall back to simple linear projection with last two points
    if (length(values) >= 2) {
      return(project_linear(values[length(values)-1], values[length(values)], 
                           years[length(years)-1], years[length(years)], target_year))
    } else {
      return(NA)
    }
  }
  
  # Use only the most recent 3 points for projection
  recent_n <- min(3, length(values))
  recent_values <- tail(values, recent_n)
  recent_years <- tail(years, recent_n)
  
  # Simple linear regression on recent data
  recent_model <- lm(recent_values ~ recent_years)
  predicted <- predict(recent_model, newdata = data.frame(recent_years = target_year))
  
  as.integer(max(0, predicted))
}

#' Robust linear regression using median-based trend
project_robust_linear <- function(values, years, target_year) {
  if (length(values) != length(years) || length(values) < 3) {
    return(project_linear_regression(values, years, target_year))
  }
  
  # Calculate year-over-year changes
  changes <- diff(values)
  time_gaps <- diff(years)
  annual_changes <- changes / time_gaps
  
  # Use median annual change (more robust to outliers)
  median_annual_change <- median(annual_changes)
  
  # Project from last known value
  last_value <- values[length(values)]
  years_to_project <- target_year - max(years)
  
  projected <- last_value + median_annual_change * years_to_project
  
  as.integer(max(0, projected))
}

#' Growth rate ensemble projection with confidence intervals
#' Similar to hurricane prediction - uses multiple growth rate scenarios
project_growth_ensemble <- function(values, years, target_year, confidence_levels = c(0.1, 0.5, 0.9)) {
  if (length(values) != length(years) || length(values) < 3) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      growth_rates = NULL
    ))
  }
  
  # Calculate all possible growth rate dyads
  growth_rates <- c()
  time_spans <- c()
  
  for (i in 1:(length(values)-1)) {
    for (j in (i+1):length(values)) {
      start_val <- values[i]
      end_val <- values[j]
      start_year <- years[i]
      end_year <- years[j]
      
      if (start_val > 0 && end_val > 0) {
        # Calculate annualized growth rate
        time_span <- end_year - start_year
        annual_rate <- (end_val / start_val)^(1/time_span) - 1
        
        growth_rates <- c(growth_rates, annual_rate)
        time_spans <- c(time_spans, time_span)
      }
    }
  }
  
  if (length(growth_rates) == 0) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      growth_rates = NULL
    ))
  }
  
  # Weight recent periods more heavily
  recency_weights <- exp(seq(0, 2, length.out = length(growth_rates)))
  
  # Calculate weighted statistics
  weighted_mean <- weighted.mean(growth_rates, recency_weights)
  weighted_sd <- sqrt(weighted.mean((growth_rates - weighted_mean)^2, recency_weights))
  
  # Use last known value as starting point
  last_value <- values[length(values)]
  years_to_project <- target_year - max(years)
  
  # Generate ensemble projections
  projections <- c()
  
  # Best estimate (weighted mean)
  best_estimate <- last_value * (1 + weighted_mean)^years_to_project
  
  # Confidence intervals using normal distribution of growth rates
  confidence_intervals <- list()
  for (conf_level in confidence_levels) {
    # Calculate quantiles of the growth rate distribution
    z_score <- qnorm(conf_level)
    rate_quantile <- weighted_mean + z_score * weighted_sd
    
    # Apply demographic realism constraints
    rate_quantile <- pmax(pmin(rate_quantile, 0.03), -0.02)  # Cap at ±2-3% annually
    
    projection <- last_value * (1 + rate_quantile)^years_to_project
    confidence_intervals[[paste0("p", round(conf_level * 100))]] <- as.integer(max(0, projection))
  }
  
  return(list(
    best_estimate = as.integer(max(0, best_estimate)),
    confidence_intervals = confidence_intervals,
    growth_rates = growth_rates,
    weighted_mean_rate = weighted_mean,
    weighted_sd_rate = weighted_sd,
    time_spans = time_spans
  ))
}

#' Monte Carlo simulation for population projection uncertainty
project_monte_carlo <- function(values, years, target_year, n_simulations = 1000) {
  if (length(values) != length(years) || length(values) < 3) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL
    ))
  }
  
  # Calculate historical growth rates
  growth_rates <- c()
  for (i in 1:(length(values)-1)) {
    if (values[i] > 0 && values[i+1] > 0) {
      time_span <- years[i+1] - years[i]
      annual_rate <- (values[i+1] / values[i])^(1/time_span) - 1
      growth_rates <- c(growth_rates, annual_rate)
    }
  }
  
  if (length(growth_rates) == 0) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL
    ))
  }
  
  # Estimate parameters of growth rate distribution
  mean_rate <- mean(growth_rates)
  sd_rate <- sd(growth_rates)
  
  # Monte Carlo simulation
  last_value <- values[length(values)]
  years_to_project <- target_year - max(years)
  
  simulated_projections <- c()
  
  for (sim in 1:n_simulations) {
    # Sample growth rate from normal distribution
    sampled_rate <- rnorm(1, mean_rate, sd_rate)
    
    # Apply demographic constraints
    sampled_rate <- pmax(pmin(sampled_rate, 0.03), -0.02)
    
    # Project forward
    projection <- last_value * (1 + sampled_rate)^years_to_project
    simulated_projections <- c(simulated_projections, max(0, projection))
  }
  
  # Calculate confidence intervals
  confidence_intervals <- list(
    p10 = as.integer(quantile(simulated_projections, 0.1)),
    p25 = as.integer(quantile(simulated_projections, 0.25)),
    p50 = as.integer(quantile(simulated_projections, 0.5)),
    p75 = as.integer(quantile(simulated_projections, 0.75)),
    p90 = as.integer(quantile(simulated_projections, 0.9))
  )
  
  return(list(
    best_estimate = as.integer(median(simulated_projections)),
    confidence_intervals = confidence_intervals,
    simulations = simulated_projections,
    mean_rate = mean_rate,
    sd_rate = sd_rate
  ))
}

#' Sequential Ensemble Projection (year-by-year forecasting)
project_growth_ensemble_sequential <- function(values, years, target_year, confidence_levels = c(0.1, 0.5, 0.9)) {
  if (length(values) != length(years) || length(values) < 3) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      yearly_projections = NULL
    ))
  }
  
  # Calculate all possible growth rate dyads (same as original)
  growth_rates <- c()
  time_spans <- c()
  
  for (i in 1:(length(values)-1)) {
    for (j in (i+1):length(values)) {
      start_val <- values[i]
      end_val <- values[j]
      start_year <- years[i]
      end_year <- years[j]
      
      if (start_val > 0 && end_val > 0) {
        time_span <- end_year - start_year
        annual_rate <- (end_val / start_val)^(1/time_span) - 1
        growth_rates <- c(growth_rates, annual_rate)
        time_spans <- c(time_spans, time_span)
      }
    }
  }
  
  if (length(growth_rates) == 0) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      yearly_projections = NULL
    ))
  }
  
  # Weight recent periods more heavily
  recency_weights <- exp(seq(0, 2, length.out = length(growth_rates)))
  weighted_mean <- weighted.mean(growth_rates, recency_weights)
  weighted_sd <- sqrt(weighted.mean((growth_rates - weighted_mean)^2, recency_weights))
  
  # Sequential projection year by year
  last_value <- values[length(values)]
  last_year <- max(years)
  current_value <- last_value
  yearly_projections <- list()
  
  # Project each year sequentially
  for (year in (last_year + 1):target_year) {
    # Add increasing drift/uncertainty each year (growth rate can change)
    year_offset <- year - last_year
    drift_factor <- 1 + (year_offset - 1) * 0.25  # More aggressive increasing uncertainty over time
    
    # Calculate projections for this year
    year_projections <- list()
    
    # Best estimate
    projected_value <- current_value * (1 + weighted_mean)
    year_projections$best <- as.integer(max(0, projected_value))
    
    # Confidence intervals with increasing uncertainty
    adjusted_sd <- weighted_sd * drift_factor * 1.5  # Make bands significantly wider
    for (conf_level in confidence_levels) {
      z_score <- qnorm(conf_level)
      rate_quantile <- weighted_mean + z_score * adjusted_sd
      rate_quantile <- pmax(pmin(rate_quantile, 0.05), -0.03)  # Wider demographic constraints
      
      projection <- current_value * (1 + rate_quantile)
      year_projections[[paste0("p", round(conf_level * 100))]] <- as.integer(max(0, projection))
    }
    
    yearly_projections[[as.character(year)]] <- year_projections
    current_value <- year_projections$best  # Use best estimate for next year
  }
  
  # Extract final year results
  final_projections <- yearly_projections[[as.character(target_year)]]
  confidence_intervals <- final_projections[paste0("p", round(confidence_levels * 100))]
  
  return(list(
    best_estimate = final_projections$best,
    confidence_intervals = confidence_intervals,
    yearly_projections = yearly_projections,
    weighted_mean_rate = weighted_mean,
    weighted_sd_rate = weighted_sd
  ))
}

#' Sequential Monte Carlo simulation (year-by-year with evolving uncertainty)
project_monte_carlo_sequential <- function(values, years, target_year, n_simulations = 1000) {
  if (length(values) != length(years) || length(values) < 3) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      yearly_projections = NULL
    ))
  }
  
  # Calculate historical growth rates
  growth_rates <- c()
  for (i in 1:(length(values)-1)) {
    if (values[i] > 0 && values[i+1] > 0) {
      time_span <- years[i+1] - years[i]
      annual_rate <- (values[i+1] / values[i])^(1/time_span) - 1
      growth_rates <- c(growth_rates, annual_rate)
    }
  }
  
  if (length(growth_rates) == 0) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      yearly_projections = NULL
    ))
  }
  
  mean_rate <- mean(growth_rates)
  sd_rate <- sd(growth_rates)
  
  # Sequential Monte Carlo simulation
  last_value <- values[length(values)]
  last_year <- max(years)
  yearly_projections <- list()
  
  # Run simulations for each year sequentially
  for (year in (last_year + 1):target_year) {
    year_simulations <- c()
    
    for (sim in 1:n_simulations) {
      # Start from last known value or previous year's simulation result
      if (year == last_year + 1) {
        current_value <- last_value
      } else {
        # Use previous year's simulation result for this simulation path
        prev_year_sims <- yearly_projections[[as.character(year - 1)]]$simulations
        current_value <- prev_year_sims[sim]
      }
      
      # Add uncertainty that increases with time
      year_offset <- year - last_year
      uncertainty_multiplier <- 1 + (year_offset - 1) * 0.3  # More aggressive increasing uncertainty
      adjusted_sd <- sd_rate * uncertainty_multiplier * 1.8  # Much wider uncertainty bands
      
      # Sample growth rate with evolving uncertainty
      sampled_rate <- rnorm(1, mean_rate, adjusted_sd)
      sampled_rate <- pmax(pmin(sampled_rate, 0.05), -0.03)  # Wider demographic constraints
      
      # Project this year
      projected_value <- current_value * (1 + sampled_rate)
      year_simulations <- c(year_simulations, max(0, projected_value))
    }
    
    # Calculate statistics for this year
    confidence_intervals <- list(
      p10 = as.integer(quantile(year_simulations, 0.1)),
      p25 = as.integer(quantile(year_simulations, 0.25)),
      p50 = as.integer(quantile(year_simulations, 0.5)),
      p75 = as.integer(quantile(year_simulations, 0.75)),
      p90 = as.integer(quantile(year_simulations, 0.9))
    )
    
    yearly_projections[[as.character(year)]] <- list(
      best_estimate = as.integer(median(year_simulations)),
      confidence_intervals = confidence_intervals,
      simulations = year_simulations
    )
  }
  
  # Extract final year results
  final_results <- yearly_projections[[as.character(target_year)]]
  
  return(list(
    best_estimate = final_results$best_estimate,
    confidence_intervals = final_results$confidence_intervals,
    yearly_projections = yearly_projections,
    mean_rate = mean_rate,
    sd_rate = sd_rate
  ))
}