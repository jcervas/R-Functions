progress_update <- function(i, n, freq=10) {
  # Calculate the percentage completion
  progress <- (i / n) * 100
  
  # Check if the progress is at a 10% increment
  if (progress %% freq == 0) {
    message <- paste0("Progress: ", progress, "% complete")
    print(message)
  }
}