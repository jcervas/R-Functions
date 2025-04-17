# Great Functions needed for EC Game Analysis

library(jsonlite)
library(GameTheory)

url_test <- try(url("https://raw.githubusercontent.com/jcervas/R-Functions"), silent = TRUE)
internet <- !inherits(url_test, "try-error")

url_directory <- 'https://raw.githubusercontent.com/jcervas/R-Functions/refs/heads/main/EC_Game/'
directory <- '/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/GitHub/R-Functions/EC_Game/'

if (internet == TRUE) {
     source(paste0(url_directory, 'blotto_compare.R'))
     source(paste0(url_directory, 'banzhaf.R'))
     source(paste0(url_directory, 'generate_random_allocations.R'))
     source(paste0(url_directory, 'is_minimum.R'))
     source(paste0(url_directory, 'progress_update.R'))
     source(paste0(url_directory, 'validate_distribution.R'))
     source(paste0(url_directory, 'all_combinations.R'))
} else {
     source(paste0(directory, 'blotto_compare.R'))
     source(paste0(directory, 'banzhaf.R'))
     source(paste0(directory, 'generate_random_allocations.R'))
     source(paste0(directory, 'is_minimum.R'))
     source(paste0(directory, 'progress_update.R'))
     source(paste0(directory, 'validate_distribution.R'))
     source(paste0(directory, 'all_combinations.R'))
}


  # marketvalue <- function(member, votes, quota) {
  #   n <- length(votes)
  #   vote <- votes
  #   member <- member
  #   return(data.frame(name=member, score=swingsPerIndex / sum(swingsPerIndex)))
  # }
