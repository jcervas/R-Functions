
## Define the paths to the required files
```
custom_geo <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/villages-block-equiv.csv"
plan <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/Plans/nassau-county-adopted-2023.csv"
census_blocks <- "/Users/cervas/My Drive/GitHub/Data Files/Census/NY2020.pl/clean data/blocks.csv"

Call the countSplits function with the specified arguments
countSplits(
  plan = plan,
  census_blocks = census_blocks,
  geo = "COUNTY",
  custom_geo = NULL,
  plan_id = "GEOID20",
  block_id = "GEOID20",
  custom_geo_id = NULL,
  save = "/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/splits.csv")
```
