    rm(list=ls(all=TRUE))   # Remove all objects just to be safe.
    options(scipen=999)     # Turn off Scientific Notation
    options(stringsAsFactors = FALSE)
tn_blocks <- read.csv("/Volumes/GoogleDrive/My Drive/GitHub/Data Files/Census/TN2020.pl/clean data/TN_blocks.csv")

House13a <- read.csv("/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/TN House 13a.csv")
House13b <- read.csv("/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/TN House 13b.csv")
House14a <- read.csv("/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/TN House 14a.csv")
demconcept <- read.csv("/Volumes/GoogleDrive/My Drive/Projects/Redistricting/2022/TN/House Plans/House Dem Concept 12-15-21/House Dem Concept 12-15-21.csv")
enacted <- read.csv("/Volumes/GoogleDrive/My Drive/Projects/Redistricting/2022/TN/House Plans/2022 Enacted/2022 Enacted.csv")
test = read.csv("/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/test.csv")
plan_sum <- function(plan=NULL) {

  plan_tmp <- merge(plan, tn_blocks, by.x="GEOID20", by.y="GEOCODE")
    plan_tmp$uniq <- paste0(plan_tmp$District, "_", plan_tmp$COUNTY)
    uni_dist_cnty <- aggregate(plan_tmp$TOTAL, by=list(plan_tmp$uniq), FUN = sum)
  plan_tmp <- merge(plan_tmp, uni_dist_cnty, by.x="uniq", by.y= "Group.1")
  a = split(plan_tmp, plan_tmp$COUNTY)

  b <- list()
  bpop <- list()
      for (i in 1:length(a)) {
        b[[i]] <- unique(a[[i]]$District)
        }

  cntysplits <- c()
  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
      cntysplits <- c(cntysplits, 1)
    }
  }
  print(sum(cntysplits))

  totalsplits <- c()
  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
      totalsplits <- c(totalsplits, length(b[[i]]))
    }
  }
  print(sum(totalsplits) - length(totalsplits))

  tnsplits <- list()
  data_new <- list()
  for (i in 1:length(a)) {
    if (length(b[[i]]) > 1) {
      data_new[[i]] <- a[[i]][!duplicated(a[[i]]$District),]
    }
  }
  tnsplits_tmp <- do.call(rbind, data_new)
  tnsplits <- tnsplits_tmp[(tnsplits_tmp$x < 66316),]
  print(length(aggregate(tnsplits$x, by=list(tnsplits$COUNTY), FUN=sum)[,1]))

  dist_pop <- aggregate(list(TOTAL=plan_tmp$TOTAL), by=list(District=plan_tmp$District), FUN=sum)
  ideal <- sum(dist_pop$TOTAL)/length(dist_pop$TOTAL)

  print(min(dist_pop$TOTAL))
  print(round(100*((min(dist_pop$TOTAL)-ideal)/ideal),2))
  print(max(dist_pop$TOTAL))
  print(round(100*((max(dist_pop$TOTAL)-ideal)/ideal),2))
  print(round(100*((max(dist_pop$TOTAL)-ideal)/ideal + abs((min(dist_pop$TOTAL)-ideal)/ideal)),2))
  print(round(100*(mean(abs(dist_pop$TOTAL-ideal)/ideal)),2))
}
