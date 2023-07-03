
# TEST

custom_geo <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/villages-block-equiv.csv"
plan <- "/Users/cervas/My Drive/Projects/Redistricting/2023/Nassau/data/Plans/nassau-county-adopted-2023.csv"
census_blocks <- "/Users/cervas/My Drive/GitHub/Data Files/Census/NY2020.pl/clean data/blocks.csv"

countSplits(plan=plan, census_blocks=census_blocks,custom_geo=custom_geo)

# Function

countSplits <- function(plan=NULL,census_blocks=NULL,geo="COUNTY",custom_geo=NULL) {

read.equiv <- function(x) read.csv(x, colClasses=c("character"))

  plan <- read.equiv(plan)
  census_blocks <- read.equiv(census_blocks)
  plan_tmp <- merge(plan, census_blocks, by.x=colnames(plan)[1], by.y=colnames(census_blocks)[3])

  dist_pop <- aggregate(list(TOTAL=as.numeric(plan_tmp$TOTAL)), by=list(District=plan_tmp$District), FUN=sum)
  ideal <- sum(dist_pop$TOTAL)/length(dist_pop$TOTAL)
  ideal_minus_5 <- ideal - (ideal * 0.05)
  ideal_plus_5 <- ideal + (ideal * 0.05)

if (!is.null(custom_geo)) {
  custom_geo <- read.equiv(custom_geo)
  plan_tmp <- merge(plan_tmp, custom_geo, by.x=colnames(plan_tmp)[1], by.y=colnames(custom_geo)[1])
} else {
# Rename the "COUNTY" column to "GEO"
  colnames(plan_tmp)[colnames(plan_tmp) == geo] <- "geo"
}



    plan_tmp$uniq <- paste0(plan_tmp$District, "_", plan_tmp$geo)
    uni_dist_geo <- aggregate(as.numeric(plan_tmp$TOTAL), by=list(plan_tmp$uniq), FUN = sum)
  plan_tmp <- merge(plan_tmp, uni_dist_geo, by.x="uniq", by.y= "Group.1")


  a = split(plan_tmp, plan_tmp$geo)

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
  tnsplits <- tnsplits_tmp[(tnsplits_tmp$x < ideal_minus_5),]
  print(length(aggregate(tnsplits$x, by=list(tnsplits$geo), FUN=sum)[,1]))


splits.table <- 
  rbind(
    sum(cntysplits),
    sum(totalsplits) - length(totalsplits),
    length(aggregate(tnsplits$x, by=list(tnsplits$COUNTY), FUN=sum)[,1]),
    min(dist_pop$TOTAL),
    round(100*((min(dist_pop$TOTAL)-ideal)/ideal),2),
    max(dist_pop$TOTAL),
    round(100*((max(dist_pop$TOTAL)-ideal)/ideal),2),
    round(100*((max(dist_pop$TOTAL)-ideal)/ideal + abs((min(dist_pop$TOTAL)-ideal)/ideal)),2),
    round(100*(mean(abs(dist_pop$TOTAL-ideal)/ideal)),2)
    )
row.names(splits.table) <- 
  c(
    "Geos Splits", 
    "Total Splits", 
    "TN Splits", 
    "Smallest District",
    "Smallest Percentage",
    "Largest District",
    "Largest Percentage",
    "Overall Deviation",
    "Average Deviation")

  return(splits.table)
}
}
