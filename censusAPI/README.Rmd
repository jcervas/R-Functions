---
title: "Census Data API"
output: sumter_census
---
# DETAILED RACE
C02003_001E - Estimate!!Total
C02003_002E - Estimate!!Total!!Population of one race
C02003_003E - Estimate!!Total!!Population of one race!!White
C02003_004E - Estimate!!Total!!Population of one race!!Black or African American


# P1
P001001 - TOTAL POPULATION

# P3 - RACE
P003001 - Race Total (Same as P001001)
P003002 - Total!!White alone
P003003 - Total!!Black or African American alone

# P10 - RACE FOR THE POPULATION 18 YEARS AND OVER
P010001 - Total
P010002 - Total!!Population of one race
P010003 - Total!!Population of one race!!White alone
P010004 - Total!!Population of one race!!Black or African American alone

# P5 - HISPANIC OR LATINO ORIGIN BY RACE
P005001 - Total (Same as P001001)
P005002 - Total!!Not Hispanic or Latino
P005003 - Total!!Not Hispanic or Latino!!White alone
P005004 - Total!!Not Hispanic or Latino!!Black or African American alone

# P6 - RACE (TOTAL RACES TALLIED)
P006001 - Total races tallied
P006002 - Total races tallied!!White alone or in combination with one or more other races
P006003 - Total races tallied!!Black or African American alone or in combination with one or more other races

# P7 - HISPANIC OR LATINO ORIGIN BY RACE (TOTAL RACES TALLIED)
P007001 - Total races tallied
P007002 - Total races tallied!!Not Hispanic or Latino
P007003 - Total races tallied!!Not Hispanic or Latino!!White alone or in combination with one or more other races
P007004 - Total races tallied!!Not Hispanic or Latino!!Black or African American alone or in combination with one or more other races

# P42 - GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE
P042002 - Total!!Institutionalized population (101-106, 201-203, 301, 401-405)
P042003 - Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Correctional facilities for adults (101-106)
P042004 - Total!!Institutionalized population (101-106, 201-203, 301, 401-405)!!Juvenile facilities (201-203)

# BLKGRP - Block Group
# PLACE - Place
# CD111 - 111th Congressional District

"P001001","P003001","P003002","P003003","P010001","P010002","P010003","P010004","P005001","P005002","P005003","P005004","P006001","P006002","P006003","P007001","P007002","P007003","P007004","PLACE","BLKGRP","NAME"


Attach Libraries
```{r}
attachLibraries <- c("rgdal", "censusapi", "tigris", "tidyverse", "spdep", "rmapshaper")
lapply(attachLibraries, library, character.only = TRUE)
key <- "7865f31139b09e17c5865a59c240bdf07f9f44fd"

```


## Get data from API
### SF1 Data

###### (Count Level, 2010 SF!)
```{r}

source("https://raw.githubusercontent.com/jcervas/R-Functions/main/getCensusApi.R")

write.csv(
	getCensusApi(
		"https://api.census.gov/data/2010/dec/sf1?", 
		key=key, 
		get=c("P001001", "P010001","P010004","NAME"), 
		region = "for=county:*&in=state:55"), 
	"/Users/user/Downloads/wi_cnt.csv")

write.csv(
getCensus(name="acs/acs5", vintage=2011, vars=c("NAME","C02003_001E","C02003_002E","C02003_003E","C02003_004E"), region="county:*", regionin="state:55", key=key), 
	"/Users/user/Downloads/wi_cnt_ACS.csv")
```

###### (Block Level Total Population, 2010 SF1)
```{r}
sumterSF1_2010 <- getCensus(name="dec/sf1", vintage=2010, vars=c(vars, "PLACE","BLKGRP","NAME"), region="block:*", regionin="state:13+county:261+tract:*", key=key)
# https://api.census.gov/data/2010/dec/sf1?get=P001001,P003001,P003003,P010002,P010003,P010004,P005003,P005004,P006002,P006003,P007001,P007002,P007003,P007004,PLACE,BLKGRP,NAME&for=block:*&in=state:13%20county:261
# https://api.census.gov/data/2010/dec/sf1?get=P001001,P010002,P010003,P010004,NAME&for=place%20(or%20part):*&in=state:13%20county:261
```

### ACS Data
###### (5-year 2012 [2008-2012])
##### County Data
```{r}
# https://api.census.gov/data/2011/acs/acs5?get=C02003_001E,C02003_002E,C02003_003E,C02003_004E&for=county:261&in=state:13
county.2011.ACS5 <- getCensus(name="acs/acs5", vintage=2011, vars=c("C02003_001E","C02003_002E","C02003_003E","C02003_004E"), region="county:261", regionin="state:13", key=key)


county.2018.ACS5 <- getCensus(name="acs/acs5", vintage=2018, vars=c("B01001_001E","C02003_001E","C02003_002E","C02003_003E","C02003_004E"), region="county:*", regionin="state:55", key=key)
```

#### Tract Data
###### Median Income
```{r}
income.2017.ACS5 <- getCensus(name="acs/acs5", vintage=2017, vars=c("B19013_001E"), region="tract:*", regionin="state:13+county:261", key=key)

###### Population Estimates
# https://api.census.gov/data/2017/acs/acs5?get=C02003_001E,C02003_002E,C02003_003E,C02003_004E&for=tract:*&in=state:13%20county:261
tracts.2011.ACS5 <- getCensus(name="acs/acs5", vintage=2011, vars=c("C02003_001E","C02003_002E","C02003_003E","C02003_004E"), region="tract:*", regionin="state:13+county:261", key=key)

```

###### Block-Group Population Estimates
```{r}
# https://api.census.gov/data/2017/acs/acs5?get=C02003_001E,C02003_002E,C02003_003E,C02003_004E&for=block%20group&in=state:13%20county:261%20tract:*

bg.2018.ACS5 <- getCensusApi("https://api.census.gov/data/2019/acs/acs5?", key=key, get=c("C02003_001E","C02003_002E","C02003_003E","C02003_004E"), region = "for=block%20group&in=state:55%20county:001%20tract:*")

getCensus(name="acs/acs5", vintage=2018, vars=c("B01001_001E","C02003_001E","C02003_002E","C02003_003E","C02003_004E"), region="block%20group:*", regionin="state:55%20county:001", key=key)

 # blockgroups.2011.ACS5 <- getCensus(name="acs/acs5", vintage=2011, vars=c("C02003_001E","C02003_002E","C02003_003E","C02003_004E"), region="block%20group", regionin="state:13+county:261+tract:*", key=key)
```


# https://geo.fcc.gov/api/census/#!/block/get_block_find
# https://www.latlong.net/
