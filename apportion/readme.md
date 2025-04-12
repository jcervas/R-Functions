# apportion.R

Use this function to calculate apportionment for U.S. House of Representatives (and Electoral College). Vary inputs to calculate hypotheticals.

``` 
pop <- read.csv("https://raw.githubusercontent.com/jcervas/R-Functions/main/apportion/pop.csv")
pop <- pop[!pop$state %in% "District of Columbia",]
apportion(STATES=pop$state, POP=pop$pop2020, nseats=435, autoseats=1)
```
## Quotas
- A quota is the number of representatives each state deserves in proportion to its population. 
- "Let N be the total number of representatives, A, B, C,... the populations of the several states, and a,b,c,... the number of representatives assigned to each." (<a href="https://www.pnas.org/content/7/4/123">Huntington 1921</a>)


### pop.xlsx and pop.csv data sources and variables

| state | State Name |
| --- | ---|
| abs | State 2 letter abbreviation |
| pop2010 | 2010 apportionment population |
| pop2020 | 2020 apportionment population |
| pop2002PEP | July 2020 Population Estimates Program population (https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.html)|
| popchange | pop2020-pop2010 |
| popchange_per | (pop2020-pop2010)/pop2010 * 100 |
| popestdiff | (pop2020PEP-pop2020)/pop2020 |
| house_seats__2010 | 2010 Apportionment |
| house_seats_2020 | 2020 Apportionment |
| house_seats_change | house_seats_2020 - house_seats_2010 |
| ec2010 | 2010 Electoral College seats |
| ec2020 | 2020 Electoral College seats |
| ec_change | ec2020-ec2010 |
| 2020pres | Statewide 2020 Presidential winner |
| tot_covid_cases_4_1_20 | Total cases of COVID as of 4/1/2020 (https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36/data) |
| tot_covid_death_4_1_20 | Total deaths of COVID as of 4/1/2020 |
| new_covid_death_4_1_20 | I think this is deaths reported on 3/31/2020 (need to confirm) |
| land_area_miles |  Area of the state in miles (https://www.census.gov/prod/cen2010/cph-2-1.pdf, Table 18) |
| density_per_mile | pop2020/land_area_miles |
| density_rank | most to least dense, Most=1, Least=51 |
| hispanic_per | Percent Hispanic of any Race (DP05_0071PE, https://data.census.gov/cedsci/table?q=hispanic&g=0100000US.04000.001&tid=ACSDP1Y2019.DP05&hidePreview=true) |
| hispanic_rank | most to least Hispanic, Most=1, Least=51 |
| undoc_pop | Number of Unauthorized Immigrants, source: Migration Policy Institute, National and State Estimates of the Unauthorized Immigrant Population, 2014-18, https://www.migrationpolicy.org/programs/us-immigration-policy-program-data-hub/unauthorized-immigrant-population-profiles |
| undoc_per | State Share of the Total Unauthorized Immigrant Population |
| black_pop | Estimate!!RACE!!Total population!!One race!!Black or African American, DP05_0038E, ACS 1-year 2019 |
| black_error | Margin of Error!!RACE!!Total population!!One race!!Black or African American |
| black_per | Percent!!RACE!!Total population!!One race!!Black or African American, DP05_0038PE |
| biden |  Biden 2020 vote share (two-party)  |
| biden_rank | most to least democratic, most=1, least=51 |
| gov_party | Party of the governor, as of April 1, 2020, source: Wikipedia |
| count_funding | Amount spent by the state to “get out the count”, (https://www.ncsl.org/research/redistricting/2020-census-resources-and-legislation.aspx, “Legislative Funding and Supporting Census Work” Accessed April 28, 2021 11:56pEST), in dollars |
| count_funding_per | count_funding/pop2020, in dollars per person |



## Packages

- https://rpubs.com/jalapic/apportRvig
