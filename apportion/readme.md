# apportion.R

Use this function to calculate apportionment for U.S. House of Representatives (and Electoral College). Vary inputs to calculate hypotheticals.

### Example

https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.html



### pop.xlxs and pop.csv

| state | State Name |
| --- | ---|
| abs | State 2 letter abbreviation |
| pop2010 | 2010 apportionment population |
| pop2020 | 2020 apportionment population |
| pop2002PEP | July 2020 Population Estimates Program population |
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
| tot_covid_cases_4_1_20 | Total cases of COVID as of 4/1/2020 |
| tot_covid_death_4_1_20 | Total deaths of COVID as of 4/1/2020 |
| new_covid_death_4_1_20 | I think this is deaths reported on 3/31/2020 (need to confirm) |
| land_area_miles |  Area of the state in miles (https | //www.census.gov/prod/cen2010/cph-2-1.pdf, Table |8) |
| density_per_mile | pop2020/land_area_miles |
| density_rank | most to least dense, Most=1, Least=51 |
| hispanic_per | Percent Hispanic of any Race (DP05_0071PE, https | //data.census.gov/cedsci/table?|=hispanic- &g=0100000US.04000.001&tid=ACSDP1Y2019.DP05&hidePreview=true) |
| hispanic_rank | most to least Hispanic, Most=1, Least=51 |
| biden |  Biden 2020 vote share (two-party)  |
| biden_rank | most to least democratic, most=1, least=51 |
| count_funding | Amount spent by the state to “get out the count”, (https | //www.ncsl.org/research/- |edistricting/2020-census-resources-and-legislation.aspx, “Legislative Funding and Supporting Ce- nsus | Work” Accessed April 28, 2021 11 | 56pEST), in dollars |
| count_funding_per | count_funding/pop2020, in dollars per person |