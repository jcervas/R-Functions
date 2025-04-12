
# decennialAPI() function

Use this function to get 2020 decennial census data from the Census API in R.


```
censusAPI(state, geo, table)
```

- `state`, use state abbreviation such as `"PA"` or `"HI"`

- `geo`, choose from 
     `"state"`, 
     `"county"`, 
     `"tract"`, 
     `"block"`

- `table`, choose from:
     `P1` (total population), 
     `P2` (Hispanic/Non-Hispanic population), 
     `P3` (18+ total population), 
     `P4` (18+ Hispanic/Non-Hispanic population), 
     `P5` (Group Quarters)


## Example

### Get Hawaii counties from table P1
```
censusAPI(state="HI", geo="county", table="P1")
```

### Get South Carolina tracts from table P4
```
censusAPI(state="SC", geo="tract", table="P4")
```

### Resources
https://geo.fcc.gov/api/census/#!/block/get_block_find
https://www.latlong.net/


https://api.census.gov/data/2020/dec/pl?get=group(P1)&for=block:*&in=state:26&in=county:*&in=tract:*
