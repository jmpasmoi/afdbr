# afdbr

IE from unstructured data 

###Not yet finished

## Installation

You can install afdbr from github with:

``` r
# install.packages("devtools")
devtools::install_github("jmcimula/afdbr")
```

## Example

``` r
#snippet

library(afdbr) #for functions

> df <- afr_sector_df(sector = c("health", "education", "environment", "agriculture"),
                    pr.status = c("ongoing","approved"), na.rm = TRUE)
					
> attributes(df)
#$names
#[1] "data"   "report"

> df$report
# "Missing value: 0"

> df$data

``` 
