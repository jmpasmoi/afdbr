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

afr_sector_df(sector = c("health", "education", "environment", "agriculture"),
                    project_status = c("ongoing","approved"), na.rm = TRUE)

``` 
