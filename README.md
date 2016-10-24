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

> df <- afr_segment_df(
                        segment = c("health", "education", "environment", "agriculture"),
                        pr.status = c("ongoing","approved"), na.rm = TRUE)				
> attributes(df)
#$names
#[1] "data"   "report"

> df$report
# "Missing value: 0"

> df$data

``` 
Below script to run all segments

``` r
df <- afr_segment_df(
			segment = c("climate.change","agriculture","economic",
					"water","education","energy","environment",
					"transport","health","information","infrastructure",
					"gender","human"),
			pr.status = c("ongoing","approved","lending","pipeline"), na.rm = TRUE
                    )
```
