
df <- read.csv("afdb.csv")

colnames(df) <- c(
"num","country", "project_id", "title","status","amount","implementing_agency", 
"appraisal_date", "approval_date", "start_date","board_presentation","segment", "report"
)

#Getting the count of project per country, status, segment
an <- dplyr::select(df, country, status, segment, project_id)
an <- cbind(an,nb=1)
		  
ap <- an
app <- sqldf::sqldf("select country, status, segment, sum(nb) nb from ap group by country, status, segment")

apk <- sqldf::sqldf(" 
select p.country, p.status, p.segment, b.nb number, group_concat(p.project_id) group_nb 
from ap p, app b where p.country = b.country and p.status = b.status and p.segment = b.segment 
group by p.country, p.status, p.segment, b.nb"
)

#Getting the count of segment per status
aps <- dplyr::select(df, segment, status)
aps <- cbind(aps,nb=1)
aps <- sqldf::sqldf("
select segment, status,sum(nb) cnt from aps group by segment,status
")

#Getting count of project per country
apc <- sqldf::sqldf("
select country, sum(number) number from apk group by country
")

#Getting the project, country, status and amount
apm <- dplyr::select(df, segment, country, status, segment, project_id, amount)

## afr_dashboard_country <- function(data) {
##if(missing(data)){
##    objects <- ls(pos = 1)    
##    if(length(objects) == 0) stop("No objects found. Please create a data.frame to continue", call. = FALSE)   
##    findf <- objects[sapply(objects, function(x) is.data.frame(get(x)))]
##}else {
##    findf <- as.character(match.call())[2]
##  }
##  findf
##}
