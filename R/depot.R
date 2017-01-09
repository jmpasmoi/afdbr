
df <- read.csv("~/GitHub/afdbr/afdb.csv")

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
apm <- dplyr::select(df, country, status, segment, project_id, amount)

#Getting date of projects
aprj <- dplyr::select(df, segment, country, status, project_id, amount, appraisal_date, approval_date, start_date)

##pmpr <- dplyr::filter(aprj, as.Date(start_date,"%d/%m/%Y") >= as.Date("01/01/2010","%d/%m/%Y") & as.Date(start_date,"%d/%m/%Y") <= as.Date("21/05/2017","%d/%m/%Y"))

pmpr <- dplyr::filter(aprj, as.Date(start_date,"%d/%m/%Y") >= as.Date("01/01/2010","%d/%m/%Y"))

cf <- sqldf::sqldf("
select segment, country, status, substr(start_date,7,10) startdate,
                   count(case when status = 'ongoing' then status end) as ongoing,
                   count(case when status = 'approved' then status end) as approved,
                   count(case when status = 'pipeline' then status end) as pipeline,
                   count(case when status = 'lending' then status end) as lending
                   from pmpr group by segment, status, country, substr(start_date,7,10)
                   ")
#only ongoing
dfon <- cf[,c("segment","status","country","startdate","ongoing")]

#DECISION TREE
cfon <- sqldf::sqldf("
select country, status, segment, startdate, sum(ongoing) ongoing
from dfon where startdate >= 2010 and startdate <= 2016 and country not like '%national%'
group by segment, status, country, startdate
")

cfon <- tidyr::spread(cfon,startdate,ongoing)
cfon[ is.na(cfon) ] <- 0 
cfon[["sums"]] <- rowSums(cfon[,4:ncol(cfon)])
tcfon <- cfon #this data will be used to visualize the bubble report of countries stands by the choice of sums
cfon <- dplyr::filter(cfon, sums >= 3)

##################BUBBLE##########################################################
library(bubbles)
fc <- dplyr::filter(tcfon, sums >= 5)
t <- data.frame(tbl = unique(fc$country))
n <- nrow(t)
bubbles(value = runif(n), label = t$tbl, color = rainbow(n, alpha = NULL)[sample(n)])
###################################################################################

cfon[["sums"]] <- NULL
begin <- last <- 1:nrow(cfon)
cfon <- cbind(begin,cfon,last)

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(formula = `2010` ~ ., data = cfon, control = rpart.control(minsplit = 2, minbucket = 1, cp = 0.001), parms = list(split = "information"))
fancyRpartPlot(fit)

#SLOPEGRAPH
cfon <- sqldf::sqldf("
select country, startdate, sum(ongoing) ongoing
from dfon where startdate >= 2010 and startdate <= 2016 and country not like '%national%'
group by  country, startdate
")

cfon <- tidyr::spread(cfon,startdate,ongoing)
cftmp <- cfon
rownames(cfon) <- cftmp$country
remove(cftmp)
cfon <- cfon[,-1]
begin <- last <- 1:nrow(cfon)
cfon <- cbind(begin,cfon,last)

slopegraph::slopegraph(head(cfon, n=20), main = 'Ongoing Project of AfDB')


