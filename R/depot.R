
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

#########Getting date of projects
aprj <- dplyr::select(df, segment, country, status, project_id, amount, appraisal_date, approval_date, start_date, board_presentation)


cf <- sqldf::sqldf("
select segment, country, status, substr(start_date,7,10) startdate,
                   count(case when status = 'ongoing' then status end) as ongoing,
                   count(case when status = 'approved' then status end) as approved
                   from aprj where substr(start_date,7,10) >= 2010 and length(board_presentation) = 0
				   group by segment, status, country, substr(start_date,7,10)
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


###################################################################################
AHP 
###################################################################################


Nota:
(1) Ongoing & Approved use the column start_date 
    
	In that case, length(board_presentation) = 0

(2) Lending & Pipeline use board_presentation for the date 
   
    In that case, length(start_date) = 0

aprj <- dplyr::select(df, segment, country, status, project_id, amount, appraisal_date, approval_date, start_date, board_presentation)


cf <- sqldf::sqldf("
select segment, country, status, substr(start_date,7,10) startdate,
                   count(case when status = 'ongoing' then status end) as ongoing,
                   count(case when status = 'approved' then status end) as approved
                   from aprj where substr(start_date,7,10) >= 2010 and length(board_presentation) = 0
				   group by segment, status, country, substr(start_date,7,10)
                   ")

cf <- sqldf::sqldf("
select segment,status, substr(start_date,7,10) startdate, count(status) cnt
from aprj where CAST(substr(start_date,7,10) AS SIGNED INTEGER) >= 2010 AND length(board_presentation) = 0 
group by segment, status,substr(start_date,7,10)
UNION
select segment,status, substr(board_presentation,7,10) startdate, count(status) cnt
from aprj where CAST(substr(board_presentation,7,10) AS SIGNED INTEGER) >= 2010 AND length(start_date) = 0 
group by segment, status,substr(board_presentation,7,10)
")

cfon <- tidyr::spread(cf,startdate,cnt)
cfon[ is.na(cfon) ] <- 0 
cfon[["sums"]] <- rowSums(cfon[,3:ncol(cfon)])


