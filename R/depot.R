
df <- read.csv("~/GitHub/afdbr/afdb.csv")

library(magrittr)
library(dplyr)

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
#	AHP 
###################################################################################

#Nota:
# Ongoing & Approved use the column start_date 
#    
#	In that case, length(board_presentation) = 0
#
# Lending & Pipeline use board_presentation for the date 
#   In that case, length(start_date) = 0

df <- read.csv("~/GitHub/afdbr/afdb.csv")
colnames(df) <- c(
"num","country", "project_id", "title","status","amount","implementing_agency", 
"appraisal_date", "approval_date", "start_date","board_presentation","segment", "report"
)

cf <- sqldf::sqldf("
select segment,status, substr(start_date,7,10) startdate, count(status) cnt
from df where CAST(substr(start_date,7,10) AS SIGNED INTEGER) >= 2010 AND length(board_presentation) = 0 
group by segment, status,substr(start_date,7,10)
UNION
select segment,status, substr(board_presentation,7,10) startdate, count(status) cnt
from df where CAST(substr(board_presentation,7,10) AS SIGNED INTEGER) >= 2010 AND length(start_date) = 0 
group by segment, status,substr(board_presentation,7,10)
")

###################################################################################
#	Data progress 
###################################################################################
 
###Completez VisualResume based on status
dtrs <- sqldf::sqldf("select * from cf where status = 'approved' order by startdate")
draft <- sqldf::sqldf("select startdate, sum(cnt) cnt from dtrs where status = 'approved'")
tf <- sqldf::sqldf("select segment, startdate,  sum(cnt) cnt from cf where status = 'approved'")

mm <- transform(tf,newcol=interaction(substr(toupper(segment),1,2),paste0("(",cnt,")"), sep=''))
mkit <- mm %>%  group_by(startdate) %>%
        summarise(newcol = toString(sort(unique(newcol))), total =sum(cnt)) %>%
		select(startdate, newcol, total)

nice <- transform(tf,newcol=interaction(paste0("rep('",segment,"',",cnt,")"), sep=''))
nkit <- nice %>%  group_by(startdate) %>%
        filter(cnt >= 5 ) %>% #Only cnt greater or egal to 5
        summarise(newcol = toString(sort(unique(newcol))),total =sum(cnt)) %>%
		select(startdate, newcol, total)
nkit <- as.data.frame(nkit)
##nkit <- transform(nkit,newcols=interaction(paste0("'",startdate,"' = " ,"c(",newcol,")"), sep=''))
nkit <- transform(nkit,newcols=interaction(paste0("'",startdate,"' = " ,"c(",newcol,")"), sep=''))
nkit <- nkit[,-2]
nkit <- head(nkit[order(nkit$total,decreasing = T),],n=4)
nkit <- nkit %>% 
      summarise(newcol = toString(sort(unique(newcols)))) %>%
	  select(newcol)

VisualResume::VisualResume(
  titles.left = c("Project Tracker Dashboard","MYU Lab", 
                  "*Using Big Data and Machine Learning for Business Dashboards from Large-Scale Data"),
  titles.left.cex = c(3, 2.5, 1),
  titles.right.cex = c(3, 2.5, 1),
  titles.right = c("African Development Bankit","Removing blindfold", 
                   "https://www.afdb.org"),
  timeline.labels = c("Report"),
  timeline = data.frame(title = draft$cnt,
                        sub = "Nb.of Project",
                        start = as.integer(draft$startdate),
                        end = as.integer(draft$startdate),
                        side = rep(c(0,1), times = length(unique(as.integer(draft$startdate))),
                                   length.out = length(unique(as.integer(draft$startdate))), each = 1)),
  milestones = data.frame(title = 0,sub = 0,year = 0),
  events = data.frame(years=mkit$startdate,title=mkit$newcol),
  ##Replace by countries per sector. E.g. programming = Agriculture R=country 10= number projects in this sector
  interests = eval(parse(text=paste("list(",nkit$newcol,")"))),
  year.steps = 1
)

##SLOPEGRAPH
slope <- sqldf::sqldf(paste0("select status, startdate, cnt from cf where segment = '",agriculture, "' order by startdate"))
slp <- tidyr::spread(slope,startdate,cnt)
slp[is.na(slp)] <- 0
tmp <- slp
rownames(slp) <- tmp$status
remove(tmp)
slp <- slp[,-1]
begin <- last <- 1:nrow(slp)
slp <- cbind(begin,slp, last)

##BUBBLE
rawdec <- sqldf::sqldf("select status, country, startdate from rawcf where segment = 'agriculture' and country not like '%national%'")

rd <- sqldf::sqldf("select country, count(*) sums from rawcf group by country")
bbles <- dplyr::filter(rd, sums >= 10)
tbbles <- data.frame(tbl = unique(bbles$country))
n <- nrow(tbbles)
bubbles(value = runif(n), label = tbbles$tbl, color = rainbow(n, alpha = NULL)[sample(n)])

##TREE
rd <- sqldf::sqldf(paste0("select country, status, startdate, count(*) cnt from rawcf where segment = '",tolower(input$selected),"' and country not like '%ALI%' group by status, startdate, country"))
rdtree <- tidyr::spread(rd,startdate,cnt)
rdtree[ is.na(rdtree) ] <- 0 
rdtree[["sums"]] <- rowSums(rdtree[,3:ncol(rdtree)])
rdftree <- dplyr::filter(rdtree, sums >= 3)

##VisualResume based on segment
dtrs <- sqldf::sqldf("select * from cf where segment = 'agriculture'")
draft <- sqldf::sqldf("select startdate, sum(cnt) cnt from dtrs where segment = 'agriculture' group by startdate")
tf <- sqldf::sqldf("select status, startdate,  sum(cnt) cnt from cf where segment = 'agriculture' group by status, startdate")

mm <- transform(tf,newcol=interaction(status,paste0("(",cnt,")"), sep=''))
mkit <- mm %>%  group_by(startdate) %>%
        summarise(newcol = toString(sort(unique(newcol))), total =sum(cnt)) %>%
		select(startdate, newcol, total)
nice <- transform(tf,newcol=interaction(paste0("rep('",status,"',",cnt,")"), sep=''))
nkit <- nice %>%  group_by(startdate) %>%
        filter(cnt >= 1 ) %>% #Only cnt greater or egal to 5
        summarise(newcol = toString(sort(unique(newcol))),total =sum(cnt)) %>%
		select(startdate, newcol, total)
nkit <- as.data.frame(nkit)
##nkit <- transform(nkit,newcols=interaction(paste0("'",startdate,"' = " ,"c(",newcol,")"), sep=''))
nkit <- transform(nkit,newcols=interaction(paste0("'",startdate,"' = " ,"c(",newcol,")"), sep=''))
nkit <- nkit[,-2]
nkit <- head(nkit[order(nkit$total,decreasing = T),],n=4)
nkit <- nkit %>% 
      summarise(newcol = toString(sort(unique(newcols)))) %>%
	  select(newcol)


VisualResume::VisualResume(
  titles.left = c("Project Tracker Dashboard","MYU Lab", 
                  "*Using Big Data and Machine Learning for Business Dashboards from Large-Scale Data"),
  titles.left.cex = c(3, 2.5, 1),
  titles.right.cex = c(3, 2.5, 1),
  titles.right = c("African Development Bankit","Removing blindfold", 
                   "https://www.afdb.org"),
  timeline.labels = c("Report"),
  timeline = data.frame(title = draft$cnt,
                        sub = "Nb.of Project",
                        start = as.integer(draft$startdate),
                        end = as.integer(draft$startdate),
                        side = rep(c(0,1), times = length(unique(as.integer(draft$startdate))),
                                   length.out = length(unique(as.integer(draft$startdate))), each = 1)),
  milestones = data.frame(title = 0,sub = 0,year = 0),
  events = data.frame(years=mkit$startdate,title=mkit$newcol),
  ##Replace by countries per sector. E.g. programming = Agriculture R=country 10= number projects in this sector
  interests = eval(parse(text=paste("list(",nkit$newcol,")"))),
  year.steps = 1
)
