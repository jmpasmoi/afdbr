#' Segmenting African Market From African Development Bank Group
#' @details
#'Getting the title of the project
#' @export
getTitle <- function (x){
  y <- xml2::read_html(x)
  y <- rvest::html_nodes(y,"title")
  y <- rvest::html_text(y)
  y <- stringr::str_replace_all(y, afr_extract()[2], "")
  y <- substr(y, 1, stringr::str_length(y) - 2)

  return (stringr::str_trim(y))
}
#' Segmenting African Market From African Development Bank Group
#' @details
#'Cleaning the amount
#' @export
cleanAmount <- function(x) {
stopword <- c('[:alpha:]','[:punct:]','[.]')
 y <- x
 for(i in 1:length(stopword)){
    y <- trimws(stringr::str_replace_all(y,stopword[i], ""))
  }
  return (y)
}
#' Segmenting African Market From African Development Bank Group
#' @details
#'Getting some details on the project like implementing agency, appraisal date, approval date, start date
#' @export
getProjectDetail <- function (x){
z <- x
if(length(z)== 7){
    ima <- z[6];aps <- z[4];apr <- z[2];st <- z[3];bp <- ""
}else{
    ima <- "";aps <- z[2];apr <- "";st <-"";bp <- z[3]
}
 dp <- cbind(
    implementing_agency = ima,
    appraisal_date = aps,
    approval_date = apr,
    start_date = st,
    board_presentation = bp
  )
  dp <- as.data.frame(dp)
  return(dp)
}
#' Segmenting African Market From African Development Bank Group
#' @details
#'Number of pages to explore
#' @export
getPageNumber <- function(x){
  a <- c("[:alpha:]","[:punct:]",">","<","=")
  v <- x
  for (j in 1 : length(a)){
    v <- stringr::str_replace_all(v,a[j],"")
  }
  v <- trimws(stringr::str_replace_all(v,"\n",""))
  vf <- 0

  ifelse(v=="", vf <- 0, vf <- v)
  v <- as.integer(stringr::str_trim(vf))
  y <- as.integer(v/20)

  if( (v %% 20 > 0) == TRUE){
    y <- y + 1
  }
  return (y)
}
#' Segmenting African Market From African Development Bank Group
#' @details
#'Getting the status of the project
#' @export
getStatus <- function (x){
  return (stringr::str_trim(tolower(substr(x, 1, stringr::str_length(x) - 4))))
}
#' Segmenting African Market From African Development Bank Group
#' @details
#'Getting the name of the country
#' @export
getCountry <- function (x){
  e <- unlist(gregexpr(pattern = ",", x)) - 1
  x <- substr(x, 1, e)
  e <- unlist(gregexpr(pattern = ":", x)) + 1
  y <- stringr::str_trim(substr(x, e, stringr::str_length(x)))
  det <- stringr::str_length(y)

if (identical(class(det),"integer") && det >= 30 && identical(y, character(0)) == FALSE){
    e <- unlist(gregexpr(pattern = ":", y)) + 1
    y <- stringr::str_trim(substr(y, e, stringr::str_length(y)))
    y <- stringi::stri_extract_last_boundaries(y)
}
  if(stringr::str_detect (tolower(y),"ivoire") == TRUE) { y <- "Cote d'Ivoire"}
  else if (stringr::str_detect (tolower(y),"ncipe") == TRUE) {y <- "Sao Tome and Principe"}
  else { y <- y }
  return (stringr::str_replace_all(y, "[:punct:]", ""))
}
#' Segmenting African Market From African Development Bank Group
#' @details
#'Getting the link of the project
#' @export
getLink <- function (x){
  return (stringr::str_trim(tolower(substr(x, 1, stringr::str_length(x)))))
}
#' Segmenting African Market From African Development Bank Group
#' @details
#'Function which checks the url of all the project and their status
#' @export
getSearchReference <- function (x){
 y <- xml2::read_html(x)
 y <- rvest::html_nodes(y,"tr td")
 y <- rvest::html_text(y)
 v <- as.data.frame(y)
 ol <-  seq(1, nrow(v), by=3)

 valr <- data.frame()
 for(j in 1:length(ol)){
   id <- paste0(v[ol[j],])
   ovw <- paste(afr_extract()[1],tolower(id),sep="")
   ovx_un <- xml2::read_html(ovw)
   ovx_un <- rvest::html_nodes(ovx_un,"ul li strong")
   ovx_un <- rvest::html_text(ovx_un)
   ovx_deux <- xml2::read_html(ovw)
   ovx_deux <- rvest::html_nodes(ovx_deux,"p")
   ovx_deux <- rvest::html_text(ovx_deux)
   ovx_deux <- ovx_deux[length(ovx_deux)]
   ovx_trois <- xml2::read_html(ovw)
   ovx_trois <- rvest::html_nodes(ovx_trois,"tr td")
   ovx_trois <- rvest::html_text(ovx_trois)
   cty <- readLines(ovw,warn = F)
   pat <- "search:taxonomy:countries"
   bcty <- cty[grep(pat,cty)]
   ptv <- unlist(gregexpr(pattern = pat, bcty))
   bp <-  substr(bcty,ptv,nchar(bcty))
   ptv <- unlist(gregexpr(pattern = ">", bp)) - 1
   bp <- substr(bp,1,ptv[1])

   stopword <- c(pat, '[[:digit:]]','[:punct:]',  "content","id","title","quot","=")
   for(i in 1:length(stopword)){
     bp <- trimws(stringr::str_replace_all(bp,stopword[i], ""))
   }
   country <- bp
   key_contact <- trimws(unlist(strsplit(ovx_deux, "-"))[2])
   project_id <- ovx_un[1]
   title  <- getTitle(ovw)
   status <- getStatus(ifelse(length(ovx_un)==7, ovx_un[5],ovx_un[4]))
   amount <- trimws(cleanAmount(ovx_trois[1]))
   details <- getProjectDetail(ovx_un)

   vloop <- data.frame(country,project_id,title,status,amount,details,key_contact)
   valr  <- rbind(valr,vloop)
 }
 return(valr)
}
#' Segmenting African Market From African Development Bank Group
#' @details
#'Browsing all the subpages related to the choice of the user from the interface
#' @export
#'
getData <- function (x,link){
  pg <- link
  val <- x
  valr <- data.frame()
  for (j in 1 : val - 1){
    if (j > 0) {
      urlr <- paste(pg, j, sep = "/")
      vloop <- getSearchReference(urlr)
      }else{
      vloop <- getSearchReference(pg)
    }
    valr <- rbind(valr, vloop)
  }
  return (valr)
}
