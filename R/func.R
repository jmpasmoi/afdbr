
#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#' Status of the projects are approved, ongoing, lending and pipeline
#'
#' @export
#'
getAmount <- function (xx, yy){

  w <- xml2::read_html(xx)

  if ( stringr::str_trim(yy) == "approved" || stringr::str_trim(yy) == "ongoing"){

    z <- rvest::html_nodes(w,"table")

    z <- rvest::html_table(z)[[1]]

    colnames(z) <- c("A","B")

    amount <- dplyr::filter(z,stringr::str_trim(tolower(A)) %in% "total")

    amount <- stringr::str_replace_all(amount[2], "[:alpha:]", "")

  }else{

    z <- rvest::html_nodes (w,"table tbody")

    z <- rvest::html_text(z)

    amount <- stringr::str_replace_all(z, "[:alpha:]", "")

    amount <- stringr::str_trim (amount)
  }

  return (stringr::str_trim(amount))
}


#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Getting the title of the project
#'
#' @export
#'
getTitle <- function (x){

       y <- xml2::read_html(x)
       y <- rvest::html_nodes(y,"title")
       y <- rvest::html_text(y)
       y <- stringr::str_replace_all(y, afr_extract()[2], "")
       y <- substr(y, 1, stringr::str_length(y) - 2)

       #return the title
       return (stringr::str_trim(y))
}

#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Cleaning the amount
#'
#' @export
#'
cleanAmount <- function(x) {
  pattern <- "[:punct:]"
  y <- stringr::str_replace_all(x, pattern, "")
  return (y)
}

#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Getting some details on the project like implementing agency, appraisal date, approval date, start date
#'
#' @export
#'
getProjectDetail <- function (x){

  y <- xml2::read_html (x)
  z <- rvest::html_nodes(y, "div li strong")
  z <- rvest::html_text(z)

  if (length(z) == 7){ ima <- z[6];apsdt <- z[4];aprdt <- z[2];st <- z[3];bp <- NULL}
  else{ ima <- NULL;apsdt <- z[2];aprdt <- NULL;st <-NULL;bp <- z[3] }

          w <- cbind(
                          implementing_agency = ima,
                          appraisal_date = apsdt,
                          approval_date = aprdt,
                          start_date = st,
                          board_presentation = bp
                    )
         return(w)
}
#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Number of pages to explore
#'
#' @export
#'
getPageNumber <- function(x){

  a <- c("[:alpha:]","[:punct:]",">","<","=")
  v <- x

  for (j in 1 : length(a)){

    v <- stringr::str_replace_all(v,a[j],"")

  }

  v <- as.integer(stringr::str_trim(v))

  y <- as.integer(v/20)

  if (v %% 20 > 0) {
           y <- y + 1
  }else{   y <- y}

  return (y)
}

#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Getting the status of the project
#'
#' @export
#'

getStatus <- function (x){
  return (stringr::str_trim(tolower(substr(x, 1, stringr::str_length(x) - 4))))
}


#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Getting the name of the country
#'
#' @export
#'

getCountry <- function (x){

  e <- unlist(gregexpr(pattern = ",", x)) - 1

  x <- substr(x, 1, e)

  e <- unlist(gregexpr(pattern = ":", x)) + 1

  y <- stringr::str_trim(substr(x, e, stringr::str_length(x)))

  if (stringr::str_length(y) >= 30){

    e <- unlist(gregexpr(pattern = ":", y)) + 1
    y <- stringr::str_trim(substr(y, e, stringr::str_length(y)))
    y <- stringi::stri_extract_last_boundaries(y)

  }

  if (stringr::str_detect (tolower(y),"ivoire") == TRUE) { y <- "Cote d'Ivoire"}
  else if (stringr::str_detect (tolower(y),"ncipe") == TRUE) {y <- "Sao Tome and Principe"}
  else { y <- y }

  return (y)

}

#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Getting the link of the project
#'
#' @export
#'

getLink <- function (x){

  return (stringr::str_trim(tolower(substr(x, 1, stringr::str_length(x)))))

}

#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Function which checks the url of all the project and their status
#'
#' @export
#'
getSearchReference <- function (x){

  x <- x
  x <- xml2::read_html(x)
  x <- rvest::html_nodes(x,"tr td")
  x <- rvest::html_text(x)

  ui <- x

  ui <- as.data.frame(ui)

  nbrow <- nrow(ui)

  valr <- data.frame()

  for (j in 1 : nbrow){

    if (j %% 3 == 0){

               status  <- getStatus(ui[j,])
               country <- getCountry(ui[j-1,])
               link    <- paste(afr_extract()[3], getLink(ui[j-2,]),sep="")
               projectID <- getLink(ui[j-2,])
               amount  <- getAmount(link,status)
               amount  <- cleanAmount(amount)
               title   <- getTitle(link)
               details <- getProjectDetail(link)

               vloop <- data.frame(country,projectID,title,status,amount,details)
               valr  <- rbind(valr,vloop)
    }

  }
  return(valr)
}

#' Segmenting African Market From African Development Bank Group
#'
#' @details
#'
#'Browsing all the subpages related to the choice of the user from the interface
#'
#' @export
#'
getData <- function (x,link){


        valr <- data.frame ()

       for (j in 1 : x - 1){

            if (j > 0) {

                urlr <- paste(link, j, sep = "")

                vloop <- getSearchReference(urlr)

            }else{

                vloop <- getSearchReference(link)
            }

        valr <- rbind(valr, vloop)
    }
        return (valr)
}


