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
