.afr.env <- new.env()
.read.afr <- function() {
  filename <- system.file("afdbr", "afdbr.txt", package="afdbr")
  if (!file.exists(filename)) stop("Hm, file", filename, "is missing.", call.=FALSE)
  data <- readLines(filename, encoding="UTF-8", warn = F)
  data <- data[! grepl("^##", data)]
}

#' Segmenting African Market From African Development Bank Group
#' @details
#' Extract textual information
#' @export
afr_extract <- function (){
  if (is.null(.afr.env$afr.data)) .afr.env$afr.data <- .read.afr()
  afr <- .afr.env$afr.data
  return(afr)
}
