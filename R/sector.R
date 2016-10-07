#' Segmenting African Market From African Development Bank Group
#'
#' @details
#' List of public sectors
#'
#' @export

afr_list_sector <- function() {

  y <- list(
    title = "List of public sectors",
    sector1 = c("climate.change","agriculture","economic", "water"),
    sector2 = c("education","energy","environment", "transport"),
    sector3 = c("health","information","infrastructure","gender","human")
  )

  print(y)

}

#' Segmenting African Market From African Development Bank Group
#'
#'@export
#'
afr_sct_value <- function (sector){

  sct <- tolower(stringr::str_trim(sector))

  val <- NULL

  if(sct == "climate.change" || sct == "climate_change" || sct == "climate"){val <- "climate-change"}
  else if(sct == "agriculture"){val <- "agriculture-agro-industries"}
  else if(sct == "economic" || sct == "economy"){val <- "economic-financial-governance"}
  else if(sct == "education"){val <- "education"}
  else if(sct == "energy"){val <- "energy-power"}
  else if(sct == "environment"){val <- "environment"}
  else if(sct == "human"){val <- "human-capital-development"}
  else if(sct == "health"){val <- "health"}
  else if(sct == "information"){val <- "information-communication-technology"}
  else if(sct == "infrastructure"){val <- "infrastructure"}
  else if(sct == "gender"){val <- "gender"}
  else if(sct == "transport"){val <- "transport"}
  else if(sct == "water"){val <- "water-supply-sanitation"}
  else {val <- NULL }

  return (val)
}

#' Segmenting African Market From African Development Bank Group
#'
#' @details
#' List of the status of projects
#'
#' @export
afr_project_st <- function(){
   st <- c("ongoing","approved","lending","pipeline")
   print(st)
}




