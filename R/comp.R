#' Project comparison
#'
afr_data_comp <- function(project, src.comp=c("un", "worldbank", "eu"), search=TRUE){

  src <- match.arg(src.comp)
  site <- base::grep(scr, afr_extract())
  wb <- afr_extract()[site]

  if(search){
    st <- httr::GET(paste0(wb,project))$status_code
  }else{
    stop("stop",call.=F)
  }

ifelse(st!=404,"yes","no")

}
