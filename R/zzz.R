#' Segmenting African Market From African Development Bank Group
#'
#' @param sector the name of the sector or list of sectors
#' @param project_status you can filter the result of sectors by status.
#'
#' @details
#'
#' Reference for to visualize all statuses \code{\link{afr_project_st}()}
#'
#' Reference for to visualize all sectors \code{\link{afr_list_sector}()}
#'
#' @export
#'
afr_sector_df <- function(sector,  ...,  project_status = c("ongoing", "approved", "lending", "pipeline"),  na.rm = TRUE){

  fct <-  formals(afr_sector_df)

  fct_name <- names(fct)

  fct_st <- do.call(missing,  list(fct_name[3]))

  if(fct_st == TRUE){ fct_st <- c("ongoing", "approved", "lending", "pipeline") }else{ fct_st <- project_status}

  x <- sector

  z <- length(x)

  df <- data.frame()

  for(i in 1 : z){

     sct <- afr_sct_value(x[i])

     if(sct == "1" ){
         warning(

                  paste("The sector",  x[i],
                                 "is not found. Please type afr_list_sector() and conform",
                                  sep=" "
                      )
                 )
       break

     }else{

       h <- 0

       d <- data.frame (df = h)

       df <- rbind(df, d)

     }

  }

  dfr <- data.frame()

  if(nrow(df) == z){

     st <- afr_extract()[4]

       for (i in 1 : z){

            link <- paste(afr_extract()[1], sct, sep="")

            y <- xml2::read_html(link)

            np <- getPageNumber(substr(y, unlist(gregexpr(pattern=st, y))[1], unlist(gregexpr(pattern=st, y))[1]+30))

            y <- getData(np, link)

            dfr <- rbind(dfr,  cbind(y,  sector = x[i]))
       }

     dfr <- dfr[dfr$status %in% tolower(fct_st), ]

     rownames(dfr) <- NULL

    return(dfr)

  }

}
