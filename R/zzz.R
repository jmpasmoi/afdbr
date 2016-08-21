#' Segmenting African Market From African Development Bank Group
#'
#' @export
#'
afr_sector_df <- function (sector){

  x <- sector

  z <- length(x)

  df <- data.frame()

  for(i in 1 : z){

     sct <- afr_sct_value(x[i])

     if (sct == "1" ){
         warning(paste ("The sector", x[i],
                        "is not found. Please type afr_list_sector() and conform",
                         sep=" ")
                 )
       break
     }

     h <- 0

     d <- data.frame (df = h)

     df <- rbind(df,d)

  }

  dfr <- data.frame()

  if(nrow(df) == z){

     st <- afr_extract()[4]

       for (i in 1 : z){

            link <- paste(afr_extract()[1],x[i],sep="")

            y <- xml2::read_html(link)

            np <- getPageNumber(substr(y,unlist(gregexpr(pattern=st,y))[1],unlist(gregexpr(pattern=st,y))[1] + 30))

            dfr <- rbind(dfr, cbind(getData(np,link), sector = x[i] ) )
         }
    }
  return(dfr)
}
