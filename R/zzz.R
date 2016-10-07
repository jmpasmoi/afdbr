#' IE from unstructured data
#'
#' @param sector the name of the sector or list of sectors
#' @param project_status you can filter the result of sectors by status.
#'
#' @details
#'
#' Reference for to visualize all statuses \code{\link{afr_project_st}()}
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
  chr <- data.frame()

  for(i in 1 : z){

     sct <- afr_sct_value(x[i])

     chr <- rbind(chr, data.frame(sct = paste0(sct)))

     if(is.null(sct)){

         warning(

                  paste("The sector",  x[i],
                                 "is not found. See afr_list_sector() for more details",
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

  if(nrow(df) == z){


     rp <- ""
     st <- afr_extract()[4]
     gp <- afr_extract()[5]

     chr <- as.data.frame(chr)
     dfr <- data.frame(check.rows= FALSE)

       for (i in 1 : z){

            link <- paste(afr_extract()[1], chr[i,], sep = "")

            w <- base::readLines(link, warn = F)

            m <- grep(gp,w)

            if(identical(class(m),"integer") == TRUE && identical(m, integer(0)) == TRUE){

                 y <- xml2::read_html(link)

                 np <- getPageNumber(substr(y, unlist(gregexpr(pattern = st, y))[1], unlist(gregexpr(pattern = st, y))[1]+30))

                 y <- getData(np, link)

                 dfr <- rbind(dfr,  cbind(y,  sector = x[i]))

            }else{

                 t <- x[i]

                 rp <- paste(t, rp, sep = ",")
            }

       }

     dfr <- dfr[dfr$status %in% tolower(fct_st), ]

     rownames(dfr) <- NULL

     if(nrow(dfr) == 0){ dfr <- "No data found" }else{ dfr <- dfr}
     if(nchar(rp) < 5){
              rpo <- 0
     }else{
              rpo <- paste("Missing value", rp, sep = ": ")
              rpo <- substr(rpo,1,nchar(rpo)-1)
      }

     rpt <- list(data = dfr, report = rpo)

     invisible(rpt)

  }

}
