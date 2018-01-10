#' IE from unstructured data
#' @param segment the name of the segment or list of segments
#' @param pr.status you can filter the result of segments by status.
#' @details
#' Reference for to visualize all statuses \code{\link{afr_project_st}()}
#' Reference for to visualize all segments \code{\link{afr_list_segment}()}
#' @export
#'
afr_segment_df <- function(segment,  ...,  pr.status = c("ongoing", "approved", "lending", "pipeline"),  na.rm = TRUE){

  pr_st <- match.arg(pr.status,several.ok=TRUE)
  fct <-  formals(afr_segment_df)
  fct_name <- names(fct)
  fct_st <- do.call(missing,list(fct_name[3]))

if(fct_st == TRUE){ fct_st <- c("ongoing", "approved", "lending", "pipeline") }else{ fct_st <- pr_st}

  x <- segment;z <- length(x)
  df <- data.frame();chr <- data.frame()
  for(i in 1 : z){

     sct <- afr_sct_value(x[i])
     chr <- rbind(chr, data.frame(sct = paste0(sct)))
     if(is.null(sct)){
         warning(
                  paste("The segment",  x[i],
                                 "is not found. See afr_list_segment() for more details",
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

     rp <- "";st <- afr_extract()[4]
     gp <- afr_extract()[5];chr <- as.data.frame(chr)
     dfr <- data.frame(check.rows= FALSE)
       for (i in 1 : z){
            link <- paste(afr_extract()[1], chr[i,], sep = "")
            con <- file(link, open = "r")
            w <- base::readLines(con, warn = F)
            m <- grep(gp,w)
            close(con)
            if(identical(class(m),"integer") == TRUE && identical(m, integer(0)) == TRUE){
                 y <- xml2::read_html(link)
                 np <- getPageNumber(substr(y, unlist(gregexpr(pattern = st, y))[1], unlist(gregexpr(pattern = st, y))[1]+30))
                 y <- getData(np, link)
                 dfr <- rbind(dfr,  cbind(y, segment = x[i]))
            }else{
                 t <- x[i]
                 rp <- paste(t, rp, sep = ",")
            }
       }
     dfr <- dfr[dfr$status %in% tolower(fct_st), ]
     rownames(dfr) <- NULL
     if(nrow(dfr) == 0){ dfr <- "No data found" }else{ dfr <- dfr}
     if(nchar(rp) < 5){
              rpo <- 0     }else{
              rpo <- paste("Missing value", rp, sep = ": ")
              rpo <- substr(rpo,1,nchar(rpo)-1)
      }
     invisible(list(data = unique(dfr), report = rpo))
  }
}
