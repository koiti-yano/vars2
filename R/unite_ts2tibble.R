#' Unite multiple ts objects, and convert them to a tibble object 
#' 
#' @param ... 
#' @importFrom stats ts.union
#' @importFrom tidyr pivot_wider
#' @importFrom tsbox ts_tbl
#' @export
#' @examples \dontrun{
#' ts1 <- ts(1:12, freq=4, start=c(2020,1))
#' ts2 <- ts(12:1, freq=4, start=c(2020,1))
#' ts3 <- ts(13:24, freq=4, start=c(2020,1))
#' 
#' unite_ts2tbl(ts1, ts2, ts3)
#' }

"unite_ts2tbl" <- function(...){
  ts_united_tmp <- (ts.union(...))
  
  tsbox::ts_tbl(ts_united_tmp) -> ts_united_tidy
  #canada_tidy %>% view()
  
  ts_united_tidy |> tidyr::pivot_wider(names_from = id,
                                     values_from=value) -> data_tbl
  return(data_tbl)
  
}
