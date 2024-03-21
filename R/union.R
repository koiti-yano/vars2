#' Unite data from API (ex. FRED)
#'
#' @param data_list impulse responses of VAR or SVAR
#' @param var_names subtitile of plot (main title is generated automatically)
#' @param \dots further arguments passed to or from other methods 
#' (currently not used).
#' 
#' @examples
#' \dontrun{
#' library(fredr)
#' library(tidyverse)
#' # Set your FRED API Key
#' fredr_set_key("d88c682c8274047b72e7e64da46a7801")
#' #fredr_set_key("abcdefghijklmnopqrstuvwxyz")
#' #freq_flag <- "m" # Monthly
#' freq_flag <- "q" # Quarterly
#' #freq_flag <- "a" # Annual
#' # In FRED, the start and the end of the observation period
#' start1_date <- "2018-01-01"
#' end1_date <- Sys.Date()
#' 
#' # Real Private Final Consumption Expenditure for United States
#' # https://fred.stlouisfed.org/series/NAEXKP02USQ189S
#' us_prv_c <- fredr_series_observations(
#' series_id = "NAEXKP02USQ189S",
#' observation_start = as.Date(start1_date),
#' observation_end = as.Date(end1_date),
#' frequency = freq_flag,
#' units = "log"
#' )
#' 
#' # Real Government Final Consumption Expenditure for United States
#' # https://fred.stlouisfed.org/series/NAEXKP03USQ652S
#' us_gov_e <- fredr_series_observations(
#' series_id = "NAEXKP03USQ652S",
#' observation_start = as.Date(start1_date),
#' observation_end = as.Date(end1_date),
#' frequency = freq_flag,
#' units = "log"
#' )
#' 
#' # GDP Deflator for United States
#' # https://fred.stlouisfed.org/series/USAGDPDEFQISMEI
#' us_def <- fredr_series_observations(
#' series_id = "USAGDPDEFQISMEI",
#' observation_start = as.Date(start1_date),
#' observation_end = as.Date(end1_date),
#' frequency = freq_flag,
#' units = "log"
#' )
#' 
#' fredr_list <- list(us_prv_c, us_gov_e, us_def)
#' var_names <- c("priv_c", "gov_e", "defl")
#' us_macro <- union.fredr(fredr_list, var_names)  
#' print(us_macro)
#' }
#' @export
"union" <- function(data_list, var_names){
  UseMethod("union", data_list, var_names)
}
