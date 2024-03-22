# fredr.union
# https://fred.stlouisfed.org/searchresults/?st=national%20account%20japan%20constant%20prices
# https://fred.stlouisfed.org/searchresults/?st=national%20account%20japan%20deflator
rm(list=ls())

union.fredr <- function(fredr_list, var_names, tmp_list=NULL){
  num_epoch <- length(fredr_list)
  
  if (is.null(tmp_list)){
    tmp_list <- list()
  } 
  
  # Pick up "date" and "value" and set names
  for (ii in 1:num_epoch) {
#    browser()
    col_names <- c("date", var_names[ii])
    # https://stackoverflow.com/questions/36520813/r-dplyr-rename-and-select-using-string-variable
    fredr_list[[ii]] %>%  select(date, value) %>% setNames(col_names) -> tmp
    tmp_list <- c(tmp_list, list(tmp))
  }
#  print(tmp_list)

  # Joint "fredr tibbles" by left_joint
  fredr_united <- NULL
#  browser()
  for (ii in 1:num_epoch) {
    if(is.null(fredr_united)){
      fredr_united <- tmp_list[[ii]]
    } else {
#      browser()
      left_join(fredr_united, tmp_list[[ii]]) -> fredr_united
    }
  }
#  print(fredr_united)
  return(fredr_united)
}

require("fredr")
library(tidyverse)

# FREDのAPIを利用するキーを指定します
fredr_set_key("d88c682c8274047b72e7e64da46a7801")

#freq_flag <- "m" # Monthly
freq_flag <- "q" # Quarterly
#freq_flag <- "a" # Annual

# In FRED, the start and the end of the observation period
start1_date <- "2018-01-01"
end1_date <- Sys.Date()


# https://fred.stlouisfed.org/series/NAEXKP02JPQ189S
jp_prv_c <- fredr_series_observations(
  series_id = "NAEXKP02JPQ189S",
  observation_start = as.Date(start1_date),
  observation_end = as.Date(end1_date),
  frequency = freq_flag,
  units = "log"
)

# https://fred.stlouisfed.org/series/NAEXKP03JPQ189S
jp_gov_e <- fredr_series_observations(
  series_id = "NAEXKP03JPQ189S",
  observation_start = as.Date(start1_date),
  observation_end = as.Date(end1_date),
  frequency = freq_flag,
  units = "log"
)

# https://fred.stlouisfed.org/series/JPNGDPDEFQISMEI
jp_def <- fredr_series_observations(
  series_id = "JPNGDPDEFQISMEI",
  observation_start = as.Date(start1_date),
  observation_end = as.Date(end1_date),
  frequency = freq_flag,
  units = "log"
)

# https://stackoverflow.com/questions/36520813/r-dplyr-rename-and-select-using-string-variable
fredr_list <- list(jp_prv_c, jp_gov_e, jp_def)
var_names <- c("priv_c", "gov_e", "defl")

union.fredr(fredr_list, var_names) -> jp_macro
print(jp_macro)
