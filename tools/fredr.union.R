# fredr.union
rm(list=ls())

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
jp_prv_c %>% rename(value_jp_prv_c = value) -> jp_prv_c
jp_prv_c %>% head()

# https://fred.stlouisfed.org/series/NAEXKP03JPQ189S
jp_gov_c <- fredr_series_observations(
  series_id = "NAEXKP03JPQ189S",
  observation_start = as.Date(start1_date),
  observation_end = as.Date(end1_date),
  frequency = freq_flag,
  units = "log"
)
jp_gov_c %>% rename(value_jp_gov_c = value) -> jp_gov_c
jp_gov_c %>% head()

left_join(jp_prv_c, jp_gov_c, by="date") -> jp_macro
jp_macro %>% head()
