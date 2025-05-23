% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unite_fredr.R
\name{unite_fredr}
\alias{unite_fredr}
\title{Unite tibble objects generated by fredr (CRAN)}
\usage{
unite_fredr(fredr_list, var_name)
}
\arguments{
\item{fredr_list}{the list of tibble objects from fredr}

\item{var_name}{the name list of variables}
}
\description{
Unite tibble objects generated by fredr (CRAN)
}
\examples{
\dontrun{
# devtools::check() make errors because fredr_set_key is required.
# The following example is not run.
library(fredr)
library(tidyverse)
# Set your FRED API Key
fredr_set_key("abcdefghijklmnopqrstuvwxyz") # Set your FRED API Key
#freq_flag <- "m" # Monthly
freq_flag <- "q" # Quarterly
#freq_flag <- "a" # Annual
# In FRED, the start and the end of the observation period
start1_date <- "2018-01-01"
end1_date <- Sys.Date()

# Real Private Final Consumption Expenditure for United States
# https://fred.stlouisfed.org/series/NAEXKP02USQ189S
us_prv_c <- fredr_series_observations(
series_id = "NAEXKP02USQ189S",
observation_start = as.Date(start1_date),
observation_end = as.Date(end1_date),
frequency = freq_flag,
units = "log"
)

# Real Government Final Consumption Expenditure for United States
# https://fred.stlouisfed.org/series/NAEXKP03USQ652S
us_gov_e <- fredr_series_observations(
series_id = "NAEXKP03USQ652S",
observation_start = as.Date(start1_date),
observation_end = as.Date(end1_date),
frequency = freq_flag,
units = "log"
)

# GDP Deflator for United States
# https://fred.stlouisfed.org/series/USAGDPDEFQISMEI
us_def <- fredr_series_observations(
series_id = "USAGDPDEFQISMEI",
observation_start = as.Date(start1_date),
observation_end = as.Date(end1_date),
frequency = freq_flag,
units = "log"
)

fredr_list <- list(us_prv_c, us_gov_e, us_def)
var_name <- c("priv_c", "gov_e", "defl")
us_macro <- unite_fredr(fredr_list, var_name)  
print(us_macro)
}
}
