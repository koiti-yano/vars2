#
require(tidyverse)
library(lubridate)
library(nycflights13)

flights %>% select(year, month, day, hour, minute) %>% 
  slice(1:10) -> ff10
flights %>% select(year, month, day, hour, minute) %>% 
  slice(11:20) -> ff20

ddd <- function(...)
{
  for (ii in c(...)){
    browser()
  }
}

ddd(ff10, ff20)