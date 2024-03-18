# tblからdoubleだけ取り出す（Bad knowhow?）

# https://koiti-yano.hatenablog.com/entry/2024/03/09/230721
# https://cran.rstudio.com/web/packages/tsibble/vignettes/intro-tsibble.html
# https://stackoverflow.com/questions/62583977/why-is-select-if-is-double-returning-dates

require(dplyr)
require(nycflights13)

weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)

weather %>% select_if(is.double)

weather %>% select_if(is.numeric)