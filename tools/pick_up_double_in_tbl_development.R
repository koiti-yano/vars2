# tblからdoubleだけ取り出す（Bad knowhow?）
# VAR()にいれるコードに近いもの
# https://koiti-yano.hatenablog.com/entry/2024/03/09/230721
# https://cran.rstudio.com/web/packages/tsibble/vignettes/intro-tsibble.html
# https://stackoverflow.com/questions/62583977/why-is-select-if-is-double-returning-dates

require(tidyverse)
#install.packages("nycflights13")
require(nycflights13)
rm(list=ls())
nycflights13::weather -> weather_all
y <- dplyr::select(weather_all,
                          origin, time_hour, temp, humid, precip)
y <- as.data.frame(y)
# tsdatはoriginがchar, time_hourがdate,
# それ以外がdoubleになっている。

# うまくいかない方法
dplyr::select_if(y, is.double) -> tbl_notwork
tbl_notwork

# うまくいく方法（うまくいく理由がわからない）

#ts1 <- ts(1:10, start=c(1990,1), freq=4)
#ts2 <- ts(10:1, start=c(1990,1), freq=4)
#y <- ts.union(ts1,ts2)

class(y)
head(y)
if (is.ts(y)) {
  print("ts data")
  as.matrix(y) -> y
} else if (is.data.frame(y) || tibble::is_tibble(y)){
  # https://www.geeksforgeeks.org/select-only-numeric-columns-from-dataframe-in-r/
  print("df or tibble")
  dplyr::select_if(y, is.numeric) -> y_tmp
  as.matrix(y_tmp) -> y
  #    browser()
} else {
  # Do nothing
}
head(y)
class(y)
