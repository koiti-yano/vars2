# tblからdoubleだけ取り出す（Bad knowhow?）
# VAR()にいれるコードに近いもの
# https://koiti-yano.hatenablog.com/entry/2024/03/09/230721
# https://cran.rstudio.com/web/packages/tsibble/vignettes/intro-tsibble.html
# https://stackoverflow.com/questions/62583977/why-is-select-if-is-double-returning-dates

#require(dplyr)
#require(nycflights13)
rm(list=ls())
nycflights13::weather -> weather_all
y <- dplyr::select(weather_all,
                          origin, time_hour, temp, humid, precip)
y
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
if (is.ts(y)) {
  print("ts data")
  as.matrix(y) -> y
} else if (tibble::is_tibble(y)){
  #  print("tibble")
  dplyr::select_if(y, is.numeric) -> y_tmp
  as.matrix(y_tmp) -> y
} else
  # Warning
  stop("\n The matrix 'y' should be ts or tbl_df.\n")
}

head(y)
class(y)
