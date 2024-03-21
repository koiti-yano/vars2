#' union
#' @import stats
#' @import dplyr
#' @export
"union.fredr" <- function(data_list, var_names){
  fredr_list <- data_list
  num_epoch <- length(fredr_list)
  tmp_list <- list()

  # Pick up "date" and "value" and set names
  for (ii in 1:num_epoch) {
#    browser()
    col_names <- c("date", var_names[ii])
    # https://stackoverflow.com/questions/36520813/r-dplyr-rename-and-select-using-string-variable
    fredr_list[[ii]] |>  dplyr::select(date, value) |>
      stats::setNames(col_names) -> tmp
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
      dplyr::left_join(fredr_united, tmp_list[[ii]]) -> fredr_united
    }
  }
#  print(fredr_united)
  return(fredr_united)
}
