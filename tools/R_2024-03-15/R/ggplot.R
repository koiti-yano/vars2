#' ggplot: generic function
#'
#' @param irf a list of impulse responses
#' @export

"ggplot" <- function(irf){
  UseMethod("ggplot")
}
