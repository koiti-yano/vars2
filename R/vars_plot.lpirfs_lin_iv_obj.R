#' Plot impulse responses of lp_lin_iv in lpirfs 
#' 
#' @import stats
#' @importFrom ggplot2 ggplot labs facet_grid geom_hline 
#' @importFrom ggplot2 scale_linetype_manual scale_x_continuous
#' @importFrom dplyr full_join mutate
#' @importFrom tibble is_tibble add_column
#' @importFrom tidyr unnest pivot_longer
#'
#' @references Victor Espinoza, (2022), 
#' "Plot() impulse response function - show more than one in one window?,"
#' stackoverflow. \url{https://stackoverflow.com/questions/68010256/plot-impulse-response-function-show-more-than-one-in-one-window}
#' @author Victor Espinoza (stackoverflow), Koichi (Koiti) Yano
#' @export

vars_plot.lpirfs_lin_iv_obj <- function(irf, main=NULL, sub=NULL, cap=NULL,
                                   var_name=NULL, dev_new=FALSE, ...){

  # Check class
  if (class(irf) %in% "lpirfs_lin_iv_obj") {
  } else {
    stop("Object is not 'lpirfs_lin_iv_obj' lpirfs::lp_lin_iv()")
  }
  
}

