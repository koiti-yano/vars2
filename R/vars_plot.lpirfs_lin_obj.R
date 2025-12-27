#' Plot impulse responses of lp_lin in lpirfs 
#'  
#' @param irf impulse responses of lp_lin in lpirfs 
#' @param main main title of plot (The default is NULL, in which case the main
#' title is generated automatically.)
#' @param sub subtitile of plot, (not implemented yet)
#' @param cap caption of plot, (not implemented yet)
#' @param imp_name variable names of impulse: ex. imp_name=c("Emp", "Prod", "RW", "Unemp")
#' @param resp_name variable names of reponse: ex. resp_name=c("Emp", "Prod", "RW", "Unemp")
#' @param dev_new logical. If TRUE, open a new graphics device.
#' @param \dots further arguments passed to or from other methods 
#' (currently not used).
#' @return A ggplot object
#' 
#' @import stats
#' @importFrom ggplot2 ggplot labs facet_grid geom_hline 
#' @importFrom ggplot2 scale_linetype_manual scale_x_continuous
#' @importFrom dplyr full_join mutate
#' @importFrom tibble is_tibble add_column
#' @importFrom tidyr unnest pivot_longer
#'
#' @importFrom patchwork wrap_plots plot_annotation
#' @author Koichi (Koiti) Yano
#'
#' @examples
#'\dontrun{
#' require(vars2)
#' require(lpirfs)
#'
#' lp_p2_irf <- lp_lin(endog_data= Canada_tbl[,2:5], lags_endog_lin = 2,
#' trend = 0, shock_type  = 1, confint = 1.96, hor = 10)
#' # Show all impule responses
#' vars_plot(lp_p2_irf, dev_new=TRUE)
#' }
#' @export
vars_plot.lpirfs_lin_obj <- function(irf, main=NULL, sub=NULL, cap=NULL,
                                     imp_name=NULL, resp_name=NULL, dev_new=FALSE, ...){
  
  if (!inherits(irf, "lpirfs_lin_obj")) stop("Object is not 'lpirfs_lin_obj' lpirfs::lp_lin()")
  if (isTRUE(dev_new)) dev.new()
  
  irf_mean <- irf$irf_lin_mean
  irf_lower <- irf$irf_lin_low
  irf_upper <- irf$irf_lin_up
  
  t_end <- irf$specs$hor
  
  impulse <- irf$specs$column_names
  response <- irf$specs$column_names
  
  if (!is.null(imp_name) && (length(impulse) == length(imp_name))) impulse <- imp_name
  if (!is.null(resp_name) && (length(response) == length(resp_name))) response <- resp_name
  
  num_imp <- length(impulse)
  num_rsp <- length(response)
  
  plot_list <- list()
  plot_num <- 1
  
  for (imp in 1:num_imp) {
    for (rsp in 1:num_rsp) {
      
      mean_val <- as.matrix(t(irf_mean[, 1:t_end, imp]))[, rsp]
      
      if (!is.null(irf_lower) && !is.null(irf_lower[,,imp])) {
        low_val <- as.matrix(t(irf_lower[, 1:t_end, imp]))[, rsp]
        upp_val <- as.matrix(t(irf_upper[, 1:t_end, imp]))[, rsp]
      } else {
        low_val <- mean_val
        upp_val <- mean_val
      }
      
      irf_tbl <- data.frame(time = 1:t_end, mean = mean_val, low = low_val, upp = upp_val)
      
      p <- ggplot(irf_tbl, aes(x = time, y = mean)) +
        geom_line() +
        geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.3) +
        geom_hline(yintercept = 0, col = "red", linewidth = 0.5, linetype = "dashed") +
        labs(x = "Time", y = paste("Resp. of", response[rsp]))
      
      if (rsp == 1) {
        p <- p + ggtitle(paste("Impulse from", impulse[imp]))
      }
      
      plot_list[[plot_num]] <- p
      plot_num <- plot_num + 1
    }
  }
  
  patchwork::wrap_plots(plot_list, byrow = FALSE, axes = "collect_x",
                        ncol = num_imp, nrow = num_rsp) +
    patchwork::plot_annotation(title = main, caption = cap)
}