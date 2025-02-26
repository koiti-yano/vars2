#' Plot impulse responses of lp_lin in lpirfs 
#'  
#' @param irf impulse responses of lp_lin in lpirfs 
#' @param main main title of plot (The default is NULL, in which case the main
#' title is generated automatically.)
#' @param sub subtitile of plot 
#' @param cap caption of plot
#' @param var_name variable names: ex. var_name=c("Emp", "Prod", "RW", "Unemp")
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
                                     var_name=NULL, dev_new=FALSE, ...){
  
  # Check class
  if (class(irf) %in% "lpirfs_lin_obj") {
  } else {
    stop("Object is not 'lpirfs_lin_obj' lpirfs::lp_lin()")
  }
  
  # dev.new() if dev_new is TRUE.
  if (isTRUE(dev_new)){ dev.new() } else { } 
  
  plot_list <- list(NaN)
  
  irf_mean <- irf$irf_lin_mean
  irf_lower <- irf$irf_lin_low
  irf_upper <- irf$irf_lin_up
  impulse <- irf$specs$column_names
  response <- irf$specs$column_names
  
  t_end <- irf$specs$hor
  
  num_imp <- length(impulse)
  num_rsp <- length(response)
  
  plot_num <- 1
  for (imp in 1:num_imp) {
    for (rsp in 1:num_rsp) {
      #q:「信頼区間がある場合はlowとuppに値を入れる」を英語に訳して
      #q: 「信頼区間がない場合はmeanをlowとuppに入れる」を英語に訳して
      #a: "If there is a confidence interval, put the value in low and upp."  
      #a: "If there is no confidence interval, put the mean in low and upp."
      if(is.null(irf_lower[,,imp][,rsp])){
        data.frame(time=1:t_end, mean=as.matrix(t(irf_mean[,1:t_end,imp]))[, rsp], 
                   low=(as.matrix(t(irf_mean[,1:t_end,imp]))[, rsp]), # enter mean in low
                   upp=as.matrix(t(irf_mean[,1:t_end,imp]))[, rsp]) -> irf_tbl # enter mean in upp
      } else {
        data.frame(time=1:t_end, mean=as.matrix(t(irf_mean[,1:t_end,imp]))[, rsp], 
                   low=(as.matrix(t(irf_lower[,1:t_end,imp]))[, rsp]), # 
                   upp=as.matrix(t(irf_upper[,1:t_end,imp]))[, rsp]) -> irf_tbl # p
      } 
      
      if(rsp==1){
        irf_tbl |> ggplot() +
          geom_line(aes(x = time, y = mean)) +
          geom_ribbon(aes(x=time, y=mean, ymin=low,ymax=upp),alpha=0.3) + 
          ylab(paste("Resp. of ",response[rsp])) + xlab("Time") +
          ggtitle(paste("Impulse from ",impulse[imp])) +
          geom_hline(yintercept = 0, col = "red", 
                     linewidth = 0.5, linetype = "dashed") -> plot_list[[plot_num]] 
      } else {
        irf_tbl |> ggplot() + 
          geom_line(aes(x = time, y = mean)) +
          geom_ribbon(aes(x=time, y=mean, ymin=low,ymax=upp),alpha=0.3) + 
          ylab(paste("Resp. of ",response[rsp])) + xlab("Time") +
          #        ggtitle(paste("Impulse from ",impulse[imp])) +
          geom_hline(yintercept = 0, col = "red", 
                     linewidth = 0.5, linetype = "dashed") -> plot_list[[plot_num]]
        
      }
      plot_num <- plot_num + 1
      
    }
  }
  patchwork::wrap_plots(plot_list, byrow=F, 
                        axes = "collect_x", 
                        ncol = num_imp, nrow = num_rsp) -> plot
  
  return(plot)
  
}