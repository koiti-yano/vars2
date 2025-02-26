#' Plot impulse responses of lp_lin_iv in lpirfs 
#' 
#' @import stats
#' @importFrom ggplot2 ggplot labs facet_grid geom_hline 
#' @importFrom ggplot2 scale_linetype_manual scale_x_continuous
#' @importFrom dplyr full_join mutate
#' @importFrom tibble is_tibble add_column
#' @importFrom tidyr unnest pivot_longer
#'
#' @author Koichi (Koiti) Yano
#' @export

vars_plot.lpirfs_lin_iv_obj <- function(irf, main=NULL, sub=NULL, cap=NULL,
                                   var_name=NULL, dev_new=FALSE, ...){

  # Check class
  if (class(irf) %in% "lpirfs_lin_iv_obj") {
  } else {
    stop("Object is not 'lpirfs_lin_iv_obj' lpirfs::lp_lin_iv()")
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
  
  num_rsp <- length(response)
  
  plot_num <- 1
  for (rsp in 1:num_rsp) {
    #q:「信頼区間がある場合はlowとuppに値を入れる」を英語に訳して
    #q: 「信頼区間がない場合はmeanをlowとuppに入れる」を英語に訳して
    #a: "If there is a confidence interval, put the value in low and upp."  
    #a: "If there is no confidence interval, put the mean in low and upp."
    if(is.null(irf_lower)){
      data.frame(time=1:t_end, mean=as.matrix(t(irf_mean[,1:t_end])[,rsp]), 
                 low=(as.matrix(t(irf_mean[,1:t_end])[,rsp])), # enter mean in low
                 upp=as.matrix(t(irf_mean[,1:t_end])[,rsp])) -> irf_tbl # enter mean in upp
    } else {
      data.frame(time=1:t_end, mean=as.matrix(t(irf_mean[,1:t_end])[,rsp]), 
                 low=(as.matrix(t(irf_lower[,1:t_end])[,rsp])), # 
                 upp=as.matrix(t(irf_upper[,1:t_end])[,rsp])) -> irf_tbl # p
    } 
    
    if(rsp==1){
      irf_tbl |> ggplot() +
        geom_line(aes(x = time, y = mean)) +
        geom_ribbon(aes(x=time, y=mean, ymin=low,ymax=upp),alpha=0.3) + 
        ylab(paste("Resp. of ",response[rsp])) + xlab("Time") +
        ggtitle(paste("Impulse from ",impulse)) +
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
#  browser()
  
  patchwork::wrap_plots(plot_list, byrow=F, 
                        axes = "collect_x", 
                        ncol = 1, nrow = num_rsp) -> plot
 return(plot) 
}

