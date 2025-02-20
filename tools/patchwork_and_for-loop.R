# patchwork and for-loop

# plot the list of plots
# https://stackoverflow.com/questions/10706753/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange
# byrow=FALSE: plots are arranged in columns
rm(list=ls())
require(tidyverse)
require(vars2)
require(lpirfs)
#require(patchwork)

lp_p2_irf <- lp_lin(endog_data= Canada_tbl[,2:5], lags_endog_lin = 2,
                    trend = 0, shock_type  = 1, confint = 1.96, hor = 10)
# Show all impule responses
# Compare with Figure 5 in Jordà (2005)
vars_plot(lp_p2_irf)

irf <- lp_p2_irf

plot_list <- list(NaN)

irf_mean <- irf$irf_lin_mean
irf_lower <- irf$irf_lin_low
irf_upper <- irf$irf_lin_up
impulse <- irf$specs$column_names
response <- irf$specs$column_names

t_end <- irf$specs$hor

num_imp <- length(impulse)
num_rsp <- length(response)

dev.new()
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
                      ncol = num_imp, nrow = num_rsp) 

