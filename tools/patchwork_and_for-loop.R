# patchwork and for-loop

# plot the list of plots
# https://stackoverflow.com/questions/10706753/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange
# byrow=FALSE: plots are arranged in columns
rm(list=ls())
require(tidyverse)
require(vars2)

data("Canada_tbl")
var_p2 <- VAR(Canada_tbl,2)

# boot=FALSE
irf <- irf(var_p2, boot=T)

plot_list <- list(NaN)

irf_mean <- irf$irf
irf_lower <- irf$Lower
irf_upper <- irf$Upper
impulse <- irf$impulse
response <- irf$response

t_end <- dim(irf_mean[[1]])[1]

num_imp <- length(impulse)
num_rsp <- length(response)

plot_num <- 1
for (imp in 1:num_imp) {
  for (rsp in 1:num_rsp) {
    
    if(is.null(irf_lower[[imp]][,rsp])){
      data.frame(time=1:t_end, mean=irf_mean[[imp]][,rsp],
                 low=irf_mean[[imp]][,rsp], 
                 upp=irf_mean[[imp]][,rsp]) -> irf_tbl
    } else {
      data.frame(time=1:t_end, mean=irf_mean[[imp]][,rsp],
                 low=irf_lower[[imp]][,rsp], 
                 upp=irf_upper[[imp]][,rsp]) -> irf_tbl
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

