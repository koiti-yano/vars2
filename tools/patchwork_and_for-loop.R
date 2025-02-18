# patchwork and for-loop

require(tidyverse)
#require(patchwork)

# create a list of plots
plot_list <- list(NaN)

for (i in 1:10) {
  plot_list[[i]]  <- ggplot(mtcars, aes(x = mpg, y = hp)) +
    geom_point() +
    ggtitle(paste("Plot", i))
}

# plot the list of plots
# https://stackoverflow.com/questions/10706753/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange
# byrow=FALSE: plots are arranged in columns
patchwork::wrap_plots(plot_list, byrow=FALSE) 

require(tidyverse)
# q: varsのvarirfをggplot2でプロットするプログラムを作ってください
# a: 以下のプログラムを実行してください

