# Compare lpirfs and vars2
# https://github.com/AdaemmerP/lpirfs/blob/master/R/lp_lin_iv.R
rm(list=ls())
require(tidyverse)
require(vars2a)
require(lpirfs)
#require(patchwork)

ag_data       <- ag_data
sample_start  <- 7
sample_end    <- dim(ag_data)[1]

# Endogenous data
endog_data    <- ag_data[sample_start:sample_end,3:5]

# [Blanchard and Perotti (2002)]
# Variable to shock with. Here government spending due to
# Blanchard and Perotti (2002) framework
shock         <- ag_data[sample_start:sample_end, 3]

irf_lp_iv1 <- lp_lin_iv(endog_data,
                       lags_endog_lin = 4,
                       shock          = shock,
                       trend          = 0,
                       confint        = 1.96,
                       hor            = 10)
vars_plot(irf_lp_iv1)

# Set seed for reproducibility
set.seed(007)

# Generate instrument variable that is correlated with government spending
instrum       <- as.data.frame(0.9*shock$Gov + rnorm(length(shock$Gov), 0, 0.02) )

irf_lp_iv2 <- lp_lin_iv(endog_data,
                        lags_endog_lin = 4,
                        shock          = shock,
                            instrum        = instrum,
                            use_twosls     = TRUE,
                            trend          = 0,
                            confint        = 1.96,
                            hor            = 20)

# Show all responses
plot(irf_lp_iv2)
vars_plot(irf_lp_iv2,dev_new = T)
