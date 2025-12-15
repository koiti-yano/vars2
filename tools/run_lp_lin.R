# Compare lpirfs and vars2

rm(list=ls())
require(tidyverse)
require(vars2a)
require(lpirfs)
#require(patchwork)

# lpirfs
lp_p2_irf <- lp_lin(endog_data= Canada_tbl[,2:5], lags_endog_lin = 2,
                    trend = 0, shock_type  = 1, confint = 1.96, hor = 10)
# Show all impule responses
# Compare with Figure 5 in Jordà (2005)
vars_plot(lp_p2_irf, main="Canada", imp_name=c("Emp"),
 resp_name=c("Emp", "Prod", "Real Wage", "Unemp"))

var_p2 <- VAR(Canada_tbl,2)

# boot=TRUE
var_p2_irf_boot <- irf(var_p2)
vars_plot(var_p2_irf_boot, main="Canada")
