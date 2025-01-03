# run lp_lin and ggplot()
rm(list=ls())
require(lpirfs)
require(vars2)

# Load (endogenous) data
endog_data <- interest_rules_var_data

# Estimate linear model
lp_irf <- lp_lin(endog_data, lags_endog_lin = 4, trend = 0,
                 shock_type = 1, confint  = 1.96, hor = 12)
# Compare with Figure 5 in Jordà (2005)
ggplot(lp_irf, dev_new=F)

# vars or vars2
vars_res <- VAR(endog_data, p=2)
var_irf <- irf(vars_res)
ggplot(var_irf, dev_new=F)  

if(0){
  par(mfrow=c(3,1))
  plot(endog_data$GDP_gap, type="l")
  plot(endog_data$Infl, type="l")
  plot(endog_data$FF, type="l")
}
