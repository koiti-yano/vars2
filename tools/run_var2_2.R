require(vars2)
data(Canada_tbl)
## For VAR
var_p2 <- VAR(Canada_tbl, p = 2, type = "const")
var_p2_irf <- irf(var_p2, impulse = "e",
                  response = c("e", "prod", "rw", "U"), boot = TRUE)
plot(var_p2_irf)
ggplot(var_p2_irf, var_name=c("Emp", "Prod", "Real Wage", "Unemp"))

## For SVAR
amat <- diag(4)
diag(amat) <- NA
svar_p2_a <- SVAR(var_p2, estmethod = "direct", Amat = amat)
svar_p2_a1_irf <- irf(svar_p2_a, impulse = "e",
                     response = c("e", "prod", "rw", "U"), boot = TRUE)
plot(svar_p2_a1_irf)
ggplot(svar_p2_a1_irf, var_name=c("Emp", "Prod", "Real Wage", "Unemp"))

# 
svar_p2_a2_irf <- irf(svar_p2_a, boot = TRUE)
#plot(svar_p2_a2_irf)
ggplot(svar_p2_a2_irf, var_name=c("Emp", "Prod", "Real Wage", "Unemp"), 
       dev_new=TRUE)
