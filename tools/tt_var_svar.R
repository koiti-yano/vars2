data(Canada_tbl)
## For VAR
var_p2 <- VAR(Canada, p = 2, type = "const")
var_p2_irf <- irf(var_p2, impulse = "e",
                  response = c("e", "prod", "rw", "U"), boot = TRUE)
plot(var_p2_irf)
ggplot(var_p2_irf)

## For SVAR
amat <- diag(4)
diag(amat) <- NA
svar_p2_a <- SVAR(var_p2, estmethod = "direct", Amat = amat)
svar_p2_a_irf <- irf(svar_p2_a, impulse = "e",
                     response = c("e", "prod", "rw", "U"), boot = TRUE)
plot(svar_p2_a_irf)
ggplot(svar_p2_a_irf)
