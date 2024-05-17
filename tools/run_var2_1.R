require(vars2)
data("Canada_tbl")
var_p2 <- VAR(Canada_tbl,2)

# boot=TRUE
var_p2_irf_boot <- irf(var_p2)
ggplot(var_p2_irf_boot)

# boot=TRUE
var_p2_irf_boot <- irf(var_p2)
ggplot(var_p2_irf_boot)

# boot=FALSE
var_p2_irf <- irf(var_p2, boot=F)
ggplot(var_p2_irf)

# boot=TRUE
var_p2_irf_e_boot <- irf(var_p2,impulse = "e",
                         response = c("e", "prod", "rw", "U"))
ggplot(var_p2_irf_e_boot)

# boot=FALSE
var_p2_irf_e_boot <- irf(var_p2,impulse = "e",
                         response = c("e", "prod", "rw", "U"),
                         boot=FALSE)
ggplot(var_p2_irf_e_boot, main="Canada", sub="Orthogonal IRF", cap=NULL)
