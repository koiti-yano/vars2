require(vars2)
data("Canada_tbl")
var_p2 <- VAR(Canada_tbl,2)

# boot=TRUE
var_p2_irf_boot <- irf(var_p2)
vars_plot(var_p2_irf_boot)

# boot=FALSE
var_p2_irf <- irf(var_p2, boot=F)
vars_plot(var_p2_irf)


# boot=TRUE
var_p2_irf_e_boot <- irf(var_p2,impulse = "e",
                         response = c("e", "prod", "rw", "U"))
vars_plot(var_p2_irf_e_boot)

# boot=FALSE
var_p2_irf_e_boot <- irf(var_p2,impulse = "e",
                         response = c("e", "prod", "rw", "U"),
                         boot=FALSE)
vars_plot(var_p2_irf_e_boot, main="Canada", sub="Orthogonal IRF", cap=NULL)
