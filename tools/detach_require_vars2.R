# Script for debug
detach("package:vars2", unload=TRUE)
install.packages("~/8_vars2/vars2_1.6-1.zip", repos = NULL, type = "win.binary")

require(vars2)

data(Canada_tbl)
VAR(Canada_tbl, p = 2, type = "trend") -> var_p2
#plot(var_p2)

irf(var_p2, impulse = "e", response = c("e", "prod", "rw", "U")) -> irf_var_p2_boot
plot(irf_var_p2_boot)

irf(var_p2, impulse = "e", response = c("e", "prod", "rw", "U"), boot =
      FALSE) -> irf_var_p2
plot(irf_var_p2)



