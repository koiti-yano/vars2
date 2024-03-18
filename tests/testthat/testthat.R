# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

#library(testthat)
library(vars2)

#test_check("vars2")


data("Canada")
var_p2 <- VAR(Canada,2)

# boot=TRUE
var_p2_irf_boot <- irf(var_p2)
ggplot(var_p2_irf_boot)
