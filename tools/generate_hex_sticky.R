#
require(vars2)
data("Canada_tbl")
var_p2 <- VAR(Canada_tbl,2)

var_p2.irf <- irf(var_p2, impulse = "e",
                  response = c("prod", "rw", "U"))
plot(var_p2.irf)

library(hexSticker)
s <- sticker(~plot(var_p2.irf),
             package="vars2a", p_size=20, s_x=1.0, s_y=0.8, s_width=2, s_height=1.0,
             filename="tools/hex_vars2a.png")
