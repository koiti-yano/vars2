# Convert Canada to canada_tbl
require(vars2)
#require(tidyr)
#require(tsbox)
#require(tidyverse)
data(Canada)

tsbox::ts_tbl(Canada) -> canada_tidy
#canada_tidy %>% view()

canada_tidy %>% tidyr::pivot_wider(names_from = id,
                            values_from=value) -> Canada_tbl
View(Canada_tbl)