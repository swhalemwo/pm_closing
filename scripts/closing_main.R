## * closing main file

args <- commandArgs(trailingOnly = T)
options(width = 110)

detach("package:pmdata", unload = T)
library(pmdata)
library(memoise)
library(collapse)

LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/",
             MOICACHE = "/home/johannes/memoi_cache_closing/")


DATA_LOCS <- gc_data_locs()
dt_pmdb_excl <- gd_pmdb_excl(only_pms = F)
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)

dt_pmdb

## find non-standardized columns




dt_pmdb[, .(endowment, gvtsupport, donorprogram)] %>% print(n=20)






# dt_pmdb[, .N, `Floor size` == ""]
# only  166 have floor size

dt_pmdb[, .N, staff_size == ""]

dt_pmdb
