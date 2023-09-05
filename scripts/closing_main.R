## * closing main file

args <- commandArgs(trailingOnly = T)
options(width = 110)

detach("package:pmdata", unload = T)
library(pmdata)
library(memoise)

LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/",
             MOICACHE = "/home/johannes/memoi_cache_closing/")


DATA_LOCS <- gc_data_locs()


gd_pmdb_excl(only_pms = F)

dt_pmdb <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb(verbose = T)

dt_pmdb[!is.na(Activities), .(ID, name, Activities, `Educational / outreach / social / artistic programs`)]

dt_pmdb[, .(name, museum_status, `Indoor facilities`)] %>% print(n=2000)

# dt_pmdb[, .N, `Floor size` == ""]
# only  166 have floor size

dt_pmdb[, .N, staff_size == ""]
