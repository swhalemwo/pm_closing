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
debug("gd_pmdb_excl")
undebug("gd_pmdb_excl")

debug("gd_pmdb")
dt_pmdb <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb

wasdf



