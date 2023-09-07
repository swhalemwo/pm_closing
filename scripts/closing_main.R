## * closing main file

args <- commandArgs(trailingOnly = T)
options(width = 110)

detach("package:pmdata", unload = T)

library(pmdata)
library(memoise)
library(collapse)
library(purrr)

LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/",
             MOICACHE = "/home/johannes/memoi_cache_closing/")


DATA_LOCS <- gc_data_locs()
dt_pmdb_excl <- gd_pmdb_excl(only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")] # yeet bad PMs
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)
    

dt_pmdb %>% str

gd_pmdb_excl_splong <- function(dt_pmdb_excl, vrbls_tocheck) {
    ## generate coverage of dt_pmdb_excl (before cleaning) to assess
    ## contribution of cleaning to (lack of) data coverage

    ## actually pretty unneccessary since my cleaning doesn't really change coverage, but meaning:
    ## presence of string is converted into availability (e.g. clctnhldngs)

    ## set up dt with names for comparison
    ## recycle some code from gd_pmdb_excl
    dt_rename_list <- c(list(ID = "ID",
                             country = "country",
                             iso3c = "iso3c",
                             name = "name",
                             year_opened =  "year_opened_str",
                             year_closed = "year_closed_str",
                             museum_status = "museum_status"),
                        gc_rename_list()) %>%     
        map(~vrbl_fndr(dt_pmdb_excl, .x)) %>% 
        imap(~list(vrbl_orig = .x, vrbl_new = .y)) %>% rbindlist
    
    
    ## select relevant vars, rename, fill up empty strings with NA, melt to superlong
    dt_pmdb_excl_splong <- slt(dt_pmdb_excl, dt_rename_list$vrbl_orig) %>%
        frename(setNames(dt_rename_list$vrbl_new, dt_rename_list$vrbl_orig)) %>%
        slt(vrbls_tocheck) %>% # only select the ones to check
        tfmv(vars = names(.), FUN = \(x) replace(x, x=="", NA)) %>%
        melt(id.vars = c("ID", "museum_status"), variable.name = "vrbl")

    return(dt_pmdb_excl_splong)
}

## find non-standardized columns

## get relevant char columns: convert "" to NA
vrbls_relchars <- .c(
    museum_status, iso3c, nationality, gender, ticket_price, opng_time, nbr_visitrs, website, mission, 
    staff_size, buildgtype, slfidfcn, city, clctn_med_fcs, clctn_cry_fcs, clctn_reg_fcs, clctn_modctmp,
    insta_handle)
    

vrbls_tocheck <- c(
    setdiff(num_vars(dt_pmdb, return = "names"), .c(llid)), vrbls_relchars)

## replace empty string with NA, use variables to check
dt_pmdb_splong <- tfmv(dt_pmdb, vars = vrbls_relchars, FUN = \(x) replace(x, x=="", NA)) %>%
    fselect(vrbls_tocheck) %>%
    melt(id.vars = c("ID", "museum_status"), variable.name = "vrbl")


## plot variable coverage by museum type
dt_vrblcvrg <- merge(
    dt_pmdb_splong[, .(all_PMs = sum(!is.na(value))/.N), vrbl],
    dt_pmdb_splong[, .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%  # prop_cpltns by status
    dcast(vrbl ~ museum_status, value.var = "vlus_present"), on = "vrbl") %>%
    .[order(`private museum`)] %>% .[, vrbl := factor(vrbl, levels = vrbl)] %>% 
    melt(id.vars = "vrbl") %>% .[, src := "pmdb"]



p_vrblcvrg <- dt_vrblcvrg %>% 
    ## .[!grepl("^act_", vrbl)] %>%  # exclude activities
    ggplot(aes(x=value, y=vrbl, color = variable)) +
    geom_jitter(width= 0, height = 0.3) +
    theme(legend.position = "bottom")
p_vrblcvrg




## ** combine with dt_pmdb_excl: check how much of the lack of availability is due to incomplete standardization
dt_pmdb_excl_splong <- gd_pmdb_excl_splong(dt_pmdb_excl, vrbls_tocheck)

dt_vrblcvrg_excl <- merge(
    dt_pmdb_excl_splong[, .(all_PMs = sum(!is.na(value))/.N), vrbl],
    dt_pmdb_excl_splong[, .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%
    dcast(vrbl ~ museum_status, value.var = "vlus_present"), on = "vrbl") %>%
    melt(id.vars = "vrbl") %>% .[, src := "pmdb_excl"]

## combine both coverage dts
dt_vrblcvrg_cbnd <- rbind(dt_vrblcvrg, dt_vrblcvrg_excl) %>% 
    .[order(src, variable, value)] %>% # order by private museum completeness (most entities there)
    .[, vrbl := factor(vrbl, levels = unique(vrbl))]

## just focus on where the coverage discrepancy between 
dcast(dt_vrblcvrg_cbnd, vrbl + variable ~ src) %>%
    fmutate(diff = abs(pmdb - pmdb_excl)) %>%
    sbt(diff > 0.02)




    
