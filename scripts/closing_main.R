## * closing central file

## * stuff to load

## ** libraries

args <- commandArgs(trailingOnly = T)
options(width = 115)

library(jtls)        # custom functions package
library(pmdata)      # private museum data package
library(memoise)     # caching time intensive functions
library(collapse)    # some fast but incomprehensible data transformation framework
library(purrr)       # parallel processing
library(docstring)   # for annotating 
library(ggbeeswarm)  # move points up/down on vrblcvrg plots.. hopefully avoid
library(survival)    # survival/event history models
library(ggsurvfit)   # easy visualizations of survival objects, hopefully avoid
library(countrycode) # will always be necessary when working with countries
library(muhaz)       # for smooth hazard curves
library(patchwork)   # for stichting plots together, hopefully avoid
library(tinytest)    # for looking at pmdata tests
## library(parallel) # parallel processing
library(furrr)       # parallel processing
library(splines)     # for gp_schoenfeld, maybe neeeded later too for regressions
library(texreg, include.only = c("screenreg", "texreg")) ## inspection of results
library(Hmisc, include.only = "latexTranslate")  # latexTranslate
library(wpp2022)     # population data for Taiwan
data(pop1dt) # population data for taiwan
library(khroma)      # plotting
library(ggrepel)     # plotting


## LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/")
## LOCS$FIGDIR <- paste0(FIG

## ** functions


## *** nbrs generation
gd_nbrs <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    dt_descs <-
        list(nbr_name = "pmdb_n_nlpm",
             nbr_fmt = dt_pmdb[museum_status == "no longer a private museum", .N],
             grp = "descs") %>% adt

    ## calculate average hazard rate numbers, depending on maximum age
    dt_pehaz <- gd_pehaz(dt_pmcpct, cutwidth = 2)
    dt_meanhaz <- map(c(20, 30, 40, 100), ~list(upper_bound = .x, mean_haz = dt_pehaz[cuts <= .x, mean(haz)])) %>%
        rbindlist %>%
        .[, .(nbr_name = sprintf("meanhaz_upto_%s", upper_bound),
              nbr_fmt = sprintf("%s%%", format(mean_haz*100, digits = 2, nsmall = 2)),
              grp = "meanhaz")]

    
        
    ## plots: yanking (insertion) and in-text referencing
    dt_ynkplt <- gc_ynkplt()
    dt_refplt <- gc_refplt()

    ## dt_ynktbl <- gc_ynktbl()
    dt_reftbl <- gc_reftbl()

    dt_nbrs_cbnd <- Reduce(rbind, list(dt_descs, dt_meanhaz, dt_ynkplt, dt_refplt, dt_reftbl))

    
    return(dt_nbrs_cbnd)
}




## gl_dts <- function(dt_pmdb) {
##     #' absolutely cursed idea to just collect all the dts in one block
       #' the callgraph tec isn't there yet sadly
##     gw_fargs(match.call())
    
##     dt_pmx <- gd_pmx(dt_pmdb)
##     dt_pmtiv <- gd_pmtiv(dt_pmx)

##     dt_pmyear <- gd_pmyear(dt_pmx, dt_pmtiv)
##     dt_pmcpct <- gd_pmcpct(dt_pmyear)

##     l_dts <- list(
##         dt_pmx =     dt_pmx, 
##         dt_pmtiv=    dt_pmtiv,                              
##         dt_pmyear=   dt_pmyear,
##         dt_pmcpct =  dt_pmcpct)

##     attr(l_dts, "gnrtdby") <- as.character(match.call()[[1]])
##     return(l_dts)
## }

## l_dts <- gl_dts(dt_pmdb)





## * main
if (interactive()) {stop("it's interactive time")}





## set up constants used for object management
c_dirs <- gc_dirs(dir_proj = "/home/johannes/Dropbox/phd/papers/closing/") ## project dirs
PMDATA_LOCS <- gc_pmdata_locs() # pmdata source
l_plts <- list() # list of plots
c_pltargs <- list() # arguments to pass to gc_plts
l_tbls <- list() # list of tables 
c_tblargs <- list() # arguments to pass to gc_tbls
system(sprintf("rm %s", paste0(c_dirs$misc, "farg_calls.csv")))


source(paste0(c_dirs$code, "pm_dimred.R"))  # dimensionality reduction: generates PC
source(paste0(c_dirs$code, "regression.R")) # all kind of data prep/regression
source(paste0(c_dirs$code, "cfg.R")) # config functions
source(paste0(c_dirs$code, "descriptives.R")) # descriptive tables/plots



dt_pmdb_excl <- gd_pmdb_excl(only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")] # yeet bad PMs
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)


## variable coverage
source(paste0(c_dirs$code, "vrblcvrg.R"))
l_vrblcvrg <- gl_vrblcvrg(dt_pmdb)

if ("memoised" %!in% class(gd_mow_info)) {
    gd_mow_info <- memoise(gd_mow_info) # memoizing gd_mow_info: saves the fread of 55k file
}

START_YEAR <- 2000
END_YEAR <- 2021


c_lvrs <- list(
    dtti = c("af_size"),
    ## dtti = c(""),
    af_vrbls = c("exhbany", "exhbrollany")) # only import selected vrbls to not pollute dt_pmyear
    
    

## actual pm data
## l_pca_dimred <- gl_pca_dimred(dt_pmdb)
## l_pca_dimred_woclosed <- gl_pca_dimred(dt_pmdb[museum_status != "closed"])

dt_pmx <- gd_pmx(dt_pmdb) # extract of main variables

## generate PC cores, only use open PMs for loading calculation
l_pca_dimred_woclosed <- gl_pca_dimred_closed_imputed(dt_pmdb, dt_pmx)

dt_pmtiv <- gd_pmtiv(dt_pmx, l_pca_dimred_woclosed) # time invariant variables



dt_pmyear_prep <- gd_pmyear_prep(dt_pmx, dt_pmtiv, c_lvrs) # combine all data sources, as complete as possible
dt_pmyear <- gd_pmyear(dt_pmyear_prep, c_lvrs) # trim down dt to no NAs



dt_pmcpct <- gd_pmcpct(dt_pmyear) # time-invariant variables (UoA PM, not pm-year)

l_mdls <- gl_mdls(dt_pmyear, dt_pmcpct) # generate models
                                        # set model names for t_reg_coxph
l_mdlnames_coxph <- c("r_pop4")# , "r_pop4_wyr")
## l_mdlnames_coxph <- c("r_pop4", paste0("r_wsize", 3:1))
## c("r_more", paste0("r_pop", c(1, 3:6)))
## "r_woaf", "r_waf_year", "r_waf_roll", "r_waf_roll2")
## "r_waf_cy", "r_waf_proplog", "r_waf_prop", "r_waf_year_sqrd") 



screenreg2(list(l_mdls$r_pop4)) # just smoe display

## screenreg2(list(l_mdls$r_west_cpct, l_mdls$r_west_year, l_mdls$r_west_year2), digits = 4)


## write numbers
dt_nbrs <- gd_nbrs()
fwrite(dt_nbrs, paste0(c_dirs$misc, "nbrs.csv"), quote = F)

## generate plots/tables and write them to file
## plan(multicore, workers = 4)
plan(sequential)
future_walk(names(gc_plts()), ~lapply(c(gplt, wplt), \(f) f(.x)))
future_walk(names(gc_tbls()), ~lapply(c(gtbl, wtbl), \(f) f(.x)))
plan(sequential)


## callgraph testing
jtls::gwd_clgrph()
## dpltF("callgraph2")

## ** table testing 


## texreg(l_mdls$r_more, single.row = T, file = paste0(c_dirs$tbls, "r_more.tex"), label = "tbl:t_r_more")

gp_coxphdiag(l_mdls$r_pop4)

check_if_file_is_open(paste0(c_dirs$tbls, "t_testtable.pdf"))


screenreg(l_mdls$r_pop4)
## ejj <- test_package("pmdata")

## test_all("~/Dropbox/phd/pmdata")

## build_install_test("~/Dropbox/phd/pmdata/", at_home = F)

## build_install_test("~/home/johannes/R/x86_64-pc-linux-gnu-library/4.3/pmdata/")



## gt_reg_coxph(l_mdls, )

    
gtbl("t_reg_coxph")
wtbl("t_reg_coxph")
dtblF("t_reg_coxph_wcpF")
## wtbl_pdf("t_reg_coxph_wcpF", F)
## hmm question becomes whether I should use multiple groups

gdtbl("t_reg_coxph")
## gd_reg_coxph(l_mdls$r_more, "r_more")


print("everything is DONE")

## longitudinal development of variables





