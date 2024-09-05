## * closing central file

## * stuff to load

## ** libraries

args <- commandArgs(trailingOnly = T)
options(width = 115)

library(jtls)            # custom functions package
library(pmdata)          # private museum data package
library(memoise)         # caching time intensive functions
library(collapse)        # some fast but incomprehensible data transformation framework
library(purrr)           # parallel processing
library(docstring)       # for annotating 
library(ggbeeswarm)      # move points up/down on vrblcvrg plots.. hopefully avoid
library(survival)        # survival/event history models
library(ggsurvfit)       # easy visualizations of survival objects, hopefully avoid
library(countrycode)     # will always be necessary when working with countries
library(muhaz)           # for smooth hazard curves
library(patchwork)       # for stichting plots together, hopefully avoid
library(tinytest)        # for looking at pmdata tests
## library(parallel) # parallel processing
library(furrr)           # parallel processing
library(splines)         # for gp_schoenfeld, maybe neeeded later too for regressions
library(wpp2022)         # population data for Taiwan
data(pop1dt)             # population data for taiwan
library(khroma)          # plotting
library(ggrepel)         # plotting
library(epiR)            # testing instantaneous hazard SE calculations
library(boot)            # bootstrapping for instantaneous hazard SE calculations
library(marginaleffects) # for condmef plots (conditional marginal effects)
library(texreg, include.only = c("screenreg", "texreg")) ## inspection of results
library(Hmisc, include.only = "latexTranslate")  # latexTranslate
library(stringr)         # for str_count, makes MOW naming convention processing easier
library(scales, include.only = c("trans_format", "label_number")) ## for heatmap legend transformation


## LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/")
## LOCS$FIGDIR <- paste0(FIG

## ** functions


## *** nbrs generation
gd_nbrs <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    ## descripti nbrs
    dt_descs_prep1 <- list(
        list(nbr_name = "pmdb_n_all",    nbr_fmt = dt_pmdb[, .N]),
        list(nbr_name = "pmdb_open_atm", nbr_fmt = dt_pmdb[museum_status == "private museum", .N]),
        list(nbr_name = "pm_n_closed",   nbr_fmt = dt_pmyear[, sum(closing)]),
        list(nbr_name = "n_pmyears",     nbr_fmt = dt_pmyear[, .N]),
        list(nbr_name = "pmdb_n_nlpm",   nbr_fmt = dt_pmdb[museum_status == "no longer a private museum", .N]),
        list(nbr_name = "pmdb_n",        nbr_fmt = dt_pmdb[museum_status %in% c("private museum", "closed"), .N]),
        list(nbr_name = "pmx_n",         nbr_fmt = dt_pmx[, .N]),
        list(nbr_name = "n_founder_dead",nbr_fmt = dt_pmyear[founder_dead5 == "recently_dead", fnunique(ID)]),
        list(nbr_name = "pm_n_used",     nbr_fmt = dt_pmyear[, fnunique(ID)]),
        list(nbr_name = "n_pm_close_after_recent_death",
             nbr_fmt = dt_pmyear[founder_dead5 == "recently_dead" & closing == 1, .N]),
        list(nbr_name = "pm_open_from2021",
             nbr_fmt=dt_pmdb[year_opened>= 2021 & museum_status %in% c("private museum", "closed"),.N])) %>%        
        rbindlist
        

    ## get descriptive stats for slfidfcn
    dt_descs_prep2 <- dt_pmyear[, .(ID, slfidfcn)] %>% funique %>% .[, .N, slfidfcn] %>%
        .[, perc := N/sum(N)*100] %>%
        melt(id.vars = "slfidfcn") %>%
        .[, .(nbr_name = sprintf("%s_slfid_%s", variable, slfidfcn),
              nbr_fmt = format(value, digits = 2, nsmall = 0, trim = T))]


    dt_descs <- rbind(dt_descs_prep1, dt_descs_prep2) %>% .[, grp := "descs"]
    

    ## calculate average hazard rate numbers, depending on maximum age

    
    dt_pehaz <- gd_pehaz(dt_pmcpct, cutwidth = 2)[src == "w1"]

    dt_meanhaz <- map(c(2, 4, 5, 8, 10, 20, 30, 40, 100),
                      ~list(upper_bound = .x, mean_haz = dt_pehaz[age < .x, mean(est)])) %>%
        rbindlist %>%
        .[, .(nbr_name = sprintf("meanhaz_upto_%s", upper_bound),
              nbr_fmt = sprintf("%s%%", format(mean_haz*100, digits = 2, nsmall = 2)),
              grp = "meanhaz")]

    dt_meanhaz_later <- data.table(section_id = c("8onwards", "8to30"),
                                   nbr = c(dt_pehaz[age >= 8, mean(est)],
                                           dt_pehaz[age >= 8 & age <= 30, mean(est)])) %>%
        .[, .(nbr_name = paste0("meanhaz_", section_id),
              nbr_fmt = sprintf("%s%%", format(nbr*100, digits = 2, nsmall = 2)),
              grp = "meanhaz")]

    ## median life expectancy assuming constant hazard for first 8 and 8 onwards
    dt_med_life_expect <- data.table(
        nbr_fmt = log((0.5/(1-dt_pehaz[age < 8, mean(est)])^8),
                      base = 1-dt_pehaz[age %between% c(8, 30), mean(est)]) %>%
            round %>% as.character,
        nbr_name = "median_life_expectancy",
        grp = "meanhaz")

    ## hazards by year
    dt_yearhaz <- dt_pmyear[, .(.N, rate_closing = sum(closing)/.N), year]
    
    ## what is good selection of slices?
    ## setting slice lengths, then generating for each slice length all the periods?
    ## but is mess wiht last period (2020+) in most cases
    ## just do some manually

    dt_nbr_yearhaz <- data.table(year_id = c("all","allw", "upto2010", "from2010"),
                                 nbr = c(dt_yearhaz[, mean(rate_closing)],
                                         dt_yearhaz[, weighted.mean(rate_closing, N)],
                                         dt_yearhaz[year < 2010, mean(rate_closing)], 
                                         dt_yearhaz[year >= 2010, mean(rate_closing)])) %>%
        .[, .(nbr_name = paste0("yearhaz_", year_id),
              nbr_fmt = sprintf("%s%%", format(nbr*100, digits = 2, nsmall = 2)),
              grp = "yearhaz")]
    
    
    
    dt_mow_prop_museum <- gn_mow_prop_museum()

    ## ---------- regression coefs ---------

    dt_coefs <- gd_reg_coxph(l_mdls$r_pop4, "r_pop4") %>% chuck("dt_coef")

    ## just export them all
    dt_coefnbrs <- rbind(
        dt_coefs[, .(nbr_name = paste0("coef_", term), # original coefs
                     nbr_fmt = format(coef, digits =2, nsmall = 2))],
        dt_coefs[, .(nbr_name = paste0("pvlu_", term), # pvalues
                     nbr_fmt = format(pvalue, digits = 2, nsmall = 2)), .I][, .(nbr_name, nbr_fmt)],
        dt_coefs[, .(nbr_name = sprintf("coef_%s_exp", term), # exponentiated
                     nbr_fmt = format(exp(coef), digits =2, nsmall = 2))],
        dt_coefs[, .(nbr_name = sprintf("coef_%s_perc", term), # percentage change
                     nbr_fmt = format((exp(abs(coef))-1)*100, digits =2, nsmall = 0)), .I] %>%
        .[, .(nbr_name, nbr_fmt)]) %>%
        .[, grp := "coefs"]
            

    ## add where do coefs of proxcnt10/popm_circle10 become 0?

    ## get threshold values in dt
    dt_thlds <- data.table(
        popm_circle10_marg0 = dt_coefs[term=="proxcnt10", coef]/
            dt_coefs[term=="proxcnt10:popm_circle10", abs(coef)],
        proxcnt10_marg0 = dt_coefs[term=="popm_circle10", coef]/
                              dt_coefs[term=="proxcnt10:popm_circle10", abs(coef)])

    ## prep for printing with melting
    dt_thlds_fmt <- melt(dt_thlds, measure.vars = names(dt_thlds)) %>%
        .[, .(nbr_name = variable, nbr_fmt = sprintf("%.2f", value))]

    ## formt the 
    dt_marg0_fmt <- 
        dt_pmyear[, .(N_below_popm_circle0_marg0 = .SD[popm_circle10 < dt_thlds$popm_circle10_marg0, .N],
                      perc_below_popm_circle10_marg0 = .SD[popm_circle10 < dt_thlds$popm_circle10_marg0, .N]/.N*100,
                      N_below_proxcnt10_marg0 = .SD[proxcnt10 < dt_thlds$proxcnt10_marg0, .N],
                      perc_below_proxcnt10_marg0 = .SD[proxcnt10 < dt_thlds$proxcnt10_marg0, .N]/.N*100)] %>%
        melt(measure.vars = names(.)) %>%
        .[, .(nbr_name = variable, nbr_fmt = format(value, digits = 2, trim = T, nsmall = 0))]

    ## generate summary stats for "zones", the division of the proxcnt/popm space by the coxph reg model

    dt_zonenbrs <- rbind(dt_pmyear %>% copy %>%
          .[proxcnt10 < dt_thlds$proxcnt10_marg0 & popm_circle10 < dt_thlds$popm_circle10_marg0, zone := "BL"] %>%
          .[proxcnt10 > dt_thlds$proxcnt10_marg0 & popm_circle10 < dt_thlds$popm_circle10_marg0, zone := "BR"] %>%
          .[proxcnt10 > dt_thlds$proxcnt10_marg0 & popm_circle10 > dt_thlds$popm_circle10_marg0, zone := "TR"] %>%
          .[proxcnt10 < dt_thlds$proxcnt10_marg0 & popm_circle10 > dt_thlds$popm_circle10_marg0, zone := "TL"] %>%
          .[, .N, zone] %>% .[, zonetype := "quarter"],
          dt_pmyear %>% copy %>% 
          .[popm_circle10 < dt_thlds$popm_circle10_marg0, zone := "bottom"] %>%
          .[popm_circle10 > dt_thlds$popm_circle10_marg0, zone := "top"] %>% 
          .[, .N, zone] %>% .[, zonetype := "half1"],
          dt_pmyear %>% copy %>% 
          .[proxcnt10 > dt_thlds$proxcnt10_marg0, zone := "right"] %>%
          .[proxcnt10 < dt_thlds$proxcnt10_marg0, zone := "left"] %>%
          .[, .N, zone] %>% .[, zonetype := "half2"]) %>%
        .[, perc := (N/sum(N))*100, zonetype] %>% 
        melt(id.vars ="zone", measure.vars = c("N", "perc")) %>%
        .[, nbr_fmt := format(value, digits = 1, nsmall = 0), .I] %>%
        .[, .(nbr_fmt, nbr_name = sprintf("%s_zone%s", variable, zone))]


    dt_coefnbrs2 <- rbindlist(list(dt_thlds_fmt, dt_marg0_fmt, dt_zonenbrs), use.names = T) %>%
        .[, grp := "coefs2"]
    

    ## slopes(l_mdls$r_pop4, variables = "proxcnt10", type = "lp") %>% adt %>%
    ##     .[, .(rowid, estimate, proxcnt10, popm_circle10)] %>%
    ##     .[which.min(abs(estimate)), popm_circle10]
    
        
    ## plots: yanking (insertion) and in-text referencing
    dt_ynkplt <- gc_ynkplt()
    dt_refplt <- gc_refplt()

    ## dt_ynktbl <- gc_ynktbl()
    dt_reftbl <- gc_reftbl()

    dt_nbrs_cbnd <- Reduce(rbind, list(dt_descs, dt_meanhaz, dt_meanhaz_later, dt_med_life_expect,
                                       dt_nbr_yearhaz,
                                       dt_ynkplt, dt_refplt, dt_reftbl,
                                       dt_mow_prop_museum,
                                       dt_coefnbrs, dt_coefnbrs2))

    ## replace ^2 with _sq (org-macros don't like ^2)
    dt_nbrs_cbnd[, nbr_name := gsub("\\^2", "_sq", nbr_name)]
    # also yeet brackets and interaction indicator
    dt_nbrs_cbnd[, nbr_name := gsub("I\\(|\\)", "", nbr_name)] 
    
    
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

## levers: configuration options
c_lvrs <- list(
    dtti = c("af_size"),
    af_vrbls = c("exhbany", "exhbrollany")
    ## dtti = c(""),
    ## af_vrbls = c()
) # only import selected vrbls to not pollute dt_pmyear



## actual pm data
## l_pca_dimred <- gl_pca_dimred(dt_pmdb)
## l_pca_dimred_woclosed <- gl_pca_dimred(dt_pmdb[museum_status != "closed"])

dt_pmx <- gd_pmx(dt_pmdb) # extract of main variables

## generate PC cores, only use open PMs for loading calculation
l_pca_dimred_woclosed <- gl_pca_dimred_closed_imputed(dt_pmdb, dt_pmx)

dt_pmtiv <- gd_pmtiv(dt_pmx, l_pca_dimred_woclosed) # time invariant variables



dt_pmyear_prep <- gd_pmyear_prep(dt_pmx, dt_pmtiv, c_lvrs) # combine all data sources, as complete as possible
dt_pmyear <- gd_pmyear(dt_pmyear_prep, c_lvrs) # trim down dt to no NAs


## ** model running
dt_pmcpct <- gd_pmcpct(dt_pmyear) # time-invariant variables (UoA PM, not pm-year)

l_mdls <- gl_mdls(dt_pmyear, dt_pmcpct) # generate models
## set model names for t_reg_coxph

l_mdlnames_coxph <- list("r_pop4")

l_mdlnames_dens <- list("r_pop4", "r_comp1", "r_pmdens1", "r_pmdens2", "r_pmdens3", "r_pmdens_aud1", "r_pmdens_aud2")

## "r_smol")#, "r_pop5", "r_pop42")#  , ",r_pop4_wyr", "r_pop4_wcrises")
## l_mdlnames_coxph <- c("r_pop4", paste0("r_wsize", 3:1))
## c("r_more", paste0("r_pop", c(1, 3:6)))
## "r_woaf", "r_waf_year", "r_waf_roll", "r_waf_roll2")
## "r_waf_cy", "r_waf_proplog", "r_waf_prop", "r_waf_year_sqrd") 


## compare whether proxcnt/popm_circle10 patterns are stable across data ranges
l_mdlnames_coxph_density <- list("r_pop4", "r_onlycryside", "r_wocryside", "r_pop42") 

## compare whether patterns change if only recent data is used
l_mdlnames_timeslice <- list("r_pop4", "r_2005", "r_2010", "r_2015")

## check whether patterns depend on itme configuration (time period, year_opened, both)
l_mdlnames_timecfg <- list("r_pop4", "r_pop4_wyo", "r_pop4_wtp", "r_pop4_wyotp")

## alternative competition specficication
l_mdlnames_comp <- c("r_pop4", "r_comp1", "r_audience1", "r_audience2", "r_audience_log1", "r_audience_log2")

## combination of density/competition specifications
l_mdlnames_env <- c("r_pop4", "r_pmdens1", "r_pmdens2", "r_audience1", "r_audience_log1")

dt_drop1 <- gd_drop1(l_mdls)

## ** number/figure/table export
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



stop("everything is DONE")

## longitudinal development of variables



## ** additions for later

## add these if people complain about local density specification
## gp_pred_heatmap("r_pop42", l_mdls, dt_pmyear, mortbound_lo = 0.15, mortbound_hi = 0.25)
## ## gp_condmef("r_pop42", l_mdls, dt_pmyear)
## gp_pred_heatmap("r_onlycryside", l_mdls, dt_pmyear, mortbound_lo = 0.15, mortbound_hi = 0.25)
## ## gp_condmef("r_pop42", l_mdls, dt_pmyear)








