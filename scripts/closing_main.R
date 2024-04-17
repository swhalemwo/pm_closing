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


## LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/")
## LOCS$FIGDIR <- paste0(FIG

## ** functions

## *** config functions

## moved to jtls
## gc_dirs <- function(dir_proj) {
##     list(
##         proj = dir_proj,
##         figs = paste0(dir_proj, "figures/"),
##         tbls = paste0(dir_proj, "tables/"),
##         data = paste0(dir_proj, "data/"),
##         code = paste0(dir_proj, "scripts/"),
##         misc = paste0(dir_proj, "misc/")
##     )
## }



## *** data generation










## *** plotting








## *** tables




gt_coxzph <- function(rx) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    #' generate a table of the cox.zph result (whether hazards are proportional) 

    coxzph_trfms <- list(km = "Kaplan-Meier", identity = "Identity", rank = "Rank")

    ## generate the actual data
    dt_coxzph_prep <- map(names(coxzph_trfms), ~cox.zph(rx, terms = F, transform = .x) %>%
                                                   chuck("table") %>% adt(keep.rownames = "term") %>%
                                                   .[, src := .x]) %>% list_rbind %>%
                      .[, .(term, src, p)]

    
    ## merge with variable labels, cast to wide
    dt_coxzph <- gc_vvs() %>% chuck("dt_termlbls") %>% .[dt_coxzph_prep, on = "term"] %>%
        .[, p_fmtd := fmt_cell(coef = p, pvalue = p,  type = "coef-stars"), .(term, src)] %>%
        dcast(term_lbl + vrblgrp + vrblgrp_lbl ~ src, value.var = "p_fmtd") %>%
        .[order(vrblgrp)]

    
    ## arrange table that in a way that it is to be written to file
    dt_coxzph_viz <- dt_coxzph[, c("term_lbl", names(coxzph_trfms)), with = F] %>%
        cbind(grp_filler = "", .) %>% # add column in beginning for faking group indentation
        .[, term_lbl := latexTranslate(term_lbl)]
            
    ## generate the variable add.to.row components      

    dt_grpstrs <- gc_grpstrs(dt_coxzph, "vrblgrp_lbl", 2) # get the group strings: go to add.to.row

    signote <- gc_signote(se_mention = F, ncol = 3) # get the significance note

    c_colnames <- gc_colnames(col_names = names(dt_coxzph_viz), # generate the column names
                              col_lbls = c(coxzph_trfms, list(grp_filler = "", term_lbl = "Variable")))


    ## add to row cfg
    c_atr <- list(
        pos = c(list(-1, nrow(dt_coxzph)), dt_grpstrs$pos), # pos needs to be a list
        command = c(c_colnames, signote, dt_grpstrs$grpstr))
    

    list(
        dt_fmtd = dt_coxzph_viz,
        align_cfg = c("l", "p{0mm}", "l", rep("D{.}{.}{5}",3)),
        ## hline_after = c(0, nrow(dt_coxzph)),
        hline_after = -1,
        add_to_row = c_atr,
        number_cols = c(rep(F,2), rep(T,3)))

}




gt_sumstats <- function(dt_pmyear, dt_pmcpct) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())

    c_vvs <- gc_vvs()

    ## expand categorical variables to dummies
    ## requires calling model.matrix for each variable

    dt_dummies <- map(chuck(c_vvs, "dt_vrblinfo")[vrbltype == "cat", vrbl],
                      ~model.matrix(as.formula(sprintf("~ %s -1", .x)), dt_pmyear)) %>%
        Reduce(cbind, .) %>% adt %>%
        melt(measure.vars = names(.), variable.name = "term")

    ## get numeric and dummy variables, only get those that are in the table (no squares/interactions)
    vrbls_num <- intersect(names(dt_pmyear), chuck(c_vvs, "dt_vrblinfo")[vrbltype %in% c("num", "bin"), vrbl])
    dt_numvrbls <- dt_pmyear[, vrbls_num, with = F] %>% melt(measure.vars = names(.), variable.name = "term")

    ## summary functions to apply
    funcs_sumry <- .c(mean, sd, min, max)

    ## apply summary functions, merge with term labels to get variables names and groupings
    dt_cbn <- rbind(dt_dummies, dt_numvrbls) %>%
        .[, map(funcs_sumry, ~get(.x)(value, na.rm = T)), term] %>%
        setnames(old = paste0("V", 1:len(funcs_sumry)), new = funcs_sumry) %>%
        c_vvs$dt_termlbls[., on = "term"]
        
    ## FIXME: sum needs to be generalized when I have non-varying numerical variables (e.g. size)
    ## there sum does'nt make sense but mean does


    ## get variables that I want on PM-level
    ## have to make as character to remove factors
    vrbls_pm <- c(c_vvs$dt_vrblinfo[vrbl_tv == 0, as.character(vrbl)], "founder_dead")
    
    
    ## expand each variable separately, otherwise model.matrix drops reference categories
    dt_sumry_pm <- map(vrbls_pm, ~model.matrix(as.formula(sprintf("~ %s -1", .x)), dt_pmcpct)) %>%
        Reduce(cbind, .) %>% adt %>%
        melt(measure.vars = names(.), variable.name = "term") %>%
        .[, .(pm_mean = mean(value), pm_sum = sum(value)), term]
                     
    ## combine pm-year-level and pm-level data, reorder
    dt_cbn2 <- copy(dt_cbn)[dt_sumry_pm, `:=`("pm_mean" = i.pm_mean, "pm_sum" =  i.pm_sum), on = "term"] %>%
        .[, vrbl := factor(vrbl, levels = levels(c_vvs$dt_vrblinfo$vrbl))] %>% # somehow necessary to relevel? 
        .[order(vrblgrp, vrbl)]
    
    ## create summary column names
    sumstats_cols <- c("grp_filler" = "", "term_lbl" = "Variable", "pm_sum" = "Count", "pm_mean" = "Mean",
                       "mean" = "Mean", "sd" = "SD", min = "Min.", max = "Max.")
    
    ## format the columns
    dt_cbn_viz <- copy(dt_cbn2) %>%
        .[, min_fmtd := format(min, digits = 2, scientific = F, trim = F), .I] %>% # row-wise min for small mins
        .[, max_fmtd := format(max, digits = 2, scientific = F, trim = F,
                               nsmall = fifelse(max %% 1 == 0, 0, 2)), .I] %>% # non-Ints: two decimal places
        .[, .(grp_filler = "", term_lbl, pm_sum = as.character(pm_sum),
              pm_mean = format(pm_mean, digits=2, trim = T),
              mean = format(mean, digits = 2, trim = F),
              sd = format(sd, digits = 2, trim = F),
              min = min_fmtd,
              max = max_fmtd
              ## max = format(max, digits = 2, trim = F)
              )] %>%        
        recode_char("NA" = NA) # recode "NA"-strings to actual NA (format() can't deal with it in numeric)

    
    ## spanner for grouping museum-level and PM-year-level variables
    top_spanner <- paste0("\\hline\n & & \\multicolumn{2}{c}{Museum} & \\multicolumn{4}{c}{Museum-year} \\\\ \n",
                          "\\cmidrule(r){3-4}\\cmidrule(r){5-8}")

    ## generate the other table elements: groupstrings and column names
    dt_grpstrs <- gc_grpstrs(dt_cbn2, grp ="vrblgrp_lbl", nbr_cols = ncol(dt_cbn_viz))
    c_colnames <- gc_colnames(col_names = names(sumstats_cols),
                              col_lbls = sumstats_cols, hline_above = F)
    c_atr <- list(
        pos = c(list(-1, -1), dt_grpstrs$pos),
        command = c(top_spanner, c_colnames, dt_grpstrs$grpstr))
                
    list(dt_fmtd = dt_cbn_viz,
         align_cfg = c(rep("l",3), rep("r", 6)),
         hline_after = c(-1, nrow(dt_cbn_viz)),
         add_to_row = c_atr,
         number_cols = c(rep(F, 2), rep(T, 6)))
    
    
    ## pm_density doesn't really make sense on museum-level
    ## funnily enough, founder_dead does make sense (number of founders who have died in total),
    ## even tho both are time-varying...

    ## would also make sense to have west/mow/muem_fndr name -> not about including categorical variables,
    ## but excluding time-varying
    

}

## gt_sumstats(dt_pmyear)





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


source(paste0(c_dirs$code, "pm_dimred.R")) 
source(paste0(c_dirs$code, "regression.R"))
source(paste0(c_dirs$code, "cfg.R"))



dt_pmdb_excl <- gd_pmdb_excl(only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")] # yeet bad PMs
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)


## variable coverage
source(paste0(c_dirs$code, "vrblcvrg.R"))
l_vrblcvrg <- gl_vrblcvrg(dt_pmdb)

if ("memoised" %!in% class(gd_mow_info)) {
    gd_mow_info <- memoise(gd_mow_info) # memoizing gd_mow_info: saves the fread of 55k file
}

END_YEAR <- 2021



## actual pm data
## l_pca_dimred <- gl_pca_dimred(dt_pmdb)
## l_pca_dimred_woclosed <- gl_pca_dimred(dt_pmdb[museum_status != "closed"])

dt_pmx <- gd_pmx(dt_pmdb) # extract of main variables

## generate PC cores, only use open PMs for loading calculation
l_pca_dimred_woclosed <- gl_pca_dimred_closed_imputed(dt_pmdb, dt_pmx)

dt_pmtiv <- gd_pmtiv(dt_pmx, l_pca_dimred_woclosed) # time invariant variables


dt_pmyear_prep <- gd_pmyear_prep(dt_pmx, dt_pmtiv) # combine all data sources, as complete as possible
dt_pmyear <- gd_pmyear(dt_pmyear_prep) # trim down dt to no NAs


dt_pmcpct <- gd_pmcpct(dt_pmyear) # time-invariant variables (UoA PM, not pm-year)

l_mdls <- gl_mdls(dt_pmyear, dt_pmcpct) # generate models
                                        # set model names for t_reg_coxph
l_mdlnames_coxph <- c("r_pop4", "r_pop4_wyr")
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

gp_lngtdvelp <- function(dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    ## categorical and binary variables: proportion
    dt_vrblinfo <- gc_vvs() %>% chuck("dt_vrblinfo")
    
    dt_cat <- dt_pmyear[, .SD, .SDcols = c("ID", "year", dt_vrblinfo[vrbltype %in% c("cat","bin"),
                                                                     achr(funique(vrbl))])] %>%
        melt(id.vars = c("ID", "year"), variable.name = "vrbl") %>%
        .[, .N, .(year, vrbl, value)] %>%
        .[, value_y := N/sum(N), .(year, vrbl)] %>%
        .[, N := NULL]

    ## numeric variables: mean
    dt_num <- dt_pmyear[, .SD, .SDcols = c("ID", "year", "closing",
                                           dt_vrblinfo[vrbltype == "num" & !grepl("I\\(|:", vrbl),
                                                       achr(funique(vrbl))])] %>%
        .[, cnt := .N, year] %>% # set up count (gets meaned)
        .[, will_close := fifelse(any(closing == 1), 1, 0), ID] %>% # whether a museum will close
        melt(id.vars = c("ID", "year"), variable.name = "vrbl") %>%
        .[, .(value_y = mean(value)), .(year, vrbl)] %>%
        .[, `:=`(value = vrbl)] %>%
        .[vrbl != "year"] # don't need avg year, is just year

    dt_viz <- rbind(dt_cat, dt_num)
        
    ## set colors based on variable value
    dt_color <- dt_viz[, .(vrbl, value)] %>% funique %>%
        .[, lnbr := 1:.N, vrbl]
    

    dt_viz_colored <- join(dt_viz, dt_color, on = c("vrbl", "value"))
    
    ## add labels for lines of categorical variables at end of line
    dt_lbls <- dt_viz_colored[dt_vrblinfo[vrbltype %in% c("cat", "bin"), .(vrbl)], on = "vrbl"] %>%
        .[, .SD[which.max(year)], .(vrbl, value)]
    
    ## library(ggrepel)
    ggplot(dt_viz_colored, aes(x=year, y=value_y, group = value, color = factor(lnbr))) +
        geom_line(show.legend = F) +
        facet_wrap(~vrbl, scales = "free") +
        geom_text_repel(dt_lbls, mapping = aes(label = value), hjust = 0, direction = "y", show.legend = F) +
        coord_cartesian(xlim = c(1990, 2035))
                                                         
    
}





