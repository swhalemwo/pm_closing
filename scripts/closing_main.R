## * closing central file

## * stuff to load

## ** libraries

args <- commandArgs(trailingOnly = T)
options(width = 115)

library(jtls)
library(pmdata)
library(memoise)
library(collapse)
library(purrr)
library(docstring) # for annotating 
library(ggbeeswarm) # move points up/down on vrblcvrg plots.. hopefully avoid
library(survival) 
library(ggsurvfit) # easy visualizations of survival objects, hopefully avoid
library(countrycode) 
library(muhaz) # for smooth hazard curves
library(patchwork) # for stichting plots together, hopefully avoid
library(texreg, include.only = c("screenreg", "texreg")) ## inspection of results
library(tinytest) # for looking at pmdata tests
## library(parallel) # parallel processing
library(furrr) # parallel processing
library(splines) # for gp_schoenfeld, maybe neeeded later too for regressions
library(Hmisc, include.only = "latexTranslate") # latexTranslate

## LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/")
## LOCS$FIGDIR <- paste0(FIG

## ** functions

## *** config functions

gc_dirs <- function(dir_proj) {
    list(
        proj = dir_proj,
        figs = paste0(dir_proj, "figures/"),
        tbls = paste0(dir_proj, "tables/"),
        data = paste0(dir_proj, "data/"),
        code = paste0(dir_proj, "scripts/"),
        misc = paste0(dir_proj, "misc/")
    )
}

gc_vvs <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    #' generate variable vectors/data.tables with all kind of information
    #' vrbls_base: vector of variables necessary for survival analysis
    #' vrbls_tiv: vector of substantial time-invariant variables
    #' vrbls_tv: vector of substantial time-varying variables
    ## FIXME: see if I can integrate these vectors with dt_vrblinfo
    #' dt_vrblinfo: data.table with vrbl (corresponding to columns in dt_pmyear/dt_pmcpct etc), vrbl_lbl (label),
    #'   vrblgrp (variable group; some thematic substantial grouping to aid interpretation),
    #'   vrblgrp_lbl (label of variable group)
    #' dt_ctgterm_lbls: data.table with info of terms (values) of categorical variables: vrbl,
    #' term (the particular value), term_lbl. 
    #' dt_cof_cfg: data.table with info of goodness-of-fit stats: gof_name, digits (for rounding), gof_lbl (label)
    #' dt_termlbls: EXPERIMENTAL combination of dt_vrblinfo and dt_ctgterm_lbls:
    #' should save merging the two dts in each function that needs both vrblinfo and lbls;
    #' for categorical variables contains both the information on vrbls and term (e.g. term=gender and term=genderF)
    #' so far it works that by right-joining on dt_termlbls (dt_termlbls[dt_coef]) substantive coef tables can pick 
    #' the things they need, and ignore those that they don't include
    #' vrbl, term, term_lbl, vrblgrp, vrblgrp_lbl

    #' ADDME: atm it's not super flexible with multiple variable groupings (e.g. different themes),
    #' or different GOF for different models

    

    ## group variables thematically, add also labels

    l_vrbl_lbls <- list( # variable labels
        gender = "Founder Gender",
        founder_dead = "Founder died",
        slfidfcn = "Self- Identification",
        muem_fndr_name = "Founder name in Museum name",
        mow = "MOW inclusion",
        pm_dens = "PM density",
        "I(pm_dens^2)" = "PM density^2",
        west = "Europe and North America",
        reg6 = "Region",
        GLOBAL = "Global") # from cox.zph
    
    l_vrblgrps <- list(# variable groups
        founder = .c(gender, founder_dead),
        museum = .c(slfidfcn, muem_fndr_name, mow),
        envir = .c(pm_dens, "I(pm_dens^2)", west, reg6),
        misc = .c(GLOBAL)
    )

    l_vrblgrp_lbls <- list(# variable group labels
        founder = "Founder",
        museum = "Museum",
        envir = "Environment",
        misc = "Miscellaneous")
    
    
    dt_vrbl_lbls = data.table(vrbl = names(l_vrbl_lbls), vrbl_lbl = unlist(l_vrbl_lbls))

    dt_vrblgrp_lbls <- data.table(vrblgrp = names(l_vrblgrp_lbls), vrblgrp_lbl = unlist(l_vrblgrp_lbls)) %>%
        .[, vrblgrp := factor(vrblgrp, levels = names(l_vrblgrps))]

    dt_vrblgrps <- imap(l_vrblgrps, ~data.table(vrbl = .x, vrblgrp = .y)) %>% rbindlist %>%
        .[, vrblgrp := factor(vrblgrp, levels = names(l_vrblgrps))]

    l_ctgterm_lbls <- list(# labels of terms of categorical variables
        list(vrbl = "gender",   term = "genderF",               term_lbl = "Gender - Female"),
        list(vrbl = "gender",   term = "gendercouple",          term_lbl = "Gender - Couple"),
        list(vrbl = "gender",   term = "genderM",               term_lbl = "Gender - Male"),        
        list(vrbl = "slfidfcn", term = "slfidfcnmuseum",        term_lbl = "Self-Identification - Museum"),
        list(vrbl = "slfidfcn", term = "slfidfcnfoundation",    term_lbl = "Self-Identification - Foundation"),
        list(vrbl = "slfidfcn", term = "slfidfcncollection",    term_lbl = "Self-Identification - Collection"),
        list(vrbl = "slfidfcn", term = "slfidfcnother",         term_lbl = "Self-Identification - Other"))

    dt_ctgterm_lbls <- rbindlist(l_ctgterm_lbls) %>%
        .[, term := factor(term, levels = term)]

    ## FIXME: write test that the variable of every term is defined in dt_vrbl_lbls

    
    ## information for gofs
    c_gof <- list(
        list(gof_name = "nobs",   digits = 0, gof_lbl = "Museum-years"),
        list(gof_name = "nevent", digits = 0, gof_lbl = "Closures"),
        list(gof_name = "logLik", digits = 2, gof_lbl = "log. Likelihood"),
        list(gof_name = "AIC",    digits = 2, gof_lbl = "AIC"),
        list(gof_name = "BIC",    digits = 2, gof_lbl = "BIC"))

    dt_gof_cfg <- rbindlist(c_gof) %>%
        .[, gof_name := factor(gof_name, levels = gof_name)]


    

    ## l_mdl_lbls <- list()
        


    ## check that the variables that are grouped/labelled are the same
    if (!setequal(dt_vrbl_lbls$vrbl, dt_vrblgrps$vrbl)) {
        stop("something wrong with vrbl labels and groups")}

    dt_vrblinfo <- join(dt_vrbl_lbls, dt_vrblgrps, on = "vrbl") %>%
        join(dt_vrblgrp_lbls, on = "vrblgrp")

    
    ## generate dt_term_lbls: get labels for all terms (still includes variables)
    dt_termlbls <- copy(dt_ctgterm_lbls)[dt_vrblinfo,
                                         `:=`("vrblgrp" = i.vrblgrp, "vrblgrp_lbl" = i.vrblgrp_lbl),
                                         on = "vrbl"] %>% 
        rbind(dt_vrblinfo[, .(term = vrbl, vrbl, term_lbl = vrbl_lbl, vrblgrp, vrblgrp_lbl)])    


    list(
        ## time-invariant variables
        vrbls_base = .c(ID, iso3c, year, tstart, tstop, age),
        vrbls_tiv = .c(gender, slfidfcn, muem_fndr_name, mow, west, reg6),
        vrbls_tv= .c(pm_density, founder_dead),
        dt_vrblinfo = dt_vrblinfo,
        dt_ctgterm_lbls = dt_ctgterm_lbls,
        dt_gof_cfg = dt_gof_cfg,
        dt_termlbls = dt_termlbls)
        

    
}


        
gc_plts <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    l_pltcfgs <- list(
        p_vrblcvrg_ugrpd = list(
            dt_vrblcvrg = quote(l_vrblcvrg$dt_vrblcvrg_all),
            yeet_acts = F,
            caption = "PMDB variable coverage by museum status",
            width = 19,
            height = 24),
        p_vrblcvrg = list(
            dt_vrblcvrg = quote(l_vrblcvrg$dt_vrblcvrg_all),
            yeet_acts = F,
            caption = "PMDB variable coverage by museum status and variable group",
            width = 19,
            height = 24),
        p_vrblcvrg_ratio = list(
            dt_vrblcvrg = quote(l_vrblcvrg$dt_vrblcvrg_fcs),
            caption = "PMDB variable coverage (abs/rel prop) by museum status and variable group",
            width = 18,
            height = 24),
        p_surv = list(
            dt_pmcpct = quote(dt_pmcpct),
            caption = "Private Museum Survival probability",
            width = 14,
            height = 8),
        p_hazard = list(
            dt_pmcpct = quote(dt_pmcpct),
            caption = "Private Museum hazard function",
            cutwidth = 2,
            bw.smooth = 5,
            width = 16,
            height = 9),
        p_agedens = list(
            dt_pmcpct = quote(dt_pmcpct),
            caption = "Age distribution of private museums",
            width = 17,
            height = 16),
        p_yeardens = list(
            dt_pmyear = quote(dt_pmyear),
            caption = "Private Museum-year Distribution",
            width = 19,
            height = 12),
        p_coxphdiag_more = list(
            rx = quote(l_mdls$r_more),
            caption = "Model Diagnostics",
            width = 19,
            height = 25
            )        
    )

    ## check that there are no duplicate 
    if (any_duplicated(names(l_pltcfgs))) stop("duplicated names")

    ## check that all entries have width, heigh, caption
    if (!all(unlist(map(l_pltcfgs, ~all(c("width", "height", "caption") %in% names(.x)))))) {
        stop("not all configs have width, height and caption")}

    
    return(l_pltcfgs)
        
}

gc_pmdb_vrblgrps <- function(dt_pmdb) {
    #' generate list of thematic variable groups of PMDB vrbls,
    

    gw_fargs(match.call())
    
    l_vrblgrps <- list(
        sm = .c(insta_handle, insta_flwrs, insta_posts, fb_flwrs, fb_likes, google_rating, google_nbrrvws,
                trpadvsr_rating, trpadvsr_nbrrvws, twitter_flwrs, insta_bluetick, youtube_flwrs),
        founder = .c(gender, birthyear, deathyear, founder_gvrnc, an_nyears, an_lyear, an_fyear, founder_wealth,
                     nationality, industry, founder_name, founder_weal_ustd),
        clctn = .c(clctn_gnr_fcs, realism, clctn_modctmp, clctn_reg_fcs, avbl_clctnhldngs,
                   clctn_med_fcs, clctn_size,
                   clctn_med_fcs_nms, clctn_cry_fcs),
        identity = .c(mission, avbl_legalstruct, slfidfcn, muem_fndr_name, foundation, avbl_gvrncstruct, 
                      staff_diversity),
        relations = .c(gvtsupport, donorprogram, endowment, sponsorship, cooperation),
        operations = c(keep(names(dt_pmdb), ~grepl("^act_", .x)), ## all the activities
                       .c(cafe_restrnt, avbl_floorsize, avbl_exhibsize, museumshop, buildgtype, website,
                          reducedtickets, staff_size, rentalpossblt, webshop, nbr_visitrs, ticket_price,
                          opng_time, temp_exhibs, avbl_exhibhist, architect)),
        existence = .c(city, iso3c, multiplelocs, year_opened, year_closed), #
        technical = .c(ID, name, museum_status, llid, origin)
    )

    dt_vrblgrps <- imap(l_vrblgrps, ~data.table(grp = .y, vrbl = .x)) %>% rbindlist
    if (len(setdiff( names(dt_pmdb), dt_vrblgrps$vrbl)) >0) {
        stop("vrbls has typos, or not all variables are grouped")
    }
    ## print(setdiff(names(dt_pmdb), dt_vrblgrps$vrbl))

    attr(dt_vrblgrps, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_vrblgrps)
}

## *** data generation

gd_vrblcvrg <- function(dt_pmdb_splong, all_statuses) {
    gw_fargs(match.call())
    
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate dataframe with coverage of variables,
    #' depending all_statuses for all pmdb_status values (open, closed, no longer PM) as well as overall average
    #' if !all_statuses: only for open and closed

    if (all_statuses) {

        ## variable coverage for open, closed, no_longer, and overall
        dt_vrblcvrg <- merge(
            dt_pmdb_splong[, .(all_PMs = sum(!is.na(value))/.N), vrbl], # overall cpltns
            ## prop_cpltns by status
            dt_pmdb_splong[, .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%
            dcast(vrbl ~ museum_status, value.var = "vlus_present"), on = "vrbl") %>%
            .[order(`private museum`)] %>% .[, vrbl := factor(vrbl, levels = vrbl)] %>% 
            melt(id.vars = "vrbl", variable.name = "museum_status") %>% .[, src := "pmdb"]

    } else if (!all_statuses) {
        
        ## only proportions of open and closed
        dt_vrblcvrg <- dt_pmdb_splong[museum_status %in% c("private museum", "closed"),
                                      .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%
            dcast(vrbl ~ museum_status, value.var = "vlus_present") %>%
            .[order(`private museum`)] %>% .[, vrbl := factor(vrbl, levels = vrbl)] %>% 
            melt(id.vars = "vrbl", variable.name = "museum_status") %>% .[, src := "pmdb"]
    }

    ## no good reason to maintain vrbl coverage which isn't grouped
    ## if I don't wanna use it in plot, can just not use it, but always should have option
    ## move the variable grouping step here as well
    dt_vrblgrps <- gc_pmdb_vrblgrps(dt_pmdb)

    dt_vrblcvrg_grpd <- dt_vrblgrps[dt_vrblcvrg, on = "vrbl"] %>%
        .[, vrbl := factor(vrbl, levels = levels(dt_vrblcvrg$vrbl))]

    attr(dt_vrblcvrg_grpd, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_vrblcvrg_grpd)

}

## check which variables are not considered in dt_vrblgrps
## are variables that are grouped (all pmbd variables are), but not used (e.g. technical)
## setdiff(gc_pmdb_vrblgrps(dt_pmdb)$vrbl, dt_vrblcvrg_all$vrbl)





gd_pmdb_splong <- function(dt_pmdb) {
    gw_fargs(match.call())
    #' generate super long PM dt, ;
    ## FIXME: this func now includes a bunch of configuration (vrbls_relchars)
    ## which is probably better as external argument/config

    ## get relevant char columns: convert "" to NA
    vrbls_relchars <- .c(
        museum_status, iso3c, nationality, gender, ticket_price, opng_time, nbr_visitrs, website, mission, 
        staff_size, buildgtype, slfidfcn, city, clctn_med_fcs, clctn_cry_fcs, clctn_reg_fcs, clctn_modctmp,
        insta_handle, architect, industry)
    
    ## set variables to check: all numeric vrbls, selected char vrbls, yeet llid
    vrbls_tocheck <- c(
        setdiff(num_vars(dt_pmdb, return = "names"), .c(llid)), vrbls_relchars)

    ## setdiff(names(dt_pmdb), vrbls_tocheck)

    ## replace empty string with NA, use variables to check
    dt_pmdb_splong <- tfmv(dt_pmdb, vars = vrbls_relchars, FUN = \(x) replace(x, x=="", NA)) %>%
        fselect(vrbls_tocheck) %>%
        melt(id.vars = c("ID", "museum_status"), variable.name = "vrbl")

    attr(dt_pmdb_splong, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmdb_splong)
}


gl_vrblcvrg <- function(dt_pmdb) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())

    #' bundle all the variable coverage objects in one list for more comfy handling
    
    dt_pmdb_splong <- gd_pmdb_splong(dt_pmdb)

    dt_vrblcvrg_all <- gd_vrblcvrg(dt_pmdb_splong, all_statuses = T)
    dt_vrblcvrg_fcs <- gd_vrblcvrg(dt_pmdb_splong, all_statuses = F)

    l_vrblcvrg <- list(dt_pmdb_splong = dt_pmdb_splong,
                       dt_vrblcvrg_all =dt_vrblcvrg_all,
                       dt_vrblcvrg_fcs = dt_vrblcvrg_fcs)

    attr(l_vrblcvrg, "gnrtdby") <- as.character(match.call()[[1]])

    return(l_vrblcvrg)
    
}



## *** plotting



gp_vrblcvrg_ugrpd <- function(dt_vrblcvrg, yeet_acts) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## mtcars %>% adt %>% 
    ##     ggplot(aes(x=cyl, y=gear)) +
    ##     ## geom_point()
    ##     geom_beeswarm()

    ## vignette("usageExamples")
        
    
    dt_vrblcvrg %>% # .[value > 0.95] %>% 
        .[if(yeet_acts) !grepl("^act_", vrbl) else T] %>% # filtering out activities if requested
        ggplot(aes(x=value, y=vrbl, color = museum_status)) +
        geom_beeswarm(side = 0) + 
        ## geom_jitter(width= 0, height = 0.3) +
        theme(legend.position = "bottom") +
        labs(x="proportion data available")
    
}

gp_vrblcvrg <- function(dt_vrblcvrg_grpd, yeet_acts) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' variable coverage, facetted by group
    
    dt_vrblcvrg_grpd %>% # .[value > 0.95] %>%
        .[if(yeet_acts) !grepl("^act_", vrbl) else T] %>% # filtering out activities if requested
        ggplot(aes(x=value, y=vrbl, color = museum_status)) +
        geom_beeswarm(side = 0) + 
        theme(legend.position = "bottom") +
        facet_grid(grp ~ ., scales = "free", space = "free", switch = "y") + 
        theme(strip.text.y.left = element_text(angle = 0)) +
        labs(x="proportion data available", y = element_blank())
}

gp_vrblcvrg_ratio <- function(dt_vrblcvrg) {
    gw_fargs(match.call())
    #' variable coverage with log points


    dcast(dt_vrblcvrg, grp + vrbl ~ museum_status) %>%
        .[, ratio := log(`private museum`/closed)] %>% # calculate open/closed ratio
        replace_Inf() %>% # set cases where no closed have value to NA
        melt(id.vars = .c(grp, vrbl), variable.name = "museum_status") %>%
        ## set order of actors 
        .[, xfacet := factor(fifelse(museum_status == "ratio", "log(prop_open/prop_closed)", "prop"),
                             levels = c("prop", "log(prop_open/prop_closed)"))] %>% 
        ggplot(aes(x=value, y=vrbl, color = museum_status)) +
        geom_beeswarm(side = 0) + 
        theme(legend.position = "bottom") +
        facet_grid(grp ~ xfacet, scales = "free", space = "free_y", switch = "y") + 
        theme(strip.text.y.left = element_text(angle = 0)) +
        labs(x="data availability", y = element_blank()) +
        theme_closing()
}



theme_closing <- function(extra_space_top=0) {
    ## pmdb theme for minimal layout
    
    ## theme_minimal() %+replace%

    base_size = 11
    half_line <- base_size/2


    theme(
        ## panel.grid.major = element_blank(),
        ## panel.background = element_rect(fill = "white"),
        ## axis.ticks = element_blank(),
        ## axis.ticks.length = unit(0, "pt"),
        ## axis.title = element_blank(),
        ## axis.text = element_blank(),
        ## plot.margin = unit(c(extra_space_top,0,0,0), "points"),
        axis.title = element_text(size = base_size)
        )

}


## *** tables


gc_tbls <- function(c_tblargs) {
    list(
        t_testtable = list(
            input_data = quote(mtcars),
            caption = "this is a great test table"),
        t_coxzph = list(
            rx = quote(l_mdls$r_more),
            caption = "Z-test of proportional hazards"),
        t_reg_coxph = list(
            l_mdls = quote(l_mdls),
            l_mdlnames = quote(l_mdlnames_coxph), # FIXME: gw_fargs should be able to handle vectors
            caption = "Cox Proportional Hazards Regression Results")
    )

    
}




gt_testtable <- function(input_data) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    ##

    list(
        dt_fmtd = adt(input_data, keep.rownames = "model")[1:12, 1:4],
        align_cfg = rep("l",5),
        hline_after = c(0,1,4, 6,9),
        add_to_row = NULL,
        number_cols = c(F, rep(T, 3)),
        landscape = F)
    
    
}

gt_coxzph <- function(rx) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    #' generate a table of the cox.zph result (whether hazards are proportional) 

    coxzph_trfms <- list(km = "Kaplan-Meier", identity = "Identity", rank = "Rank")

    ## generate the actual data
    dt_coxzph_prep <- map(names(coxzph_trfms), ~cox.zph(l_mdls$r_more, terms = F, transform = .x) %>%
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

gt_reg_coxph <- function(l_mdls, l_mdlnames) {
    #' collect some models from l_mdls and format them into nice custom regression table
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())


    l_mdls_slct <- map(l_mdlnames, ~chuck(l_mdls, .x))

    l_mdlres <- map2(l_mdls_slct, l_mdlnames, ~gd_reg_coxph(.x, .y))

    dt_coef <- map(l_mdlres, ~chuck(.x, "dt_coef")) %>% rbindlist
    dt_gof <- map(l_mdlres, ~chuck(.x, "dt_gof")) %>% rbindlist

    c_vvs <- gc_vvs()

    gt_reg(dt_coef, dt_gof, dt_vrblinfo = c_vvs$dt_vrblinfo, dt_ctgterm_lbls = c_vvs$dt_ctgterm_lbls,
           dt_gof_cfg = c_vvs$dt_gof_cfg, mdl_lbls = setNames(l_mdlnames, l_mdlnames))
    
    
}






## *** nbrs generation
gd_nbrs <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
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

    dt_nbrs_cbnd <- Reduce(rbind, list(dt_meanhaz, dt_ynkplt, dt_refplt, dt_reftbl))

    
    return(dt_nbrs_cbnd)
}




gc_clgrphattrs <- function() {
    #' generate configs of Rgraphviz rendering, required for gwd_clgrph
    attrs <- list()
    attrs$node <- list()
    attrs$node$shape <- "box"
    attrs$node$fixedsize <- F
    attrs$graph <- list()
    attrs$graph$splines <- F
    ## attrs$graph$com
    ## attrs$node$fontsize = 11

    return(attrs)
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


dt_pmdb_excl <- gd_pmdb_excl(only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")] # yeet bad PMs
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)

## variable coverage
l_vrblcvrg <- gl_vrblcvrg(dt_pmdb)

if ("memoised" %!in% class(gd_mow_info)) {
    gd_mow_info <- memoise(gd_mow_info) # memoizing gd_mow_info: saves the fread of 55k file
}

END_YEAR <- 2021

source(paste0(c_dirs$code, "regression.R"))


dt_pmx <- gd_pmx(dt_pmdb)
dt_pmtiv <- gd_pmtiv(dt_pmx)

dt_pmyear <- gd_pmyear(dt_pmx, dt_pmtiv)
dt_pmcpct <- gd_pmcpct(dt_pmyear)

l_mdls <- gl_mdls(dt_pmyear, dt_pmcpct)
l_mdlnames_coxph <- c("r_more")

screenreg2(list(l_mdls$r_more))

screenreg2(list(l_mdls$r_west_cpct, l_mdls$r_west_year, l_mdls$r_west_year2), digits = 4)



## generate plots and write them to file
## FIXME: parallelize this: can use some overarching mclapply
## mclapply(names(gc_plts()), \(x) lapply(c(gplt, wplt), \(f) f(x)), mc.cores = 6)
## mclapply seems to break gwd_clgrph (?????) for whatever reason..


## write numbers
dt_nbrs <- gd_nbrs()
fwrite(dt_nbrs, paste0(c_dirs$misc, "nbrs.csv"), quote = F)


plan(multicore, workers = 4)
future_walk(names(gc_plts()), ~lapply(c(gplt, wplt), \(f) f(.x)))
future_walk(names(gc_tbls()), ~lapply(c(gtbl, wtbl), \(f) f(.x)))
plan(sequential)


## callgraph testing
jtls::gwd_clgrph()
## dpltF("callgraph2")

## ** table testing 


## texreg(l_mdls$r_more, single.row = T, file = paste0(c_dirs$tbls, "r_more.tex"), label = "tbl:t_r_more")

gp_coxphdiag(l_mdls$r_more)

check_if_file_is_open(paste0(c_dirs$tbls, "t_testtable.pdf"))


screenreg(l_mdls$r_more)



## gt_reg_coxph(l_mdls, )

    
gtbl("t_reg_coxph")
wtbl("t_reg_coxph")
dtblF("t_reg_coxph_wcpF")
## wtbl_pdf("t_reg_coxph_wcpF", F)
## hmm question becomes whether I should use multiple groups

gdtbl("t_reg_coxph")
## gd_reg_coxph(l_mdls$r_more, "r_more")
