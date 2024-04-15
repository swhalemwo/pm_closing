
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

gc_tbls <- function(c_tblargs) {
    list(
        t_coxzph = list(
            rx = quote(l_mdls$r_pop4),
            caption = "Z-test of proportional hazards"),
        t_reg_coxph = list(
            l_mdls = quote(l_mdls),
            l_mdlnames = quote(l_mdlnames_coxph), # FIXME: gw_fargs should be able to handle vectors
            caption = "Cox Proportional Hazards Regression Results"),
        t_sumstats = list(
            dt_pmyear = quote(dt_pmyear),
            dt_pmcpct = quote(dt_pmcpct),
            caption = "Summary Statistics")
    )

    
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

gc_plts <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    l_pltcfgs <- c(
        if (exists("gc_plts_vrblcvrg")) gc_plts_vrblcvrg(),
        ## gc_plts_dimred()
        list(
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
            rx = quote(l_mdls$r_pop4),
            caption = "Model Diagnostics",
            width = 19,
            height = 25),
        p_pred_popprxcnt = list(
            caption = "Predicted Avg. Hazard Rate on Regional PM Density and Population",
            width = 18,
            height = 8)
    ))

    ## check that there are no duplicate 
    if (any_duplicated(names(l_pltcfgs))) stop("duplicated names")

    ## check that all entries have width, heigh, caption
    if (!all(unlist(map(l_pltcfgs, ~all(c("width", "height", "caption") %in% names(.x)))))) {
        stop("not all configs have width, height and caption")}

    
    return(l_pltcfgs)
        
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
        gender                    = "Founder Gender",
        founder_dead              = "Founder died",
        slfidfcn                  = "Self- Identification",
        muem_fndr_name            = "Founder name in Museum name",
        mow                       = "MOW inclusion",
        pmdens_cry                = "PM density (country)",
        "I(pmdens_cry^2)"         = "PM density^2 (country)",
        popm_circle10             = "Pop. (millions) within 10km",
        popm_country              = "Pop. (millions) country",        
        proxcnt10                 = "Nbr PM within 10km",
        "I(proxcnt10^2)"          = "Nbr PM^2 within 10km",
        pmdens_circle10           = "PM density (10km)",
        "I(pmdens_circle10^2)"    = "PM density^2 (10km)",
        "proxcnt10:popm_circle10" = "Nbr PM (10km) * Pop (10km)",
        west                      = "Europe and North America",
        reg6                      = "Region",
        an_inclusion              = "ArtNews Ranking inclusion",
        ## exhbqntl_year        = "Exhibition Quantile (year)",
        ## "I(exhbqntl_year^2)" = "Exhibition Quantile (year)^2",
        ## exhbqntl_cy          = "Exhibition Quantile (CY)",
        ## exhbprop_top10_log   = "Exh. top 10% (log)",
        ## exhbprop_top10_utf   = "Exh. top 10%",
        ## exhbqntl_roll        = "Exhibition Quantile (year, rolled)",
        ## "I(exhbqntl_roll^2)" = "Exhibition Quantile^2 (year, rolled)",
        PC1                       = "PC1 (Size)",
        PC2                       = "PC2 (Support)",
        GLOBAL          = "Global") # from cox.zph
    
    l_vrblgrps <- list(# variable groups
        founder  = .c(gender, founder_dead),
        museum   = .c(slfidfcn, muem_fndr_name, mow, an_inclusion, PC1, PC2), # exhbqntl_cy),
        ## exhbqntl_year, "I(exhbqntl_year^2)",
        ## exhbprop_top10_log, exhbprop_top10_utf, exhbqntl_roll, "I(exhbqntl_roll^2)"), 
        envir    = .c(pmdens_cry, "I(pmdens_cry^2)", popm_circle10, popm_country, proxcnt10, "I(proxcnt10^2)",
                      west, reg6,
                      pmdens_circle10, "I(pmdens_circle10^2)", "proxcnt10:popm_circle10"),
        misc     = .c(GLOBAL)
    )

    ## specify whether variable is time-varying or not
    vrbls_tiv <- .c(gender, slfidfcn, muem_fndr_name, mow, west, reg6, PC1, PC2)
    vrbls_tv <- .c(pmdens_cry, "I(pmdens_cry^2)", popm_circle10, popm_country, proxcnt10, "I(proxcnt10^2)",
                   founder_dead,
                   an_inclusion, pmdens_circle10, "I(pmdens_circle10^2)", "proxcnt10:popm_circle10")
    ## exhbqntl_cy, exhbqntl_year, "I(exhbqntl_year^2)", exhbprop_top10_log, exhbprop_top10_utf,
    ## exhbqntl_roll, "I(exhbqntl_roll^2)")

    ## specify variable type: binary, numeric, categorical
    l_vrbltypes <- list(        
        bin = .c(founder_dead, muem_fndr_name, mow, west),
        num = .c(pmdens_cry, "I(pmdens_cry^2)", popm_circle10, popm_country, proxcnt10, "I(proxcnt10^2)",
                 pmdens_circle10, "I(pmdens_circle10^2)", "proxcnt10:popm_circle10", PC1, PC2),
        ## exhbqntl_cy, exhbqntl_year, "I(exhbqntl_year^2)",
        ## exhbprop_top10_log, exhbprop_top10_utf, exhbqntl_roll, "I(exhbqntl_roll^2)"),
        cat = .c(gender, slfidfcn, reg6, an_inclusion))


    l_vrblgrp_lbls <- list(# variable group labels
        founder = "Founder",
        museum  = "Museum",
        envir   = "Environment",
        misc    = "Miscellaneous")

    
    dt_vrbl_lbls = data.table(vrbl = names(l_vrbl_lbls), vrbl_lbl = unlist(l_vrbl_lbls)) %>%
        .[, vrbl := factor(vrbl, levels = vrbl)]

    dt_vrblgrp_lbls <- data.table(vrblgrp = names(l_vrblgrp_lbls), vrblgrp_lbl = unlist(l_vrblgrp_lbls)) %>%
        .[, vrblgrp := factor(vrblgrp, levels = names(l_vrblgrps))]

    dt_vrblgrps <- imap(l_vrblgrps, ~data.table(vrbl = .x, vrblgrp = .y)) %>% rbindlist %>%
        .[, vrblgrp := factor(vrblgrp, levels = names(l_vrblgrps))]

    ## ## list of variable configs
    ## l_cvrbl <- list(
    ##     list(vrbl = "gender", vrbl_label = "Founder Gender", vrblgrp = "founder", vrblgrp_lbl = "Founder",
    ##          tv = F, vrbltype = "cat"),
    ## check that the variables that are grouped/labelled are the same
    if (!setequal(dt_vrbl_lbls$vrbl, dt_vrblgrps$vrbl)) {
        stop("something wrong with vrbl labels and groups")}

    

    dt_vrbltypes <- imap(l_vrbltypes, ~data.table(vrbl = .x, vrbltype = .y)) %>% rbindlist

    ## combine variable labels, variable groups, time-varying indicators, and variable types

    dt_vrblinfo <- join(dt_vrbl_lbls, dt_vrblgrps, on = "vrbl") %>%
        join(dt_vrblgrp_lbls, on = "vrblgrp") %>%
        .[(vrbl %in% vrbls_tiv), vrbl_tv := 0] %>%
        .[(vrbl %in% vrbls_tv), vrbl_tv := 1] %>%
        .[dt_vrbltypes, vrbltype := i.vrbltype, on = "vrbl"]        

    ## make all the terms

    l_ctgterm_lbls <- list(# labels of terms of categorical variables
        list(vrbl = "gender", term = "genderF",      term_lbl = "Gender - Female"),
        list(vrbl = "gender", term = "gendercouple", term_lbl = "Gender - Couple"),
        list(vrbl = "gender", term = "genderM",      term_lbl = "Gender - Male"),        
        list(vrbl = "slfidfcn", term = "slfidfcnmuseum",     term_lbl = "Self-Identification - Museum"),
        list(vrbl = "slfidfcn", term = "slfidfcnfoundation", term_lbl = "Self-Identification - Foundation"),
        list(vrbl = "slfidfcn", term = "slfidfcncollection", term_lbl = "Self-Identification - Collection"),
        list(vrbl = "slfidfcn", term = "slfidfcnother",      term_lbl = "Self-Identification - Other"),
        list(vrbl = "reg6", term = "reg6AF",    term_lbl = "Region - Africa"),
        list(vrbl = "reg6", term = "reg6AS",    term_lbl = "Region - Asia"),
        list(vrbl = "reg6", term = "reg6EU",    term_lbl = "Region - Europe"),
        list(vrbl = "reg6", term = "reg6LA",    term_lbl = "Region - Latin America"),
        list(vrbl = "reg6", term = "reg6NALUL", term_lbl = "Region - North America"),
        list(vrbl = "reg6", term = "reg6OC",    term_lbl = "Region - Oceania"),
        list(vrbl = "an_inclusion", term = "an_inclusionincluded",     term_lbl = "AN Ranking - Included"),
        list(vrbl = "an_inclusion", term = "an_inclusionnot_included", term_lbl = "AN Ranking - Not Included"),
        list(vrbl = "an_inclusion", term = "an_inclusiondropped",      term_lbl = "AN Ranking - Dropped")
        
        )

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
        

    

    
    ## generate dt_term_lbls: get labels for all terms (still includes variables)
    dt_termlbls <- copy(dt_ctgterm_lbls)[dt_vrblinfo,
                                         `:=`("vrblgrp" = i.vrblgrp, "vrblgrp_lbl" = i.vrblgrp_lbl),
                                         on = "vrbl"] %>% 
        rbind(dt_vrblinfo[, .(term = vrbl, vrbl, term_lbl = vrbl_lbl, vrblgrp, vrblgrp_lbl)])    


    list(
        ## time-invariant variables
        vrbls_base = .c(ID, iso3c, year, tstart, tstop, age),
        ## vrbls_tiv = .c(gender, slfidfcn, muem_fndr_name, mow, west, reg6),
        ## vrbls_tv= .c(pm_dens, founder_dead),
        dt_vrblinfo = dt_vrblinfo,
        dt_ctgterm_lbls = dt_ctgterm_lbls,
        dt_gof_cfg = dt_gof_cfg,
        dt_termlbls = dt_termlbls)
        

    
}


        
