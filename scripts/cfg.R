
## * config functions

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

    l_tblcfgs_mnl <- list(
        t_coxzph = list(
            rx = quote(l_mdls$r_pop4),
            caption = "Z-test of proportional hazards"),
        ## FIXME: gw_fargs should be able to handle vectors
        t_sumstats = list(
            dt_pmyear = quote(dt_pmyear),
            dt_pmcpct = quote(dt_pmcpct),
            caption = "Summary Statistics"),
        t_selfid = list(
            dt_pmx = quote(dt_pmx),
            dt_pmyear = quote(dt_pmyear),
            caption = "Selfidentification"),
        t_reg_coxph_deathcfg = list(
            l_mdls = quote(l_mdls),
            caption = "Cox PH regression results with different death configurations") ,
        t_drop1 = list(
            dt_drop1 = quote(dt_drop1),
            caption = "Model Fit Improvement by Single Term Deletions")
    )
            



    ## regression model tables have all similar structure: table name, list of models, caption
    ## can be stored more compactly
    dt_cfg_prep <- tribble(
        ~tblname,    ~l_mdlnames,    ~caption,
        "",          "coxph",        "Cox Proportional Hazards Regression Results",                     
        "_density",  "coxph_density","Cox PH models of different local density specifications",         
        "_timeslice","timeslice",    "Cox PH regression results with different time slices",            
        "_comp",     "comp",         "Alternative competition specifications",                          
        "_dens",     "dens",         "Alternative competition specifications",                          
        "_env",      "env",          "Alternative specification of environment",                        
        "_af",       "af",           "Alternative specification: include Exhibition",                   
        "_timecfg",  "timecfg",      "Cox PH regression results with different time configurations",    
        "_dimred",   "dimred",       "Cox PH regression models with PCA factors of museum facilities",
        "_addgns",   "addgns",       "Cox PH regression models with additional specifications",
        "_reg",      "reg",          "Cox PH regression results with different region dummies included")

    l_tblcfgs_coxph <- split(dt_cfg_prep, 1:nrow(dt_cfg_prep)) %>%
        lapply(\(x) list(# tblname = paste0("t_reg_coxph", x$tblname),
                        ## l_mdlnames = sym(paste0("l_mdlnames_",x$l_mdlnames)),
                        l_mdlnames = sym(paste0("l_mdlnames_",x$l_mdlnames)),
                        l_mdls = sym("l_mdls"),
                        caption = x$caption)) %>%
        setNames(paste0("t_reg_coxph", dt_cfg_prep$tblname))
                
    l_tblcfgs <- c(l_tblcfgs_mnl, l_tblcfgs_coxph)

    return(l_tblcfgs)

    
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
        gc_plts_dimred(),
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
            p_hazard_time = list(
                dt_pmyear = quote(dt_pmyear),
                caption = "Private Museum hazard function by year",
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
                l_mdlnames = quote(l_mdlnames_coxph),
                l_mdls = quote(l_mdls),
                dt_pmyear = quote(dt_pmyear),
                caption = "Predicted Avg. Hazard Rate on Regional PM Density and Population",
                width = 18,
                height = 8),
            p_pred_proxcntpop = list(
                l_mdlnames = quote(l_mdlnames_coxph),
                l_mdls = quote(l_mdls),
                dt_pmyear = quote(dt_pmyear),
                caption = "Predicted Avg. Hazard Rate on Regional PM Density and Population",
                width = 18,
                height = 8),
            p_condmef = list(
                mdlname = "r_pop4",
                l_mdls = quote(l_mdls),
                dt_pmyear = quote(dt_pmyear),
                caption = "Conditional effects of Regional PM Density and Population",
                width = 18,
                height = 10),            
            p_pred_heatmap = list(
                mdlname = 'r_pop4',
                l_mdls = quote(l_mdls),
                dt_pmyear = quote(dt_pmyear),
                vx = "proxcnt10",
                vy = "popm_circle10",
                add_cross = T,
                caption = "Predicted Avg. Hazard Rate on Regional PM Density and Population (at available values)",
                width = 18,
                height = 10),
            p_heatmap_info = list(
                dt_pmyear = quote(dt_pmyear),
                caption = "heatmap info plots",
                width = 35,
                height = 22),
            p_pred_heatmap_wocryside = list(
                mdlname = "r_wocryside",
                l_mdls = quote(l_mdls),
                vx = "proxcnt10",
                vy = "popm_circle10",
                add_cross = T,
                dt_pmyear = quote(dt_pmyear[!(proxcnt10 < 2 & popm_circle10 <= 2)]),
                caption = paste0("Predicted Avg. Hazard Rate from Regional PM Density and Population",
                                 " (without proxcnt < 2 & popm_circle10 <2)"),
                width = 18,
                height = 10),
            p_pred_heatmap_onlycryside = list(
                mdlname = "r_onlycryside",
                l_mdls = quote(l_mdls),
                dt_pmyear = quote(dt_pmyear[proxcnt10 <= 1 & popm_circle10 <= 1]),
                add_cross = F,
                vx = "proxcnt10",
                vy = "popm_circle10",
                caption = paste0("Predicted Avg. Hazard Rate from Regional PM Density and Population",
                                 " (only proxcnt < 2 & popm_circle10 <2)"),
                width = 18,
                height = 10),
            p_pred_heatmap_alt = list(
                l_mdls = quote(l_mdls),
                dt_pmyear = quote(dt_pmyear),
                caption = "Prediction Heatmap of Alternative model specifications",
                width = 18,
                height = 15),
            p_surv_death = list(
                l_mdls = quote(l_mdls),
                name_mainmdl = "r_pop4",
                caption = "Comparison of Survival Estimates by Founder Death (95% CI)",
                width = 18,
                height = 10),
            p_lngtdvelp = list(
                dt_pmyear = quote(dt_pmyear),
                caption = "Composition of private museum population",
                width = 18,
                height = 14)
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
        founder_dead1             = "Founder died",
        founder_dead_binary       = "Founder died",
        slfidfcn                  = "Self-Identification",
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
        pmdens_circle10_log       = "PM density (10km, log)",
        "I(pmdens_circle10_log^2)"= "PM density (10km, log^2)",
        "proxcnt10:popm_circle10" = "Nbr PM (10km) * Pop (10km)",
        proxcnt10_log             = "Nbr PM within 10km (log)",

        popm_circle10_log         = "Pop. (millions) within 10km (log)",
        
        
        "popm_circle10:I(proxcnt10^2)"    = "Nbr PM^2 (10km) * Pop (10km)",
        "proxcnt10_log:popm_circle10_log" = "Nbr PM (10km, log) * Pop (10km, log)",
        "popm_circle10:proxcnt10_log"     = "Nbr PM (10km, log) * Pop (10km)",
        "proxcnt10:popm_circle10_log"     = "Nbr PM (10km) * Pop (10km, log)",
        
        "founder_dead_binary:muem_fndr_name" = "Founder name in Museum name * Founder died",
        
        audience10                = "Local Audience per PM",
        "I(audience10^2)"         = "Local Audience per PM^2",
        audience10_log            = "Local Audience per PM (log)",
        "I(audience10_log^2)"     = "Local Audience per PM (log)^2",
        recession                 = "Great Recession (2008/09)",
        covid                     = "Covid Pandemic (2020/21)",
        iso3c                     = "Country",
        west                      = "Europe and North America",
        reg6                      = "Region",
        regsub                    = "UN Subregion",
        an_inclusion              = "ArtNews Ranking inclusion",
        exhbany                   = "Exhibition any",
        exhbrollany               = "Exhibition any last 5 years",

        exhbqntl_roll             = "Exhibition Quantile (year, rolled)",
        "I(exhbqntl_year^2)"      = "Exhibition Quantile (year)^2",
        exhbqntl_cy               = "Exhibition Quantile (CY)",
        exhbcnt                   = "Exhibition count",
        exhbrollsum_avg           = "Exhibition avg rolled",
        exhbqntl_year             = "Exhibition Quantile (year)",
        exhbprop_top10_log        = "Exh. top 10% (log)",
        exhbprop_top10_utf        = "Exh. top 10%",

        "I(exhbqntl_roll^2)"      = "Exhibition Quantile^2 (year, rolled)",
        PC1                       = "PC1 (Size)",
        PC2                       = "PC2 (Support)",
        year                      = "year",
        "I(year^2)"               = "year^2",
        year_opened               = "Opening year",
        time_period               = "Time Period (5 years)",
        GLOBAL                    = "Global") # from cox.zph
    
    l_vrblgrps <- list(# variable groups
        founder  = .c(gender, founder_dead1, founder_dead_binary),
        museum   = .c(slfidfcn, muem_fndr_name, "founder_dead_binary:muem_fndr_name",
                      mow, an_inclusion, PC1, PC2,
                      exhbany, exhbrollany,
                      year_opened,
                      exhbqntl_cy, exhbcnt, exhbrollsum_avg,
                      exhbqntl_year, "I(exhbqntl_year^2)",
                      exhbprop_top10_log, exhbprop_top10_utf, exhbqntl_roll, "I(exhbqntl_roll^2)"), 
        envir    = .c(pmdens_cry, "I(pmdens_cry^2)", popm_circle10, popm_country, proxcnt10, "I(proxcnt10^2)",
                      west, reg6, "popm_circle10:I(proxcnt10^2)",  iso3c,
                      pmdens_circle10, "I(pmdens_circle10^2)", "proxcnt10:popm_circle10",
                      year, "I(year^2)",  time_period, covid, recession,
                      audience10, "I(audience10^2)", audience10_log, "I(audience10_log^2)",
                      proxcnt10_log, popm_circle10_log, "popm_circle10:I(proxcnt10^2)",
                      "proxcnt10_log:popm_circle10_log", "proxcnt10:popm_circle10_log",
                      "popm_circle10:proxcnt10_log", pmdens_circle10_log, "I(pmdens_circle10_log^2)",
                      regsub),
        misc     = .c(GLOBAL)
    )

    ## specify whether variable is time-varying or not
    vrbls_tiv <- .c(gender, slfidfcn, muem_fndr_name, mow, west, reg6, PC1, PC2, year_opened, regsub)
    vrbls_tv <- .c(pmdens_cry, "I(pmdens_cry^2)", popm_circle10, popm_country, proxcnt10, "I(proxcnt10^2)",
                   founder_dead1, founder_dead_binary, 
                   an_inclusion, pmdens_circle10, "I(pmdens_circle10^2)", "proxcnt10:popm_circle10",
                   year, "I(year^2)", time_period, "popm_circle10:I(proxcnt10^2)",
                   exhbany, exhbrollany, "founder_dead_binary:muem_fndr_name",
                   exhbqntl_cy, exhbqntl_year, "I(exhbqntl_year^2)", exhbprop_top10_log, exhbprop_top10_utf,
                   exhbqntl_roll, "I(exhbqntl_roll^2)",
                   exhbcnt, exhbrollsum_avg, audience10, "I(audience10^2)", audience10_log, "I(audience10_log^2)",
                   covid, recession, proxcnt10_log, popm_circle10_log, "popm_circle10:I(proxcnt10^2)",
                   "proxcnt10_log:popm_circle10_log", "proxcnt10:popm_circle10_log",
                   "popm_circle10:proxcnt10_log", pmdens_circle10_log, "I(pmdens_circle10_log^2)")

    ## specify variable type: binary, numeric, categorical
    l_vrbltypes <- list(        
        bin = .c(muem_fndr_name, mow, west, exhbany, exhbrollany, covid, recession, founder_dead_binary),
        num = .c(pmdens_cry, "I(pmdens_cry^2)", popm_circle10, popm_country, proxcnt10, "I(proxcnt10^2)",
                 pmdens_circle10, "I(pmdens_circle10^2)", "proxcnt10:popm_circle10", PC1, PC2,
                 year, "I(year^2)", "popm_circle10:I(proxcnt10^2)",
                 "founder_dead_binary:muem_fndr_name",
                 year_opened, audience10, "I(audience10^2)", audience10_log, "I(audience10_log^2)",
                 exhbqntl_cy, exhbqntl_year, "I(exhbqntl_year^2)",
                 exhbcnt, exhbrollsum_avg,
                 exhbprop_top10_log, exhbprop_top10_utf, exhbqntl_roll, "I(exhbqntl_roll^2)",
                 proxcnt10_log, popm_circle10_log, "popm_circle10:I(proxcnt10^2)",
                 "proxcnt10_log:popm_circle10_log", "proxcnt10:popm_circle10_log", "popm_circle10:proxcnt10_log",
                 pmdens_circle10_log, "I(pmdens_circle10_log^2)"),
        cat = .c(gender, founder_dead1, slfidfcn, reg6, an_inclusion, time_period, regsub))


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

    dt_vrblinfo <- join(dt_vrbl_lbls, dt_vrblgrps, on = "vrbl", verbose = 0) %>%
        join(dt_vrblgrp_lbls, on = "vrblgrp", verbose = 0) %>%
        .[(vrbl %in% vrbls_tiv), vrbl_tv := 0] %>%
        .[(vrbl %in% vrbls_tv), vrbl_tv := 1] %>%
        .[dt_vrbltypes, vrbltype := i.vrbltype, on = "vrbl"]        

    ## make all the terms

    l_ctgterm_lbls <- list(# labels of terms of categorical variables
        list(vrbl = "gender", term = "genderF",      term_lbl = "Gender - Female"),
        list(vrbl = "gender", term = "gendercouple", term_lbl = "Gender - Couple"),
        list(vrbl = "gender", term = "genderM",      term_lbl = "Gender - Male"),
        list(vrbl = "founder_dead1", term = "founder_dead1alive", term_lbl = "Founder - alive"),
        list(vrbl = "founder_dead1", term = "founder_dead1recently_dead", term_lbl = "Founder - died recently"),
        list(vrbl = "founder_dead1", term = "founder_dead1long_dead", term_lbl = "Founder - died 2+ years ago"),
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
        ## list(vrbl = "regsub", term = 
        list(vrbl = "regsub", term = "regsub21",  term_lbl = "UN Subregion - Northern America"),
        list(vrbl = "regsub", term = "regsub53",  term_lbl = "UN Subregion - Australia and New Zealand"),
        list(vrbl = "regsub", term = "regsub151", term_lbl = "UN Subregion - Eastern Europe"),
        list(vrbl = "regsub", term = "regsub39",  term_lbl = "UN Subregion - Southern Europe"),
        list(vrbl = "regsub", term = "regsub30",  term_lbl = "UN Subregion - Eastern Asia"),
        list(vrbl = "regsub", term = "regsub35",  term_lbl = "UN Subregion - South-eastern Asia"),
        list(vrbl = "regsub", term = "regsub155", term_lbl = "UN Subregion - Western Europe"),
        list(vrbl = "regsub", term = "regsub34",  term_lbl = "UN Subregion - Southern Asia"),
        list(vrbl = "regsub", term = "regsub419", term_lbl = "UN Subregion - Latin America and the Caribbean"),
        list(vrbl = "regsub", term = "regsub145", term_lbl = "UN Subregion - Western Asia"),
        list(vrbl = "regsub", term = "regsub154", term_lbl = "UN Subregion - Northern Europe"),
        list(vrbl = "regsub", term = "regsub202", term_lbl = "UN Subregion - Sub-Saharan Africa"),
        list(vrbl = "regsub", term = "regsub15",  term_lbl = "UN Subregion - Northern Africa"),

        list(vrbl = "iso3c", term = "iso3cUSA", term_lbl = "Country - United States"),
        list(vrbl = "iso3c", term = "iso3cGRC", term_lbl = "Country - Greece"),
        list(vrbl = "iso3c", term = "iso3cDEU", term_lbl = "Country - Germany"),
        list(vrbl = "iso3c", term = "iso3cFRA", term_lbl = "Country - France"),
        list(vrbl = "iso3c", term = "iso3cKOR", term_lbl = "Country - South Korea"),
        list(vrbl = "iso3c", term = "iso3cJPN", term_lbl = "Country - Japan"),
        list(vrbl = "iso3c", term = "iso3cCHN", term_lbl = "Country - China"),
        list(vrbl = "iso3c", term = "iso3cBEL", term_lbl = "Country - Belgium"),
        list(vrbl = "iso3c", term = "iso3cBRA", term_lbl = "Country - Brazil"),
        list(vrbl = "iso3c", term = "iso3cITA", term_lbl = "Country - Italy"),
        list(vrbl = "iso3c", term = "iso3cESP", term_lbl = "Country - Spain"),
        list(vrbl = "iso3c", term = "iso3cGBR", term_lbl = "Country - United Kingdom"),
        list(vrbl = "iso3c", term = "iso3cCHE", term_lbl = "Country - Switzerland"),
        list(vrbl = "iso3c", term = "iso3cRUS", term_lbl = "Country - Russia"),

        list(vrbl = "an_inclusion", term = "an_inclusionincluded",     term_lbl = "AN Ranking - Included"),
        list(vrbl = "an_inclusion", term = "an_inclusionnot_included", term_lbl = "AN Ranking - Not Included"),
        list(vrbl = "an_inclusion", term = "an_inclusiondropped",      term_lbl = "AN Ranking - Dropped"),
        list(vrbl = "time_period", term = "time_periodtp2000", term_lbl = "Time Period 2000-2004"),
        list(vrbl = "time_period", term = "time_periodtp2005", term_lbl = "Time Period 2005-2009"),
        list(vrbl = "time_period", term = "time_periodtp2010", term_lbl = "Time Period 2010-2014"),
        list(vrbl = "time_period", term = "time_periodtp2015", term_lbl = "Time Period 2015-2019"),
        list(vrbl = "time_period", term = "time_periodtp2020", term_lbl = "Time Period 2020-2024")        
    )

    dt_ctgterm_lbls <- rbindlist(l_ctgterm_lbls) %>%
        .[, term := factor(term, levels = term)]

    ## FIXME: write test that the variable of every term is defined in dt_vrbl_lbls

    
    ## information for gofs
    c_gof <- list(
        list(gof_name = "nunits", digits = 0, gof_lbl = "Museums"),
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
        dt_vrblinfo = dt_vrblinfo,
        dt_ctgterm_lbls = dt_ctgterm_lbls,
        dt_gof_cfg = dt_gof_cfg,
        dt_termlbls = dt_termlbls)
    

    
}



