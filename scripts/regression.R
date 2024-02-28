## * regression (survival analysis)





## ** data functions

gd_popcircle <- function(dt_pmx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## set up GSHL years
    GHSL_YEARS <- seq(1975,2020, by = 5)
    dt_ghsl_years <- data.table(ghsl_year = GHSL_YEARS)

    ## dt_pmx %>% copy() %>% .[, N := .N, .(lat, long)] %>% .[N > 1] %>% print(n=300)

    ## construct base for GSHL queries: get unique time points per museum
    ## keep this dt to merge GHSL results back to full years later on
    dt_pmyear_popprep <- dt_pmx[, .(ID, year_opened, year_closed, lat, long)] %>%
        .[is.na(year_closed), year_closed := END_YEAR] %>%
        .[, .(year = seq(year_opened, year_closed)), .(ID,lat, long)] %>%
        .[, year5 := floor(year/5)*5] %>% # round dow to 5 year intervals
        .[year >= min(GHSL_YEARS)]

    ## construct DT of positions to query
    dt_pmdb_year5 <- funique(dt_pmyear_popprep[, .(ID, year5, lat, lon = long)])

    ## ## some testing code
    ## imp_ghsl(dt_pmdb_year5[year5  == 2010, .SD[1:10]], # [, ID2 := as.character(ID)],
    ##          id_vrbl = "ID", year = 2010, radius = 10000) %>% .[, pop]

    ## split into year lists for more comfy parallel processing
    ## could squeeze out some more optimization by yeeting duplicate locations, but they shouldn't exist..
    l_dt_pmdb_ghsl <- split(dt_pmdb_year5, dt_pmdb_year5$year5)

    
    ## actual multithreaded processing
    plan(multicore, workers = 6)
    l_popres <- future_imap(l_dt_pmdb_ghsl, ~imp_ghsl(.x, id_vrbl = "ID", year = .y, radius = 10000))
    plan(sequential)

    ## process results
    ## year5 needs to be re-assigned as int probably since future_imap's .y converts it to string
    dt_popres2 <- rbindlist(l_popres)[, `:=`(year5 = as.integer(year), year = NULL)] %>%
        .[, pop := pop/1e6]

    ## merge to yearly data
    dt_popcircle <- join(dt_pmyear_popprep, dt_popres2, on = c("ID", "year5"))
    
    attr(dt_popcircle, "gnrtdby") <- as.character(match.call()[[1]])

    return(dt_popcircle)



}

gd_artnews <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' get the artnews data
    #' later merge on founder_id -> founder_id-year has to be UoA of this later on
    #' kinda have to merge in circle: start with artnews time, then to go to artnews person,
    #' then to PMDB person, then to founder_id

    dt_artnews_time <- gd_artnews_time() %>% slt(year, an_entry_id = id)

    dt_artnews_collector_person <- gd_artnews_collector_person() %>% slt(an_entry_id, an_person_id) 

    dt_artnews_pmdb_matchres <- gd_artnews_pmdb_matchres()[an_person_id != "nomatch"]

    dt_pmdb_founder_person <- gd_pmdb_founder_person()

    ## first combine ranking-time and and an_clctr_person to get APE
    dt_join1 <- dt_artnews_time[dt_artnews_collector_person, on = "an_entry_id", allow.cartesian = T]
    
    ## combine with matchres to get PPE
    dt_join2 <- dt_join1[dt_artnews_pmdb_matchres, on ="an_person_id"]

    ## summary(dt_join2)
    

    ## combine with dt_pmdb_founder_person to get founder_id
    dt_join3 <- dt_pmdb_founder_person[dt_join2, on = "pmdb_person_id", allow.cartesian = T] 
    
    ## reduce multiple persons to get any match of AN collector to any PMDB founder
    dt_artnews <- dt_join3[, .(an_inclusion = "included"), .(founder_id, year)]
    
    attr(dt_artnews, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_artnews)


}


gd_pmx <- function(dt_pmdb) {
    gw_fargs(match.call())
    #' eXtract relevant PMDB data: do the case/variable selection here
    #' also do some variable recoding

    with(dt_pmdb, {
        nbr_closing_year_missing <- dt_pmdb[museum_status == "closed" & is.na(year_closed), .N];
        if (nbr_closing_year_missing > 0) {
            warning(sprintf("FIXME: %s closed PMs have no closing year", nbr_closing_year_missing))}})
    
    with(dt_pmdb, {
        nbr_opng_year_missing <- dt_pmdb[is.na(year_opened), .N];
        if (nbr_opng_year_missing > 0) {
            warning(sprintf("FIXME: %s PMs have no opening year", nbr_opng_year_missing))}})
    
    ## filter out cases for all kinds of reasons
    dt_pmdb_fltrd <- copy(dt_pmdb) %>% 
        .[museum_status %in% c("private museum", "closed")] %>%  # yeet NLPM
        .[!(museum_status == "closed" & is.na(year_closed))] %>% # yeet closed with missing closing year
        .[!is.na(year_opened)] %>% # yeet all without proper opening year
        .[year_opened < END_YEAR] # only use those opened before end year (age has to be > 0)


    ## only basic variables for now to test overall flow, later add more variables
    dt_pmx <- copy(dt_pmdb_fltrd) %>% 
        .[, .(ID, name, iso3c, museum_status, year_opened, year_closed, deathyear,
              slfidfcn, muem_fndr_name, gender, founder_id, lat, long)]

    
    
    

    ## dt_pmx[!complete.cases(dt_pmx), .N, museum_status]
    ## dt_pmx[, .N, museum_status]
    
    attr(dt_pmx, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmx)
    
}

    

gd_pmyear_prep <- function(dt_pmx, dt_pmtiv) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate dt in pm-year format
    dt_pmyear <- dt_pmx[, last_year := fifelse(museum_status == "closed", year_closed, END_YEAR)] %>%
        ## expand to pm_year UoA
        .[, .(year = year_opened:last_year), .(ID, iso3c, museum_status, year_opened, year_closed,
                                               deathyear, founder_id)] %>%
        ## set closing variable
        .[, closing := fifelse(museum_status == "closed" & year_closed == year, 1, 0)] %>% 
        .[, age := year - year_opened] %>% # set age
        ## founder death: 1 if year > deathyear, else 0 (before death or not dead at all)
        .[, founder_dead := fifelse(!is.na(deathyear),fifelse(year > deathyear, 1, 0), 0)] %>% 
        .[, `:=`(tstart = age, tstop = age+1)] %>% # set survival time interval variables
        .[, pm_dens := .N, .(iso3c, year)] # calculate PM density 

    
    ## generate the artnews ranking states: not (yet) included, currently included, previously included
    dt_artnews <- gd_artnews()
    
    dt_pmyear_wan <- dt_artnews[copy(dt_pmyear), on = .(founder_id, year)] %>%  # wan: with artnews
        .[, ever_an_included := any(!is.na(an_inclusion)), ID] %>% # .[, .N, ever_an_included]
        .[ever_an_included == T, first_an_inclusion := .SD[!is.na(an_inclusion), min(year)], ID] %>% 
        ## .[, .(founder_id, year, an_inclusion, ever_an_included, first_inclusion)] %>% # .[ever_an_included == T]
        .[ever_an_included == T & year < first_an_inclusion, an_inclusion := "not_included"] %>%
        .[ever_an_included == T & year > first_an_inclusion & is.na(an_inclusion), an_inclusion := "dropped"] %>%
        .[ever_an_included == F, an_inclusion := "not_included"] %>%
        .[, `:=`(ever_an_included = NULL, first_an_inclusion = NULL)] %>%
        .[, an_inclusion := factor(an_inclusion, levels = c("not_included", "included", "dropped"))] %>%
        .[ year < 1990, an_inclusion := NA] # AN starts in 1990

    if (dt_pmyear_wan[year >= 1990, any(is.na(an_inclusion))]) {stop("artnews variable not properly defined")}


    ## integrate population circle data
    dt_popcircle <- gd_popcircle(dt_pmx)

    dt_pmyear_wpop <- join(dt_pmyear_wan, dt_popcircle[, .(ID, year, pop)], on = c("ID", "year"))

    if (dt_pmyear_wpop[year >= 1975,  any(is.na(pop))]) {stop("some pop is NA")}


    ## combine with time-invariant variables
    dt_pmyear3 <- join(dt_pmyear_wpop,
                        copy(dt_pmtiv)[, `:=`(iso3c=NULL, name = NULL)], ## yeet non-essential columns
                        on = "ID") 

    if (any(is.na(dt_pmyear3$mow))) {stop("some MOW is NA")}

    

    ## yeet unused variables
    dt_pmyear3[, `:=`(museum_status = NULL, year_closed = NULL, deathyear = NULL)]

    
    attr(dt_pmyear3, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmyear3)

}

gd_pmyear <- function(dt_pmyear_prep) {
    #' yeet observations with NAs on an_inclusion (starts 1990) and pop (starts 1975)
    gw_fargs(match.call())
    

    dt_pmyear <- dt_pmyear_prep[!is.na(an_inclusion) & !is.na(pop)]

    attr(dt_pmyear, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmyear)
}




gd_pmcpct <- function(dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    ## PM compact: survival data on time invariant variables for testing compability between long and compact

    dt_pmcpct <- dt_pmyear[, .SD[which.max(age)], ID]

    ## dt_pmcpct <- dt_pmx %>% copy() %>%
    ##     .[, last_year := fifelse(museum_status == "closed", year_closed, END_YEAR)] %>% 
    ##     .[, .(ID, name,
    ##           ## reg6 = rcd_iso3c_reg6(iso3c),
    ##           years = last_year - year_opened,
    ##           status = fifelse(museum_status == "closed", 1, 0))] %>%
    ##     .[, west := fifelse(reg6 %in% .c(EU, NALUL),1, 0)] %>% 
    ##     .[years > 0] # FIXME should not be necessary to filter here
    

    attr(dt_pmcpct, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmcpct)
}

gd_pmtiv <- function(dt_pmx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())
    #' add all kinds of time-invariant data to pmdb: MOW, tax incentives

    dt_pmx2 <- copy(dt_pmx) %>% 
        .[, reg6 := rcd_iso3c_reg6(iso3c)] %>%
        .[, west := fifelse(reg6 %in% .c(EU, NALUL),1, 0)] %>%
        .[, slfidfcn := fifelse(slfidfcn %in% c("Collection", "Foundation", "Museum"),
                                tolower(slfidfcn), "other")] %>%
        .[, slfidfcn := factor(slfidfcn, levels = c("museum", "foundation", "collection", "other"))] %>% 
        .[, muem_fndr_name := fifelse(is.na(muem_fndr_name), 0, muem_fndr_name)] %>% # FIXME in PMDB google sheets
        .[, gender := factor(fifelse(gender %in% c("F", "M"), gender, "couple"),
                             levels = c("M", "F", "couple"))] # recode other genders to couple
    

    
    ## get MOW data: get MOW entries which have PMDB match
    dt_mow_info <- gd_mow_info()[!is.na(PMDB_ID), .(PMDB_ID, mow = 1)]

    
    ## construct tiv vrbl vector: first get all time-invariant variables
    vrbls_tiv_temp_prep <- c("ID", "iso3c", "name",
                             gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl_tv==0, as.character(vrbl)])
    ## then yeet those that aren't there yet, .e.g MOW (gets added later)
    vrbls_tiv_temp <- intersect(vrbls_tiv_temp_prep, names(dt_pmx2))
    

    ## left join PMX subset with MOW
    ## later will probably have more sophisticated infrastructure
    ## dt_pmtiv <- merge(dt_pmx2[, .(ID, iso3c, name, reg6, west, slfidfcn, muem_fndr_name)],
    dt_pmtiv <- merge(dt_pmx2[, vrbls_tiv_temp, with = F],
                      dt_mow_info, by.x = "ID", by.y = "PMDB_ID", all.x = T) %>%
        .[, mow := fifelse(is.na(mow), 0, mow)]
    ## dt_mow_info[dt_pmx, on = .(PMDB_ID =  ID)]

    attr(dt_pmtiv, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmtiv)


}




## ** plots
gp_surv <- function(dt_pmcpct) {
    gw_fargs(match.call())
    #' just plot survival curve

    ## risk and cumhaz don't seem useful -> use default survival here
    ## maybe make more pretty later,
    ## for now this is just basic diagnostics to automatically update when data is updated

    survfit2(Surv(age, closing) ~ 1, dt_pmcpct) %>% 
        ggsurvfit() + add_confidence_interval()

}

gd_pehaz <- function(dt_pmcpct, cutwidth) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' get dt of "baseline" hazard (just don't account for anything, just age and closing)
    #' pehaz: Piecewise-Exponential Hazard
    #' cutwidth: allows different aggregations
    
    

    

    ## details of pehaz/muhaz functions can be figured out later
    res_pehaz <- pehaz(dt_pmcpct$age, dt_pmcpct$closing, width = cutwidth)
    dt_pehaz <- data.table(cuts = res_pehaz$Cuts,
                           haz = c(res_pehaz$Hazard, tail(res_pehaz$Hazard, 1)))

    return(dt_pehaz)

}



gp_hazard <- function(dt_pmcpct, cutwidth, bw.smooth) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    ## smooth hazard curves
    ## i think the muhaz kernel is more appropriate than the default geom_smooth kernel (goes negative)

    ## survfit2(Surv(age, closing) ~ 1, dt_pmcpct, stype= 1) %>% plot

    ##     library(pch)
    ##     rx <- pchreg(Surv(age, closing) ~ 1, data = dt_pmcpct)

    ## some way to try to get SEs for hazard function
    ##     summary(rx)
    ##     dt_new <- data.table(age = unique(dt_pmcpct$age))[order(age)]
    
    ##     dt_new_pred <- cbind(dt_new, predict(rx, newdata = dt_new))

    ##     ggplot(dt_new_pred, aes(x=age, y=f)) + geom_line()

    ##     r_mdl <- coxph(Surv(age, closing) ~ 1, dt_pmcpct)
    ##     survival_fit <- survfit(r_mdl)

    ##     ## time_points <- survival_fit$time
    ##     ## survival_prob <- survival_fit$surv

    ##     ## hazard_func <- -diff(log(survival_prob)) / diff(time_points)

    ##     ## cov_matrix <- vcov(r_mdl)

    ##     # Extract the cumulative hazard estimates and time points
    ##     cum_hazard <- -log(survival_fit$surv)
    ##     time_points <- survival_fit$time

    ##     ## Calculate the hazard function
    ##     hazard_func <- c(diff(cum_hazard) / diff(time_points), NA)

    ##     se_hazard <- sqrt(cumsum(survival_fit$std.err^2))



    dt_pehaz <- gd_pehaz(dt_pmcpct, cutwidth)

    dt_pehaz5 <- gd_pehaz(dt_pmcpct, 5)

    ## Muhaz: probably "mu" because Mueller (guy who wrote some of the kernel algorithms)

    res_muhaz <- muhaz(dt_pmcpct$age, dt_pmcpct$closing, bw.smooth = 5, b.cor = "none", max.time = 60,
                       bw.method = "local")
    dt_muhaz <- data.table(grid = res_muhaz$est.grid,
                           haz = res_muhaz$haz.est)

    
    ## try to get SE out of muhaz, but doesn't seem like there is any reported
    ## leave if for now.. 
    ## data.table(msemin = res_muhaz$msemin,
    ##            bias.min = res_muhaz$bias.min,
    ##            var.min = res_muhaz$var.min)[, x := 1:.N] %>%
    ##     melt(id.vars = "x") %>%
    ##     ggplot(aes(x=x, y=value, color = variable)) + geom_line() + facet_grid(variable~., scales = "free")

    y_upper_border <- 0.02 # FIXME: put as function argument

    ggplot() +
        geom_step(dt_pehaz, mapping = aes(x=cuts, y=haz, linetype = 'pehaz')) +
        ## geom_step(dt_pehaz5, mapping = aes(x=cuts, y=haz, linetype = 'pehaz5')) +
        geom_line(dt_muhaz, mapping = aes(x=grid, y=haz, linetype = "muhaz")) +
        labs(x="year", y="hazard") +
        coord_cartesian(ylim = c(0,y_upper_border)) +
        geom_label(dt_pehaz[haz > y_upper_border],
                   mapping = aes(x=cuts, y=y_upper_border, label = format(haz, digits = 2,nsmall = 2))) +
        scale_linetype_manual(name = element_blank(), 
            values = c('muhaz'=1, 'pehaz'=2, 'pehaz5' = 3),
                              labels = c("Epanechnikov-Kernel (5 years bandwidth)",
                                         sprintf("Piecewise-Constant (%s years)", cutwidth)),
                              guide = "legend") +
        scale_x_continuous(breaks = seq(0,60,10)) + # FIXME : generalize upper limit
        theme(legend.position = "bottom") + 
        labs(caption = sprintf("Piecewise constant hazard rates above %s demarcated by text boxes", y_upper_border))
     
    ## the peak after t=40: in the end: 16 are over 40 years old, 2 out of them die

}

gp_agedens <- function(dt_pmcpct) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## age distribution
    ## density
    p_agedens1 <- ggplot(dt_pmcpct, aes(x=age)) + geom_density()
    ## doesn't really show numbers tho.. 

    
    ## maybe points
    p_agedens2 <- dt_pmcpct %>% copy() %>% .[order(age, -closing)] %>% 
        .[, y_offset := 1:.N, age] %>%
        .[, status := factor(closing, levels = c(1,0))] %>% 
        ggplot(aes(x=age, y=y_offset,
                   color = factor(status),
                   ## fill = factor(status)
                   )) +
        ## geom_tile() + 
        geom_point() +
        theme(legend.position = c(0.8,0.7)) 
        
        

    p_agedens1 / p_agedens2
}

gp_yeardens <- function(dt_pmyear) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    
    dt_year_agg <- dt_pmyear[, .N, .(reg6, year)] %>%
        .[order(reg6, year)] %>% 
        .[, prop := N/sum(N), reg6] %>%
        .[, cumprop := cumsum(prop), reg6]

    
    p_yeardens1 <- ggplot(dt_year_agg[, .(N = sum(N)), year], aes(x=year, y=N)) + geom_line()
    ## ggplot(dt_pmyear, aes(x=year, color = reg6)) + geom_density()
        
    ## N by region 
    p_yeardens2 <- ggplot(dt_year_agg, aes(x=year, y=N, color = reg6)) + geom_line()
        
    ## within-prop by region
    p_yeardens3 <- ggplot(dt_year_agg, aes(x=year, y=prop, color = reg6)) + geom_line()
        
    p_yeardens4 <- ggplot(dt_year_agg, aes(x=year, y=cumprop, color = reg6)) + geom_line()

    p_yeardens <- (p_yeardens1 + p_yeardens2) /  (p_yeardens4 + p_yeardens3) +
        plot_layout(guides = "collect") & theme(legend.position = "bottom") & guides(color = guide_legend(nrow = 1))
   
    return(p_yeardens)


}
    
## gp_yeardens(dt_pmyear)

## gwdplt("p_agedens")



gd_inflcases <- function(rx) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' check influential cases
    #' generate dt of dfbeta-residuals (residuals.coxph)

    ## get the full coefs: might want to pass that as argument tho.. 
    dt_coefs <- data.table(vrbl = names(rx$coef), coef = rx$coef,
                           se = coef(summary(rx))[, "se(coef)"])
        
    ## retrieve ID from data.frame from which model was fitted (mdldt: model data.table)
    ## hope that works for subsets should I use them...
    mdldt <- rx$call %>% as.list %>% chuck("data")
    
    dt_inflcases <- residuals(rx, type = "dfbeta", collapse = get(mdldt)[, ID]) %>% # "collapse":aggregate by museum
        adt %>% 
        setnames(names(rx$coef)) %>% # assign variable names
        .[, ID := get(mdldt)[, funique(ID)]] %>% ## add ID from mdldt
        melt(id.vars = "ID", variable.name = "vrbl", value.name = "dfbeta") %>%
        dt_coefs[., on = "vrbl"] %>%
        .[, coef_wo := coef - dfbeta] %>% # coef without PM in question
        .[, `:=`(ratio_dfbeta = coef_wo/coef, ratio_se = dfbeta/se)]

    
    ## dt_inflcases[ratio_dfbeta > 1.5]
    ## dt_inflcases[ratio_se > 1.5]

    ## ggplot(dt_inflcases, aes(x=ratio_dfbeta)) + geom_density() + facet_grid(vrbl ~., scales = "free")

    attr(dt_inflcases, "gnrtdby") <- as.character(match.call()[[1]])
    ## criteria: large absolute, large as ratio, large in relation to SE (changes significance)
    return(dt_inflcases)

}

gp_inflcases <- function(dt_inflcases, dt_coefs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate histogram of coefficient changes due to dropping each observation once (jacknife/dfbeta-residuals)
    
    ## could regenerate dt_coefs quite quickly from dt_inflcases, but might need it more often
    ## need to sketch proper processing steps (which objects needed where),
    ## also with table outputs
    if (missing(dt_coefs)) {
        dt_coefs <- dt_inflcases[, unique(.SD), .SDcols = .c(vrbl, coef, se)]
    }

    ## histogram is better than density: not so much drawn to heights by narrow spread by discretizing
    ggplot() +
        geom_histogram(dt_inflcases, mapping = aes(x=coef_wo)) +
        geom_point(dt_coefs, mapping = aes(x=coef, y=50)) +
        geom_vline(xintercept = 0, linetype = 2) + 
        geom_errorbarh(dt_coefs, mapping = aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se, y=50), height = 50) + 
        facet_wrap(~vrbl, scales = "free_x") +
        labs(y="Frequency", x="coefficient without PM",
             caption = paste0("Histogram: Distribution of coefficient with each PM excluded once\n",
                              "Point, Error-bar: overal coefficient 95% Confidence interval"))

}



## * main
## if (interactive()) {stop("it's interactive time")}

## memoise functions for more snappy iterations

gc_pmdb_tests <- function(dt_pmx, dt_pmyear, dt_pmcpct) {
    #' bunch of checks to run on PMDB derived data
    #' FIXME: implement later more properly with tinytest

    if (any(is.na(match(dt_pmyear[, funique(ID)], dt_pmx[, ID])))) {
        stop("some how IDs went missing between dt_pmx and dt_pmyear")}

    if (any(is.na(match(dt_pmx[, ID], dt_pmcpct[, ID])))) {
        stop("some how IDs went missing between dt_pmx and dt_pmcpct")}

}

## library(glmmTMB)
## glmmTMB(closing ~ age + I(age^2), dt_pmyear, family = poisson) %>% summary

gl_mdls <- function(dt_pmyear, dt_pmcpct) {
    gw_fargs(match.call())

    l_mdls <- list(
        r_null = coxph(Surv(tstart, tstop, closing) ~ 1, dt_pmyear),

        ## some regional covariates
        r_reg6 = coxph(Surv(age, closing) ~ reg6, dt_pmcpct), # doesn't like to convert 

        ## compare cpct and long (year) data
        r_west_cpct = coxph(Surv(age, closing) ~ west, dt_pmcpct),
        r_west_year = coxph(Surv(age, closing) ~ west, dt_pmyear),
        r_west_year2 = coxph(Surv(age, closing) ~ west, dt_pmyear[, .SD[which.max(age)], ID]),
        
        ## test model for table testing
        r_less1 = coxph(Surv(tstart, tstop, closing) ~ mow + pm_dens + I(pm_dens^2), dt_pmyear),
        r_less2 = coxph(Surv(tstart, tstop, closing)~ founder_dead + mow + slfidfcn + muem_fndr_name, dt_pmyear),

        ## fullest model:
        ## FIXME: add founder_dead*muem_fndr_name
        r_more = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow +
                           slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop, dt_pmyear)

        

        
    )

    attr(l_mdls, "gnrtdby") <- as.character(match.call()[[1]])
    return(l_mdls)
    
}



## gd_inflcases(l_mdls$r_more) %>% gp_inflcases


    



## FIXME: generate proper cox.zph result table for any coxph model
## cox.zph(l_mdls$r_more)
## cox.zph(r_more, transform = "rank")
##  %>% plot





gp_schoenfeld <- function(rx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' schoenfeld resid plots for all variables
    #' y is beta(t): how coef depends on time
    #' also plots coxph coef as constant line to easily estimate diversion
    
    ## residuals(rx, type = "schoenfeld") %>% adt %>%

    ## dpltR("p_hazard")
    cox_zph_res <- cox.zph(rx, transform = "identity", terms = F)
    ## https://stats.stackexchange.com/questions/616468/schoenfeld-residuals-for-factors-with-cox-zph
    ## terms = F to get one for each dummy variable
        
    ## plot(cox_zph_res, var = "mow")    
    

    ## cox.zph based approach
    dt_schoen_coxzph <- data.table(x = cox_zph_res$x) %>% # x is transformed time axis
        cbind(adt(cox_zph_res$y)) %>%
        melt(id.vars = "x", variable.name = "vrbl")
        
    ## ## residuals() based approach
    ## dt_schoen_residuals <- data.table(x=cox_zph_res$x) %>%
    ##     cbind(adt(residuals(rx, type = "schoenfeld"))) %>%
    ##     melt(id.vars = "x", variable.name = "vrbl")
    
    
    ## m_schoenfeld_resids <- residuals(rx, type = "schoenfeld")
    
    ## m_vcov <- vcov(rx)
        
    ## ## manual approach with scaling raw schoenfeld resids by vcov
    ## dt_schoen_mnl <- data.table(x=cox_zph_res$x) %>%
    ##     cbind(m_schoenfeld_resids %*% m_vcov) %>%
    ##     melt(id.vars = "x", variable.name = "vrbl")

    ## ## check if schoen_mnl is scaler of schoen_coxzph
    ## join(dt_schoen_coxzph[, .(x, vrbl, vlu_coxzph = value)], dt_schoen_mnl[, .(x, vrbl, vlu_mnl = value)],
    ##      on = .c(x, vrbl)) %>%
    ##     .[, ratio := vlu_coxzph/vlu_mnl] %>% ggplot(aes(x=ratio)) + geom_density()
    ## ## yikes
    

    
    dt_coef <- adt(coef(rx), keep.rownames = T) %>% setnames(c("vrbl", "coef"))
        
    ggplot(dt_schoen_coxzph, aes(x=x, y=value)) +
        geom_point(mapping = aes(color = "schoenfeld"), size = 0.7) +
        geom_smooth(mapping = aes(linetype = "spline"), method = lm, formula = y ~ bs(x, df = 3), size = 0.7) + 
        ## geom_smooth(method = lm, formula = y ~ pspline(x, df = 4)) +
        ## geom_smooth(method = lm, formula = y ~ ns(x, df = 3)) +
        geom_hline(dt_coef, mapping = aes(yintercept = coef, linetype = "coef")) + 
        facet_wrap(~vrbl, scales ="free_y") +
        labs(x="time", y="beta(t)") +
        scale_linetype_manual(name = element_blank(),
                              values = c("spline" = 1, "coef" = 2),
                              labels = c("spline" = "Spline", "coef" = "Cox-PH coefficient")) +
        scale_color_manual(name = element_blank(),
                           values = c("schoenfeld" = "black"),
                           labels = c("schoenfeld" = "Schoenfeld residual")) + 
        theme(legend.position = "bottom") 
                              ## guide = "legend")
        ## scale_color_manual(name = element_blank(),
        ##                    values = c("schoenfeld" = "black", "spline" = "blue", "coef" = "grey"),
        ##                    labels = c("Schoenfeld Residuals", "Spline", "Cox-PH coefficient"),
        ##                    guide = "legend") +
        
     
     
    




}

## gp_schoenfeld(l_mdls$r_more)

## gp_schoenfeld(l_mdls$r_west_cpct)

gp_coxphdiag <- function(rx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' combine diagnostic plots with patchwork:
    #' top: influential cases (jacknife/dfbeta residuals) and
    #' bottom: coefficient variation over time (schoenfeld residuals)
    

    dt_inflcases <- gd_inflcases(rx)
    p_inflcases <- gp_inflcases(dt_inflcases)

    p_schoenfeld <- gp_schoenfeld(rx)

    p_inflcases / p_schoenfeld

}


##  gp_coxphdiag(l_mdls$r_west_cpct)

gp_coxphdiag_more <- gp_coxphdiag
