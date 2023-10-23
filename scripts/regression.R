## * regression (survival analysis)




## ** data functions

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
    
    

    ## only basic variables for now to test overall flow, later add more variables
    dt_pmx <- dt_pmdb[museum_status %in% c("private museum", "closed"), # yeet NLPM
                      .(ID, name, iso3c, museum_status, year_opened, year_closed,
                        gender, deathyear)] %>% # select main variables
        .[!(museum_status == "closed" & is.na(year_closed))] %>% # yeet closed with missing closing year
        .[!is.na(year_opened)] %>% # yeet all without proper opening year
        .[, gender := factor(fifelse(gender %in% c("F", "M"), gender, "couple"),
                             levels = .c(M, F, "couple"))] # recode gender to couple
    
    

    ## dt_pmx[!complete.cases(dt_pmx), .N, museum_status]
    ## dt_pmx[, .N, museum_status]
    
    attr(dt_pmx, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmx)
    
}

    

gd_pmyear <- function(dt_pmx) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate dt in pm-year format


    dt_pmyear <- dt_pmx[, last_year := fifelse(museum_status == "closed", year_closed, END_YEAR)] %>%
        ## expand to pm_year UoA
        .[, .(year = year_opened:last_year), .(ID, iso3c, museum_status, year_opened, year_closed,
                                               deathyear, gender)] %>%
        ## set closing variable
        .[, closing := fifelse(museum_status == "closed" & year_closed == year, 1, 0)] %>% 
        .[!(year > END_YEAR)] %>% # yeet pm years beyond END_YEAR
        .[!(year_opened > year)] %>% # yeet pm-years where expansion went into negative direction
        .[, age := year - year_opened] %>%
        ## founder death: 1 if deathyear > year, else 0 (before death or not dead at all)
        .[, founder_dead := fifelse(!is.na(deathyear),fifelse(deathyear > year, 1, 0), 0)] %>% 
        .[, `:=`(tstart = age, tstop = age+1)] %>%
        .[, pm_dens := .N, .(iso3c, year)] # calculate PM density 

    ## try to get comfy groupby counts with collapse, but super incomprehensible API
    ## fcount(asdf = .c(iso3c, year), add = T, name = "pm_dens") # works, but name comes last -> not consistent
    ## fmutate(pm_dens = fcount(ID, .c(iso3c, year))) # doesn't work
    ## fmutate(pm_dens = fmean(year, iso3c)) ## doesn't work
    ## gby(iso3c, year) %>% #  fmutate(pm_dens = fcount(year_opened))
    ## fmutate(pm_dens = fnobs(ID, g = .c(iso3c))) ## requires gby call
    ## ftransform(fcount(pm_dens = ID), iso3c, year) ## doesn't work either
        
    
    ## dt_pmyear[, .N, .(closing, museum_status)] # check closing variable, looks good
    ## dt_pmyear[year > END_YEAR]
    ## fnunique(dt_pmyear$ID)
    
    ## filter out late entries: but wanna keep musuems that closed
    ## later: should be marked as not closed should be good now: 
    ## dt_pmyear[museum_status == "closed", max(closing), name]
    ## when setting END_YEAR to e.g. 2015, museums that close later don't have a closing year, as intended

    ## yeet unused variables
    dt_pmyear[, `:=`(museum_status = NULL, year_closed = NULL, deathyear = NULL)]

    
    attr(dt_pmyear, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmyear)

}

gd_pmcpct <- function(dt_pmx) {
    gw_fargs(match.call())
    ## PM compact: survival data on time invariant variables
    dt_pmcpct <- dt_pmx %>% copy() %>%
        .[, last_year := fifelse(museum_status == "closed", year_closed, END_YEAR)] %>% 
        .[, .(ID, name,
              reg6 = rcd_iso3c_reg6(iso3c),
              years = last_year - year_opened,
              status = fifelse(museum_status == "closed", 1, 0))] %>%
        .[, west := fifelse(reg6 %in% .c(EU, NALUL),1, 0)] %>% 
        .[years > 0] # FIXME should not be necessary to filter here

    attr(dt_pmcpct, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmcpct)
}

## ** plots
gp_surv <- function(dt_pmcpct) {
    gw_fargs(match.call())
    #' just plot survival curve

    ## risk and cumhaz don't seem useful -> use default survival here
    ## maybe make more pretty later,
    ## for now this is just basic diagnostics to automatically update when data is updated

    survfit2(Surv(years, status) ~ 1, dt_pmcpct) %>% 
        ggsurvfit() + add_confidence_interval()

}


gp_hazard <- function(dt_pmcpct, cutwidth, bw.smooth) {
    gw_fargs(match.call())
    ## smooth hazard curves
    ## i think the muhaz kernel is more appropriate than the default geom_smooth kernel (goes negative)

    ## details of pehaz/muhaz functions can be figured out later
    res_pehaz <- pehaz(dt_pmcpct$years, dt_pmcpct$status, width = cutwidth)
    dt_pehaz <- data.table(cuts = res_pehaz$Cuts,
                           haz = c(res_pehaz$Hazard, tail(res_pehaz$Hazard, 1)))

    res_muhaz <- muhaz(dt_pmcpct$years, dt_pmcpct$status, bw.smooth = bw.smooth, b.cor = "none", max.time = 60,
                       bw.method = "local")
    dt_muhaz <- data.table(grid = res_muhaz$est.grid,
                           haz = res_muhaz$haz.est)

    ggplot() +
        geom_step(dt_pehaz, mapping = aes(x=cuts, y=haz), linetype = 2) +
        geom_line(dt_muhaz, mapping = aes(x=grid, y=haz)) +
        labs(x="year", y="hazard")

    
    ## the peak after t=40: in the end: 16 are over 40 years old, 2 out of them die

}

gp_agedens <- function(dt_pmcpct) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## age distribution
    ## density
    p_agedens1 <- ggplot(dt_pmcpct, aes(x=years)) + geom_density()
    ## doesn't really show numbers tho.. 

    
    ## maybe points
    p_agedens2 <- dt_pmcpct %>% copy() %>% .[order(years, -status)] %>% 
        .[, y_offset := 1:.N, years] %>%
        .[, status := factor(status, levels = c(1,0))] %>% 
        ggplot(aes(x=years, y=y_offset,
                   color = factor(status),
                   ## fill = factor(status)
                   )) +
        ## geom_tile() + 
        geom_point() +
        theme(legend.position = c(0.8,0.7))
        

    p_agedens1 / p_agedens2
}

## gwdplt("p_surv")

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
    #' generate histogram of coefficient changes due to dropping each observation once (jacknife/dfbeta-residuals)
    
    ## could regenerate dt_coefs quite quickly from dt_inflcases, but might need it more often
    ## need to sketch proper processing steps (which objects needed where),
    ## also with table outputs
    dt_coefs <- dt_inflcases[, unique(.SD), .SDcols = .c(vrbl, coef, se)]

    ## histogram is better than density: not so much drawn to heights by narrow spread by discretizing
    ggplot() +
        geom_histogram(dt_inflcases, mapping = aes(x=coef_wo)) +
        geom_point(, mapping = aes(x=coef, y=0)) +
        geom_vline(xintercept = 0, linetype = 2) + 
        geom_errorbarh(dt_coefs, mapping = aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se, y=0), height = 50) + 
        facet_wrap(~vrbl, scales = "free_x")

}



## * main
if (interactive()) {stop("it's interactive time")}

END_YEAR <- 2021

dt_pmx <- gd_pmx(dt_pmdb)

dt_pmyear <- gd_pmyear(dt_pmx)
dt_pmcpct <- gd_pmcpct(dt_pmx)



## library(glmmTMB)
## glmmTMB(closing ~ age + I(age^2), dt_pmyear, family = poisson) %>% summary

r.null <- coxph(Surv(tstart, tstop, closing) ~ 1, dt_pmyear)

## some regional covariates
r.reg6 <- coxph(Surv(years, status) ~ reg6, dt_pmcpct) # doesn't like to convert 
r.west <- coxph(Surv(years, status) ~ west, dt_pmcpct)


r.more <- coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + founder_dead, dt_pmyear)

screenreg2(list(r.more))



cox.zph(r.more)
cox.zph(r.more, transform = "rank")
 %>% plot







gp_schoenfeld <- function(rx) {
    ## schoenfeld resid plots for all variables

}
