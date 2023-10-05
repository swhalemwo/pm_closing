## * regression (survival analysis)


## ** data functions

gd_pmx <- function(dt_pmdb) {
    gw_fargs(match.call())
    
    #' eXtract relevant data: do the case/variable selection here

    with(dt_pmdb, {
        nbr_closing_year_missing <- dt_pmdb[museum_status == "closed" & is.na(year_closed), .N];
        if (nbr_closing_year_missing > 0) {
            warning(sprintf("FIXME: %s closed PMs have no closing year", nbr_closing_year_missing))}})
    
    with(dt_pmdb, {
        nbr_opng_year_missing <- dt_pmdb[is.na(year_opened), .N];
        if (nbr_opng_year_missing > 0) {
            warning(sprintf("FIXME: %s PMs have no opening year", nbr_opng_year_missing))}})
     
        

    ## only basic variables for now to test overall flow, later add more variablesg
    dt_pmx <- dt_pmdb[museum_status %in% c("private museum", "closed"), # yeet NLPM
            .(ID, name, iso3c, museum_status, year_opened, year_closed)] %>% # select main variables
        .[!(museum_status == "closed" & is.na(year_closed))] %>% # yeet closed with missing closing year
        .[!is.na(year_opened)] # yeet all without proper opening year


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
        .[, .(year = year_opened:last_year), .(ID, iso3c, museum_status, year_opened, year_closed)] %>%
        ## set closing variable
        .[, closing := fifelse(museum_status == "closed" & year_closed == year, 1, 0)] %>% 
        .[!(year > END_YEAR)] %>% # yeet pm years beyond END_YEAR
        .[!(year_opened > year)] %>% # yeet pm-years where expansion went into negative direction
        .[, age := year - year_opened]
    
    ## dt_pmyear[, .N, .(closing, museum_status)] # check closing variable, looks good
    ## dt_pmyear[year > END_YEAR]
    ## fnunique(dt_pmyear$ID)

    ## filter out late entries: but wanna keep musuems that closed
    ## later: should be marked as not closed should be good now: 
    ## dt_pmyear[museum_status == "closed", max(closing), name]
    ## when setting END_YEAR to e.g. 2015, museums that close later don't have a closing year, as intended

    ## yeet unused variables
    dt_pmyear[, `:=`(museum_status = NULL, year_closed = NULL)]

    
    attr(dt_pmyear, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmyear)

}


## * main
if (interactive()) {stop("it's interactive time")}

END_YEAR <- 2021

dt_pmx <- gd_pmx(dt_pmdb)


dt_pmyear <- gd_pmyear(dt_pmx)

library(glmmTMB)
glmmTMB(closing ~ age + I(age^2), dt_pmyear, family = poisson) %>% summary










