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

    

gd_pmyear <- function(dt_pmdb) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate dt in pm-year format

    


    dt_pmdb[, .(ID, name, year_opened, year_closed, museum_status)] %>%
        .[!is.na(year_opened)]
        

    


}


## * main
if (interactive()) {stop("it's interactive time")}

END_YEAR <- 2021

gd_pmyear(dt_pmdb)

## * foodweb testing




