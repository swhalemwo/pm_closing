## * regression (survival analysis)


## ** data functions

gd_pmx <- function(dt_pmdb) {
    #' eXtract relevant data: do the case/variable selection here

    

}

    

gd_pmyear <- function(dt_pmdb) {
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




