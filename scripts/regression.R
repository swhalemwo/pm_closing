
## * regression (survival analysis)





## ** data functions


gd_af_grid <- function(dt_af_exhbs, dt_pmyear_size, dt_matches_pmdb_af) {
    #' grid: expansion to org-year first
    #' merge country later: use that where most of exhibitions take place
    
    
    ## AF grid
    ## first generate size per year
    dt_af_exhbcnt <- dt_af_exhbs %>% .[, begin_year := year(BeginDate)] %>%
        .[, .N, .(year = begin_year, AF_IID = InstitutionID)]
    
    ## then make AF grid
    dt_af_oy_prep <- dt_af_exhbs %>% .[, begin_year := year(BeginDate)] %>%
        .[!is.na(begin_year), .(year  = min(begin_year):max(begin_year)), .(AF_IID = InstitutionID)]

    ## then make join AF exhbcnt to AF grid
    dt_af_oy <- join(dt_af_oy_prep, dt_af_exhbcnt, on = c("AF_IID", "year"))


    ## PMDB grid with AF_IID
    dt_pmdb_oy <- join(dt_pmyear_size[, .(PMDB_ID = ID, year)], dt_matches_pmdb_af, on = "PMDB_ID")

    ## merge both grids with full join
    dt_af_grid_prep1 <- join(dt_af_oy, dt_pmdb_oy, on = c("AF_IID", "year"), how = "full")
    
    
    ## fix overlaps missing IDs with update join (e.g. when AF and PM differ in years covered)
    dt_af_grid_prep2 <- copy(dt_af_grid_prep1)[dt_matches_pmdb_af, AF_IID := i.AF_IID, on = "PMDB_ID"]
    dt_af_grid_prep3 <- copy(dt_af_grid_prep2)[dt_matches_pmdb_af, PMDB_ID := i.PMDB_ID, on= "AF_IID"]

    ## create general organization ID
    dt_af_grid <- copy(dt_af_grid_prep3)[, ORG_ID := sprintf("AF%sPM%s", AF_IID, PMDB_ID)]

    ## summary(dt_af_grid_prep1)
    ## summary(dt_af_grid_prep2)
    ## summary(dt_af_grid)

    ## check ID uniqueness with new ID
    dt_orgid <- dt_af_grid %>% .[, .(AF_IID, PMDB_ID, ORG_ID)] %>% funique 
    
    if (dt_orgid[!is.na(AF_IID), .N, AF_IID][, max(N) > 1] | dt_orgid[!is.na(PMDB_ID), .N, PMDB_ID][, max(N) > 1]){
        stop("IDs not sufficiently unique")}

    if (chuck(varying(dt_af_grid[!is.na(AF_IID)], ~AF_IID), "PMDB_ID") |
        chuck(varying(dt_af_grid[!is.na(PMDB_ID)], ~PMDB_ID), "AF_IID")) {
        stop("PMDB_ID/AF_IID are not properly configured, one still varies within the other")}

    ## varying(dt_af_grid[!is.na(PMDB_ID)], ~AF_IID)

    ## here you got all the AF_IIDs that are NA creating the idea of variation
    ## all the PMDBs that don't have AF entry: one kind of AF entry (NA) corresponds to different PMDB_IDs
    ## dt_af_grid[!is.na(PMDB_ID)] %>% copy %>% .[, N := fnunique(PMDB_ID), AF_IID] %>% .[N > 1]
    
    return(dt_af_grid)

}


gd_af_grid_wcry <- function(dt_af_grid, dt_pmx)  {
    #' add iso3c to grid
    #' join iso3c from PMDB
    dt_af_grid_wcry_prep1 <- join(copy(dt_af_grid),
                                  dt_pmx[, .(ID, iso3c_pm = iso3c)], on = c(PMDB_ID = "ID"))
    
    ## generate iso3c for AF
    dt_af_instn_cry <- gd_af_instns() %>%
        .[, .(AF_IID = ID, iso3c_af = countrycode(Country, "country.name", "iso3c"))] %>%
        na.omit
    
    ## join iso3c for AF
    dt_af_grid_wcry_prep2 <- join(copy(dt_af_grid_wcry_prep1), dt_af_instn_cry, on = "AF_IID")
    
    ## harmonize iso3c: use PM in case there is disagreement (also throw error)
    dt_af_grid_wcry_prep3 <- dt_af_grid_wcry_prep2 %>% copy %>%
        .[is.na(iso3c_pm) & !is.na(iso3c_af), iso3c := iso3c_af] %>%
        .[!is.na(iso3c_pm) & is.na(iso3c_af), iso3c := iso3c_pm] %>%
        .[!is.na(iso3c_pm) & !is.na(iso3c_af), iso3c := iso3c_pm]

    if (dt_af_grid_wcry_prep3[!is.na(iso3c_pm) & !is.na(iso3c_af)][iso3c_pm != iso3c_af, .N > 0]) {
        stop("not all iso3cs agree")}

    ## yeet columns not needed anymore
    dt_af_grid_wcry <- dt_af_grid_wcry_prep3 %>% copy %>% .[, `:=`(iso3c_pm = NULL, iso3c_af = NULL)]

    return(dt_af_grid_wcry)
}


gd_af_size <- function(dt_pmx) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## get match dt
    dt_matches_pmdb_af <- gd_af_pmdb_matches() %>%
        .[AF_IID != "nomatch"] %>% .[, AF_IID := as.integer(AF_IID)] %>%
        .[!(PMDB_ID == 167 & AF_IID == 51974)] # awkward update to accomodate ID change FIXME
    
    ## get AF exhb dt
    dt_af_exhbs <- gd_af_exhbs()[, iso3c := countrycode(CountryName, "country.name", "iso3c")] %>%
        .[, CountryName := NULL] %>%
        .[InstitutionID ==51974, InstitutionID :=36493] # awkward update to accomodate ID change FIXME
    
    ## doesn't seem a lot of issues
    ## dt_af_exhbs[CountryName == "Kosovo"]
    ## dt_af_exhbs[is.na(iso3c), .N, CountryName]

    ## expand to pm_year UoA
    dt_pmyear_size <- copy(dt_pmx)[, last_year := fifelse(museum_status == "closed", year_closed, END_YEAR)] %>%
        .[, .(year = year_opened:last_year), .(ID, iso3c, year_opened, year_closed)]
    
    ## construct organization-year (OY) grid
    dt_af_grid <- gd_af_grid(dt_af_exhbs, dt_pmyear_size, dt_matches_pmdb_af)

    ## dt_af_grid_wcry[AF_IID == 574]
    
    dt_af_grid_wcry <- gd_af_grid_wcry(dt_af_grid, dt_pmx)
    

    dt_af_qntlprep <- replace_NA(dt_af_grid_wcry, cols = "N", value = 0)

    ## dt_af_qntlprep[is.na(iso3c)]
    
    ## check that NAs on iso3c are dealt with
    dt_pmdb_na_check <- join(dt_af_qntlprep[is.na(iso3c), .N, .(ID = PMDB_ID)] %>% na.omit,
                             gd_pmdb_excl(sel = "all")[, .(name, museum_status, ID)],
                             on = "ID")
    
    if (dt_pmdb_na_check[, any(museum_status %!in% c("no longer a private museum", "not open yet"))]) {
        stop("at least one of the orgs that has no iso3c is not NLPM or not open yet")}
    
    ##      dt_af_exhbs_pmdb[, .N, .(InstitutionID, begin_year)],
    ##      on = c("InstitutionID", "begin_year"), verbose = 0) %>%
    
    
    ecdf_fun <- function(x,perc) ecdf(x)(perc) # get quantile of value

    ## rolling sum of last 5 years
    dt_af_roll <- copy(dt_af_qntlprep) %>%
        .[order(ORG_ID,year)] %>% 
        .[, paste0("rollsum_prep", 1:5) := shift(N, 0:4), ORG_ID] %>% # set up shift columns
        .[, `:=`(exhbrollsum5 = rowSums(.SD, na.rm = T), # summing: include those with NAs
                 exhbnNA = Reduce(`+`, lapply(.SD, is.na))), # get nbr of NA indicator
          .SDcols = paste0("rollsum_prep", 1:5)] %>%
        .[, paste0("rollsum_prep", 1:5) := NULL] %>% # yeet prep columns
        .[, exhbrollsum_avg := exhbrollsum5/(5-exhbnNA)] %>%
        .[, exhbany := fifelse(all(N == 0), 0, 1), ORG_ID] %>% 
        .[, exhbrollany := fifelse(exhbrollsum5 == 0, 0, 1)] %>%        
        .[, exhbqntl_roll := ecdf_fun(exhbrollsum_avg, exhbrollsum_avg), year] %>%
        .[!is.na(PMDB_ID)]
    
    ## dt_af_roll[iso3c == "DEU" & !is.na(PMDB_ID)] %>%
    ##     melt(id.vars = c("PMDB_ID", "begin_year"), measure.vars = c("quantile_roll")) %>%
    ##     ggplot(aes(x=begin_year, y=value, color = variable)) + facet_wrap(~PMDB_ID) + geom_line()
    
    ## dt_af_roll[!is.na(PMDB_ID)]    

    ## calculate quantile using all orgs, filter down to PMs
    ## dt_af_qntl <- copy(dt_af_qntlprep)[, quantile := ecdf_fun(N, N), .(iso3c, year)] %>%
    dt_af_qntl_simple <- copy(dt_af_qntlprep) %>%
        .[, exhbcnt := N] %>% # renaming
        .[, exhbqntl_cy := ecdf_fun(N, N), .(year, iso3c)] %>% # quantile by country year
        .[, exhbqntl_year := ecdf_fun(N, N), .(year)] %>% # quantile by year
        .[, exhbprop_top10_utf := N/quantile(N, probs = 0.90), year] %>%  # prop by year
        .[, exhbprop_top10_log := log(N+1)/quantile(log(N+1), probs = 0.90), year] %>% # prop (log) by year
        .[!is.na(PMDB_ID)] # focus on PMs
    
    ## combine "simple quantiles" (dt_af_qntl) and rolled sums (dt_af_roll),
    dt_af_qntl <- join(dt_af_qntl_simple,
                       dt_af_roll[, .(PMDB_ID, year, exhbrollsum5, exhbany, exhbrollany,  exhbnNA,
                                      exhbrollsum_avg, exhbqntl_roll)],
                       on = c("PMDB_ID", "year"), verbose = 0)

    ## filter some variables: the more fine-grained variables have higher data requirements?
    ## first set up sets
    
    ## this set is fully included
    set_always_include <- c("exhbany", "exhbrollany", "exhbcnt")

    ## more complex variable set: 
    ## yeet CYs where more than half institutions have no exhibitions
    set_conditional_include <- c("exhbqntl_cy", "exhbqntl_year","exhbprop_top10_utf",
                                 "exhbprop_top10_log", "exhbrollsum5", "exhbnNA",
                                 "exhbrollsum_avg", "exhbqntl_roll")

    dt_af_qntl_plausbl <- join(
        dt_af_qntl[, .SD, .SDcols = c("PMDB_ID", "year", set_always_include)],
        dt_af_qntl[, .SD[!any(N == 0 & exhbqntl_cy > 0.5)], .(iso3c, year),
                   .SDcols = c("PMDB_ID", "year", set_conditional_include)],
        on = c("PMDB_ID", "year"), verbose = 0)


    ## dt_af_qntl[, .SD[any(N == 0 & quantile > 0.5)], .(iso3c, begin_year)] %>%
    ##     .[, .N, iso3c] %>% print(n=300)

    ## ggplot(dt_af_qntl[iso3c == "USA"], aes(x=begin_year, y=quantile, group = PMDB_ID)) +
    ## geom_line(alpha = 0.1, position = position_jitter(width = 0.2,  height = 0.02))

    ## ggplot(dt_af_qntl[exhbprop_top10_utf > 0], aes(x= exhbprop_top10_utf)) + geom_density()

    attr(dt_af_qntl_plausbl, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_af_qntl_plausbl)
    
}




gd_pmdb_popcircle <- function(dt_pmx, radius_km) {
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
    dt_pmdb_year5 <- funique(dt_pmyear_popprep[, .(ID, year5, lat, long)])

    ## ## some testing code
    ## gd_popcircle(dt_pmdb_year5[year5  == 2010, .SD[1:10]], # [, ID2 := as.character(ID)],
    ## id_vrbl = "ID", year = 2010, radius_km = 10) %>% .[, pop]

    ## 3409326.47   13646.44 1495132.81 2738349.75 3549412.25  447843.49 1551780.82 2189097.23   71458.25  223607.55
    ## 3455098.75   13710.06 1507754.35 2760191.15 3574606.98  454468.42 1580232.92 2205837.25   73393.40  228170.74

    ## split into year lists for more comfy parallel processing
    ## could squeeze out some more optimization by yeeting duplicate locations, but they shouldn't exist..
    l_dt_pmdb_ghsl <- split(dt_pmdb_year5, dt_pmdb_year5$year5)

    
    ## actual multithreaded processing
    plan(multicore, workers = 6)
    l_popres <- future_imap(l_dt_pmdb_ghsl, ~gd_popcircle(.x, id_vrbl = "ID", year = .y, radius_km = 10))
    plan(sequential)

    ## process results
    ## year5 needs to be re-assigned as int probably since future_imap's .y converts it to string
    dt_popres2 <- rbindlist(l_popres)[, `:=`(year5 = as.integer(year), year = NULL)] %>%
        .[, popm_circle10 := pop/1e6] %>% .[, pop := NULL]

    ## merge to yearly data
    dt_popcircle <- join(dt_pmyear_popprep, dt_popres2, on = c("ID", "year5"))
    
    ## ggplot(dt_popcircle, aes(x=year, y=pop, group = ID)) +
    ##     geom_line(alpha = 0.2)

    

    attr(dt_popcircle, "gnrtdby") <- as.character(match.call()[[1]])

    return(dt_popcircle)



}

gd_proxcnt <- function(dt_pmx, radius_km) {

    
    ## generate list PMs open for each year
    l_dt_pmcoords <- dt_pmx[, last_year := fifelse(museum_status == "closed", year_closed, END_YEAR)] %>%
        .[, .(year = year_opened:last_year), .(ID, lat, long)] %>% ## expand to pm_year UoA
        split(by = "year") # split by year

    ## calculate proximity counts using parallized pairwise distance (from pmdata)
    plan(multicore, workers = 6)
    l_proxcnt_res <- future_imap(l_dt_pmcoords,
                                 ~gd_pmdb_proxcnt(.x, radius_km = radius_km)[, year := as.integer(.y)])
    plan(sequential)

    dt_proxcnt_res <- rbindlist(l_proxcnt_res) # aggregate results

    ## visualization check, looks good
    ## ggplot(dt_proxcnt_res, aes(x=year, y=proxcnt10, group = ID)) +
    ##     geom_line(alpha = 0.2, position = position_jitter())

    return(dt_proxcnt_res)

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

    ## manually keep track of some entries that are just too garbage to worry about  
    garbage_ids <- c(39, 79)

    with(dt_pmdb, {
        nbr_closing_year_missing <- dt_pmdb[museum_status == "closed" & is.na(year_closed) &
                                            ID %!in% garbage_ids, .N];
        if (nbr_closing_year_missing > 0) {
            warning(sprintf("FIXME: %s closed PMs have no closing year", nbr_closing_year_missing))}})
    
    with(dt_pmdb, {
        nbr_opng_year_missing <- dt_pmdb[is.na(year_opened) & ID %!in% garbage_ids, .N];
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
        .[, .(ID, name, iso3c, museum_status, year_opened, year_closed, birthyear, deathyear,
              slfidfcn, muem_fndr_name, gender, founder_id, lat, long)]

    
    
    

    ## dt_pmx[!complete.cases(dt_pmx), .N, museum_status]
    ## dt_pmx[, .N, museum_status]
    
    attr(dt_pmx, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmx)
    
}


gd_pop <- function() {
    #' generate population data from WB and UN
    dt_pop_wb <- gd_WB(c("SP.POP.TOTL"), DIR_WBproc = c_dirs$data) %>%
        .[, popm_country := SP.POP.TOTL/1e6] %>% .[, SP.POP.TOTL := NULL]

    ## for Taiwan use wpp2022 (https://github.com/PPgp/wpp2022)
    dt_pop_un <- pop1dt[country_code == 158] %>%
        .[, .(iso3c = "TWN", country = "Taiwan", year, popm_country = pop/1e3)]
    
    dt_pop <- rbind(dt_pop_wb, dt_pop_un)
    return(dt_pop)
}



gd_pmyear_prep <- function(dt_pmx, dt_pmtiv, c_lvrs = c_lvrs) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' @param dt_pmx extract of PMDB with museums to use
    #' @param dt_pmtiv dt with time invariant variables
    #' @c_lvrs flags/switches of which datasets to optionally/additionally include
    
    ## set up founder_dead variable names
    l_founder_dead_years <- seq(0,11)
    l_vrbls_founder_dead <- paste0("founder_dead", l_founder_dead_years)

    
    #' generate dt in pm-year format
    dt_pmyear <- dt_pmx[, last_year := fifelse(museum_status == "closed", year_closed, END_YEAR)] %>%
        ## expand to pm_year UoA
        .[, .(year = year_opened:last_year), .(ID, iso3c, museum_status, year_opened, year_closed,
                                               deathyear, founder_id)] %>%
        ## set closing variable
        .[, closing := fifelse(museum_status == "closed" & year_closed == year, 1, 0)] %>% 
        .[, age := year - year_opened] %>% # set age
        ## .[, founder_dead := fifelse(!is.na(deathyear),
        ##                             fifelse(year %between% list(deathyear, deathyear + 2), "recently_dead", 
        ##                                     fifelse(year > deathyear + 2, "long_dead", "alive")),
        ##                             "alive")] %>%
        ## .[, founder_dead := factor(founder_dead, levels = c("alive", "recently_dead", "long_dead"))] %>% 
        .[, paste0("founder_dead", l_founder_dead_years) := # can't just pass vector, need to paste in DT
                map(l_founder_dead_years, #' map over time specifications
                    ~fifelse(!is.na(deathyear),
                             ## when founder just died -> recently dead
                             fifelse(year %between% list(deathyear, deathyear + .x), "recently_dead",
                                     ## when year is longer ago -> long dead; else alive
                                     fifelse(year > deathyear + .x, "long_dead", "alive")),
                             "alive"))] %>%
        .[, paste0("founder_dead", l_founder_dead_years) :=
          lapply(.SD, \(x) factor(x, levels = c("alive", "recently_dead", "long_dead"))),
          .SDcols = l_vrbls_founder_dead] %>%
        .[, founder_dead_binary := fifelse(founder_dead0 == "alive", 0, 1)] %>%
        .[, `:=`(tstart = age, tstop = age+1)] %>% # set survival time interval variables
        .[, time_period := as.factor(paste0("tp", 5*(floor(year/5))))] %>%
        .[, covid := fifelse(year %in% c(2020, 2021), 1, 0)] %>%
        .[, recession := fifelse(year %in% c(2008, 2009), 1, 0)]
    
    ## dt_pmyear[founder_dead1 != "alive" & ID==227, .SD, .SDcols = c("ID", "deathyear", "year", l_vrbls_founder_dead)] %>% print(n=300)
        


    ## get country population data
    dt_pop_country <- gd_pop()

    ## calculate PM country density
    dt_pmyear_wpop_country <- join(dt_pmyear, dt_pop_country, on = c("iso3c", "year"),
                                   how = "left", verbose = 1) %>%
        .[, pmdens_cry := .N/popm_country, .(iso3c, year)]
    
    ## dt_pmyear_wpop_country[, .SD[ID < 10]] %>% ggplot(aes(x=year, y=pmdens_cry, group = ID, color = iso3c)) +
    ## geom_line()
    
    ## generate the artnews ranking states: not (yet) included, currently included, previously included
    dt_artnews <- gd_artnews()
    
    dt_pmyear_wan <- dt_artnews[copy(dt_pmyear_wpop_country), on = .(founder_id, year)] %>%  # wan: with artnews
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
    dt_popcircle <- gd_pmdb_popcircle(dt_pmx, radius_km = 10)

    dt_pmyear_wpop_circle <- join(dt_pmyear_wan, dt_popcircle[, .(ID, year, popm_circle10)],
                                  on = c("ID", "year"))        

    if (dt_pmyear_wpop_circle[year >= 1975,  any(is.na(popm_circle10))]) {stop("some popm_circle10 is NA")}

    ## ## integrate PM proximity counts 
    dt_proxcnt <- gd_proxcnt(dt_pmx, radius_km = 10)

    dt_pmyear_wproxcnt <- join(dt_pmyear_wpop_circle, dt_proxcnt, on = c("ID", "year")) %>%
        .[, proxcnt10 := proxcnt10-1] %>% # -1: don't count itself
        .[, pmdens_circle10 := (proxcnt10)/popm_circle10] %>%
        .[, pmdens_circle10_log := log(pmdens_circle10+1)] %>%
        .[, audience10 := popm_circle10/(proxcnt10+1)] %>%
        .[, audience10_log := log(audience10)] %>%
        .[, `:=`(proxcnt10_log = log(proxcnt10+1), popm_circle10_log = log(popm_circle10))]
        
    

    ## ## all museums
    ## ggplot(dt_pmyear_wproxcnt[, .SD, ID], aes(x=year, y=pmdens_circle10, group = ID)) + geom_line()

    ## dt_pmyear_wproxcnt[, .SD[any(pmdens_circle10 > 30)], ID] %>% .[, head(.SD,1), ID] %>% .[, .N, iso3c]
    ##     ggplot(aes(x=year, y=pmdens_circle10, group = ID)) + geom_line()

    
    ## dt_pmyear_wproxcnt[, head(.SD[any(pmdens_circle10 > 10)],1), ID][, .(ID)] %>%
    ##     ## dt_pmx[., on = "ID"] %>% .[, .(ID, achr(lat), achr(long))]
    ##     dt_pmdb[., on = "ID"] %>% .[, .(name, museum_status, address_formatted, founder_name, city,
    ##                                     achr(lat), achr(long))] %>% view_xl
    

    if (dt_pmyear_wproxcnt[, any(is.na(.SD)), .SDcols = keep(names(dt_pmyear_wproxcnt), ~grepl("proxcnt", .x))]) {
        stop("some NAs in proxcnt")}

    ## ## integrate artfacts size indicators
    if ("af_size" %in% chuck(c_lvrs, "dtti")) {
        
        ## dt_af_size <- gd_af_size(dt_pmx)[, .(ID = PMDB_ID, year,
        ##                                      exhbany, exhbrollany, # any inclusions in last 1/5 years
        ##                                      exhbqntl_year, exhbqntl_cy, exhbcnt, # simple quantiles
        ##                                      exhbprop_top10_log, exhbprop_top10_utf, # proportions of top10
        ##                                      exhbrollsum5, exhbnNA, exhbrollsum_avg, exhbqntl_roll)]

        dt_af_size <- gd_af_size(dt_pmx)[, .SD, .SDcols = c("PMDB_ID", "year", chuck(c_lvrs, "af_vrbls"))] %>%
            setnames(old = "PMDB_ID", new = "ID")

        dt_pmyear_waf <- join(dt_pmyear_wproxcnt, dt_af_size, on = c("ID", "year"))
    } else {
        dt_pmyear_waf <- dt_pmyear_wproxcnt
    }


    ## combine with time-invariant variables
    dt_pmyear_wtiv <- join(dt_pmyear_waf, 
                           copy(dt_pmtiv)[, `:=`(iso3c=NULL, name = NULL)], ## yeet non-essential columns
                           on = "ID",
                           drop.dup.cols = "x") # don't use year_opened from dt_pmyear_waf, but from dt_pmtiv

    ## dt_pmyear_wtiv[, .SD, .SDcols = patterns("year_opened")] %>% setnames(c("x1", "x2")) %>%
    ##     .[, diff := x1-x2] %>% .[, .N, diff]

    if (any(is.na(dt_pmyear_wtiv$mow))) {stop("some MOW is NA")}

    

    ## yeet unused variables
    dt_pmyear_wtiv[, `:=`(museum_status = NULL, year_closed = NULL, deathyear = NULL)]


    
    attr(dt_pmyear_wtiv, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmyear_wtiv)

}

gd_pmyear <- function(dt_pmyear_prep, c_lvrs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' yeet observations with NAs on an_inclusion (starts 1990) and pop (starts 1975)
    gw_fargs(match.call())
    
    ## lag time-varying variables: only lag variables where it makes sense:
    ## - an_inclusion: possible rev causality
    ## - exhbqntl: possible rev causality

    ## doesn't make sense:
    ## - proxcnt, pm_dens: pretty sure a PM still counts as there when closed that year
    ## - pop: kinda doubt it, pop is so much bigger
    ## - founder_dead: nope, separate processes
    
    vrbls_tolag <- c("an_inclusion")

    ## if af_size variables are to be included, add them to variables to lag
    if ("af_size" %in% chuck(c_lvrs, "dtti")) {
        vrbls_tolag <- c(vrbls_tolag,
                         keep(names(dt_pmyear_prep), ~startsWith(.x, "exhb")))
    }

    dt_pm_lagged <- dt_pmyear_prep[order(year), .SD, ID] %>% copy() %>% 
        .[, (vrbls_tolag) := shift(.SD), ID, .SDcols = vrbls_tolag]

    ## filter out missing values
    ## don't filter on exhbqntl yet.. still iffy -> FIXME
    ## i guess i'm not doing this year to not yeet too much? but easier with switch.. 
    ## dt_pmyear <- dt_pm_lagged[!is.na(an_inclusion) & !is.na(popm_circle10)]
    
    dt_pmyear_lagfiltered <- na.omit(dt_pm_lagged, cols = vrbls_tolag)

    dt_pmyear_trimmed <- dt_pmyear_lagfiltered[year >= START_YEAR]

    ## yeet unused time period factors, if it exists
    if ("time_period" %in% names(dt_pmyear_trimmed)) {
        dt_pmyear_trimmed[, time_period := factor(time_period)]
    }

    ## set iso3c reference category to US
    dt_pmyear_trimmed[, iso3c := factor(iso3c,
                                        levels = c("USA", dt_pmyear_trimmed[iso3c != "USA", funique(iso3c)]))]


    
    attr(dt_pmyear_trimmed, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmyear_trimmed)
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

gd_pmtiv <- function(dt_pmx, l_pca_dimred_woclosed) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())
    #' add all kinds of time-invariant data to pmdb: MOW, tax incentives

    dt_pmx2 <- copy(dt_pmx) %>% 
        .[, reg6 := rcd_iso3c_reg6(iso3c)] %>%
        ## set US as reference
        .[, iso3c := factor(iso3c, levels = c("USA", dt_pmx[iso3c != "USA", funique(iso3c)]))] %>% 
        .[, regsub := countrycode(iso3c, "iso3c", 'un.regionsub.code',
                               custom_match = c(TWN = 30L)) %>% as.factor] %>% 
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

    ## add PCA scores
    dt_pmtiv_wpca <- join(dt_pmtiv, l_pca_dimred_woclosed$dt_scores[, .(ID, PC1, PC2)], on = "ID")

    
    attr(dt_pmtiv_wpca, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmtiv_wpca)


}




## ** plots
gp_surv <- function(dt_pmcpct) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' just plot survival curve

    ## risk and cumhaz don't seem useful -> use default survival here
    ## maybe make more pretty later,
    ## for now this is just basic diagnostics to automatically update when data is updated
    

    

    survfit2(Surv(age, closing) ~ 1, dt_pmcpct) %>% 
        ggsurvfit() + add_confidence_interval()

}

quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
}

gd_pehaz <- function(dt_pmcpct, cutwidth) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' get dt of "baseline" hazard (just don't account for anything, just age and closing)
    #' pehaz: Piecewise-Exponential Hazard
    #' cutwidth: allows different aggregations


    ## r_cpct <- survfit(Surv(age, closing) ~ 1,
    ##                    ## Reduce(rbind, lapply(1:1, \(x) dt_pmcpct))) %>% # having more dts reduces CI/SE
    ##                   dt_pmcpct, conf.type = "plain")
    
    ## ## look at ggsurvfit plot for comparison plot
    ## r_cpct %>% ggsurvfit() + add_confidence_interval()

    ## r_epi %>% ggsurvfit() + add_confidence_interval()
    
    ## ## use epiR 
    ## library(epiR)
    
    ## epi.insthaz(r_epi) %>% adt %>% .[, .(time, hest, hlow, hupp)] %>%
    ##     rbind(data.table(time = 0, hest = 0, hlow= 0, hupp= 0)) %>% 
    ##     ggplot(aes(x=time, y=hest, ymin = hlow, ymax = hupp)) +
    ##     geom_step() + geom_ribbon(alpha = 0.3, stat = "stepribbon") +
    ##     coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.1))

    
    r_epi <- survfit(Surv(age, closing) ~ 1,
                     ## dt_pmcpct,
                     copy(dt_pmcpct)[, age := floor(age/cutwidth)*cutwidth]) # or floor? 
    ## copy(dt_pmcpct)[, age := age + 1], # shift times to get the year = 0 estimate, no work
    ## rbind(copy(dt_pmcpct)[, .(age, closing)], data.table(age = 0, closing = 1)),
    

    dt_pehaz_epi <- epi.insthaz(r_epi) %>% adt %>%
        .[, .(age = time, est = hest, upper = hupp, lower = hlow, src = "epi")]

    ## epi.insthaz(r_epi) %>% ggplot(aes(x=time, y=hest, ymin=hlow, ymax = hupp)) +
    ##     geom_step() +
    ##     geom_ribbon(stat = "stepribbon", alpha = 0.3) +
    ##     coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.04))
    
    ## ## ceiling or flooring? inst.haz can't work with t=0 -> ceiling? 
    

    ## ## compare epi.insthaz to pehaz
    ## left_join(
    ##     dt_pmcpct %$% pehaz(age, closing, width = 1) %$% data.table(time = head(Cuts,-1), pehaz = Hazard),
    ##     epi.insthaz(r_epi) %>% adt %>% .[, .(time, hest)], by = "time") %>%
    ##     .[pehaz > 0.075 &  hest < 0.04]
    ##     ## ggplot(aes(x=pehaz, y=hest)) + geom_point()
    
    
    
    ## function to run as bootstrap
    run_pehaz <- function(data, indices) {
        dtx <- data[indices]
        r_pehaz <- quiet(pehaz(dtx$age, dtx$closing, width = cutwidth))
        dt_pehaz <- data.table(cuts = head(r_pehaz$Cuts, -1), hazard = r_pehaz$Hazard)
        age_cutoff <- 50
        if (max(dtx$age) < age_cutoff) {
            setNames(rep(-1, age_cutoff/2), seq(0, age_cutoff-2, 2))
        } else {
            dt_pehaz[cuts < age_cutoff, setNames(hazard, cuts)]
        }
    }

    ## ## test that it works
    ## run_pehaz(dt_pmcpct, indices = sample(1:nrow(dt_pmcpct), replace = T))

    ## actdually do the bootstrapping
    reps <- boot(dt_pmcpct[, .(age, closing)], statistic = run_pehaz, R=1000,
                 parallel = "multicore", ncpus = 5)
    
    ## filter out invalid results
    dt_bootres <- reps$t %>% adt %>% .[V1 != -1]

    ## reshaping to long
    dt_bootres_melt <- melt(dt_bootres, measure.vars = names(dt_bootres), variable.factor = F) %>%
        .[, age := (as.integer(gsub("V", "", variable))-1)*2]

    

    ## gamma as in muhaz SO post
    dt_pehaz_gamma_prep <- dt_bootres_melt %>%
        .[, .(shape = (mean(value)/sd(value))^2,
              scale = var(value)/mean(value)), age] %>%
        .[, `:=`(lower = qgamma(0.025, shape = shape + 1, scale = scale),
                 upper = qgamma(0.975, shape = shape, scale = scale))]

    
    ## ## 
    ## dt_bootres2 <- melt(dt_bootres, measure.vars = names(dt_bootres), variable.factor = F) %>%
    ##     .[, quantile(value, probs = c(0.025, 0.975)) %>% as.list, variable] %>%
    ##     .[, age := (as.integer(gsub("V", "", variable))-1)*2] %>% .[, variable := NULL] %>%
    ##     setnames(old = c("2.5%", "97.5%"), new = c("low", "hi"))
    

    ## dt_bootres2 <- dt_bootres[, lapply(.SD, sd)] %>%
    ##     melt(measure.vars = names(.), variable.name = "tp", value.name = "se") %>%
    ##     .[, age := seq(0, .N*2-1, 2)] %>% .[, .(age, se)]


    ## details of pehaz/muhaz functions can be figured out later
    res_pehaz <- quiet(pehaz(dt_pmcpct$age, dt_pmcpct$closing, width = cutwidth))
    dt_pehaz <- data.table(age = res_pehaz$Cuts,
                           est = c(res_pehaz$Hazard, tail(res_pehaz$Hazard, 1)))

    dt_pehaz_gamma <- join(dt_pehaz, dt_pehaz_gamma_prep, on = "age") %>%
        .[, .(age, est, lower, upper, src = "gamma")]

    ## do pehaz per year, then aggregate
    res_pehaz_w1 <- quiet(pehaz(dt_pmcpct$age,dt_pmcpct$closing, width = 1))

    dt_grpd <- data.table(age = head(res_pehaz_w1$Cuts,-1),
                          est = res_pehaz_w1$Hazard) %>%
        .[, age_grpd := floor(age/2)*2] %>%
        .[, .(est = mean(est), upper = NA, lower = NA, src = "w1"), .(age = age_grpd)]


    
    dt_pehaz_ci <- rbindlist(list(dt_pehaz_gamma, dt_pehaz_epi, dt_grpd), use.names = T)

    ## dt_pehaz_ci %>% 
    ##     ggplot(aes(x=age, y=est, ymax = upper, ymin = lower)) + geom_step() +
    ##     geom_ribbon(stat = "stepribbon", alpha = 0.3) +
    ##     facet_wrap(~src)

    ## dt_pehaz_ci %>%
    ##     ggplot(aes(x=age, y=est, color = src)) +geom_step()

    

    ## dt_pehaz_bootse <- left_join(dt_pehaz, dt_bootres2, by = "age")

    ## X11()
    ## dt_pehaz_bootse[age <=22] %>% 
    ##     ggplot(aes(x=age, y=haz, ymin = low, ymax = hi)) +
    ##     geom_step() + geom_ribbon(alpha = 0.3, stat = "stepribbon") +
    ##     coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.04))
    

    return(dt_pehaz_ci)

}



gd_smooth_haz <- function(l_vrbls, dt_haz, span) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' smooth hazard rates with loess
    #' @param l_vrbls vector of variables to smooth (usually est, upper, lower)
    #' @param dt_haz data.table with age and hazard variables
    #' @param span span for loess smoothing
    
    ## generate the loess models 
    ## l_mdls <- map(l_vrbls, ~loess(get(.x) ~ time,
    ##                               dt_haz, span = span, degree = 1))
    
    ## l_fs <- map(l_vrbls, ~ sprintf("%s ~ age", .x))

    l_mdls <- map(l_vrbls, ~loess(sprintf("%s ~ age", .x), dt_haz, span = span, degree = 1))

    ## l_mdls <- map(l_fs, ~loess(, dt_haz, span = span, degree = 1))
    
    ## set up dt
    dt_insthaz_smooth <- data.table(age = seq(0, max(dt_haz$age), 0.25))

    ## combine
    dt_mres <- sapply(l_mdls, \(x) predict(x, dt_insthaz_smooth)) %>% adt %>% setnames(new = l_vrbls)
    dt_smooth_haz <- cbind(dt_insthaz_smooth, dt_mres)
    
    return(dt_smooth_haz)
}

gd_muhaz_boot <- function(dt_pmcpct) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    
    r_cpct <- survfit(Surv(age, closing) ~ 1, dt_pmcpct)

    
    ## dt_insthaz %>% copy %>%
    ##     .[, map(.SD, ~density(.x, kernel = "epa", bw = 5), .SDcols = patterns("^h"))]


    ## use kernel myself %>% this is more of a histogram, distribution of the density in total
    ## is not smoother over time
    ## dx <- density(dt_insthaz$hest, kernel = "epa")
    ## plot(dx)
    ## -> use loess seems to be the way
    
    ## insthaz SE
    dt_insthaz <- epi.insthaz(r_cpct, conf.level = 0.95) %>% adt %>%
        .[, .(age = time, est = hest, lower = hlow, upper = hupp)] %>%
        gd_smooth_haz(l_vrbls = c("est", "lower", "upper"), dt_haz = ., span = 0.20) %>%
        .[, src := "insthaz"]
    
    ## dt_insthaz_smooth %>% .[age < 20] %>% 
    ##     ggplot(aes(x=age, y=est, ymin = lower, ymax = upper)) +
    ##     geom_line() + geom_ribbon(alpha = 0.3)
    
    ## weirdwebsite SE: https://www.unistat.com/guide/survival-life-table/
    dt_weirdwebsite <- epi.insthaz(r_cpct, conf.level = 0.95) %>% adt %>%
        .[, .(time, n.risk, n.event, hest, hlow, hupp)] %>%
        .[, se := (hest*sqrt(1-(hest/2)^2))/sqrt(n.event)] %>%
        .[, `:=`(lower = hest - 1.96*se, upper = hest + 1.96*se)] %>%
        .[, .(age = time, est = hest, upper, lower)] %>%
        gd_smooth_haz(l_vrbls = c("est", "upper", "lower"), dt_haz = ., span = 0.20) %>%
        .[, src := "website"]

    ## assume SE of proportion: Singer_Willet_20023_applied
    dt_prop <- epi.insthaz(r_cpct, conf.level = 0.95) %>% adt %>%
        .[, se := sqrt((hest*(1-hest))/n.risk)] %>%
        .[, `:=`(lower = hest - 1.96*se, upper = hest + 1.96*se)] %>%
        .[, .(age = time, est = hest, upper, lower)] %>% 
        gd_smooth_haz(l_vrbls = c("est", "upper", "lower"), dt_haz = ., span = 0.20) %>%
        .[, src := "prop"]
    
    
    

    ## ggplot() +
    ##     geom_line(dt_insthaz_smooth, mapping = aes(x=time, y=hest)) +
    ##     geom_line(dt_muhaz, mapping = aes(x=grid, y=haz), color = "red")


    ## xlim(c(0,20)) 
    ## scale_y_log10()
    
    ## r_heft <- heft(dt_pmcpct$age,  dt_pmcpct$closing, penalty = 0)
    

    ## plot(r_heft, what = "h")
    ## r_heft$logse

    ## coord_cartesian(xlim = c(0, 20), ylim)

    
    ## library(boot)
    ## rsq_function <- function(formula, data, indices) {
    ##     d <- data[indices,] #allows boot to select sample
    ##     fit <- lm(formula, data=d)
    ##     return(summary(fit)$r.square)
    ## }

    ## reps <- boot(data=mtcars, statistic=rsq_function, R=3000, formula=mpg~disp)
    ## bootstrapping example
    
    ## boostrapping function
    run_muhaz <- function(data, indices) {
        ## print(len(age))
        ## print(len(closing))
        dtx <- data[indices]
        r_muhaz <- muhaz(dtx$age, dtx$closing, bw.smooth = 5, b.cor = "none", max.time = dtx[, max(age)], 
                         bw.method = "local", n.est.grid = dtx[, max(age)]*2+1)
        dt_muhaz <- r_muhaz %$% data.table(grid = est.grid, haz = haz.est)
        age_cutoff <- 60
        if (max(dtx$age) < age_cutoff) {
            c(dt_muhaz[grid < age_cutoff, setNames(haz, grid)],
              setNames(rep(-1, 120-dt_muhaz[, .N]), seq(dt_muhaz[, max(grid)]+0.5, age_cutoff - 0.5, 0.5)))
            ## setNames(rep(-1, age_cutoff*2), seq(0, age_cutoff - 0.5, 0.5))
        } else {        
            dt_muhaz[grid < age_cutoff, setNames(haz, grid)]
        }
        ## setNames(r_muhaz$haz.est, r_muhaz$est.grid)
    }

    ## test run
    ## run_muhaz(dt_pmcpct, indices = sample(1:nrow(dt_pmcpct), replace = T))

    ## actually run bootstrapping
    set.seed(42)
    reps <- boot(dt_pmcpct[, .(age, closing)], statistic = run_muhaz, R=1000,
                 parallel = "multicore", ncpus = 10)

    ## filter out garbage runs
    ## dt_bootres <- reps$t %>% adt %>% .[V1 != -1]

    ## melt into long for further processing
    dt_bootres_melt <- adt(reps$t) %>% 
        melt(measure.vars = names(.)) %>%
        .[, age := (as.integer(gsub("V", "", variable))-1)/2] %>%
        .[value != -1]


    ## get SE
    dt_bootres2 <- dt_bootres_melt[, .(se = sd(value)), age]
    
    ## run the muhaz for the estimate line
    dt_muhaz <- muhaz(dt_pmcpct$age, dt_pmcpct$closing, bw.smooth = 5, b.cor = "none", max.time = 60,
                      bw.method = "local", n.est.grid = 121) %$%
        data.table(est = haz.est, age = est.grid) %>% .[age < 60]
    
    ## create CI with est + se
    dt_muhaz_se <- join(dt_muhaz, dt_bootres2, on = "age") %>% copy %>% 
        .[, `:=`(upper = est + 1.96*se, lower = est - 1.96*se, src = "se")] %>%
        .[, .(age, est, upper, lower, src)]

    
    

    ## taken from SO: combine it with my results
    dt_muhaz_gamma <- dt_bootres_melt %>% 
        .[, .(est = mean(value), shape = (mean(value)/sd(value))^2,
              scale = var(value)/mean(value)), age] %>%
        .[, `:=`(lower = qgamma(0.025, shape = shape + 1, scale = scale),
                 upper = qgamma(0.975, shape = shape, scale = scale))] %>%
        .[, .(age, est, upper, lower, src = "gamma")]
    
    
    dt_muhaz_quantile <- dt_bootres_melt %>%
        .[, setNames(quantile(value, probs = c(0.025, 0.975)) %>% as.list, c("lower", "upper")), age] %>%
        .[dt_muhaz, on = "age"] %>% .[, src := "quantile"]
    

    dt_muhaz_boot <- rbindlist(list(dt_muhaz_se, dt_muhaz_gamma, dt_muhaz_quantile,
                                    dt_insthaz, dt_weirdwebsite, dt_prop), use.names = T)
    
    dt_muhaz_boot %>%
        ## .[src != "se"] %>%
        .[ age < 40] %>% 
        ggplot(aes(x=age, y=est, ymax = upper, ymin = lower)) + geom_line() + geom_ribbon(alpha = 0.3) +
        facet_wrap(~src)
    
    
    return(dt_muhaz_boot)

    ## https://stackoverflow.com/questions/29728795/how-do-i-extract-hazards-from-survfit-in-r
    ## bootstrap fun
    ## t0 <- 0
    ## t1 <- 61
    
    ## boot_fun <- function(dt_pmcpct) {
    ##     n <- dim(dt_pmcpct)[1]
    ##     x <- dt_pmcpct[sample.int(n, n, replace=TRUE), ]
    ##     muhaz::muhaz(x$age, x$closing, min.time=t0, max.time=t1, bw.smooth = 5, b.cor = "none", bw.method = "local")
    ## }

    ## ## bootstrap
    ## set.seed(42)
    ## R <- 100
    ## B <- replicate(R, boot_fun(dt_pmcpct))

    ## ## extract matrix from bootstrap
    ## r <- `colnames<-`(t(array(unlist(B[3, ]), dim=c(101, R))), B[2, ][[1]])

    ## ## calculate result
    ## library(matrixStats)  ## for fast matrix calculations
    ## r2 <- cbind(x=as.numeric(colnames(r)), 
    ##            y=colMeans2(r),
    ##            shape=(colMeans2(r)/colSds(r))^2, 
    ##            scale=colVars(r)/colMeans2(r))
    
    ## dt_r <- cbind(r2[, 1:2], 
    ##               lower=qgamma(0.025, shape=r2[, 'shape'] + 1, scale=r2[, 'scale']),
    ##               upper=qgamma(0.975, shape=r2[, 'shape'], scale=r2[, 'scale'])) %>% adt

    ## ggplot(dt_r, aes(x=x, y=y, ymax = upper, ymin = lower)) +
    ##     geom_line() + geom_ribbon(alpha = 0.3)

    
    
    

}


gp_hazard <- function(dt_pmcpct, cutwidth, bw.smooth) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    ## smooth hazard curves
    ## i think the muhaz kernel is more appropriate than the default geom_smooth kernel (goes negative)

    ## survfit2(Surv(age, closing) ~ 1, dt_pmcpct, stype= 1) %>% plot

    ## library(pch)
    ## rx <- pchreg(Surv(age, closing) ~ 1, data = dt_pmcpct)

    ## ## some way to try to get SEs for hazard function
    ## ## summary(rx)
    ## dt_new <- data.table(age = unique(dt_pmcpct$age))[order(age)]
    
    ## dt_new_pred <- cbind(dt_new, predict(rx, newdata = dt_new))

    ## ggplot(dt_new_pred, aes(x=age, y=f)) + geom_line()

    ## r_mdl <- coxph(Surv(age, closing) ~ 1, dt_pmcpct)
    ## survival_fit <- survfit(r_mdl)

    ## time_points <- survival_fit$time
    ## survival_prob <- survival_fit$surv

    ## hazard_func <- -diff(log(survival_prob)) / diff(time_points)

    ## ## cov_matrix <- vcov(r_mdl)

    ## ## Extract the cumulative hazard estimates and time points
    ## cum_hazard <- -log(survival_fit$surv)
    ## time_points <- survival_fit$time

    ## ## Calculate the hazard function
    ## hazard_func <- c(diff(cum_hazard) / diff(time_points), NA)

    ## se_hazard <- sqrt(cumsum(survival_fit$std.err^2))
    ## these SEs are absolutely yuuuuuuuuuuuuge
    

    ## gd_pehaz(dt_pmcpct, 1)

    dt_pehaz <- gd_pehaz(dt_pmcpct, cutwidth)

    ## dt_pehaz5 <- gd_pehaz(dt_pmcpct, 5)

    ## Muhaz: probably "mu" because Mueller (guy who wrote some of the kernel algorithms)

    ## res_muhaz <- muhaz(dt_pmcpct$age, dt_pmcpct$closing, bw.smooth = 5, b.cor = "none", max.time = 60,
    ##                    bw.method = "local")

    ## gd_insthaz_kernel(dt_pmcpct)

    ## dt_muhaz <- data.table(grid = res_muhaz$est.grid,
    ##                        haz = res_muhaz$haz.est)

    dt_muhaz_boot <- gd_muhaz_boot(dt_pmcpct)
    

    y_upper_border <- 0.02 # FIXME: put as function argument

    ## p_natrisk <- ggplot(dt_pmyear[, .N, age], aes(x=age, y=N)) + geom_line() +
    ##     coord_cartesian(xlim = c(0, 30))
    

    leg_labels <- c("gamma" = "Epanechnikov-Kernel (5 years bandwidth)",
                    "epi" = sprintf("Piecewise-Constant (%s years)", cutwidth))
    

    ggplot() +
        geom_step(copy(dt_pehaz)[src=="w1"][, src := "epi"], # awkward renaming 
                  mapping = aes(x=age, y=est, linetype = src, color = src,
                                linewidth = src)) +
        geom_line(dt_muhaz_boot[src == "gamma"],
                  mapping = aes(x=age, y=est, linetype = src, color = src, linewidth = src)) +
        geom_ribbon(dt_muhaz_boot[src == "gamma"],
                    mapping = aes(x = age, ymin = lower, ymax = upper),
                    alpha = 0.2, show.legend = F) +     
        labs(x="age", y="hazard") +
        coord_cartesian( xlim = c(0, 30), ylim = c(0,y_upper_border)) +
        scale_linewidth_manual(values = c("epi" = 2, "gamma" = 1), labels = leg_labels,
                               name = element_blank()) +
        scale_linetype_manual(values = c("epi" = "31", "gamma" = "91"), labels = leg_labels,
                              name = element_blank()) +
        scale_color_manual(values = c("epi" = "grey80", "gamma" = "black"), labels = leg_labels,
                           name = element_blank()) +
        theme(legend.position = "bottom",
              axis.title.x = element_text(size = 11, margin = margin(-7,0,0,0)),
              legend.spacing = unit(0, "pt"),
              legend.key.height = unit(0, "pt"),
              legend.box.spacing = unit(3, "pt"),
              legend.margin = margin(0,0,0,0)) + 
        geom_label(dt_pehaz[est > y_upper_border & src == "w1"],
                   mapping = aes(x=age +1, y=y_upper_border, label = format(est, digits = 2,nsmall = 2))) + 
        labs(caption = sprintf("95%% boostrapped CI, Piecewise constant hazards above %s demarcated by text boxes.",
                               y_upper_border)) 
    
    
    
    
}

gp_hazard_time <- function(dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())

    ## get data
    dt_vis <- dt_pmyear %>% copy %>%
        .[, .(mean_closing = mean(closing), .N, nbr_closed = sum(closing)), year] %>%
        .[, str_annotate := sprintf("closed: %s\nopen: %s", nbr_closed, N)] 

        ## .[, year2 := as.integer(floor(year/2)*2)] %>%
    ## .[, .(mean_closing = mean(mean_closing)), year2] %>%
    dt_vis %>% 
        ggplot(aes(x=year, y=mean_closing)) +
        geom_step() +
        ## geom_point() +
        geom_label_repel(dt_vis[year %in% c(2006, 2011, 2015, 2018, 2020, 2021)],
                        mapping = aes(label = str_annotate)) +
        labs(y = "hazard")
    ## geom_smooth()

    
    

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

gf_coxph_close <- function(vrbls_to_add = NULL, vrbls_to_yeet = NULL) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    #' generate coxph model formula
    #' @param vrbls_to_add vector of variables to add
    #' @param vrbls_to_yeet vector of variables to remove

    vrbls_base <- c("gender",
                    "pmdens_cry",
                    "I(pmdens_cry^2)", 
                    "slfidfcn",
                    "founder_dead_binary",
                    "muem_fndr_name",
                    "an_inclusion",
                    "proxcnt10",
                    "popm_circle10",
                    "proxcnt10:popm_circle10",
                    ## "exhbany",
                    "recession",
                    "covid")


    ## if (len(intersect(vrbls_to_add, vrbls_to_yeet)) > 0) {stop("can't add and yeet the same")}
    

    vrbls_touse <- setdiff(vrbls_base, vrbls_to_yeet) %>%
        union(vrbls_to_add)
    

    f_coxph_close <- sprintf("Surv(tstart, tstop, closing) ~ %s", paste(vrbls_touse, collapse = " + ")) %>%
        as.formula

    return(f_coxph_close)

}




    


gl_mdls <- function(dt_pmyear, dt_pmcpct) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())

    
    l_mdls <- list(
        r_null = coxph(Surv(tstart, tstop, closing) ~ 1, dt_pmyear),

        ## some regional covariates
        ## r_reg6 = coxph(Surv(age, closing) ~ reg6, dt_pmcpct), # doesn't like to convert 

        ## r_garbage = coxph(Surv(age, closing) ~ west + age, dt_pmcpct),

        ## compare cpct and long (year) data
        r_west_cpct = coxph(Surv(age, closing) ~ west, dt_pmcpct),
        r_west_year = coxph(Surv(age, closing) ~ west, dt_pmyear),
        r_west_year2 = coxph(Surv(age, closing) ~ west, dt_pmyear[, .SD[which.max(age)], ID]),
        
        ## test model for table testing
        r_less1 = coxph(Surv(tstart, tstop, closing) ~ mow + pmdens_cry + I(pmdens_cry^2), dt_pmyear),
        r_less2 = coxph(Surv(tstart, tstop, closing)~ founder_dead1 + mow + slfidfcn + muem_fndr_name, dt_pmyear),

                
        r_pop4 = coxph(gf_coxph_close(), dt_pmyear),
        ## only focus on museums that are not in countryside (wo = without), i.e. other PMs around and some people
        r_onlycryside = coxph(gf_coxph_close(), dt_pmyear[proxcnt10 <= 1 & popm_circle10 <= 1]),
        ## only focus on museums that are not in countryside (wo = without), i.e. other PMs around and some people
        r_wocryside = coxph(gf_coxph_close(), dt_pmyear[!(proxcnt10 < 2 & popm_circle10 <= 2)]),

        ## only museums which are not too old        
        r_smol = coxph(gf_coxph_close(), dt_pmyear[age <= 30]),

        
        ## try coxme.. looks pretty similar -> yeet for now
        ## library(coxme)
        ## r_coxme = coxme(Surv(tstart, tstop, closing) ~ gender + pmdens_cry + I(pmdens_cry^2) + 
        ##                     slfidfcn + founder_dead + muem_fndr_name + an_inclusion +
        ##                     proxcnt10*popm_circle10 + exhbany + recession + covid + (1 | iso3c),
        ##                dt_pmyear)

        r_pop42 = coxph(gf_coxph_close(vrbls_to_add ="I(proxcnt10^2)*popm_circle10"), dt_pmyear),

        ## get rid of environment variables: assume that they affect economic capital directly
        ## including them might overshadow identity effects?

        r_pop4_woenv = coxph(gf_coxph_close(
            vrbls_to_yeet = gc_vvs()$dt_vrblinfo[vrblgrp == "envir", achr(vrbl)]), dt_pmyear),

        ## with year opened
        r_pop4_wyo = coxph(gf_coxph_close(vrbls_to_add ="year_opened"), dt_pmyear),

        r_pop4_wtp = coxph(gf_coxph_close(vrbls_to_add ="time_period",
                                          vrbls_to_yeet = c("covid", "recession")), dt_pmyear),

        r_pop4_wyotp = coxph(gf_coxph_close(vrbls_to_add =c("year_opened", "time_period"),
                                            vrbls_to_yeet = c("covid", "recession")), dt_pmyear),


        ## only with data from 2010
        r_2005 = coxph(gf_coxph_close(), dt_pmyear[year>=2005]),
        r_2010 = coxph(gf_coxph_close(vrbls_to_yeet = "recession"), dt_pmyear[year>=2010]),
        r_2015 = coxph(gf_coxph_close(vrbls_to_yeet = "recession"), dt_pmyear[year>=2015]),
        ## these don't make sense: before 2010 there are only closing events,
        ## r_2011_2021 is basically equivalent to r_2010
        ## r_2000_2010 = coxph(gf_coxph_close(vrbls_to_yeet = "covid"), dt_pmyear[year <= 2010]),
        ## r_2011_2021 = coxph(gf_coxph_close(vrbls_to_yeet = "recession"), dt_pmyear[year > 2010])

        ## audience group: using amount of people available per museum
        ## assumes there's no difference between 3m people and 3 museums and 1m people and 1 museum
        
        r_audience1 = coxph(gf_coxph_close(vrbls_to_add = "audience10",
                                           vrbls_to_yeet = c("proxcnt10", "popm_circle10",
                                                             "proxcnt10:popm_circle10")), dt_pmyear),

        r_audience2 = coxph(gf_coxph_close(vrbls_to_add = c("audience10","I(audience10^2)"),
                                           vrbls_to_yeet = c("proxcnt10", "popm_circle10",
                                                             "proxcnt10:popm_circle10")), dt_pmyear),
        
        r_audience_log1 = coxph(gf_coxph_close(vrbls_to_add = "audience10_log",
                                               vrbls_to_yeet = c("proxcnt10", "popm_circle10",
                                                                 "proxcnt10:popm_circle10")), dt_pmyear),

        r_audience_log2 = coxph(gf_coxph_close(vrbls_to_add = c("audience10_log","I(audience10_log^2)"),
                                           vrbls_to_yeet = c("proxcnt10", "popm_circle10",
                                                             "proxcnt10:popm_circle10")), dt_pmyear),


        ## comp group: testing logs of variables: r_pop4 is implicitly r_comp0 (no logs)
        ## comp1-3 have different combinations of logs
        r_comp1 = coxph(gf_coxph_close(
            vrbls_to_add = c("proxcnt10_log", "popm_circle10_log", "proxcnt10_log:popm_circle10_log"),
            vrbls_to_yeet = c("proxcnt10", "popm_circle10", "proxcnt10:popm_circle10")), dt_pmyear),

        r_comp2 = coxph(gf_coxph_close(
            vrbls_to_add = c("proxcnt10_log", "popm_circle10:proxcnt10_log"),
            vrbls_to_yeet = c("proxcnt10", "proxcnt10:popm_circle10")), dt_pmyear),

        r_comp3 = coxph(gf_coxph_close(
            vrbls_to_add = c("popm_circle10_log", "proxcnt10:popm_circle10_log"),
            vrbls_to_yeet = c("popm_circle10", "proxcnt10:popm_circle10")), dt_pmyear),

        ## pmdens: use different specifications of pm density (proxcnt/popm);
        ## rather than proxcnt and popm separately
        r_pmdens1 = coxph(gf_coxph_close(
            vrbls_to_add = "pmdens_circle10",
            vrbls_to_yeet = c("proxcnt10", "popm_circle10", "proxcnt10:popm_circle10")), dt_pmyear),

        r_pmdens2 = coxph(gf_coxph_close(
            vrbls_to_add = "pmdens_circle10_log",
            vrbls_to_yeet = c("proxcnt10", "popm_circle10", "proxcnt10:popm_circle10")), dt_pmyear),

        ## this does not really make sense:
        ## no good reason why density defined as pop/pm should be quadratic
        r_pmdens3 = coxph(gf_coxph_close(
            vrbls_to_add = c("pmdens_circle10", "I(pmdens_circle10^2)"),
            vrbls_to_yeet = c("proxcnt10", "popm_circle10", "proxcnt10:popm_circle10")), dt_pmyear),

        r_pmdens4 = coxph(gf_coxph_close(
            vrbls_to_add = c("pmdens_circle10_log", "I(pmdens_circle10_log^2)"),
            vrbls_to_yeet = c("proxcnt10", "popm_circle10", "proxcnt10:popm_circle10")), dt_pmyear),

        ## pmdens_aud: combine pmdens (rate) and count measures;
        ## leads to awfully convoluted coefs interpretation
        r_pmdens_aud1 = coxph(gf_coxph_close(
            vrbls_to_add = "pmdens_circle10",
            vrbls_to_yeet = c("proxcnt10", "proxcnt10:popm_circle10")), dt_pmyear),

        r_pmdens_aud2 = coxph(gf_coxph_close(
            vrbls_to_add = "pmdens_circle10",
            vrbls_to_yeet = c("popm_circle10", "proxcnt10:popm_circle10")), dt_pmyear),

        ## include artfacts variables
        r_exhbany = coxph(gf_coxph_close(vrbls_to_add = "exhbany"), dt_pmyear),
        r_exhbroll = coxph(gf_coxph_close(vrbls_to_add = "exhbrollany"), dt_pmyear),

        
        ## add country/dummy regions
        
        r_country = coxph(gf_coxph_close(vrbls_to_add = "iso3c",
                                         vrbls_to_yeet = c("pmdens_cry", "I(pmdens_cry^2)")),
                          dt_pmyear[, .SD[sum(fnunique(ID)) > 10], iso3c] %>% droplevels),

        r_country_samplecprn = coxph(gf_coxph_close(
            vrbls_to_yeet = c("pmdens_cry", "I(pmdens_cry^2)")),
                                     dt_pmyear[, .SD[sum(fnunique(ID)) > 10], iso3c]),
        
        r_country2 = coxph(gf_coxph_close(vrbls_to_add = "iso3c",
                                          vrbls_to_yeet = c("pmdens_cry", "I(pmdens_cry^2)")),
                           dt_pmyear[, .SD[sum(fnunique(ID)) > 15], iso3c] %>% droplevels),

        ## see whether change in coefficients is due to yeeting low N countries or adding measures
        r_country2_samplecprn = coxph(gf_coxph_close(vrbls_to_yeet = c("pmdens_cry", "I(pmdens_cry^2)")),
                                      dt_pmyear[, .SD[sum(fnunique(ID)) > 15], iso3c]),
        
        r_regsub = coxph(gf_coxph_close(vrbls_to_add = "regsub"), dt_pmyear),
        r_regsub2 = coxph(gf_coxph_close(vrbls_to_add = "regsub"),
                          dt_pmyear[, .SD[sum(fnunique(ID)) > 10], regsub] %>% droplevels),

        ## see again whether change in coefficients is due to yeeting low N countries or adding measures
        r_regsub2_samplecprn = coxph(gf_coxph_close(),
                          dt_pmyear[, .SD[sum(fnunique(ID)) > 10], regsub] %>% droplevels),

        
        ## r_regsub3 = coxph(gf_coxph_close(vrbls_to_add = "regsub"),
        ##                   dt_pmyear[, .SD[sum(fnunique(ID)) > 100], regsub] %>% droplevels),
        r_reg6 = coxph(gf_coxph_close(vrbls_to_add = "reg6"), dt_pmyear),

        ## include PC
        r_pc1 = coxph(gf_coxph_close(vrbls_to_add = "PC1"), dt_pmyear),
        r_pc2 = coxph(gf_coxph_close(vrbls_to_add = "PC2"), dt_pmyear),
        r_pcboth = coxph(gf_coxph_close(vrbls_to_add = c("PC1", "PC2")), dt_pmyear)



        
        

        ## fullest model:
        ## FIXME: add founder_dead*muem_fndr_name
        ## r_more = coxph(Surv(tstart, tstop, closing) ~ gender + pmdens_cry + I(pmdens_cry^2) + mow +
        ##                    slfidfcn + founder_dead + muem_fndr_name + an_inclusion + popm_circle10 +
        ##                    proxcnt10,
        ##                dt_pmyear),


        ## r_pop5 = coxph(Surv(tstart, tstop, closing) ~ gender + pmdens_cry + I(pmdens_cry^2) + 
        ##                     slfidfcn + founder_dead + muem_fndr_name + an_inclusion +
        ##                     pmdens_circle10 + I(pmdens_circle10^2) + exhbany + recession + covid,
        ##                dt_pmyear)

        

        ## r_pop4_wyr = coxph(Surv(tstart, tstop, closing) ~ gender + pmdens_cry + I(pmdens_cry^2) + 
        ##                     slfidfcn + founder_dead + muem_fndr_name + an_inclusion +
        ##                     proxcnt10*popm_circle10 + exhbany + covid,
        ##                    dt_pmyear),

        ## r_pop4_wcrises = coxph(Surv(tstart, tstop, closing) ~ gender + pmdens_cry + I(pmdens_cry^2) + 
        ##                     slfidfcn + founder_dead + muem_fndr_name + an_inclusion +
        ##                     proxcnt10*popm_circle10 + exhbany + covid + recession,
        ##                     dt_pmyear)

        
                                        # dt_pmyear[iso3c != "KOR"])
        ## copy(dt_pmyear)[, reg6 := factor(reg6, labels = c("OC", "NALUL","EU","AS", "LA", "AF"))])
        
        
        ## r_woaf = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow +
        ##                    slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop + proxcnt10,
        ##                dt_pmyear[complete.cases(exhbqntl_cy)]),

        ## r_waf_year = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow + exhbqntl_year + 
        ##                    slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop + proxcnt10,
        ##                    dt_pmyear),

        ## r_waf_roll = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow + exhbqntl_roll + 
        ##                    slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop + proxcnt10,
        ##                    dt_pmyear),

        ## r_waf_roll2 = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow + exhbqntl_roll +
        ##                         I(exhbqntl_roll^2) + 
        ##                         slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop + proxcnt10,
        ##                     dt_pmyear)

        ## r_waf_year_sqrd = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow +
        ##                      exhbqntl_year + I(exhbqntl_year^2) + 
        ##                      slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop + proxcnt10,
        ##                      dt_pmyear)

        ## r_waf_cy = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow + exhbqntl_cy + 
        ##                    slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop + proxcnt10,
        ##                  dt_pmyear),

        
        ## r_waf_proplog = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow +
        ##                           exhbprop_top10_log + 
        ##                           slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop + proxcnt10,
        ##                       dt_pmyear),

        ## r_waf_prop = coxph(Surv(tstart, tstop, closing) ~ gender + pm_dens + I(pm_dens^2) + mow +
        ##                           exhbprop_top10_utf + 
        ##                           slfidfcn + founder_dead + muem_fndr_name + an_inclusion + pop + proxcnt10,
        ##                    dt_pmyear)


    )

    ## add models with different founder death specifications
    l_founder_dead_years <- seq(0,11)
    l_vrbls_founder_dead <- paste0("founder_dead", l_founder_dead_years)

    l_mdls <- c(l_mdls,
                map(l_vrbls_founder_dead,
                    ~coxph(gf_coxph_close(vrbls_to_add = .x, vrbls_to_yeet = "founder_dead_binary"), dt_pmyear)) %>%
                setNames(paste0("r_", l_vrbls_founder_dead))
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


## mode function https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
Mode <- function(x, na.rm = FALSE) {
    if(na.rm){
        x = x[!is.na(x)]
    }

    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
}


## generate some prediction data to test pop3

gd_predprep_popprxcnt <- function(dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate prediction DT

    ## variables that don't change
    dt_pred_prep <- cbind(
        dt_pmyear[, lapply(.SD, Mode), # categorical/binary variables: use mode
                  .SDcols = gc_vvs()$dt_vrblinfo[vrbltype %in% c("bin", "cat"), achr(vrbl)]],
        dt_pmyear[, lapply(.SD, median), .SDcols = c("pmdens_cry", "PC1", "PC2", "year")]) # numeric: use median

    ## variables to vary              
    dt_pred_prep2 <- expand.grid(proxcnt10 = c(0:15),
                                 ## popm_circle10 = c(0.01, 1.5, 3, 5),
                                 popm_circle10 = seq(0.01, 5, 0.01))
    
    ## quantile(dt_pmyear$popm_circle10, seq(0.05, 0.95, 0.1)))
    
    dt_pred <- cbind(dt_pred_prep2, dt_pred_prep) %>% adt %>%
        .[, pmdens_circle10 := proxcnt10/popm_circle10]

    return(dt_pred)

}


gd_pred <- function(mdlname, l_mdls, dt_pred, measure, year_range) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' generate average predicted hazard for first 20 years for all the specifications in dt_pred
    #' @param rx a coxph-based regression model
    #' @param dt_pred data frame with all variables and different values (one per row) for each specification
    #' @measure which measure to calculate predictions for. one of "cumhaz" (cumulative hazard),
    #' "surv" (survival curve), "hazard" (average hazard, no CI)
    #' @year_range range of years to calculate predictions for (cutoff)

    ## xx <- survfit(l_mdls$r_pop4, newdata = dt_pred, se.fit = T)

    ## ## extract CI info from upper/lower vs from std.error
    ## rbind(adt(xx$cumhaz)[, `:=`(src = "cumhaz", time = 1:.N)],
    ##       adt(xx$upper) %>% setnames(old = names(.), new = paste0("V", names(.))) %>%
    ##       .[, `:=`(src = "upper", time = 1:.N)],
    ##       adt(xx$lower) %>% setnames(old = names(.), new = paste0("V", names(.))) %>%
    ##       .[, `:=`(src = "lower", time = 1:.N)],
    ##       adt(xx$std.err) %>% .[, `:=`(src = "se", time = 1:.N)]) %>%        
    ##     melt(id.vars = c("src", "time"), measure.vars = patterns("^V")) %>%
    ##     dcast(time + variable ~ src) %>%
    ##     .[variable == "V20"] %>%
    ##     .[, `:=`(haz = cumhaz - shift(cumhaz), se_shift = se - shift(se))] %>% 
    ##     .[, `:=`(haz_hi = (1-lower) - shift(1-lower) , haz_lo = (1-upper) - shift(1-upper))] %>%
    ##     .[, `:=`(haz_hi2 = haz + 1.96*se_shift, haz_lo2 = haz - 1.96*se_shift)] %>% 
    ##     melt(id.vars = "time", measure.vars = patterns("^haz")) %>%
    ##     ggplot(aes(x=time, y = value, color = variable)) + geom_line()
    
    
    ## extract CI info from std.er
    
    r_survfit <- survfit(chuck(l_mdls, mdlname), newdata = dt_pred[],
                         se.fit = T, conf.type = "plain", conf.int = 0.95)
    if (measure == "cumhaz") {
        ## cumhaz
        data.table(est = r_survfit$cumhaz[year_range,], se = r_survfit$std.err[year_range,]) %>%
            .[, `:=`(lower = est - 1.96*se, upper = est + 1.96*se)] %>% 
            cbind(dt_pred[, .(proxcnt10, popm_circle10)])
        ## ggplot(aes(x=proxcnt10, y=cumhaz, color = factor(popm_circle10), fill = factor(popm_circle10),
        ##            ymax = cumhaz + 1.96*se, ymin = cumhaz - 1.96*se)) + geom_line() +
        ## geom_ribbon(alpha = 0.3)

    } else if (measure == "surv") {
        ## using survival curve
        data.table(est = r_survfit$surv[year_range,], upper = r_survfit$upper[year_range,],
                   lower = r_survfit$lower[year_range,]) %>%
            cbind(dt_pred[, .(proxcnt10, popm_circle10)]) %>%
            .[, src := mdlname]
    } else if (measure == "hazard") {
        
        ## ggplot(aes(x=proxcnt10, y=surv, color = factor(popm_circle10), fill = factor(popm_circle10),
        ##            ymax = upper, ymin = lower)) + geom_line() + geom_ribbon(alpha = 0.3) 
        ## ## facet_wrap(~popm_circle10, scales = "free")
        
        ## xx$cumhaz[20,]


        basehaz(chuck(l_mdls, mdlname), dt_pred) %>% adt %>%
            ## transform each settings cumhaz into hazard and take mean
            .[time < 20, lapply(.SD, \(x) mean(x - shift(x),  na.rm = T)), .SDcols = patterns("^hazard")] %>%
            melt(measure.vars = patterns("^hazard"), variable.name = "condition",
                 value.name = "avghaz") %>%
            join(dt_pred[, .(condition = sprintf("hazard.%s", seq(1:fnrow(dt_pred))),
                             proxcnt10, popm_circle10)], on = "condition") %>%
            .[, src := mdlname] %>%
            .[, .(est = avghaz, upper = avghaz, lower = avghaz, src, proxcnt10, popm_circle10)]
        ## ggplot(aes(x=time, y=cumhaz, group = condition)) + geom_line()
        ## join(dt_pred[, .(condition = sprintf("hazard.%s", seq(1:fnrow(dt_pred))),
        ##                  proxcnt10, popm_circle10)], on = "condition") %>%
        ## ggplot(aes(x=factor(proxcnt10), y=factor(popm_circle10), fill = mult)) + geom_tile()
        ## ggplot(aes(y=avghaz, x=proxcnt10, color = factor(popm_circle10))) + geom_line()

    }
}

## adjust line width
## dt_cutwidth <- dt_pmyear %>% copy %>%
##     .[, popm_circle10_cut := cut(popm_circle10,
##                                  breaks = quantile(dt_pmyear$popm_circle10, seq(0.05, 0.95, 0.1)),
##                                  labels = quantile(dt_pmyear$popm_circle10, seq(0.05, 0.85, 0.1)))] %>%
##     .[, popm_circle10_cut2 := cut(popm_circle10,
##                                   breaks = quantile(dt_pmyear$popm_circle10, seq(0.05, 0.95, 0.1)))] %>%
##     na.omit() %>% 
##     ## .[, .(ID, year, popm_circle10, popm_circle10_cut, popm_circle10_cut2)] %>%
##     .[, .N, .(proxcnt10, popm_circle10_cut = round(as.numeric(as.character(popm_circle10_cut)),5))]

gp_heatmap_info <- function(dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    #' plot distribution of PM-years across proxcnt-popm_circle10 space
    #' rounded to integers

    
    ## get the cells where data actually exists
    dt_cell_info <- dt_pmyear[, .(N = fnunique(ID)), .(proxcnt10, popm_circle10 = round(popm_circle10))] %>%
        .[, audience10 := popm_circle10/proxcnt10]

    ## combine cells and pred
    ## dt_topred_cplt <- cbind(dt_topred_cell, dt_pred_prep) %>%
    ##     .[, pmdens_circle10 := proxcnt10/popm_circle10]

    
    ## coverage plot of where data exists
    p1 <- dt_cell_info %>%
        ggplot(aes(x=proxcnt10, y= popm_circle10, fill = log(N), label = N)) +
        geom_tile() + 
        geom_text() +
        scale_fill_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj")

    ## coverage plot of where data exists
    p2 <- dt_cell_info %>%
        ggplot(aes(x=proxcnt10, y= popm_circle10, fill = N, label = N)) +
        geom_tile() + 
        geom_text() +
        scale_fill_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj")

    p5 <- dt_cell_info %>%
        ggplot(aes(x=proxcnt10, y= popm_circle10, fill = audience10, label = N)) +
        geom_tile() + 
        geom_text() +
        scale_fill_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj")



    ## observed closings
    dt_pred_obs <- dt_pmyear[, .(N = fnunique(ID), closing = sum(closing), OY = .N),
                             .(proxcnt10, popm_circle10 = round(popm_circle10))] %>%
        .[, `:=`(mort1 = closing/OY, mort2 = closing/N)]

    ## ## hmm this doesn't control for other variables.. also this mortality calculation is a complete mess

    p3 <- ggplot(dt_pred_obs, aes(x=proxcnt10, y=popm_circle10, fill = mort2,
                                  label = round(mort2, 2))) +
        geom_tile() + scale_fill_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj") +
        geom_text() +
        labs(caption = "mort2: closing/N(unique_ID)")

    p4 <- ggplot(dt_pred_obs, aes(x=proxcnt10, y=popm_circle10, fill = mort1,
                                  label = round(mort1, 2))) +
        geom_tile() + scale_fill_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj")  +
        geom_text() +
        labs(caption = "mort1: closing/OY")


    (p1 + p2) / (p3 + p4) / (p5 + p5)
                             

}


gp_pred_heatmap <- function(mdlname, l_mdls, dt_pmyear, vx, vy, add_cross) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    #' @param mdlname the model to choose from l_mdls
    #' @param l_mdls list of models
    #' @param dt_pmyear data.table with PM-year data
    #' @param vx variable to plot on x-axis
    #' @param vy variable to plot on y-axis
    #' @param add_cross whether to add cross vertical and horizontal lines for "sectors"
    #' this only really makes sense for interactions, not log variables
    
    #' the function generates a heatmap plot of the predicted 20-year hazard
    #' for a grid of present proxcnt and popm_circle10 values 

    ## generate pred (median/mode data)
    dt_pred_prep <- cbind(
        dt_pmyear[, lapply(.SD, Mode), # categorical/binary variables: use mode
                  .SDcols = gc_vvs()$dt_vrblinfo[vrbltype %in% c("bin", "cat"), achr(vrbl)]],
        dt_pmyear[, lapply(.SD, median), .SDcols = c("pmdens_cry", "PC1", "PC2", "year")]) # numeric: use median
    

    ## get the cells where data actually exists
    dt_topred_cell <- dt_pmyear[, .N, .(proxcnt10, popm_circle10, proxcnt10_log, popm_circle10_log)] %>% funique

    ## get values for 

    ## combine cells and pred, use update join, 
    dt_topred_cplt <- cbind(dt_topred_cell, dt_pred_prep) %>%
        .[, pmdens_circle10 := proxcnt10/popm_circle10] %>% ## add pmdens_circle10 in case it's needed
        .[, pmdens_circle10_log := log(pmdens_circle10 + 1)] %>%
        .[, audience10 := popm_circle10/(proxcnt10+1)] %>% # also add audience10 in case its needed
        .[, audience10_log := log(audience10)] 
    
    ## predicted for cells, and categorize
    dt_pred_cell <- gd_pred(mdlname, l_mdls, dt_pred = dt_topred_cplt, measure = "surv", year_range = 20) %>%
        join(dt_topred_cell, on = c("proxcnt10", "popm_circle10")) %>% # join frequency data
        .[, mort := 1-est] %>% # mortality calculation
        .[, .(mort = mean(mort)), .(vx = get(vx), vy = round(get(vy)))] %>% ## aggregate afterwards
        setnames(old = c("vx", "vy"), new = c(vx, vy))

    ## FIXME: could add parameter to control granularity of rounding

            
    ## construct cross vertical
    ## find the column where the overall slope is the smallest
    dt_cross <- data.table(
        cross_vert = dt_pred_cell[, .(var_mort = var(mort)), get(vx)][which.min(var_mort), get],
        cross_horiz = dt_pred_cell[, .(var_mort = var(mort)), get(vy)][which.min(var_mort), get])


    scale_ylorbr <- scale_color_YlOrBr(limits = dt_pred_cell[, c(min(mort), max(mort))],
                                       reverse = T, range = c(0, 0.88), guide = "none",
                                       scale_name = "jj") %>%
        chuck("palette")
    

    p_heatmap <- ggplot() +
        geom_tile(dt_pred_cell, mapping = aes(x=get(vx), y= get(vy), fill = mort, color = mort),
                  show.legend = T) +
        geom_rug(dt_pmyear, mapping = aes(x=get(vx), y= get(vy)),
                 position = position_jitter(width = 0.4), alpha = 0.1, linewidth = 0.1,
                 length = unit(8, "pt"),
                 sides = "tr") +  
        scale_fill_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj",
                          name = "Closing chance\nwithin 20 years") +
        scale_color_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj",
                           name = "Closing chance\nwithin 20 years") +
        theme_bw() +
        scale_y_continuous(expand = expansion(add = c(0, 0.5))) +
        scale_x_continuous(expand = expansion(add = c(0, 0.5))) + 
        ## coord_cartesian(expand = F) +
        theme(legend.position = "right",
              plot.tag.position = c(0.8, 0.3),
              legend.key.height = unit(1.5, "cm")) +
        labs(x=gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl == vx, vrbl_lbl],
             y = gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl == vy, vrbl_lbl])  
    ## p_heatmap

    if (grepl("_log", vy)) {
    
        p_heatmap <- p_heatmap +
            scale_y_continuous(
                ## expand = expansion(add = c(0, 0.5)),
                breaks = log((10**seq(2,7))/1e6),
                labels = \(x) scales::label_number(scale_cut = scales::cut_short_scale())(exp(x)*1e6))
    }


    if (add_cross == T) {
        p_heatmap <- p_heatmap +
            geom_vline(xintercept = dt_cross$cross_vert, linetype = "dashed") +
            geom_hline(yintercept = dt_cross$cross_horiz, linetype = "dashed")
    }
        

    return(p_heatmap)
    


        
}

## add a function for model without countryside
gp_pred_heatmap_wocryside <- gp_pred_heatmap
gp_pred_heatmap_onlycryside <- gp_pred_heatmap


gp_pred_heatmap_alt <- function(l_mdls, dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    #' @param l_mdlnames list of modelnames
    #' @param l_mdls list of models
    #' @param dt_pmyear data.table with PM-year data
    #' the function generates a heatmap plot of the predicted 20-year hazard
    #' for a grid of present proxcnt and popm_circle10 values
    #' for the models in l_mdlnames
    #' to quickly check different specifications

    theme_heatmap_alt <- function() {

        theme(legend.position = "bottom",
              axis.title.x = element_text(size = 11, margin = margin(0,0,0,0)),
              legend.spacing = unit(0, "pt"),
              legend.key.height = unit(8, "pt"),
              legend.box.spacing = unit(3, "pt"),
              legend.margin = margin(0,0,0,0))
    }


    p_pred_heatmap_comp1 <- gp_pred_heatmap(
        mdlname = "r_comp1",
        l_mdls = l_mdls,
        vx = "proxcnt10",
        vy = "popm_circle10_log",
        add_cross = T,
        dt_pmyear = dt_pmyear) +
        labs(title = "popm_circle10 (log)") +
        theme_heatmap_alt()

    p_pred_heatmap_pop42 <- gp_pred_heatmap(
        mdlname = "r_pop42",
        l_mdls = l_mdls,
        vx = "proxcnt10",
        vy = "popm_circle10",
        add_cross = T,
        dt_pmyear = dt_pmyear) +
        labs(title = "base + proxcnt10^2") +
        theme_heatmap_alt()


    p_pred_heatmap_dens3 <- gp_pred_heatmap(
        mdlname = "r_pmdens2",
        l_mdls = l_mdls,
        vx = "proxcnt10",
        vy = "popm_circle10",
        add_cross = F,
        dt_pmyear = dt_pmyear) +
        labs(title = "surv ~ PM density (log)") + 
        theme_heatmap_alt()

    p_pred_heatmap_audlog <- gp_pred_heatmap(
        mdlname = "r_audience_log1",
        l_mdls = l_mdls,
        vx = "proxcnt10",
        vy = "popm_circle10",
        add_cross = F,
        dt_pmyear = dt_pmyear) +
        labs(title = "survb ~ audience (log)") +
        theme_heatmap_alt()
    
    
    (p_pred_heatmap_comp1 + p_pred_heatmap_pop42) / (p_pred_heatmap_dens3 + p_pred_heatmap_audlog) 
        ## plot_layout(guides = "collect") ## scales not comparable

}

    
                
            

    



gp_pred_popprxcnt <- function(l_mdlnames, l_mdls, dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())

    #' generate plot of predicted avg hazard rate under different PM proximity counts and population numbers
    
    dt_predres_mult <- map(
        l_mdlnames,
        ~gd_pred(.x, l_mdls, gd_predprep_popprxcnt(dt_pmyear), measure = "surv", year_range = 20)) %>%
        rbindlist

    ## gd_pred("r_pop4", l_mdls, gd_predprep_popprxcnt(dt_pmyear), measure = "surv", year_range = 20)

    ## .[, popm_circle10_cut := round(popm_circle10, 5)]

    ## join(dt_predres_mult, dt_cutwidth, on = c("proxcnt10", "popm_circle10_cut")) %>% 
    ##     replace_NA %>% # fill up NAs in N with 0
    ##     .[proxcnt10 < 8] %>%

    l_quantile_probs <- c(0.1, 0.5, 0.75, 0.9)

    ## get quantiles via pmyear:
    l_popm_circle10_qntls <- dt_pmyear[, quantile(popm_circle10, probs = l_quantile_probs)] %>%
        round(digits = 2) %>% as.character

    ## construct filter dt (int-filtering not working correctly somehow)
    dt_popm_circle10_fltr <- data.table(popm_circle10_mult = l_popm_circle10_qntls)

    ## library(margins)
    ## margins(l_mdls$r_pop4, arg = "expected")
    ## library(marginaleffects)
    ## plot_comparisons(l_mdls$r_pop4, variables = list(popm_circle10 = c(1,5)),
    ## condition = "proxcnt10", type  = "lp")
    
    ## set up labels: has to be vector
    l_lbls <- c(
        sprintf("%sm pop. (%sth perc.)", l_popm_circle10_qntls, l_quantile_probs*100) %>%
        setNames(paste0("facet",l_popm_circle10_qntls)),
        setNames(l_mdlnames, l_mdlnames)) %>% unlist


    p_pred_popprxnct <- dt_predres_mult %>% copy %>%
        .[, popm_circle10_mult := as.character(popm_circle10)] %>% # set up merging column
        ## .[popm_circle10_mult == 209] %>% print(n=40)
        .[dt_popm_circle10_fltr, on = "popm_circle10_mult"] %>%
        .[, facet_popm_circle10 := paste0("facet", popm_circle10_mult)] %>%
        .[proxcnt10 < 12] %>% 
        ## .[popm_circle10_mult %in% l_popm_circle10_qntls] %>% 
        ## p_pred_popprxnct <- dt_predres_mult[popm_circle10*10 %in% c(1, 15, 30, 50)] %>% 
        ggplot(aes(x=proxcnt10, y=1-est, ymax = 1-upper, ymin = 1-lower,
                   group = factor(popm_circle10))) + 
        ## fill = factor(popm_circle10))) + 
        ## linewidth = N, alpha = N)) +
        geom_line(linewidth = 1) +
        geom_ribbon(alpha = 0.3) + 
        ## facet_wrap(~src, scales = "free") +
        ## scale_color_discrete(type = color("sunset")(4)) +
        ## scale_fill_discrete(type = color("sunset")(4)) + 
        ## coord_cartesian(ylim = c(0, 0.016), xlim = c(0,12)) +
        theme_bw() +
        theme(legend.position = "bottom") +
        labs(x=gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl == "proxcnt10", vrbl_lbl],
             y = "Predicted closing chance within 20 years,\n 95% CI")
    
    
    ## facet_grid(rows = vars(popm_circle10_mult))
    

    ## if multiple models, add facetting by model
    if (len(l_mdlnames) > 1) {
        
        p_pred_popprxnct <- p_pred_popprxnct +
            facet_grid(src~facet_popm_circle10, scales = "free", labeller = as_labeller(l_lbls))
    
        
    } else if (len(l_mdlnames) == 1) {

        p_pred_popprxnct <- p_pred_popprxnct +
            facet_grid(~popm_circle10_mult, labeller = as_labeller(l_lbls))    
        
    }

    
    

    return(p_pred_popprxnct)
    

    
}

gp_pred_proxcntpop <- function(l_mdlnames, l_mdls, dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())

    #' generate plot of predicted avg hazard rate under different PM proximity counts and population numbers
    
    dt_predres_mult <- map(
        l_mdlnames,
        ~gd_pred(.x, l_mdls, gd_predprep_popprxcnt(dt_pmyear), measure = "surv", year_range = 20)) %>%
        rbindlist

    l_quantile_probs <- c(0.5, 0.7, 0.825, 0.9)

    l_quantiles_proxcnt <- quantile(dt_pmyear$proxcnt10, probs = l_quantile_probs) %>% round(digits = 2) %>%
        as.character

    l_lbls <- sprintf("%s PMs 10km (%sth perc.)", l_quantiles_proxcnt, l_quantile_probs*100) %>%
        setNames(l_quantiles_proxcnt) %>%
        c(setNames(l_mdlnames, l_mdlnames)) %>% unlist
        

    
    p_pred_proxcntpop <- dt_predres_mult[proxcnt10 %in% l_quantiles_proxcnt] %>%
        ggplot(aes(x=popm_circle10, y=1-est, ymax = 1-upper, ymin = 1-lower,
                   group = factor(proxcnt10))) + 
        geom_line(linewidth = 1) +
        geom_ribbon(alpha = 0.3) + 
        theme_bw() +
        theme(legend.position = "bottom") +
        labs(x=gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl == "popm_circle10", vrbl_lbl],
             y = "Predicted closing chance within 20 years,\n 95% CI") +
        facet_grid(~proxcnt10, scales = "free", labeller = as_labeller(l_lbls))

    
    ## if multiple models, add facetting by model
    if (len(l_mdlnames) > 1) {
        
        p_pred_proxcntpop <- p_pred_proxcntpop + 
            facet_grid(src~proxcnt10, scales = "free", labeller = as_labeller(l_lbls))
        
    } else if (len(l_mdlnames) == 1) {

        p_pred_proxcntpop <- p_pred_proxcntpop + 
            facet_grid(~proxcnt10, labeller = as_labeller(l_lbls))    
    }

    return(p_pred_proxcntpop)
    
    ## .[, popm_circle10_cut := round

}


gp_condmef <- function(mdlname, l_mdls, dt_pmyear) {
    #' conditional effect plot
    #' @param dt_pmyear PM-year data
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())

    ## get the AME data, supress warnings LUL
    dt_condmef_popm_circle10 <- # suppressWarnings(
        slopes(chuck(l_mdls, mdlname), 
               variables = "proxcnt10",
               ## condition = "popm_circle10",
               type = "lp") %>% adt

    ## make the con
    p_condmef_popm_circle10 <- dt_condmef_popm_circle10 %>%
        ## .[, .(estimate = mean(estimate)), popm_circle10] %>%
        ggplot(aes(x=popm_circle10, y=exp(estimate), ymax = exp(conf.high), ymin = exp(conf.low))) +
        ## geom_point() +  # + geom_smooth(method = "lm")
        geom_line() +
        geom_hline(yintercept = 1, linetype = "dashed") +
        geom_rug(alpha = 0.05, sides = "b", linewidth = 0.02) + 
        geom_ribbon(alpha = 0.2) +
        labs(x = gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl == "popm_circle10", vrbl_lbl],
             y = sprintf("Hazard ratio of %s",
                         gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl == "proxcnt10", vrbl_lbl])) 

    ## generate data for condmef of proxcnt10
    dt_condmef_proxcnt10 <- suppressWarnings(
        slopes(chuck(l_mdls, mdlname), 
               variables = "popm_circle10",
               ## condition = "proxcnt10",
               type = "lp")) %>% adt

    p_condmef_proxcnt10 <- dt_condmef_proxcnt10 %>%
        ggplot(aes(x=proxcnt10, y = exp(estimate), ymax = exp(conf.high), ymin = exp(conf.low))) +
        geom_line() +
        geom_ribbon(alpha = 0.2) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        geom_rug(alpha = 0.05, sides = "b", linewidth = 0.02, position = position_jitter(width = 0.3)) +
        labs(x = gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl == "proxcnt10", vrbl_lbl],
             y = sprintf("Hazard ratio of %s",
                         gc_vvs() %>% chuck("dt_vrblinfo") %>% .[vrbl == "popm_circle10", vrbl_lbl])) + 
        ## caption = "Rugs on X-axis spread out to illustrate value distribution") +
        theme(plot.caption = element_text(margin = margin(t=-3)))
    
    ## dt_condmef_proxcnt10[proxcnt10 == 0, head(.SD,1)][, estimate]
    ## l_mdls$r_pop4 %>% coef %>% chuck("popm_circle10")

    ## l_mdls$r_pop4 %>% coef %>% chuck("proxcnt10")
    ## dt_condmef_popm_circle10[, .SD[which.min(popm_circle10)]][, estimate]

    p_condmef_popm_circle10 + p_condmef_proxcnt10


}

## ## look at distribution of PMs
## dt_pmyear %>%
##     .[, c(lapply(.SD, \(x) log(mean(x+1))), museum_status = max(closing)), ID,
##       .SDcols =c("proxcnt10", "popm_circle10")] %>% 
##     ggplot(aes(x=proxcnt10, y=popm_circle10, color = factor(museum_status))) +
##     geom_jitter(size = 0.8, width = 0.2)

gp_surv_death <- function(l_mdls, name_mainmdl) {
    #' generate survival plot by death/alive
    #' @param l_mdls list of models
    #' @param name_mainmdl name of the main model
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## set up the data to predict
    dt_pred_prep <- cbind(
        dt_pmyear[, lapply(.SD, Mode), # categorical/binary variables: use mode
                  .SDcols = gc_vvs()$dt_vrblinfo[vrbltype %in% c("bin", "cat"), achr(vrbl)]],
        dt_pmyear[, lapply(.SD, median), .SDcols = c("pmdens_cry", "year", "proxcnt10", "popm_circle10")])

    ## assing one as dead
    dt_pred_prep2 <- rbind(dt_pred_prep, dt_pred_prep) %>%
        .[2, founder_dead_binary := 1]

    ## predict for 30 years
    dt_pred_vis <- map(1:30, ~gd_pred("r_pop4", l_mdls, dt_pred_prep2, measure = "surv", year_range = .x) %>%
                  .[, `:=`(age = .x, founder_dead_binary = c(0,1))]) %>% rbindlist %>%
                  .[, founder_dead_binary := factor(founder_dead_binary)]

    leg_labels <- c("0" = "Alive", "1" = "Dead")

    dt_pred_vis %>% 
        ggplot(aes(x=age, y=est, ymax = upper, ymin=lower, group = founder_dead_binary,
                   color = founder_dead_binary, linetype = founder_dead_binary)) +                   
        geom_step(linewidth = 0.8) +
        geom_ribbon(alpha = 0.1, stat = "stepribbon", show.legend = F, linewidth = 0.2) +
        guides(color = guide_legend(title = "Founder status"),
               linetype = guide_legend(title = "Founder status")) +
        scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = leg_labels) +
        scale_linetype_manual(values = c("0" = "solid", "1" = "32"), labels = leg_labels) +
        theme(legend.position = "bottom",
              legend.spacing = unit(0, "pt"),
              legend.key.height = unit(0, "pt"),
              legend.box.spacing = unit(3, "pt"),
              legend.margin = margin(0,0,0,0)) +
        labs(x = "Age", y = "Survival")
    

}


gt_reg_coxph <- function(l_mdls, l_mdlnames) {
    #' collect some models from l_mdls and format them into nice custom regression table
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())

    
    l_mdls_slct <- map(l_mdlnames, ~chuck(l_mdls, .x))

    l_mdlres <- map2(l_mdls_slct, l_mdlnames, ~gd_reg_coxph(.x, .y, unit_name = "ID"))

    ## gd_reg_coxph(l_mdls_slct[[2]], l_mdlnames[[2]], unit_name = "ID")

    dt_coef <- map(l_mdlres, ~chuck(.x, "dt_coef")) %>% rbindlist
    dt_gof <- map(l_mdlres, ~chuck(.x, "dt_gof")) %>% rbindlist %>%
        .[, df := NULL]

    ## see how many units there are: use convention that model data.table is called "dt_"
    ## and that unit has name ID
    ## this is super not solid
    ## l_call_objs <- rx %>% chuck("call") %>% as.list
    ## dt_str <- l_call_objs[which(substring(l_call_objs, 1,3) == "dt_")]
    ## dt_str <- "dt_pmyear[year > 2010]"
    ## eval(parse(text = dt_str))
    
    
    ## dt_mdl <- get(dt_str)

    c_vvs <- gc_vvs()

    gt_reg(dt_coef,
           dt_gof,
           dt_vrblinfo = c_vvs$dt_vrblinfo,
           dt_ctgterm_lbls = c_vvs$dt_ctgterm_lbls,
           dt_gof_cfg = c_vvs$dt_gof_cfg,
           mdl_lbls = setNames(l_mdlnames, l_mdlnames)) %>%
        c(list(landscape = T))
    
    
}

gt_reg_coxph_density <- gt_reg_coxph
gt_reg_coxph_timeslice <- gt_reg_coxph
gt_reg_coxph_timecfg <- gt_reg_coxph
gt_reg_coxph_comp <- gt_reg_coxph
gt_reg_coxph_dens <- gt_reg_coxph
gt_reg_coxph_env <- gt_reg_coxph
gt_reg_coxph_af <- gt_reg_coxph
gt_reg_coxph_dimred <- gt_reg_coxph


gt_reg_coxph_reg <- function(l_mdls, l_mdlnames) {
    c(gt_reg_coxph(l_mdls, l_mdlnames),
      list(tabenv = "longtable", size = "small")) ## scalebox = 0.8), )
}




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


gt_reg_coxph_deathcfg <- function(l_mdls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' founder death robustness checks

    
    ## select models about death
    l_mdlnames <- keep(names(l_mdls), ~grepl("r_founder_dead[0-9]+", .x))
    l_mdls_slct <- l_mdls[l_mdlnames]

    ## put into tidy format
    l_mdlres <- map2(l_mdls_slct, l_mdlnames, ~gd_reg_coxph(.x, .y, unit_name = "ID"))

    ## combine into one table
    dt_death_fmtd <-

    map(l_mdlres, ~chuck(.x, "dt_coef")) %>% rbindlist %>%
        .[grepl("founder_dead", term)] %>% # only focus on death coefs
        .[, term_short := gsub("founder_dead[0-9]+", "", term)] %>% # yeet numbers from terms (use model instead)
        .[, cell_fmt := fmt_cell(coef = coef, pvalue = pvalue, se = se, type = "coef-se-stars"), .I] %>% # format
        dcast(mdl_name ~ term_short, value.var = "cell_fmt") %>% # variables as columns, models as rows
        ## extract "recent" length, add 1 for more understandable interpretation
        .[, death_recent_length := as.integer(str_extract(mdl_name, "[0-9]+"))+1L] %>% 
        .[order(death_recent_length), .(death_recent_length, recently_dead, long_dead)]

    ## construct colum names

    c_colnames <- gc_colnames(names(dt_death_fmtd),
                              setNames(c("Recent death length", "Recently dead", "Long dead"),
                                       names(dt_death_fmtd)))
                                                                                                     
    c_atr <- list(
        pos = list(-1),
        command = c_colnames)

    list(dt_fmtd = dt_death_fmtd,
         align_cfg = c("l", "r", rep("D{)}{)}{8)3}", 2)),
         hline_after = c(-1, nrow(dt_death_fmtd)),
         add_to_row = c_atr,
         number_cols = c(T, T,T))

}
         
                         


gd_drop1 <- function(l_mdls) {
    ## split formula into terms
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    main_formula <- gf_coxph_close()

    ## split formula into terms
    dt_terms <- data.table(term = terms(main_formula) %>% attr("term.labels")) %>%
        .[, type := fifelse(grepl(":", term), "interaction", 
                            fifelse(grepl("\\^2)$", term), "quadratic", "main"), "main")]
    
    ## get children
    dt_terms[, child := dt_terms$term[grepl(term, dt_terms$term) & term != dt_terms$term], .I]

    ## if (dt_terms[type == "interaction", .N] > 1) {stop("more than 1 interaction not supported")}

    l_mdls_drop1 <- split(dt_terms, 1:nrow(dt_terms)) %>%
        map(~na.omit(.[, c(term, child)])) %>% setNames(dt_terms$term) %>% 
        ## c(., setNames(dt_terms[type == "interaction", list(c(term, unlist(strsplit(term, ":"))))],
        ##               "allinteractions")) %>% 
        imap(~coxph(gf_coxph_close(vrbls_to_yeet = .x), dt_pmyear))
    
    ## summarize model performance, add comparison AIC
    dt_terms_eval <- l_mdls_drop1 %>% imap(~chuck(gd_reg_coxph(.x, mdl_name = .y), "dt_gof")) %>% rbindlist %>%
        .[, AIC_diff := AIC(l_mdls$r_pop4) - AIC]
        
    ## generate table of anova comparisons for p-value
    dt_drop1_anova <- imap(l_mdls_drop1, ~adt(anova(l_mdls$r_pop4, .x))[2][, mdl_name := .y]) %>% rbindlist
    
    ## anova(l_mdls$r_pop4, l_mdls_drop1$founder_dead_binary)
    

    ## combine anova and AIC terms
    dt_drop1 <- merge(dt_terms_eval, dt_drop1_anova, by = "mdl_name") %>%
        .[, .(mdl_name, AIC_diff, Chisq, p = `Pr(>|Chi|)`)] %>%
        .[, chisq_fmt := fmt_cell(Chisq, pvalue = p, type = "coef-stars"), .I]

    attr(dt_drop1, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_drop1)
}

gt_drop1 <- function(dt_drop1) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    
    ## get vrblinfo for order/labels
    dt_vrblinfo <- gc_vvs() %>% chuck("dt_vrblinfo")
    
    ## combine dt_drop1 with vrblinfo for labels/order
    dt_drop1_viz <- dt_vrblinfo %>% .[dt_drop1, on = c(vrbl = "mdl_name")] %>%
        .[, vrbl := factor(vrbl, levels = dt_vrblinfo$vrbl)] %>%
        .[order(vrblgrp, vrbl)]

    ## select columns
    dt_drop1_viz2 <- copy(dt_drop1_viz)[, .(grp = "", vrbl_lbl = latexTranslate(vrbl_lbl), AIC_diff, chisq_fmt)]

    ## set up column names, signote, group names
    dt_grpstrs <- gc_grpstrs(dt_drop1_viz, grp = "vrblgrp_lbl", nbr_cols = 3)

    signote <- gc_signote(se_mention = F, ncol = 2)

    ## signote <- sprintf("\\hline \n \\multicolumn{%s}{l}{\\footnotesize{%s}}\\\\\n", 3, "jj")

    c_colnames <- gc_colnames(names(dt_drop1_viz2),
                              setNames(c("", "Variable dropped", "AIC difference", "Chisq"), names(dt_drop1_viz2)))

    ## 
    ## , signote

    c_atr <- list(
        pos = c(list(-1, nrow(dt_drop1_viz2)), dt_grpstrs$pos),
        command = c(c_colnames, signote, dt_grpstrs$grpstr))

    list(dt_fmtd = dt_drop1_viz2,
         align_cfg = c("l", "l", "l", "r", "D{.}{.}{5}"),
         hline_after = c(-1),
         add_to_row = c_atr,
         number_cols = c(F, T,T))

}
    





