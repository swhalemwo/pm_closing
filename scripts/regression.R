## * regression (survival analysis)





## ** data functions

gd_af_size <- function(dt_pmx) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## get AF dts 
    dt_matches_pmdb_af <- gd_af_pmdb_matches()
    dt_af_exhbs <- gd_af_exhbs()[, iso3c := countrycode(CountryName, "country.name", "iso3c")] %>%
        .[, CountryName := NULL]
    
    ## doesn't seem a lot of issues
    ## dt_af_exhbs[CountryName == "Kosovo"]
    ## dt_af_exhbs[is.na(iso3c), .N, CountryName]

    ## expand to pm_year UoA
    dt_pmyear_size <- copy(dt_pmx)[, last_year := fifelse(museum_status == "closed", year_closed, END_YEAR)] %>%
        .[, .(year = year_opened:last_year), .(ID, iso3c, year_opened, year_closed)]
                                               

    ## add info for merging AF data to PDMB
    dt_pmdb_af <- join(dt_pmx[, .(ID, name, iso3c, museum_status)], dt_matches_pmdb_af,
                       on = c("ID" = "PMDB_ID"), how = "left", verbose = F) %>%
        .[, InstitutionID := as.integer(AF_IID)]

    ## merge AF exhibitions data to PMDB
    dt_af_exhbs_pmdb <- join(dt_af_exhbs[, .(ID, InstitutionID, begin_year = year(BeginDate), iso3c)],
                         dt_pmdb_af[, .(InstitutionID, PMDB_ID = ID, name)],
                         on = "InstitutionID", how = 'left', verbose = 0)

    ## first construct grid (expand institution-exhibition link to institution year (fill gaps)):
    ## all possible combinations with full join?
    dt_af_grid <- join(dt_af_exhbs_pmdb[!is.na(begin_year), .(begin_year = min(begin_year):max(begin_year)),
                          .(InstitutionID, PMDB_ID, iso3c)], # AF data
                       dt_pmyear_size[, .(ID, year, iso3c)],
                       on = c(PMDB_ID = "ID", begin_year = "year", "iso3c"), how = "full", overid = 2, verbose = 0)

    ## debug: more rows with non-NA PMDB_ID than in dt_af_size
    ## dt_af_grid[!is.na(PMDB_ID), .N, .(PMDB_ID, begin_year)][N > 1]
    ## dt_af_grid[PMDB_ID == 620] %>% print(n=30)
    ## seems to be that CountryName can vary within Institution: institutions can organize stuff elsewhere,
    ## doesn't seem much tho, only handful cases
              
         
    ## join with institution counts
    dt_af_qntlprep <- join(dt_af_grid,
                           dt_af_exhbs_pmdb[, .N, .(InstitutionID, begin_year)],
                           on = c("InstitutionID", "begin_year"), verbose = 0) %>%
        replace_NA(cols = "N", value = 0) %>% # fill up no exhbs with 0
        .[!is.na(iso3c)] # yeet ~300 institutions without countrycode
 
    ecdf_fun <- function(x,perc) ecdf(x)(perc) # get quantile of value

    ## rolling sum of last 5 years
    dt_af_roll <- copy(dt_af_qntlprep) %>%
        .[, org_id := sprintf("AF%s-PM%s", InstitutionID, PMDB_ID)] %>% # need to combine PMDB and AF IDs
        .[order(org_id, begin_year)] %>% 
        .[, paste0("rollsum_prep", 1:5) := shift(N, 0:4), org_id] %>% # set up shift columns
        .[, `:=`(exhbrollsum5 = rowSums(.SD, na.rm = T), # summing: include those with NAs
                 exhbnNA = Reduce(`+`, lapply(.SD, is.na))), # get nbr of NA indicator
          .SDcols = paste0("rollsum_prep", 1:5)] %>%
        .[, paste0("rollsum_prep", 1:5) := NULL] %>% # yeet prep columns
        .[, exhbrollsum_avg := exhbrollsum5/(5-exhbnNA)] %>%
        .[, exhbrollany := fifelse(exhbrollsum5 == 0, 0, 1)] %>% 
        .[, exhbqntl_roll := ecdf_fun(exhbrollsum_avg, exhbrollsum_avg), begin_year] %>%
        .[!is.na(PMDB_ID)]
    
    ## dt_af_roll[iso3c == "DEU" & !is.na(PMDB_ID)] %>%
    ##     melt(id.vars = c("PMDB_ID", "begin_year"), measure.vars = c("quantile_roll")) %>%
    ##     ggplot(aes(x=begin_year, y=value, color = variable)) + facet_wrap(~PMDB_ID) + geom_line()
        
    ## dt_af_roll[!is.na(PMDB_ID)]    

    ## calculate quantile using all orgs, filter down to PMs
    ## dt_af_qntl <- copy(dt_af_qntlprep)[, quantile := ecdf_fun(N, N), .(iso3c, begin_year)] %>%
    dt_af_qntl_simple <- copy(dt_af_qntlprep) %>%
        .[, exhbany := fifelse(all(N == 0), 0, 1), InstitutionID] %>% 
        .[, exhbcnt := N] %>% # renaming
        .[, exhbqntl_cy := ecdf_fun(N, N), .(begin_year, iso3c)] %>% # quantile by country year
        .[, exhbqntl_year := ecdf_fun(N, N), .(begin_year)] %>% # quantile by year
        .[, exhbprop_top10_utf := N/quantile(N, probs = 0.90), begin_year] %>%  # prop by year
        .[, exhbprop_top10_log := log(N+1)/quantile(log(N+1), probs = 0.90), begin_year] %>% # prop (log) by year
        .[!is.na(PMDB_ID)]
                ## focus on PMs
        ## only include CYs where half of active PMs have at least one show

    ## combine "simple quantiles" (dt_af_qntl) and rolled sums (dt_af_roll),
    ## yeet CYs where more than half institutions have no exhibitions
    dt_af_qntl <- join(dt_af_qntl_simple,
                       dt_af_roll[, .(PMDB_ID, begin_year, exhbrollsum5, exhbrollany,  exhbnNA,
                                      exhbrollsum_avg, exhbqntl_roll)],
         on = c("PMDB_ID", "begin_year"), verbose = 0) %>%
        .[, .SD[!any(N == 0 & exhbqntl_cy > 0.5)], .(iso3c, begin_year)] 

    ## dt_af_qntl[, .SD[any(N == 0 & quantile > 0.5)], .(iso3c, begin_year)] %>%
    ##     .[, .N, iso3c] %>% print(n=300)

    ## ggplot(dt_af_qntl[iso3c == "USA"], aes(x=begin_year, y=quantile, group = PMDB_ID)) +
        ## geom_line(alpha = 0.1, position = position_jitter(width = 0.2,  height = 0.02))

    ## ggplot(dt_af_qntl[exhbprop_top10_utf > 0], aes(x= exhbprop_top10_utf)) + geom_density()

    attr(dt_af_qntl, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_af_qntl)
    
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



gd_pmyear_prep <- function(dt_pmx, dt_pmtiv, c_dtti = c_dtti) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' @param dt_pmx extract of PMDB with museums to use
    #' @param dt_pmtiv dt with time invariant variables
    #' @c_dtti flags/switches of which datasets to optionally/additionally include
    

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
        .[, `:=`(tstart = age, tstop = age+1)] # set survival time interval variables
        

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
        .[, pmdens_circle10 := (proxcnt10)/popm_circle10] # 

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
    if ("af_size" %in% c_dtti) {
        
        dt_af_size <- gd_af_size(dt_pmx)[, .(ID = PMDB_ID, year = begin_year,
                                             exhbany, exhbrollany, # any inclusions in last 1/5 years
                                             exhbqntl_year, exhbqntl_cy, exhbcnt, # simple quantiles
                                             exhbprop_top10_log, exhbprop_top10_utf, # proportions of top10
                                             exhbrollsum5, exhbnNA, exhbrollsum_avg, exhbqntl_roll)]

        dt_pmyear_waf <- join(dt_pmyear_wproxcnt, dt_af_size, on = c("ID", "year"))
    } else {
        dt_pmyear_waf <- dt_pmyear_wproxcnt
    }


    ## combine with time-invariant variables
    dt_pmyear_wtiv <- join(dt_pmyear_waf,
                        copy(dt_pmtiv)[, `:=`(iso3c=NULL, name = NULL)], ## yeet non-essential columns
                        on = "ID") 

    if (any(is.na(dt_pmyear_wtiv$mow))) {stop("some MOW is NA")}

    

    ## yeet unused variables
    dt_pmyear_wtiv[, `:=`(museum_status = NULL, year_closed = NULL, deathyear = NULL)]

    
    attr(dt_pmyear_wtiv, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmyear_wtiv)

}

gd_pmyear <- function(dt_pmyear_prep, c_dtti) {
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
    if ("af_size" %in% c_dtti) {
        vrbls_tolag <- c(vrbls_tolag,
                         keep(names(dt_pmyear_prep), ~startsWith(.x, "exhb")))
    }

    dt_pm_lagged <- dt_pmyear_prep[order(year), .SD, ID] %>% copy() %>% 
        .[, (vrbls_tolag) := shift(.SD), ID, .SDcols = vrbls_tolag]

    ## filter out missing values
    ## don't filter on exhbqntl yet.. still iffy -> FIXME
    ## i guess i'm not doing this year to not yeet too much? but easier with switch.. 
    ## dt_pmyear <- dt_pm_lagged[!is.na(an_inclusion) & !is.na(popm_circle10)]
    
    dt_pmyear <- na.omit(dt_pm_lagged, cols = vrbls_tolag)

    
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

gd_pmtiv <- function(dt_pmx, l_pca_dimred_woclosed) {
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

    ## add PCA scores
    dt_pmtiv_wpca <- join(dt_pmtiv, l_pca_dimred_woclosed$dt_scores[, .(ID, PC1, PC2)], on = "ID")

    attr(dt_pmtiv_wpca, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmtiv_wpca)


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
        r_less1 = coxph(Surv(tstart, tstop, closing) ~ mow + pmdens_cry + I(pmdens_cry^2), dt_pmyear),
        r_less2 = coxph(Surv(tstart, tstop, closing)~ founder_dead + mow + slfidfcn + muem_fndr_name, dt_pmyear),

        ## fullest model:
        ## FIXME: add founder_dead*muem_fndr_name
        ## r_more = coxph(Surv(tstart, tstop, closing) ~ gender + pmdens_cry + I(pmdens_cry^2) + mow +
        ##                    slfidfcn + founder_dead + muem_fndr_name + an_inclusion + popm_circle10 +
        ##                    proxcnt10,
        ##                dt_pmyear),

        
        r_pop4 = coxph(Surv(tstart, tstop, closing) ~ gender + pmdens_cry + I(pmdens_cry^2) + mow +
                            slfidfcn + founder_dead + muem_fndr_name + an_inclusion +
                            proxcnt10*popm_circle10,
                       dt_pmyear),

        r_pop4_wyr = coxph(Surv(tstart, tstop, closing) ~ gender + pmdens_cry + I(pmdens_cry^2) + mow +
                            slfidfcn + founder_dead + muem_fndr_name + an_inclusion +
                            proxcnt10*popm_circle10 + year + reg6,
                           copy(dt_pmyear)[, reg6 := factor(reg6, labels = c("OC", "NALUL","EU","AS", "LA", "AF"))])
                       
        
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
    #' generate prediction DT

    ## variables that don't change
    dt_pred_prep <- cbind(
        dt_pmyear[, lapply(.SD, Mode), # categorical/binary variables: use mode
                  .SDcols = gc_vvs()$dt_vrblinfo[vrbltype %in% c("bin", "cat"), achr(vrbl)]],
        dt_pmyear[, lapply(.SD, median), .SDcols = c("pmdens_cry", "PC1", "PC2")]) # numeric: use median

    ## variables to vary              
    dt_pred_prep2 <- expand.grid(proxcnt10 = c(0:15),
                                 popm_circle10 = c(0.01, 0.3, 1, 3, 5))
    ## quantile(dt_pmyear$popm_circle10, seq(0.05, 0.95, 0.1)))
                                                              
    dt_pred <- cbind(dt_pred_prep2, dt_pred_prep) %>% adt %>%
        .[, pmdens_circle10 := proxcnt10/popm_circle10]

    return(dt_pred)

}


gd_pred <- function(mdlname, l_mdls, dt_pred) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate average predicted hazard for first 20 years for all the specifications in dt_pred
    #' @param rx a coxph-based regression model
    #' @param dt_pred data frame with all variables and different values (one per row) for each specification


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
    
    basehaz(chuck(l_mdls, mdlname), dt_pred) %>% adt %>%
        ## transform each settings cumhaz into hazard and take mean
        .[time < 20, lapply(.SD, \(x) mean(x - shift(x),  na.rm = T)), .SDcols = patterns("^hazard")] %>%
        melt(measure.vars = patterns("^hazard"), variable.name = "condition",
             value.name = "avghaz") %>%
        join(dt_pred[, .(condition = sprintf("hazard.%s", seq(1:fnrow(dt_pred))),
                         proxcnt10, popm_circle10)], on = "condition") %>%
        .[, src := mdlname]
    ## ggplot(aes(x=time, y=cumhaz, group = condition)) + geom_line()
    ## join(dt_pred[, .(condition = sprintf("hazard.%s", seq(1:fnrow(dt_pred))),
    ##                  proxcnt10, popm_circle10)], on = "condition") %>%
    ## ggplot(aes(x=factor(proxcnt10), y=factor(popm_circle10), fill = mult)) + geom_tile()
    ## ggplot(aes(y=avghaz, x=proxcnt10, color = factor(popm_circle10))) + geom_line()

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


gp_pred_popprxcnt <- function(l_mdlnames, l_mdls, dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' generate plot of predicted avg hazard rate under different PM proximity counts and population numbers

    dt_predres_mult <- map(l_mdlnames,
                           ~gd_pred(.x, l_mdls, gd_predprep_popprxcnt(dt_pmyear))) %>% rbindlist
    ## .[, popm_circle10_cut := round(popm_circle10, 5)]

## join(dt_predres_mult, dt_cutwidth, on = c("proxcnt10", "popm_circle10_cut")) %>% 
##     replace_NA %>% # fill up NAs in N with 0
##     .[proxcnt10 < 8] %>%

    p_pred_popprxnct <- dt_predres_mult %>% 
        ggplot(aes(x=proxcnt10, y=avghaz, group = popm_circle10, color = factor(popm_circle10))) + 
        ## linewidth = N, alpha = N)) +
        geom_line(linewidth = 2) +
        ## facet_wrap(~src, scales = "free") +
        scale_color_discrete(type = color("sunset")(5)) + 
        coord_cartesian(ylim = c(0, 0.013), xlim = c(0,12)) +
        theme_bw()

    ## if multiple models, add facetting by model
    if (dt_predres_mult[, fnunique(src) > 1]) {
        
        p_pred_popprxnct <- p_pred_popprxnct +
            facet_wrap(~src, scales = "free")
        
    }

    return(p_pred_popprxnct)
        

    
}

## ## look at distribution of PMs
## dt_pmyear %>%
##     .[, c(lapply(.SD, \(x) log(mean(x+1))), museum_status = max(closing)), ID,
##       .SDcols =c("proxcnt10", "popm_circle10")] %>% 
##     ggplot(aes(x=proxcnt10, y=popm_circle10, color = factor(museum_status))) +
##     geom_jitter(size = 0.8, width = 0.2)
    

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

    gt_reg(dt_coef,
           dt_gof,
           dt_vrblinfo = c_vvs$dt_vrblinfo,
           dt_ctgterm_lbls = c_vvs$dt_ctgterm_lbls,
           dt_gof_cfg = c_vvs$dt_gof_cfg,
           mdl_lbls = setNames(l_mdlnames, l_mdlnames)) %>%
        c(list(landscape = T))
    
    
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
