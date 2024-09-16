## * update pmdb variables

dt_pmdb_excl <- gd_pmdb_excl(sel =  "all")
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)


l_colclsfcn <- list(
    technical = c(# technical columns
        "Database updates", "Comments, additional info", "Museum_email",
        "IG_date checked_LL", "Museum_website", 
        "IG_handle_LL", "Facebook URL", "Twitter handle", "Youtube URL", "Tripadvisor URL", # maybe scraping
        "Source", "Responsible1", "Responsible2", "Possible reason to exclude1", "Posssible reason to exclude2",
        "Comments 1st responsible", "Comments 2nd responsible", "Why not in our report LL", "Reason LL",
        "Evaluation LL", "Comments CN","Comments RL","Comments JB", "Comments/remarks Luise",
        "Collector_id_larryslist"
    ),
    obsolete = c(
        "country", # iso3c is better
        "Museum_years_between_death_and_museum_closure", # inferable from deathyear and year_closed
        "Museum_region", # can be inferred from iso3c
        "Museum_age_founder_at_opening", # inferable from birthyear and year_opened
        "Collection_medium_focus_dummy", # inferable from Collection_medium focus/clctn_med_fcs
        "Collection_medium_nonmainstream", # inferable from Collection_medium focus/clctn_med_fcs
        "Collection_medium_any_focus_non_mainstream_item", # inferable from clctn_med_fcs
        "Collection_medium_list", # inferable from clctn_med_fcs
        ## "Collection_medium_non maintream list", # inferable from clctn_med_fcs
        "Collection genre focus_nonstandardized", # not standardized
        "Founder_artnews_status increase", # artnews has to be redone
        "Founder_artnews_status decrease", # artnews has to be redone
        "Foundation name", # CHECKME: not relevant atm, but maybe later for branches
        "Collection_country focus_nonstandardized",  # clctn_cry_fcs has it better
        "Private donor names", # no value
        "Sponsor names", # no value (sponsorship already has presence)
        "IG_accounttype_LL", # something like what kind of institution?
        "Number of instagram followers", # use "IG_followers_LL" -> insta instead
        "Number of Instagram posts_old", # use IG_posts_LL instead
        "Opening year additional info", # nobody cares
        "Industry founder_LL", # not solid, our industry is better
        "Industry of founder or family of founder_old" # our new industry (ISIC) is better
    ), 
    stdzd_elsewhere = c( # columns that are already standardized in a separate column
        "year_opened_str", "year_closed_str", "Architect", "Deathyear founder", 
        "Birthyear founder", "Ticket price", "Opening times", "Reduced ticket price",
        "Number of visitors", "Industry founder comments_LS", "Staff size", "Founder - director",
        "Founder - board", 
        "Size of collection", "Building type", "Restaurant or café", "Cooperation with other museums",
        "Government support", "Building rental possibility", "Private donors",
        "Educational / outreach / social / artistic programs", "pmdb_actvts" # now separate activities
    ),
    lildata = c( # principally interesting, but data coverage super limited
        "Museum_subsidy income",
        "Donor income", 
        "Endowment size", # also endowment already there
        "Sponsorship income", # 
        "Annual budget", # not also not standardized
        "Assets" # also not standardized
    ))
    ## ## columns to consider including in gd_pmdb
##     ## not-uncommented ones are 
##     v_csdrcols <- c(
##         ## "Museum_reduced ticket groups", # capabilities/accessibility?
##     ## "Museum_multiple locations", # relevant but should be modelled better
##     ## "Museum_city", # density
##     ## "Museum_cooperation with other musea_standardized", # pretty straightforward
##     ## "Museum_mission", # might be interesting for text embedding
##     ## "Museum_staff diversity", # maybe just binary?
##     ## "Collection_holdings available", # capability
##     ## "Foundation", # transparency? capability?
##     ## "Museum_legal structure", # presence -> transparency? itself not standardized,  standardization unfeasible
##     ## "Museum_governance structure", # presence -> transparency? not standardized and standardization unfeasible
##     ## "Floor size", # presence -> transparency?
##     ## "Exhibition size", # presence -> transparency?
##     "Indoor facilities", # might be worth standardizing: library, hotel, edu_facilities(studio, workplace, auditorm)
##     "Outdoor facilities" # nice coverage
##     ## "IG_posts_LL", # coverage only for currently open PMs
##     ## "IG_blue tick_LL", # having tick is conditional on having account
##     ## "Number of Facebook likes", # probably our collection,
##     ## "Google rating", #  why not lul
##     ## "Nationality_founder_LL" # huh could be interesting, but data is so limited
## )
                

dt_colclsfcn <- imap(l_colclsfcn, ~data.table(vrbl = .x, exclude_reason = .y)) %>% rbindlist

l_cols_already_processed <- c(
    .c(ID, name, museum_status, iso3c, year_opened, year_closed), # basic columns set up in gd_pmdb_excl
    pmdata::gc_rename_list() %>% unname %>% unlist, # input columns: get standardized later
    dt_colclsfcn$vrbl
) 

setdiff(names(dt_pmdb_excl), l_cols_already_processed)

dt_exclnames <- data.table(name_excl = names(dt_pmdb_excl))

dt_rename_list <- c(list(ID = "ID",
                         country = "country",
                         iso3c = "iso3c",
                         name = "name",
                         year_opened =  "year_opened_str",
                         year_closed = "year_closed_str",
                         museum_status = "museum_status"),
                    gc_rename_list()) %>%     
    map(~vrbl_fndr(dt_pmdb_excl, .x)) %>% 
    imap(~list(vrbl_orig = .x, vrbl_new = .y)) %>% rbindlist

## first combine excl names and renamed ones
dt_vrblsets <- dt_rename_list[dt_exclnames, on = .(vrbl_orig == name_excl)] %>% 
    dt_colclsfcn[., on = .(vrbl = vrbl_orig)] %>% # then add reasons for variable exclusions
    .[vrbl %!in% c("year_opened", "year_closed", "V54")] # yeet year columns (already processed in gd_pmdb_excl)

## check which are not dealth with properly: indoor/outdoor facilities
dt_vrblsets[is.na(exclude_reason) & is.na(vrbl_new)]

## write to file 
## fwrite(dt_vrblsets[order(exclude_reason, na.last = F)], paste0(c_dirs$data, "pmdb_update/variable_sets.csv"))



## data fillup
vrbls_tofillup <- .c(year_opened, year_closed, Museum_city, country, Founder_gender, pmdb_actvts,
                     "Outdoor facilities", "Indoor facilities", "Museum_institutional self-identification")



## generate data to fill up 
dt_pmdb_fillup <- dt_pmdb_excl[museum_status %in% c("private museum", "no longer a private museum", "closed"),
             c("ID", "name",  "museum_status", vrbls_tofillup), with = F]

## look at affected variables: just year_opened/year_closed?
dt_pmdb_fillup[museum_status != "no longer a private museum"] %>%
    melt(id.vars = "ID") %>% .[, .N, .(is.na(value), variable)] %>% .[is.na==T]

## year_opened
dt_pmdb_fillup[is.na(year_opened) & museum_status != "no longer a private museum",
               .(ID, name, museum_status, year_opened)] %>% print(n=200)

## year_closed
dt_pmdb_fillup[museum_status == "closed" & is.na(year_closed)]
