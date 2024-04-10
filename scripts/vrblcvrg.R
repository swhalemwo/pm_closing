## * variable coverage functions

gd_pmdb_splong <- function(dt_pmdb) {
    gw_fargs(match.call())
    #' generate super long PM dt, ;
    ## FIXME: this func now includes a bunch of configuration (vrbls_relchars)
    ## which is probably better as external argument/config

    ## get relevant char columns: convert "" to NA
    vrbls_relchars <- .c(
        museum_status, iso3c, nationality, gender, ticket_price, opng_time, nbr_visitrs, website, mission, 
        staff_size, buildgtype, slfidfcn, city, clctn_med_fcs, clctn_cry_fcs, clctn_reg_fcs, clctn_modctmp,
        insta_handle, architect, industry)
    
    ## set variables to check: all numeric vrbls, selected char vrbls, yeet llid
    vrbls_tocheck <- c(
        setdiff(num_vars(dt_pmdb, return = "names"), .c(llid)), vrbls_relchars)

    ## setdiff(names(dt_pmdb), vrbls_tocheck)

    ## replace empty string with NA, use variables to check
    dt_pmdb_splong <- tfmv(dt_pmdb, vars = vrbls_relchars, FUN = \(x) replace(x, x=="", NA)) %>%
        fselect(vrbls_tocheck) %>%
        melt(id.vars = c("ID", "museum_status"), variable.name = "vrbl")

    attr(dt_pmdb_splong, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmdb_splong)
}


gd_vrblcvrg <- function(dt_pmdb_splong, all_statuses) {
    gw_fargs(match.call())
    
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate dataframe with coverage of variables,
    #' depending all_statuses for all pmdb_status values (open, closed, no longer PM) as well as overall average
    #' if !all_statuses: only for open and closed

    if (all_statuses) {

        ## variable coverage for open, closed, no_longer, and overall
        dt_vrblcvrg <- merge(
            dt_pmdb_splong[, .(all_PMs = sum(!is.na(value))/.N), vrbl], # overall cpltns
            ## prop_cpltns by status
            dt_pmdb_splong[, .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%
            dcast(vrbl ~ museum_status, value.var = "vlus_present"), on = "vrbl") %>%
            .[order(`private museum`)] %>% .[, vrbl := factor(vrbl, levels = vrbl)] %>% 
            melt(id.vars = "vrbl", variable.name = "museum_status") %>% .[, src := "pmdb"]

    } else if (!all_statuses) {
        
        ## only proportions of open and closed
        dt_vrblcvrg <- dt_pmdb_splong[museum_status %in% c("private museum", "closed"),
                                      .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%
            dcast(vrbl ~ museum_status, value.var = "vlus_present") %>%
            .[order(`private museum`)] %>% .[, vrbl := factor(vrbl, levels = vrbl)] %>% 
            melt(id.vars = "vrbl", variable.name = "museum_status") %>% .[, src := "pmdb"]
    }

    ## no good reason to maintain vrbl coverage which isn't grouped
    ## if I don't wanna use it in plot, can just not use it, but always should have option
    ## move the variable grouping step here as well
    dt_vrblgrps <- gc_pmdb_vrblgrps(dt_pmdb)

    dt_vrblcvrg_grpd <- dt_vrblgrps[dt_vrblcvrg, on = "vrbl"] %>%
        .[, vrbl := factor(vrbl, levels = levels(dt_vrblcvrg$vrbl))]

    attr(dt_vrblcvrg_grpd, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_vrblcvrg_grpd)

}

## check which variables are not considered in dt_vrblgrps
## are variables that are grouped (all pmbd variables are), but not used (e.g. technical)
## setdiff(gc_pmdb_vrblgrps(dt_pmdb)$vrbl, dt_vrblcvrg_all$vrbl)



gc_pmdb_vrblgrps <- function(dt_pmdb) {
    #' generate list of thematic variable groups of PMDB vrbls,
    

    gw_fargs(match.call())
    
    l_vrblgrps <- list(
        sm = .c(insta_handle, insta_flwrs, insta_posts, fb_flwrs, fb_likes, google_rating, google_nbrrvws,
                trpadvsr_rating, trpadvsr_nbrrvws, twitter_flwrs, insta_bluetick, youtube_flwrs),
        founder = .c(gender, birthyear, deathyear, founder_gvrnc, an_nyears, an_lyear, an_fyear, founder_wealth,
                     nationality, industry, founder_name, founder_weal_ustd, founder_id),
        clctn = .c(clctn_gnr_fcs, realism, clctn_modctmp, clctn_reg_fcs, avbl_clctnhldngs,
                   clctn_med_fcs, clctn_size,
                   clctn_med_fcs_nms, clctn_cry_fcs),
        identity = .c(mission, avbl_legalstruct, slfidfcn, muem_fndr_name, foundation, avbl_gvrncstruct, 
                      staff_diversity),
        relations = .c(gvtsupport, donorprogram, endowment, sponsorship, cooperation),
        operations = c(keep(names(dt_pmdb), ~grepl("^act_", .x)), ## all the activities
                       .c(cafe_restrnt, avbl_floorsize, avbl_exhibsize, museumshop, buildgtype, website,
                          reducedtickets, staff_size, rentalpossblt, webshop, nbr_visitrs, ticket_price,
                          opng_time, temp_exhibs, avbl_exhibhist, architect)),
        existence = .c(city, iso3c, multiplelocs, year_opened, year_closed, lat, long, address_formatted), #
        technical = .c(ID, name, museum_status, llid, origin)
    )

    dt_vrblgrps <- imap(l_vrblgrps, ~data.table(grp = .y, vrbl = .x)) %>% rbindlist
    if (len(setdiff( names(dt_pmdb), dt_vrblgrps$vrbl)) >0) {
        stop(sprintf("vrbls has typos, or not all variables are grouped: %s",
                     paste0(setdiff(names(dt_pmdb), dt_vrblgrps$vrbl), collapse = ",")))
    }
    ## print(setdiff(names(dt_pmdb), dt_vrblgrps$vrbl))

    attr(dt_vrblgrps, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_vrblgrps)
}


gl_vrblcvrg <- function(dt_pmdb) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())

    #' bundle all the variable coverage objects in one list for more comfy handling
    
    dt_pmdb_splong <- gd_pmdb_splong(dt_pmdb)

    dt_vrblcvrg_all <- gd_vrblcvrg(dt_pmdb_splong, all_statuses = T)
    dt_vrblcvrg_fcs <- gd_vrblcvrg(dt_pmdb_splong, all_statuses = F)

    l_vrblcvrg <- list(dt_pmdb_splong = dt_pmdb_splong,
                       dt_vrblcvrg_all =dt_vrblcvrg_all,
                       dt_vrblcvrg_fcs = dt_vrblcvrg_fcs)

    attr(l_vrblcvrg, "gnrtdby") <- as.character(match.call()[[1]])

    return(l_vrblcvrg)
    
}


gp_vrblcvrg_ugrpd <- function(dt_vrblcvrg, yeet_acts) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## mtcars %>% adt %>% 
    ##     ggplot(aes(x=cyl, y=gear)) +
    ##     ## geom_point()
    ##     geom_beeswarm()

    ## vignette("usageExamples")
        
    
    dt_vrblcvrg %>% # .[value > 0.95] %>% 
        .[if(yeet_acts) !grepl("^act_", vrbl) else T] %>% # filtering out activities if requested
        ggplot(aes(x=value, y=vrbl, color = museum_status)) +
        geom_beeswarm(side = 0) + 
        ## geom_jitter(width= 0, height = 0.3) +
        theme(legend.position = "bottom") +
        labs(x="proportion data available")
    
}

gp_vrblcvrg <- function(dt_vrblcvrg_grpd, yeet_acts) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' variable coverage, facetted by group
    
    dt_vrblcvrg_grpd %>% # .[value > 0.95] %>%
        .[if(yeet_acts) !grepl("^act_", vrbl) else T] %>% # filtering out activities if requested
        ggplot(aes(x=value, y=vrbl, color = museum_status)) +
        geom_beeswarm(side = 0) + 
        theme(legend.position = "bottom") +
        facet_grid(grp ~ ., scales = "free", space = "free", switch = "y") + 
        theme(strip.text.y.left = element_text(angle = 0)) +
        labs(x="proportion data available", y = element_blank())
}

gp_vrblcvrg_ratio <- function(dt_vrblcvrg) {
    gw_fargs(match.call())
    #' variable coverage with log points


    dcast(dt_vrblcvrg, grp + vrbl ~ museum_status) %>%
        .[, ratio := log(`private museum`/closed)] %>% # calculate open/closed ratio
        replace_Inf() %>% # set cases where no closed have value to NA
        melt(id.vars = .c(grp, vrbl), variable.name = "museum_status") %>%
        ## set order of actors 
        .[, xfacet := factor(fifelse(museum_status == "ratio", "log(prop_open/prop_closed)", "prop"),
                             levels = c("prop", "log(prop_open/prop_closed)"))] %>% 
        ggplot(aes(x=value, y=vrbl, color = museum_status)) +
        geom_beeswarm(side = 0) + 
        theme(legend.position = "bottom") +
        facet_grid(grp ~ xfacet, scales = "free", space = "free_y", switch = "y") + 
        theme(strip.text.y.left = element_text(angle = 0)) +
        labs(x="data availability", y = element_blank()) +
        theme_closing()
}
