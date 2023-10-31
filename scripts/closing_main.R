## * closing central file

## * stuff to load

## ** libraries

args <- commandArgs(trailingOnly = T)
options(width = 115)

library(jtls)
library(pmdata)
library(memoise)
library(collapse)
library(purrr)
library(docstring) # for annotating 
library(ggbeeswarm) # move points up/down on vrblcvrg plots.. hopefully avoid
library(survival) 
library(ggsurvfit) # easy visualizations of survival objects, hopefully avoid
library(countrycode) 
library(muhaz) # for smooth hazard curves
library(patchwork) # for stichting plots together, hopefully avoid
library(texreg, include.only = c("screenreg", "texreg")) ## inspection of results
library(tinytest) # for looking at pmdata tests
library(parallel) # parallel processing

## LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/")
## LOCS$FIGDIR <- paste0(FIG

## ** functions

## *** config functions

gc_dirs <- function(dir_proj) {
    list(
        proj = dir_proj,
        figs = paste0(dir_proj, "figures/"),
        tbls = paste0(dir_proj, "tables/"),
        data = paste0(dir_proj, "data/"),
        code = paste0(dir_proj, "scripts/"),
        misc = paste0(dir_proj, "misc/")
    )
}

gc_vvs <- function() {
    #' generate variable vectors

    list(
        ## time-invariant variables
        vrbls_base = .c(ID, iso3c, year, tstart, tstop, age),
        vrbls_tiv = .c(gender, slfidfcn, muem_fndr_name, mow, west, reg6),
        vrbls_tv= .c(pm_density, founder_dead)

    )
}

        
gc_plts <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    l_pltcfgs <- list(
        p_vrblcvrg_ugrpd = list(
            dt_vrblcvrg = quote(l_vrblcvrg$dt_vrblcvrg_all),
            yeet_acts = F,
            caption = "PMDB variable coverage by museum status",
            width = 19,
            height = 24),
        p_vrblcvrg = list(
            dt_vrblcvrg = quote(l_vrblcvrg$dt_vrblcvrg_all),
            yeet_acts = F,
            caption = "PMDB variable coverage by museum status and variable group",
            width = 19,
            height = 24),
        p_vrblcvrg_ratio = list(
            dt_vrblcvrg = quote(l_vrblcvrg$dt_vrblcvrg_fcs),
            caption = "PMDB variable coverage (abs/rel prop) by museum status and variable group",
            width = 18,
            height = 24),
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
            width = 14,
            height = 8),
        p_agedens = list(
            dt_pmcpct = quote(dt_pmcpct),
            caption = "Age distribution of private museums",
            width = 17,
            height = 16)        
    )

    ## check that there are no duplicate 
    if (any_duplicated(names(l_pltcfgs))) stop("duplicated names")

    ## check that all entries have width, heigh, caption
    if (!all(unlist(map(l_pltcfgs, ~all(c("width", "height", "caption") %in% names(.x)))))) {
        stop("not all configs have width, height and caption")}

    
    return(l_pltcfgs)
        
}

gc_vrblgrps <- function(dt_pmdb) {
    #' generate list of thematic variable groups
    gw_fargs(match.call())
    
    l_vrblgrps <- list(
        sm = .c(insta_handle, insta_flwrs, insta_posts, fb_flwrs, fb_likes, google_rating, google_nbrrvws,
                trpadvsr_rating, trpadvsr_nbrrvws, twitter_flwrs, insta_bluetick, youtube_flwrs),
        founder = .c(gender, birthyear, deathyear, founder_gvrnc, an_nyears, an_lyear, an_fyear, founder_wealth,
                     nationality, industry, founder_name, founder_weal_ustd),
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
        existence = .c(city, iso3c, multiplelocs, year_opened, year_closed), #
        technical = .c(ID, name, museum_status, llid, origin)
    )

    dt_vrblgrps <- imap(l_vrblgrps, ~data.table(grp = .y, vrbl = .x)) %>% rbindlist
    if (len(setdiff(dt_vrblgrps$vrbl, names(dt_pmdb))) >0) {
        stop("vrbls has typos, or not all variables are grouped")
    }
    ## print(setdiff(names(dt_pmdb), dt_vrblgrps$vrbl))

    attr(dt_vrblgrps, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_vrblgrps)
}

## *** data generation

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
    dt_vrblgrps <- gc_vrblgrps(dt_pmdb)

    dt_vrblcvrg_grpd <- dt_vrblgrps[dt_vrblcvrg, on = "vrbl"] %>%
        .[, vrbl := factor(vrbl, levels = levels(dt_vrblcvrg$vrbl))]

    attr(dt_vrblcvrg_grpd, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_vrblcvrg_grpd)

}

## check which variables are not considered in dt_vrblgrps
## are variables that are grouped (all pmbd variables are), but not used (e.g. technical)
## setdiff(gc_vrblgrps(dt_pmdb)$vrbl, dt_vrblcvrg_all$vrbl)





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



## *** plotting



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




## *** nbrs generation
gd_nbrs <- function() {
    
    dt_ynkplt <- gc_ynkplt()
    dt_refplt <- gc_refplt()

    dt_nbrs_cbnd <- rbind(dt_ynkplt, dt_refplt)


    return(dt_nbrs_cbnd)
}



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



## * main
if (interactive()) {stop("it's interactive time")}


## set up constants used for object management
c_dirs <- gc_dirs(dir_proj = "/home/johannes/Dropbox/phd/papers/closing/") ## project dirs
PMDATA_LOCS <- gc_pmdata_locs() # pmdata source
l_plts <- list() # list of plots
c_pltargs <- list() # arguments to pass to gc_plts
system(sprintf("rm %s", paste0(c_dirs$tbls, "farg_calls.csv")))


dt_pmdb_excl <- gd_pmdb_excl(only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")] # yeet bad PMs
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)

## variable coverage
l_vrblcvrg <- gl_vrblcvrg(dt_pmdb)

if ("memoised" %!in% class(gd_mow_info)) {
    gd_mow_info <- memoise(gd_mow_info) # memoizing gd_mow_info: saves the fread of 55k file
}

END_YEAR <- 2021

source(paste0(c_dirs$code, "regression.R"))

dt_pmx <- gd_pmx(dt_pmdb)
dt_pmtiv <- gd_pmtiv(dt_pmx)

dt_pmyear <- gd_pmyear(dt_pmx, dt_pmtiv)
dt_pmcpct <- gd_pmcpct(dt_pmyear)

l_mdls <- gl_mdls(dt_pmyear, dt_pmcpct)

screenreg2(list(l_mdls$r_more))

screenreg2(list(l_mdls$r_west_cpct, l_mdls$r_west_year, l_mdls$r_west_year2), digits = 4)



## generate plots and write them to file
## FIXME: parallelize this: can use some overarching mclapply
## mclapply(names(gc_plts()), \(x) lapply(c(gplt, wplt), \(f) f(x)), mc.cores = 6)
## mclapply seems to break gwd_clgrph (?????) for whatever reason.. 
walk(names(gc_plts()), ~lapply(c(gplt, wplt), \(f) f(.x)))

## write numbers
dt_nbrs <- gd_nbrs()
fwrite(dt_nbrs, paste0(c_dirs$tbls, "tbl_nbrs.csv"), quote = F)


## callgraph testing
jtls::gwd_clgrph()
## dpltF("callgraph2")
## dpltF("p_vrblcvrg")
## gdplt("p_vrblcvrg_ratio")


