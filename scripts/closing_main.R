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

## LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/")
## LOCS$FIGDIR <- paste0(FIG

## ** functions

## *** config functions

gc_dirs <- function(dir_proj) {
    list(
        proj = dir_proj,
        figs = paste0(dir_proj, "figures/"),
        tbls = paste0(dir_proj, "tables/"),
        data = paste0(dir_proj, "data/")
    )
}

        
gc_plts <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    l_pltcfgs <- list(
        p_vrblcvrg_ugrpd = list(
            dt_vrblcvrg = quote(dt_vrblcvrg_all),
            yeet_acts = F,
            caption = "PMDB variable coverage by museum status",
            width = 19,
            height = 24),
        p_vrblcvrg = list(
            dt_vrblcvrg = quote(dt_vrblcvrg_all),
            yeet_acts = F,
            caption = "PMDB variable coverage by museum status and variable group",
            width = 19,
            height = 24),
        p_vrblcvrg_ratio = list(
            dt_vrblcvrg = quote(dt_vrblcvrg_fcs),
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

gd_vrblcvrg <- function(dt_vrbl_splong, all_statuses) {
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



gd_dimred_loads <- function(loadmat) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate long data.frame of dimensionality reduction loadings
    #'
    #' @param loadmat: loadings matrix
    #' @value data.table with columns vrbl, dim, value

    dt_dimred_loads <- matrix(data = as.numeric(loadmat),
                              nrow = nrow(loadmat),
                              dimnames = list(rownames(loadmat), paste0("dim", 1:ncol(loadmat)))) %>% 
                              ## dimnames = attributes(loadmat)$dimnames) %>% 
                              ## dimnames = as.list(paste0("dim", 1:ncol(loadmat)))) %>%
        adt(keep.rownames = "vrbl") %>%
        melt(id.vars = "vrbl", variable.name = "dim") %>%
        .[, dim := factor(dim, levels = sort(unique(dim)))]

        ## .[dim %in% paste0("dim", 1:10) ] %>%
        ## .[, dim := factor(dim, levels = paste0("RC", 1:10))]

    attr(dt_dimred_loads, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_dimred_loads)
}


gd_pmdb_splong <- function(dt_pmdb) {
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

gd_pmdb_excl_splong <- function(dt_pmdb_excl, vrbls_tocheck) {
    gw_fargs(match.call())
    ## generate coverage of dt_pmdb_excl (before cleaning) to assess
    ## contribution of cleaning to (lack of) data coverage

    ## actually pretty unneccessary since my cleaning doesn't really change coverage, but meaning:
    ## presence of string is converted into availability (e.g. clctnhldngs)

    ## set up dt with names for comparison
    ## recycle some code from gd_pmdb_excl
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
    
    
    ## select relevant vars, rename, fill up empty strings with NA, melt to superlong
    dt_pmdb_excl_splong <- slt(dt_pmdb_excl, dt_rename_list$vrbl_orig) %>%
        frename(setNames(dt_rename_list$vrbl_new, dt_rename_list$vrbl_orig)) %>%
        slt(vrbls_tocheck) %>% # only select the ones to check
        tfmv(vars = names(.), FUN = \(x) replace(x, x=="", NA)) %>%
        melt(id.vars = c("ID", "museum_status"), variable.name = "vrbl")

    attr(dt_pmdb_excl_splong, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_pmdb_excl_splong)
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


gp_dimred_loads <- function(dt_dimred_loads) {
    gw_fargs(match.call())
    #' plot factor loadings with ggplot in col + facetted
    #'
    #' @param dt_dimred_loads long data.frame with columns vrbl, dim (PCA/EFA outcome), value (loading)
    
    ## ADDME: automatic ordering of rows

    ggplot(dt_dimred_loads, aes(x=abs(value),y=vrbl, fill = value)) +
        geom_col() +
        facet_grid(. ~ dim) +
        scale_fill_gradient2(high = "red", low = "blue")
}

gp_scree <- function(scree_vlus) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' plot the scree plot (just column plot of vector
    #'
    #' param scree_vlus vector of scree values

    data.table(value = scree_vlus) %>% .[, dim_nbr := factor(1:.N)] %>%
        ggplot(aes(x=dim_nbr, y=value)) + geom_col() 
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



dt_pmdb_excl <- gd_pmdb_excl(only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")] # yeet bad PMs
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)
    


## ** variable selection

dt_pmdb_splong <- gd_pmdb_splong(dt_pmdb)

dt_vrblcvrg_all <- gd_vrblcvrg(dt_pmdb_splong, all_statuses = T)
dt_vrblcvrg_fcs <- gd_vrblcvrg(dt_pmdb_splong, all_statuses = F)

## plot variable coverage by museum type

    
## check which variables are not considered in dt_vrblgrps
## are variables that are grouped (all pmbd variables are), but not used (e.g. technical)
setdiff(gc_vrblgrps(dt_pmdb)$vrbl, dt_vrblcvrg_all$vrbl)


## ratio calculations

# generate plots and write them to file
walk(names(gc_plts()), ~lapply(c(gplt, wplt), \(f) f(.x)))

## write numbers
dt_nbrs <- gd_nbrs()
fwrite(dt_nbrs, paste0(c_dirs$tbls, "tbl_nbrs.csv"), quote = F)


## callgraph testing
jtls::gwd_clgrph()
dpltF("callgraph")
## dpltF("p_vrblcvrg")
## gdplt("p_vrblcvrg_ratio")



    
stop("dimred not ready")

## ** dimension reduction fun
## will have different variable sets, e.g. whether founder should be there or not
## doesn't matter so much now which variables to use, just set up framework for plotting
vrbls_dimred1 <- setdiff(num_vars(dt_pmdb, return = "names"),
                         .c(llid, ID, # technical
                            ## all time related
                            year_opened, year_closed, birthyear, deathyear, an_fyear, an_lyear, an_nyears))

dt_pca_prepped <- slt(dt_pmdb, vrbls_dimred1) %>%
    tfmv(vars = names(.), FUN = replace_NA)

l_pcares_prcomp <- prcomp(dt_pca_prepped, scale=T)

ncomp <- 2 ## len(vrbls_dimred1)
rawLoadings <- l_pcares_prcomp$rotation[,1:ncomp] %*% diag(l_pcares_prcomp$sdev, ncomp, ncomp) # diag = eigenvalues?
rotatedLoadings <- varimax(rawLoadings)$loadings


## calculating/inspecting scores, in particular size
scale(dt_pca_prepped) %*% rawLoadings %>% adt %>% cbind(dt_pmdb[, .(ID, name, iso3c)]) %>% adt %>%
    .[order(-V1)] %>% print(n=20) # hmm looks not unconvincing: the big museums are all quite well known
## social media is probably what makes Saatchi so big: shrinks basically all other to zeroes on all social medias

## check SE again if calculations are correct: uses rotmat, not loadings to calculate scores
## https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r

    
## scores <- scale(l_pcares$x) %*% varimax(rawLoadings)$rotmat %>% adt

library(psych)
l_pcares_psych <- psych::principal(dt_pca_prepped, rotate = "varimax", nfactors = ncomp) 


gd_dimred_loads(l_pcares_psych$loadings) %>% .[dim %in% paste0("dim", 1:ncomp)] %>% gp_dimred_loads # psych
gd_dimred_loads(l_pcares_prcomp$rotation) %>% .[dim %in% paste0("dim", 1:ncomp)] %>% gp_dimred_loads # unrtd prcomp
gd_dimred_loads(rotatedLoadings) %>% .[dim %in% paste0("dim", 1:ncomp)] %>% gp_dimred_loads # rotated prcomp

## X11()

library(factoextra)
fviz_screeplot(l_pcares_prcomp, choice = "variance")

l_pcares_psych$values %>% gp_scree
l_pcares_psych$Vaccounted[1,] %>% gp_scree # scree plot of rotated factors -> not so useful










