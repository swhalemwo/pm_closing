## * closing central file

## * stuff to load

## ** libraries

args <- commandArgs(trailingOnly = T)
options(width = 115)

## detach("package:pmdata", unload = T)

library(jtls)
library(pmdata)
library(memoise)
library(collapse)
library(purrr)
library(docstring)

## LOCS <- list(PROJDIR = "/home/johannes/Dropbox/phd/papers/closing/")
## LOCS$FIGDIR <- paste0(FIG

## ** functions

## *** config functions

gc_locs <- function(dir_proj) {
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
        p_vrblcvrg = list(
            dt_vrblcvrg = quote(dt_vrblcvrg),
            yeet_acts = F,
            caption = "PMDB variable coverage by museum status",
            width = 9,
            height = 12),
        p_asdf = list(
            dtx = quote(mtcars),
            width = 10,
            height = 20,
            caption = "kappa")
    )

    ## check that there are no duplicate 
    if (any_duplicated(names(l_pltcfgs))) stop("duplicated names")

    ## check that all entries have width, heigh, caption
    if (!all(unlist(map(l_pltcfgs, ~all(c("width", "height", "caption") %in% names(.x)))))) {
        stop("not all configs have width, height and caption")}

    
    return(l_pltcfgs)
        
}

## *** data generation


gd_dimred_loads <- function(loadmat) {
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

    return(dt_dimred_loads)
}


gd_pmdb_excl_splong <- function(dt_pmdb_excl, vrbls_tocheck) {
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

    return(dt_pmdb_excl_splong)
}

## *** plotting

gp_vrblcvrg <- function(dt_vrblcvrg, yeet_acts) {

    dt_vrblcvrg %>% 
        .[if(yeet_acts) !grepl("^act_", vrbl) else T] %>% # filtering out activities if requested
        ggplot(aes(x=value, y=vrbl, color = variable)) +
        geom_jitter(width= 0, height = 0.3) +
        theme(legend.position = "bottom")
    
}

gp_dimred_loads <- function(dt_dimred_loads) {
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
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' plot the scree plot (just column plot of vector
    #'
    #' param scree_vlus vector of scree values

    data.table(value = scree_vlus) %>% .[, dim_nbr := factor(1:.N)] %>%
        ggplot(aes(x=dim_nbr, y=value)) + geom_col() 
}



## * main
if (interactive()) {stop("it's interactive time")}


## set up constants used for object management
c_dirs <- gc_locs(dir_proj = "/home/johannes/Dropbox/phd/papers/closing/") ## project dirs
PMDATA_LOCS <- gc_pmdata_locs() # pmdata source
l_plts <- list() # list of plots
c_pltargs <- list() # arguments to pass to gc_plts



dt_pmdb_excl <- gd_pmdb_excl(only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")] # yeet bad PMs
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = T)
    

## ** variable selection

## get relevant char columns: convert "" to NA
vrbls_relchars <- .c(
    museum_status, iso3c, nationality, gender, ticket_price, opng_time, nbr_visitrs, website, mission, 
    staff_size, buildgtype, slfidfcn, city, clctn_med_fcs, clctn_cry_fcs, clctn_reg_fcs, clctn_modctmp,
    insta_handle)
    
## set variables to check: all numeric vrbls, selected char vrbls, yeet llid
vrbls_tocheck <- c(
    setdiff(num_vars(dt_pmdb, return = "names"), .c(llid)), vrbls_relchars)

## replace empty string with NA, use variables to check
dt_pmdb_splong <- tfmv(dt_pmdb, vars = vrbls_relchars, FUN = \(x) replace(x, x=="", NA)) %>%
    fselect(vrbls_tocheck) %>%
    melt(id.vars = c("ID", "museum_status"), variable.name = "vrbl")




## plot variable coverage by museum type


dt_vrblcvrg <- merge(
    dt_pmdb_splong[, .(all_PMs = sum(!is.na(value))/.N), vrbl],
    dt_pmdb_splong[, .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%  # prop_cpltns by status
    dcast(vrbl ~ museum_status, value.var = "vlus_present"), on = "vrbl") %>%
    .[order(`private museum`)] %>% .[, vrbl := factor(vrbl, levels = vrbl)] %>% 
    melt(id.vars = "vrbl") %>% .[, src := "pmdb"]


gdplt("p_vrblcvrg")
wdplt("p_vrblcvrg")



penl_vrbls <- .c(gvtsupport, donorprogram, endowment, sponsorship, rentalpossblt, staff_size, clctn_size,
                 cafe_restrnt, webshop, museumshop)


dt_vrblcvrg %>% 
    ## .[!grepl("^act_", vrbl)] %>%  # exclude activities
    .[, rel_var := fifelse(vrbl %in% penl_vrbls, "penl", "not_penl")] %>% 
    ggplot(aes(x=value, y=vrbl, color = variable)) +
    geom_jitter(width= 0, height = 0.3) +
    theme(legend.position = "bottom") +
    facet_grid(rel_var ~ . , space = "free", scales = "free")



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

ncomp <- 5 ## len(vrbls_dimred1)
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






## ** combine with dt_pmdb_excl: check how much of the lack of availability is due to incomplete standardization
dt_pmdb_excl_splong <- gd_pmdb_excl_splong(dt_pmdb_excl, vrbls_tocheck)

dt_vrblcvrg_excl <- merge(
    dt_pmdb_excl_splong[, .(all_PMs = sum(!is.na(value))/.N), vrbl],
    dt_pmdb_excl_splong[, .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%
    dcast(vrbl ~ museum_status, value.var = "vlus_present"), on = "vrbl") %>%
    melt(id.vars = "vrbl") %>% .[, src := "pmdb_excl"] %>%
    .[, rel_var := fifelse(vrbl %in% penl_vrbls, "penl", "not_penl")] 

## combine both coverage dts
dt_vrblcvrg_cbnd <- rbind(dt_vrblcvrg, dt_vrblcvrg_excl) %>% 
    .[order(src, variable, value)] %>% # order by private museum completeness (most entities there)
    .[, vrbl := factor(vrbl, levels = unique(vrbl))]

## just focus on where the coverage discrepancy between 
dcast(dt_vrblcvrg_cbnd, vrbl + variable ~ src) %>%
    fmutate(diff = abs(pmdb - pmdb_excl)) %>%
    sbt(diff > 0.02)




    
