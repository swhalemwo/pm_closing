## * closing playgroung
## ** docstring tests
f2 <- function(v1) {
    #' test function
    #' 
    #' prints kappa
    #' also prints kappa
    #' also prints v1
    #'
    #' @param v1 parameter that goes nowhere
    #' @param v2 parameter that doesn't exist
    #' @return fucking nothying
    
    print(v1)
}

f2(12)
f1()
## find non-standardized columns

## ** new  plotting framework tests

gplt("p_vrblcvrg")
dpltR("p_vrblcvrg")
gdplt("p_vrblcvrg")
wplt("p_vrblcvrg")

gwdplt("p_vrblcvrg")

## ** dimensionality reduction fun

rotatedLoadings2 <- promax(rawLoadings)$loadings
gd_dimred_loads(rotatedLoadings2) %>% gp_dimred_loads


l_pcares_psych_urtd <- psych::principal(dt_pca_prepped, rotate = "none", nfactors = len(vrbls_dimred1)) 

l_pcares_psych5 <- psych::principal(dt_pca_prepped, rotate = "varimax", nfactors = 5) 
l_pcares_psych_urtd5 <- psych::principal(dt_pca_prepped, rotate = "none", nfactors = 5) 

## look at effect of rotation on eigenvalue distribution: rotation makes eigenvalues more equal
l_pcares_psych5$Vaccounted[, 1:5]
l_pcares_psych_urtd5$Vaccounted[, 1:5]



l_pcares_psych5$loadings %>% gd_dimred_loads %>% gp_dimred_loads


gd_dimred_loads(l_pcares_psych_urtd$loadings) %>% .[dim %in% paste0("dim", 1:5)] %>% gp_dimred_loads

## manual variance prop calculation, but kinda unnecessary, can use get_eigenvalue instead
(l_pcares_prcomp$sdev^2/sum(l_pcares_prcomp$sdev^2)) %>% gp_scree

## check psych eigenvalues 
prop.table(l_pcares_psych$values) %>% gp_scree

## manual scree plotting
corx <- cor(dt_pca_prepped)
scree(corx)

## super old pca tests
## library(factoextra)
fviz_pca_var(l_pcares)

fviz_contrib(l_pcares, choice = "var", axes = 2)

l_pcares
                                          

pcares <- prcomp(, scale = T)

## slt(dt_pmdb, vrbls_dimred1) %>% fsum %>% 

## ** some manual selection on what counts as promising, don't think it really is good to capture what's going on
penl_vrbls <- .c(gvtsupport, donorprogram, endowment, sponsorship, rentalpossblt, staff_size, clctn_size,
                 cafe_restrnt, webshop, museumshop)


## dt_vrblcvrg_all %>% 
##     ## .[!grepl("^act_", vrbl)] %>%  # exclude activities
##     .[, rel_var := fifelse(vrbl %in% penl_vrbls, "penl", "not_penl")] %>% 
##     ggplot(aes(x=value, y=vrbl, color = museum_status)) +
##     geom_jitter(width= 0, height = 0.3) +
##     theme(legend.position = "bottom") +
##     facet_grid(rel_var ~ . , space = "free", scales = "free")


## ** combine with dt_pmdb_excl: check how much of the lack of availability is due to incomplete standardization
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



dt_pmdb_excl_splong <- gd_pmdb_excl_splong(dt_pmdb_excl, vrbls_tocheck)

dt_vrblcvrg_excl <- merge(
    dt_pmdb_excl_splong[, .(all_PMs = sum(!is.na(value))/.N), vrbl],
    dt_pmdb_excl_splong[, .(vlus_present = sum(!is.na(value))/.N), .(vrbl, museum_status)] %>%
    dcast(vrbl ~ museum_status, value.var = "vlus_present"), on = "vrbl") %>%
    melt(id.vars = "vrbl") %>% .[, src := "pmdb_excl"] %>%
    .[, rel_var := fifelse(vrbl %in% penl_vrbls, "penl", "not_penl")] 

## combine both coverage dts
dt_vrblcvrg_cbnd <- rbind(dt_vrblcvrg_all, dt_vrblcvrg_excl) %>% 
    .[order(src, variable, value)] %>% # order by private museum completeness (most entities there)
    .[, vrbl := factor(vrbl, levels = unique(vrbl))]

## just focus on where the coverage discrepancy between 
dcast(dt_vrblcvrg_cbnd, vrbl + variable ~ src) %>%
    fmutate(diff = abs(pmdb - pmdb_excl)) %>%
    sbt(diff > 0.02)


## not sure if this is worth maintaining:
## already broken as of <2023-09-25 ma>,
## and even if some improvement can be gained by standardization, it isn't much,
## and won't do much about the fact that for yuge number of variables the data is just missing


## ** test gt
## absolute clown package -> u.s.e.l.e.s.s.
install.packages("gt")

library(gt)

gt_gttest <- function(dtx) {

    tx <- dtx %>% adt(keep.rownames = "model") %>% .[, disp := disp-100] %>% head %>%
        .[disp <100, disp := disp*0.9832] %>% 
        .[mpg == 21, drat := drat + 0.0123] %>%
        ## .[, gear := sprintf("\\textbf{%s}", gear)] %>% 
        .[, .(model, mpg, disp, drat, wt, gear)] %>%   
        gt(groupname_col = "gear", caption = "t_gttest") # groupname_col to get groups,


    tx2 <- tx %>% cols_label(.list = list(mpg = "MPG", wt = "woto")) %>% # rename columns, can pass arguments as list to .list
        tab_spanner(label = "meeee", columns = 1:3) %>% # add one spanner: each call can only add one
        tab_header("i like dogs", subtitle = "LUL") %>% # doesn't seem to have location at the bottom
        tab_style(style = cell_text(weight = "bold"),
                  locations = cells_row_groups()) %>% # can make the columns bold, probably others as well
        tab_style(style = cell_text(
                      weight = "bold"
                      ## style = "italic"
                  ),
                  location = cells_body(rows = 1:3)) %>%
        tab_footnote("i like cake") %>%
        tab_options(data_row.padding = px(20),
                    row_group.padding = px(20))
    

    return(tx2)

}

l_tbls2 <- list(
    t_gttest = gt_gttest(mtcars))

gtsave(l_tbls2$t_gttest, filename = "~/Dropbox/phd/papers/closing/tabbles/t_gttest.tex")



wtbl2("t_gttest")
dtblF("t_gttest")

wtbl_pdf("t_gttest", F)
