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
