## * dimred code

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


gp_dimred_loads <- function(dt_dimred_loads, include_row_clusters = F) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    #' plot factor loadings with ggplot in col + facetted
    #'
    #' @param dt_dimred_loads long data.frame with columns vrbl, dim (PCA/EFA outcome), value (loading)
    
    ## ADDME: automatic ordering of rows
    ## use pairwise distance

    ## use factor loadings to construct pairwise variable similarity
    dt_dimred_wide <- dcast(dt_dimred_loads, vrbl ~ dim)
    mat_rowdists <- dt_dimred_wide[, .SD, .SDcols = patterns("^dim")] %>% dist
    clusters_row <- hclust(mat_rowdists, method = "ward.D2")

    
    ## assign clusters back to data: first construct data.table with cluster of variable
    dt_vrbl_cluster <- dt_dimred_wide[, .(vrbl)] %>%
        .[, cluster := clusters_cut <- cutree(clusters_row, dt_dimred_loads[, fnunique(dim)])]
    ## than match that to the entire loadings
    dt_dimred_loads_clustered <- dt_vrbl_cluster[dt_dimred_loads, on = "vrbl"]

    ## sort rows so that they are ordered in the section where they have the most mass
    dt_clusters_ordered <- dt_dimred_loads_clustered[, .(mass = sum(abs(value))), .(cluster, dim)] %>%
        .[, .SD[which.max(mass)], cluster, .SDcols = "dim"] %>% # for each cluster pick largest factor 
        .[order(dim)]

    ## in the end: want rows in an order within cluster: select from dt_dimred_loads_clustered
    ## some arbitrary reordering necessary (-abs(value), rev) due to ggplot nonsense
    row_ordered <- dt_dimred_loads_clustered[dt_clusters_ordered, on = .(cluster, dim)] %>%
        .[, .SD[order(-abs(value))][, vrbl], .(dim, cluster), ] %>% .[, V1] %>% rev
    
    
    ## assign order of rows and row clusters/sections
    dt_dimred_loads_clustered[, `:=`(vrbl = factor(vrbl, levels = row_ordered),
                                     cluster = factor(cluster, dt_clusters_ordered[, cluster]))]


    p_dimred_loads <- ggplot(dt_dimred_loads_clustered, aes(x=abs(value),y=vrbl, fill = value)) +
        geom_col() +
        scale_fill_gradient2(high = "red", low = "blue")

    ## include row clusters, if so specified (debugging)
    if (include_row_clusters) {
        p_dimred_loads <- p_dimred_loads + 
            facet_grid(cluster ~ dim, scales = "free", space = "free_y")
    } else if (!include_row_clusters) {
        p_dimred_loads <- p_dimred_loads + 
            facet_grid( ~ dim, scales = "free", space = "free_y")
    }

    return(p_dimred_loads)
    
}

gp_scree <- function(scree_vlus, dims_to_display = 10) {
    gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' plot the scree plot (just column plot of vector
    #'
    #' param scree_vlus vector of scree values

    data.table(value = scree_vlus) %>% .[, dim_nbr := factor(1:.N)] %>%
        .[dim_nbr %in% seq(1, dims_to_display)] %>% 
        ggplot(aes(x=dim_nbr, y=value)) +
        geom_col() +
        geom_point() + 
        geom_line(mapping = aes(group = 1))
    
}


gl_pca <- function(dt_pmdb, vrbls_dimred, ncomp) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1
    #' calculate PCA scores of vrbls
    #' @param dt_pdmb dt_pmdb
    #' @param vrbls variables to include in PCA
    #' @param ncomp number of factors to extract
    
    dt_pca_prepped <- slt(dt_pmdb[museum_status %in% c("private museum", "closed")], vrbls_dimred) %>%
        tfmv(vars = names(.), FUN = replace_NA) 
    
    ## actual PCA, scale = T
    l_pcares_prcomp <- prcomp(dt_pca_prepped, scale=T)

    
    rawLoadings <- l_pcares_prcomp$rotation[,1:ncomp, drop = F] %*% diag(l_pcares_prcomp$sdev, ncomp, ncomp)
    ## diag = eigenvalues?

    ## rotate, processing depends on number of factors
    if (ncomp > 1) {
        rotatedLoadings <- varimax(rawLoadings)$loadings
    } else if (ncomp == 1) {
        rotatedLoadings <- varimax(rawLoadings)
    }


    ## calculating/inspecting scores, in particular size
    dt_scores <- scale(dt_pca_prepped) %*% rawLoadings %>% adt %>%
        cbind(dt_pmdb[museum_status %in% c("private museum", "closed"), .(ID, name, museum_status, iso3c)], .) %>%
        adt 


    ## ggplot(dt_scores, aes(x=V1, y=V2)) + geom_jitter(width = 0.3, height = 0.3)
    ## dt_scores[order(-V1)] %>% print(n=20)
    ## dt_scores[order(V1)] %>% print(n=20)

    
    ## check SE again if calculations are correct: uses rotmat, not loadings to calculate scores
    ## https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r

    list(
        eigenvalues = l_pcares_prcomp$sdev^2,
        rawLoading = rawLoadings,
        rotatedLoadings = rotatedLoadings,
        dt_scores = dt_scores)


}

gp_cormat <- function(dt_pmdb, vrbls, nclust) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()};1;1;1;1;1;1;1;1;1;1;1;1;1

    ## correlations
    dt_matcor <- slt(dt_pmdb, vrbls) %>% replace_NA %>% cor %>% adt(keep.rownames = "vrblx")

    clusters <- (1-(copy(dt_matcor)[, vrblx := NULL])) %>% as.dist %>% hclust(method = "complete")
    ## plot(clusters)

    
    vrbl_order <- copy(dt_matcor)[, .(vrblx, cluster = cutree(clusters, nclust))] %>%
        .[order(cluster), vrblx]

    ## in long format
    dt_corlong <- melt(dt_matcor, id.vars = "vrblx", variable.name = "vrbly", value.name = "cor") %>%
        .[vrblx  == vrbly, cor := NA] %>% # make diag NA
        .[, `:=`(vrblx = factor(vrblx, levels = vrbl_order),
                 vrbly = factor(vrbly, levels = vrbl_order))] 
        
    
    ggplot(dt_corlong, aes(x=vrblx, y=vrbly, fill = cor)) + geom_tile() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
        scale_fill_gradient2(high = "red", low = "blue", na.value = "grey95", mid = "white")

    
    
    ## library(heatmaply)
    ## my_cor <- cor(mtcars)
    ## heatmaply_cor(my_cor)
    
    ## cor_vrbls <- slt(dt_pmdb, vrbls) %>% replace_NA %>% cor
    ## diag(cor_vrbls) <- NA
    ## heatmaply_cor(cor_vrbls)
    


}

gt_dimred <- function(dt_pmdb, vrbls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' construct summary table of how dimred vrbls change after NA imputation
    
    melt(dt_pmdb, measure.vars = vrbls, id.vars = "ID", variable.name = "vrbl", value.name = "orig") %>%
        .[, filled := replace_NA(orig)] %>% # construct filled values (NA -> 0)
        melt(id.vars = c("ID", "vrbl"), variable.name = "procstep") %>% # melt again (org/filled are procstep)
        .[, map(list(mean, min, sd), ~.x(value, na.rm = T)), .(vrbl, procstep)] %>% # generate summary stats
        setnames(old = paste0("V", seq(1,3)), new = .c(mean, min, sd)) %>% 
        dcast(vrbl ~ procstep, value.var = .c(mean, min, sd))

    
}

gp_vrblcvrg_pca <- function(dt_pmdb, l_pca_dimred2) {
    #' coverage of variables used in PCA

    dt_pmdb_dimred_splong <- dt_pmdb[, .SD, .SDcols = c("ID", "museum_status",
                                                        rownames(l_pca_dimred2$rawLoading))] %>%
        melt(id.vars = c("ID", "museum_status"), variable.name = "vrbl")

    dt_vrblcvrg_pca <- gd_vrblcvrg(dt_pmdb_dimred_splong, all_statuses = F)
    p_vrblcvrg_pca <- gp_vrblcvrg_ratio(dt_vrblcvrg_pca)

    return(p_vrblcvrg_pca)

}


gp_pca_loadings <- function(l_pca_dimred2) {
    #' loadings

    dt_dimred_loads <- gd_dimred_loads(l_pca_dimred2$rotatedLoadings)

    gp_dimred_loads(dt_dimred_loads)
}

gp_pca_scores <- function(l_pca_dimred2) {
    #' scores on PC1/2 by 

    l_pca_dimred2$dt_scores %>% ggplot(aes(x=V1, y=V2, color = museum_status)) +
        geom_jitter(width = 1, height = 1, size = 0.5)
}

    

gl_pca_dimred2 <- function(dt_pmdb) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' run a PCA on a set of variables, return scores
    
    ## set variables
    vrbls_dimred2 <- c(keep(names(dt_pmdb), ~grepl("^act_", .x)),
                       keep(names(dt_pmdb), ~grepl("^avbl_", .x)),
                       gc_pmdb_vrblgrps(dt_pmdb)[grp == "relations", vrbl],
                       .c(temp_exhibs, cafe_restrnt, reducedtickets, museumshop,
                          rentalpossblt, webshop))

    ## debug: visualize coverage 
    ## dt_pmdb_dimred_splong <- dt_pmdb[, .SD, .SDcols = c("ID", "museum_status", vrbls_dimred2)] %>%
    ##     melt(id.vars = c("ID", "museum_status"), variable.name = "vrbl")

    ## gd_vrblcvrg(dt_pmdb_dimred_splong, all_statuses = F) %>% gp_vrblcvrg_ratio
        

    ## run analysis
    l_pca_dimred2 <- gl_pca(dt_pmdb, vrbls_dimred2, ncomp = 2)

    ## debug: visualize loadings
    ## l_pca_dimred2$rotatedLoadings %>% gd_dimred_loads %>% gp_dimred_loads(include_row_clusters = F)
    

    ## return(l_pca_dimred2$dt_scores[, .(ID, museum_status, iso3c, V1, V2)])

    attr(l_pca_dimred2, "gnrtdby") <- as.character(match.call()[[1]])
    return(l_pca_dimred2)
}


## ** dimension reduction fun


## will have different variable sets, e.g. whether founder should be there or not
## doesn't matter so much now which variables to use, just set up framework for plotting
## ## just use for now all numeric
## vrbls_dimred1 <- setdiff(num_vars(dt_pmdb_prep, return = "names"),
##                          .c(llid, ID, # technical
##                             ## all time related
##                             year_opened, year_closed, birthyear, deathyear, an_fyear, an_lyear, an_nyears,
##                             lat, long))
## ## hmm looks not unconvincing: the big museums are all quite well known
## ## social media is probably what makes Saatchi so big: shrinks basically all other to zeroes on all social medias

## l_pca_dimred1 <- gl_pca(dt_pmdb, vrbls_dimred1, ncomp = 2)
## l_pca_dimred1$rotatedLoadings %>% gd_dimred_loads %>% gp_dimred_loads

## ## use only variables where absence can plausibly be understood as 0 (absence)
## vrbls_dimred2 <- c(keep(names(dt_pmdb), ~grepl("^act_", .x)),
##                    keep(names(dt_pmdb), ~grepl("^avbl_", .x)),
##                    gc_pmdb_vrblgrps(dt_pmdb)[grp == "relations", vrbl],
##                    .c(temp_exhibs, cafe_restrnt, reducedtickets, museumshop,
##                       rentalpossblt, webshop))

## l_pca_dimred2 <- gl_pca(dt_pmdb, vrbls_dimred2, ncomp = 2)
## l_pca_dimred2$rotatedLoadings %>% gd_dimred_loads %>% gp_dimred_loads

## gp_cormat(dt_pmdb, vrbls_dimred2, nclust = 2)








    
## scores <- scale(l_pcares$x) %*% varimax(rawLoadings)$rotmat %>% adt

## library(psych)
## l_pcares_psych <- psych::principal(dt_pca_prepped, rotate = "varimax", nfactors = ncomp) 

## compare PCA from psych (principal) with prcomp: with varimax(prcomp) I get same results -> use bas
## gd_dimred_loads(l_pcares_psych$loadings) %>% .[dim %in% paste0("dim", 1:ncomp)] %>% gp_dimred_loads # psych
## gd_dimred_loads(l_pcares_prcomp$rotation) %>% .[dim %in% paste0("dim", 1:ncomp)] %>% gp_dimred_loads # unrtd prcomp
## gd_dimred_loads(rotatedLoadings) %>% .[dim %in% paste0("dim", 1:ncomp)] %>% gp_dimred_loads # rotated prcomp

## gd_dimred_loads(rotatedLoadings) %>% gp_dimred_loads

## X11()



## library(factoextra)
## fviz_screeplot(l_pcares_prcomp, choice = "variance")

## (100*l_pcares_psych$values/sum(l_pcares_psych$values))[1:10] %>% gp_scree
## l_pcares_psych$Vaccounted[1,] %>% gp_scree # scree plot of rotated factors -> not so useful










