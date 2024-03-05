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


gp_dimred_loads <- function(dt_dimred_loads) {
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
    ## pmap(dt_clusters_ordered, ~sprintf("%s + %s", .x, .y))
    row_ordered <- pmap(dt_clusters_ordered,
                        ~dt_dimred_loads_clustered[cluster == .x & dim == .y][order(abs(value)), vrbl]) %>%
        unlist

    ## assign order of rows and row clusters/sections
    dt_dimred_loads_clustered[, `:=`(vrbl = factor(vrbl, levels = row_ordered),
                                     cluster = factor(cluster, dt_clusters_ordered[, cluster]))]


    ggplot(dt_dimred_loads_clustered, aes(x=abs(value),y=vrbl, fill = value)) +
        geom_col() +
        facet_grid(cluster ~ dim, scales = "free", space = "free_y") +
        scale_fill_gradient2(high = "red", low = "blue")
    
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
    #' calculate PCA scores of vrbls
    #' @param dt_pdmb dt_pmdb
    #' @param vrbls variables to include in PCA
    #' @param ncomp number of factors to extract
    
    dt_pca_prepped <- slt(dt_pmdb[museum_status %in% c("private museum", "closed")], vrbls_dimred) %>%
        tfmv(vars = names(.), FUN = replace_NA) 
    
    ## actual PCA, scale = T
    l_pcares_prcomp <- prcomp(dt_pca_prepped, scale=T)

    
    rawLoadings <- l_pcares_prcomp$rotation[,1:ncomp] %*% diag(l_pcares_prcomp$sdev, ncomp, ncomp)
    ## diag = eigenvalues?
    rotatedLoadings <- varimax(rawLoadings)$loadings


    ## calculating/inspecting scores, in particular size
    dt_scores <- scale(dt_pca_prepped) %*% rawLoadings %>% adt %>%
        cbind(dt_pmdb[museum_status %in% c("private museum", "closed"), .(ID, name, iso3c)], .) %>% adt 


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


## ** dimension reduction fun


## will have different variable sets, e.g. whether founder should be there or not
## doesn't matter so much now which variables to use, just set up framework for plotting
## just use for now all numeric
vrbls_dimred1 <- setdiff(num_vars(dt_pmdb_prep, return = "names"),
                         .c(llid, ID, # technical
                            ## all time related
                            year_opened, year_closed, birthyear, deathyear, an_fyear, an_lyear, an_nyears,
                            lat, long))
## hmm looks not unconvincing: the big museums are all quite well known
## social media is probably what makes Saatchi so big: shrinks basically all other to zeroes on all social medias


## use only variables where absence can plausibly be understood as 0 (absence)
vrbls_dimred2 <- c(keep(names(dt_pmdb), ~grepl("^act_", .x)),
                   keep(names(dt_pmdb), ~grepl("^avbl_", .x)),
                   gc_pmdb_vrblgrps(dt_pmdb)[grp == "relations", vrbl],
                   .c(temp_exhibs, cafe_restrnt, reducedtickets, museumshop,
                      rentalpossblt, webshop))


l_pca_dimred2 <- gl_pca(dt_pmdb, vrbls_dimred2, ncomp = 2)
l_pca_dimred2$rotatedLoadings %>% gd_dimred_loads %>% gp_dimred_loads




    
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










