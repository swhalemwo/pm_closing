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










