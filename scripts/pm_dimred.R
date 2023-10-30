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










