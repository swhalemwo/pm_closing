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

## ** mission comparison

## mean
dt_pmdb[, .(nchar_mission = replace_NA(nchar(mission)), museum_status)] %>%
    .[, .(mean_nchar_mission = mean(nchar_mission)), museum_status]
## mean 3.2x higher for open museums -> can pretty much forget it

## density
dt_pmdb[, .(nchar_mission = replace_NA(nchar(mission)), museum_status)] %>%
    ggplot(aes(x=nchar_mission, fill  = museum_status)) + geom_density(alpha = 0.2)


## barplot
dt_pmdb[, .(nchar_mission = floor(replace_NA(nchar(mission)/100)), museum_status)] %>%
    .[, .N, .(museum_status, nchar_mission)] %>%
    ggplot(aes(x=nchar_mission, y=N, fill = museum_status)) +
    geom_col() +
    facet_grid(museum_status ~.)


## *** self identification in mission statements 

library(stringr)

dt_selfid_mission <- dt_pmdb[dt_pmtiv[, .(ID, slfidfcn_rcd = slfidfcn)], on = "ID"] %>%
    .[, .(ID, name, mission, slfidfcn_rcd)] %>% # filter down 
    .[, c("str_muem", "str_clcn", "str_fndn") := # string matching
            map(c("muse", "collec|sammlung", "foundation|stiftung|funda"), ~str_count(mission, .x))]
    

dt_selfid_mission %>%
    .[mission != ""] %>% # yeet empty missions
    .[, map(.SD, ~mean(.x != 0)), slfidfcn_rcd, .SDcols = patterns("^str_")] %>% # aggregate to match
    melt(id.vars = "slfidfcn_rcd", variable.name = "str_match", value.name = "prop") %>%
    ggplot(aes(x=str_match, y=prop, fill = slfidfcn_rcd)) +
    geom_col(position = position_dodge())

## mission statements are just GARBAGE


## * reconstruct Gordon, assuming constant haz

dt_gordon <- data.table(
    year = c(2000, 2001, 2002, 2003),
    alive = c(1305, 1295, 1287, 1280), # assume no openings
    alive2 = c(1291, 1292, 1292, 1292), # assume openings = closings
    dead = c(9,9,9,9)) %>%
    .[, .(mort1 = dead/alive, mort2 = dead/alive2)] %>%
    .[, lapply(.SD, mean)]

## * compare my methodology with Hager
## who is still there in 2020 from those who are active in 2010

dt_open10 <- dt_pmyear[year == 2010 & closing == 0]
# 264

## see how many survived
dt_pmyear[year == 2020 & ID %in% dt_open10$ID & closing == 0]

dt_pmyear[year == 2020 & closing == 0][dt_open10, on = "ID", nomatch = NULL]
# 228

# 36 closed
## how many which were alive in 2010 closed in next decade

dt_pmyear[closing == 1 & year %between% c(2010, 2020) & year_opened < 2010]

(264/228)^0.1

# pretty sure taking the limit of what Hager is doing is the average annual hazard

## ** Bowen

## dt_bowen <-

data.table(
    year = seq(1981, 1990),
    exits = rep(27, 10)) %>%
    ## .[order(-year)] %>% 
    ## .[year == 1990, total := 2481] %>%
    ## .[,  cumsum(exits)]
    .[, total := 2484 + 271 - cumsum(exits)]



    
    
## * liability of newness

dt_pmyear[, .(.N, N_unique = fnunique(ID), N_closed = sum(closing)), floor(year_opened/5)*5] %>%
    .[, prop_closed := N_closed/N_unique] %>%
    ggplot(aes(x=floor, y=prop_closed)) + geom_col()

## hmm that's not it, that's just general survival (older have had more time to die -> more of them are dead)

## let's do it the other way around: look at 2010-2021 period, see which proportion of those who entered there have died

dt_pmyear[year >= 2015, .(entered = fnunique(ID), closed = sum(closing)), floor(year_opened/10)*10] %>%
    .[, prop_closed := closed/entered] %>%
    ggplot(aes(x=floor, y=prop_closed)) + geom_col()

dt_pmyear[year >= 2010 & year_opened %between% c(1990, 1999), fnunique(ID)]


## * predict test

r_pred <- coxph(Surv(age, closing) ~ gender, dt_pmcpct)

dt_new <- expand.grid(gender = c("F", "M", "couple"), age = seq(0,30), closing = 0) %>% adt %>%
    .[, pred := predict(r_pred, newdata = ., type = "survival")]


dt_new %>% ggplot(aes(x=age, y=pred, color = gender)) + geom_step()


dt_new <- expand.grid(founder_dead_binary = c(0,1), age = seq(0,30), closing = 0) %>% adt %>%
    .[, pred := predict(l_mdls$r_pop4, newdata = ., type = "survival")]


## predicted effect of founder death






## predicted founder death partitioned
## would need alive for some year


## three phases: 1-5: alive, 6-7: recently dead, then long dead
## but that won't work: gd_pred will assume it has always been recently dead
## estimate hazard, then do KM myself


dt_pred_prep3 <- cbind(
        dt_pmyear[, lapply(.SD, Mode), # categorical/binary variables: use mode
                  .SDcols = gc_vvs()$dt_vrblinfo[vrbltype %in% c("bin", "cat"), achr(vrbl)]],
    dt_pmyear[, lapply(.SD, median), .SDcols = c("pmdens_cry", "year", "proxcnt10", "popm_circle10")])

## estimate the alive curve

## gd_pred_prep needs at least two conditions: add row for recently_dead, which gets yeeted later
dt_pred_prep_partition <- rbind(dt_pred_prep3, dt_pred_prep3) %>%
    .[, founder_dead1 := c("recently_dead", "alive")]

## generate the alive survival curve for first 30 yeares
map(1:30, ~gd_pred("r_founder_dead1", l_mdls, dt_pred_prep_partition, "surv", year_range = .x) %>%
              .[, `:=`(age = .x, founder_dead1 = c("recently_dead", "alive"))]) %>% rbindlist %>%
    .[founder_dead1 == "alive"]

## hazard for death
gd_pred("r_founder_dead1", l_mdls, dt_pred_prep_partition, "hazard", year_range = 10)
## fuck won't get SE for recently dead

rx <- survfit(Surv(tstart, tstop, closing) ~ founder_dead1, dt_pmyear)
lines(rx, col = "red")
rx %>% str
rx$std.chaz

## fuck i'm back to reverse engineering hazard standard errors
## which I had given up for the age dependency already..
## this should not be impossible tho, since now I 





## figuring out what exactly the predict.coxph is doing: 
## compare to survfit for men

survfit2(Surv(age, closing) ~ 1, dt_pmcpct[gender == "M"]) %>% chuck("surv") %>% chuck(30)
    ggsurvfit()

## not exactly the same, but kinda close
survfit2(Surv(age, closing) ~ 1, dt_pmcpct[gender == "M"]) %>% chuck("surv") %>% chuck(30)
dt_new[age == 30 & gender == "M", pred]

## model improvement

dt_drop1 <- drop1(l_mdls$r_pop4, trace = T) %>% adt(keep.rownames = "term") %>% .[order(AIC)]

dt_drop1 %>% copy %>% .[, AIC_none := .SD[term == "<none>", AIC]] %>%
    .[, AIC_diff := AIC_none - AIC]

## strange, AIC is lower for all the insignificant variables...
## AIC is AIC of model without variable -> higher AIC means model gets worse by yeeting variable





coxph(gf_coxph_close(vrbls_to_yeet = "gender"), dt_pmyear) %>% gd_reg_coxph("gender_gone") %>%
    chuck("dt_gof") %>% .[, AIC]
## gender gone AIC: 707.5468




## ** competition respecification


dt_pmyear %>% copy %>%
    .[, audience := popm_circle10/(proxcnt10+1)] %>%
    .[, .(ID, year, audience, popm_circle10, proxcnt10)] %>% 
    melt(id.vars = c("ID", "year", "audience")) %>%
    ggplot(aes(x=audience, y=value)) + geom_jitter() + facet_wrap(~variable, scales = "free")


    
dt_aud <- data.table(aud =  seq(dt_pmyear[, quantile(audience10_log, 0.1)],
                                dt_pmyear[, quantile(audience10_log, 0.9)], 0.01)) %>%
    .[, pred := -0.19338*aud + -0.16334*aud^2]
    ## .[, pred := 4.32*aud + -0.16334*aud^2]

dt_aud %>% 
    ggplot(aes(x=aud, y=pred)) + geom_line()

ggplot(dt_pmyear, aes(x=audience10_log)) + geom_density()
## where is the top point of the curve if the linear term is -0.19 and the squared term is -0.16?






dt_aud[which.max(pred)]



dt_pmyear[, .SD, .SDcols = c("proxcnt10", "proxcnt10_log", "popm_circle10", "popm_circle10_log", "ID", "year")] %>%
    melt(id.vars = c("ID", "year")) %>%
    ggplot(aes(x=value, color = variable)) + geom_density(show.legend = F) +
    facet_wrap(~variable, scales = "free")

## * old heatmap stuff

## ** neighbors for regions
## get neighbors: each cell can have 4 neighbors
    dt_neib_prep <- dt_pred_cell[, .(proxcnt10, popm_circle10, mort_cat)]

    ## look where you have a border to the left
    dt_border_left <- dt_neib_prep %>% copy %>%
        .[, proxcnt10_left := proxcnt10 - 1] %>% # join with left
        join(copy(dt_neib_prep)[, .(proxcnt10_left = proxcnt10, popm_circle10, mort_cat_left = mort_cat)],
             on = c("proxcnt10_left", "popm_circle10")) %>%
        .[mort_cat != mort_cat_left] # identify mort_cat transitions
    
    ## look where the border is on the top
    dt_border_up <- dt_neib_prep %>% copy %>%
        .[, popm_circle10_up := popm_circle10 + 1] %>% # 
        join(copy(dt_neib_prep)[, .(popm_circle10_up = popm_circle10, proxcnt10, mort_cat_up = mort_cat)],
             on = c("popm_circle10_up", "proxcnt10")) %>%
        .[mort_cat != mort_cat_up] # identify mort_cat transitions

    ## construct manual scale

## ** histogram legend (replaced by rug)
gp_heatmap_legend_barplot <- function(dt_pred_cell) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate colorbar with histogram, serves as legend for the heatmap

    ## the horizontal bar plot
    dt_viz_bar <- dt_pred_cell %>% copy %>%
        .[, .(sumN = sum(N), mean_mort = mean(mort)), floor(mort*40)/40] %>%
        .[, color := scale_ylorbr(mean_mort/dt_pred_cell[, max(mort)])] 
    ## .[order(floor)] %>% .[, floor := as.factor(floor)]

    ## the vertical color bar
    dt_bar <- dt_pred_cell[, .(pos = seq(0.0+0.005, (ceiling(max(mort, na.rm = T)*40)/40), 0.005))] %>%
        .[, x := 0]
    
    
    ## check that bar is working
    dt_bar %>% ggplot(aes(x=x, y=pos, fill = pos)) + geom_tile() +
        scale_fill_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj")

    ## horizontal lines on vertical color bar
    ## dt_hlines <- data.table(y=c(mortbound_lo, mortbound_hi))

    ## generate plot to use as colorbar legend: allows to have lines (segments on legend)
    p_legend <- ggplot() +
        geom_tile(dt_bar, mapping = aes(y=x, x=pos, fill = pos, color = pos), height = 50, show.legend = F,
                  position = position_nudge(y=-50)) + # the actual colorbar
        geom_col(dt_viz_bar, mapping = aes(y=sumN, x=floor, fill = floor),
                 position = position_nudge(x=0.0125), # an info histogram 
                 show.legend = F) +
        ## geom_segment(dt_hlines, mapping = aes(x = y, xend = y, y = -75, yend = -25), color = "black") +
        ## the horizontal lines
        coord_flip(expand = F) + # need to use coord_flip: can't have decimal-point y-axis with horizontal bars
        scale_fill_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj") +
        scale_color_YlOrBr(reverse = T, range = c(0, 0.88), scale_name = "jj") +
        labs(y= "Nbr. unique PMs", x = element_blank(), title = "Pred. closing chance\nwithin 20 years") +
        theme(plot.margin = margin(0, 0, 0, 0),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              title = element_text(size = 9),
              plot.title.position = "plot"
              )

    return(p_legend)
    ## p_legend
    ## scale_fill_manual(values = setNames(dt_viz_bar$color, dt_viz_bar$floor)) 
    ## geom_hline(yintercept = "0.15")
}
## tag = "lines demarcate \ndifferent closing\n chance categories") 
    ## scale_fill_nightfall(midpoint = fmean(dt_pred_cell$est, w = dt_topred_cell$N), reverse = T)

    

    ## p_heatmap + p_legend +
    ##     ## plot_layout(widths = c(0.8, 0.2))
    ##                                     # need to use awkward patchwork design to properly size legend
    ##     plot_layout(design = "1111#\n11112\n11112\n11112\n11112\n1111#") 

## * map visualization

## plots the museums as points on a map, identified by lat/long, with background map
## limit the map to New York

## https://ggplot2tutor.com/tutorials/streetmaps

library(osmdata)
getbb("New York")

streets <- getbb("New York") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()





ggplot() +
    geom_sf(data = streets$osm_lines, color = "grey") +
    geom_point(dt_pmdb[city == "New York", .(lat, long, ID, year_opened)],
               mapping = aes(x=long, y=lat)) +
    geom_text(dt_pmdb[city == "New York"],
               mapping = aes(x = long, y=lat, label = paste0(year_opened, "/", year_closed))) +     
    coord_sf(xlim = c(-74.05, -73.9), ylim = c(40.70, 40.80))
    
    theme_minimal() + theme(legend.position = "bottom")  +
    scale_x_continuous(limits = c(-74.05, -73.9)) +
    scale_y_continuous(limits = c(40.65, 40.85))
        
## data looks ok.. landau has 7 in 2017 because bunch of the others opened later

## look at which closed in high population, low PM areas
dt_pmdb[dt_pmyear[popm_circle10 > 3 & proxcnt10 < 4, .(year = max(year), closing = max(closing)), ID], on ="ID"] %>%
    .[, .(name, iso3c, closing, city, year, ID)] %>% print(n=30) %>%
    .[closing == 1]

## Jakarta, Istanbul, Paris.. 
