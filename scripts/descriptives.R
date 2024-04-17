
gp_lngtdvelp <- function(dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    ## categorical and binary variables: proportion
    dt_vrblinfo <- gc_vvs() %>% chuck("dt_vrblinfo")
    
    dt_cat <- dt_pmyear[, .SD, .SDcols = c("ID", "year", dt_vrblinfo[vrbltype == "cat",
                                                                     achr(funique(vrbl))])] %>%
        melt(id.vars = c("ID", "year"), variable.name = "vrbl") %>%
        .[, .N, .(year, vrbl, value)] %>%
        .[, value_y := N/sum(N), .(year, vrbl)] %>%
        .[, N := NULL]

    ## numeric variables: mean
    dt_num <- dt_pmyear[, .SD, .SDcols = c("ID", "year", "closing", "age",
                                           # # yeet squared/interactions
                                           dt_vrblinfo[(vrbltype %in% c("bin", "num") & !grepl("I\\(|:", vrbl)),
                                                       achr(funique(vrbl))])] %>%
        .[, cnt := .N, year] %>% # set up count (gets meaned)
        .[, will_close := fifelse(any(closing == 1), 1, 0), ID] %>% # whether a museum will close
        .[, first_year := fifelse(age == 1, 1, 0), ID] %>% 
        melt(id.vars = c("ID", "year"), variable.name = "vrbl") %>%
        .[, .(value_y = mean(value)), .(year, vrbl)] %>%
        .[, `:=`(value = vrbl)] %>%
        .[vrbl != "year"] # don't need avg year, is just year

    dt_viz <- rbind(dt_cat, dt_num)
        
    ## set colors based on variable value
    dt_color <- dt_viz[, .(vrbl, value)] %>% funique %>%
        .[, lnbr := 1:.N, vrbl]
    

    dt_viz_colored <- join(dt_viz, dt_color, on = c("vrbl", "value"))
    
    ## add labels for lines of categorical variables at end of line
    dt_lbls <- dt_viz_colored[dt_vrblinfo[vrbltype == "cat", .(vrbl)], on = "vrbl"] %>%
        .[, .SD[which.max(year)], .(vrbl, value)]
    
    ## library(ggrepel)
    ggplot(dt_viz_colored, aes(x=year, y=value_y, group = value, color = factor(lnbr))) +
        geom_line(show.legend = F) +
        facet_wrap(~vrbl, scales = "free") +
        geom_text_repel(dt_lbls, mapping = aes(label = value), hjust = 0, direction = "y", show.legend = F) +
        coord_cartesian(xlim = c(1990, 2035))
                                                         
    
}


gt_sumstats <- function(dt_pmyear, dt_pmcpct) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())

    c_vvs <- gc_vvs()

    ## expand categorical variables to dummies
    ## requires calling model.matrix for each variable

    dt_dummies <- map(chuck(c_vvs, "dt_vrblinfo")[vrbltype == "cat", vrbl],
                      ~model.matrix(as.formula(sprintf("~ %s -1", .x)), dt_pmyear)) %>%
        Reduce(cbind, .) %>% adt %>%
        melt(measure.vars = names(.), variable.name = "term")

    ## get numeric and dummy variables, only get those that are in the table (no squares/interactions)
    vrbls_num <- intersect(names(dt_pmyear), chuck(c_vvs, "dt_vrblinfo")[vrbltype %in% c("num", "bin"), vrbl])
    dt_numvrbls <- dt_pmyear[, vrbls_num, with = F] %>% melt(measure.vars = names(.), variable.name = "term")

    ## summary functions to apply
    funcs_sumry <- .c(mean, sd, min, max)

    ## apply summary functions, merge with term labels to get variables names and groupings
    dt_cbn <- rbind(dt_dummies, dt_numvrbls) %>%
        .[, map(funcs_sumry, ~get(.x)(value, na.rm = T)), term] %>%
        setnames(old = paste0("V", 1:len(funcs_sumry)), new = funcs_sumry) %>%
        c_vvs$dt_termlbls[., on = "term"]
        
    ## FIXME: sum needs to be generalized when I have non-varying numerical variables (e.g. size)
    ## there sum does'nt make sense but mean does


    ## get variables that I want on PM-level
    ## have to make as character to remove factors
    vrbls_pm <- c(c_vvs$dt_vrblinfo[vrbl_tv == 0, as.character(vrbl)], "founder_dead")
    
    
    ## expand each variable separately, otherwise model.matrix drops reference categories
    dt_sumry_pm <- map(vrbls_pm, ~model.matrix(as.formula(sprintf("~ %s -1", .x)), dt_pmcpct)) %>%
        Reduce(cbind, .) %>% adt %>%
        melt(measure.vars = names(.), variable.name = "term") %>%
        .[, .(pm_mean = mean(value), pm_sum = sum(value)), term]
                     
    ## combine pm-year-level and pm-level data, reorder
    dt_cbn2 <- copy(dt_cbn)[dt_sumry_pm, `:=`("pm_mean" = i.pm_mean, "pm_sum" =  i.pm_sum), on = "term"] %>%
        .[, vrbl := factor(vrbl, levels = levels(c_vvs$dt_vrblinfo$vrbl))] %>% # somehow necessary to relevel? 
        .[order(vrblgrp, vrbl)]
    
    ## create summary column names
    sumstats_cols <- c("grp_filler" = "", "term_lbl" = "Variable", "pm_sum" = "Count", "pm_mean" = "Mean",
                       "mean" = "Mean", "sd" = "SD", min = "Min.", max = "Max.")
    
    ## format the columns
    dt_cbn_viz <- copy(dt_cbn2) %>%
        .[, min_fmtd := format(min, digits = 2, scientific = F, trim = F), .I] %>% # row-wise min for small mins
        .[, max_fmtd := format(max, digits = 2, scientific = F, trim = F,
                               nsmall = fifelse(max %% 1 == 0, 0, 2)), .I] %>% # non-Ints: two decimal places
        .[, .(grp_filler = "", term_lbl, pm_sum = as.character(pm_sum),
              pm_mean = format(pm_mean, digits=2, trim = T),
              mean = format(mean, digits = 2, trim = F),
              sd = format(sd, digits = 2, trim = F),
              min = min_fmtd,
              max = max_fmtd
              ## max = format(max, digits = 2, trim = F)
              )] %>%        
        recode_char("NA" = NA) # recode "NA"-strings to actual NA (format() can't deal with it in numeric)

    
    ## spanner for grouping museum-level and PM-year-level variables
    top_spanner <- paste0("\\hline\n & & \\multicolumn{2}{c}{Museum} & \\multicolumn{4}{c}{Museum-year} \\\\ \n",
                          "\\cmidrule(r){3-4}\\cmidrule(r){5-8}")

    ## generate the other table elements: groupstrings and column names
    dt_grpstrs <- gc_grpstrs(dt_cbn2, grp ="vrblgrp_lbl", nbr_cols = ncol(dt_cbn_viz))
    c_colnames <- gc_colnames(col_names = names(sumstats_cols),
                              col_lbls = sumstats_cols, hline_above = F)
    c_atr <- list(
        pos = c(list(-1, -1), dt_grpstrs$pos),
        command = c(top_spanner, c_colnames, dt_grpstrs$grpstr))
                
    list(dt_fmtd = dt_cbn_viz,
         align_cfg = c(rep("l",3), rep("r", 6)),
         hline_after = c(-1, nrow(dt_cbn_viz)),
         add_to_row = c_atr,
         number_cols = c(rep(F, 2), rep(T, 6)))
    
    
    ## pm_density doesn't really make sense on museum-level
    ## funnily enough, founder_dead does make sense (number of founders who have died in total),
    ## even tho both are time-varying...

    ## would also make sense to have west/mow/muem_fndr name -> not about including categorical variables,
    ## but excluding time-varying
    

}
