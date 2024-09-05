
## * descriptives

gp_lngtdvelp <- function(dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    gw_fargs(match.call())
    
    ## categorical and binary variables: proportion
    dt_vrblinfo <- gc_vvs() %>% chuck("dt_vrblinfo")
    
    dt_cat <- dt_pmyear[, .SD, .SDcols = c("ID", "year", dt_vrblinfo[vrbltype == "cat",
                                                                     achr(funique(vrbl))])] %>%
        melt(id.vars = c("ID", "year"), variable.name = "vrbl") %>%
        .[, .N, .(year, vrbl, value)] %>%
        .[, value_y := N/sum(N), .(year, vrbl)] %>%
        .[, N := NULL]

    ## numeric variables: mean
    dt_num <- dt_pmyear %>%
        .[, .SD, .SDcols = c("ID", "year", "closing", "age",
                             ## yeet squared/interactions
                             dt_vrblinfo[(vrbltype %in% c("bin", "num") & !grepl("I\\(|:", vrbl)) &
                                         vrbl %in% names(dt_pmyear), # only use columns that are there
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
        geom_text_repel(dt_lbls, mapping = aes(label = value), hjust = 0, direction = "y", show.legend = F,
                        size = 3) +
        coord_cartesian(xlim = c(START_YEAR-1, 2035), expand = F) +
        theme(axis.text = element_text(size = 6),
              panel.spacing = unit(0.1, "lines"),
              strip.text = element_text(size = 7, margin = margin(.05, 0, .05, 0, "cm")))
    
    
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
    vrbls_pm <- c_vvs$dt_vrblinfo[vrbl_tv == 0 & vrbltype != "num", as.character(vrbl)]
    
    
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
    
    ## variables to yeet from table
    vrbls_toyeet <- c("mow", "year", "exhbrollany", "PC1", "PC2", "popm_country", "west", "year",
                      "pmdens_circle10")

    ## format the columns
    dt_cbn_viz <- copy(dt_cbn2) %>%
        .[vrbl %!in% vrbls_toyeet] %>% 
        .[, min_fmtd := format(min, digits = 2, scientific = F, trim = F), .I] %>% # row-wise min for small mins
        .[, max_fmtd := format(max, digits = 2, scientific = F, trim = F,
                               nsmall = fifelse(max %% 1 == 0, 0, 2)), .I] %>% # non-Ints: two decimal places
        .[, .(grp_filler = "",
              term_lbl,
              pm_sum = as.character(pm_sum),
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
    dt_grpstrs <- gc_grpstrs(dt_cbn2[vrbl %!in% vrbls_toyeet], grp ="vrblgrp_lbl", nbr_cols = ncol(dt_cbn_viz))
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


gt_selfid <- function(dt_pmx, dt_pmyear) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    gw_fargs(match.call())
    #' @param dt_pmx for name
    #' @param dt_pmyear for only using those that are used eventually

    
    ## manually set some examples
    l_examples <- list(
        "Museum" = c("Museum Kampa", "Museum Barberini", "Kunstmuseum Walter", "Rubin Museum Of Art"),
        "None" = c("The Bunker", "The Broad", "Magasin III", "Tank Shanghai"), # can't have empty name
        "Foundation" = c("Marciano Art Foundation", "Hill Art Foundation", "Luma Foundation", "Kunststiftung Poll"),
        "Collection" = c("The Farjam Collection", "Sammlung Boros", "Collezione Gori", "de la Cruz Collection"),
        ## "Castle" = c("Kasteel Wijlre Estate", "Castello Di Ama"),
        "Center" = c("Dairy Art Centre", "Art Center Nabi", "Faena Art Center", "Storm King Art Center"),
        "Kunsthalle" = c("Kunsthalle Würth", "G2 Kunsthalle", "Kunsthalle Hgn", "Kunsthalle Messmer"),
        "Institute" = c("Instituto Inhotim", "Woods Art Institute", "Instituto Figueiredo Ferraz"),
        "House / villa" = c("Villa La Fleur", "Casa Daros Rio", "Kunst(Zeug)Haus", "La Maison Rouge"),
        "Art space" = c("El Espacio 23", "Qiao Space", "Space*C", "Yarat Art Space"),
        "Gallery" = c("Saatchi Gallery", "Galerie C15", "Galeria EGO", "Scrap Metal Gallery"),
        "Park / garden" = c("Schlosspark Eyebesfeld", "Il Giardino dei Lauri", "Skulpturenpark Waldfrieden")
        ## "Wine estate" = c("Donum Estate",
    )

    nbr_examples <- 2

    ## convert to 
    dt_examples <- imap(l_examples,
                        ~data.table(slfidfcn = .y,
                                    example = paste0(.x[1:min(len(.x), nbr_examples)], collapse = ", "))) %>%
        rbindlist

    ## check they are all there
    if (!all(unlist(l_examples) %in% dt_pmx[dt_pmyear[, .(ID = funique(ID))], on = "ID"][, funique(name)])) {
        stop("not all examples are in dt_pmyear")}

    ## some selfids just aren't real
    slfids_to_yeet <- c("Castle", "Wine estate", "")
    
    ## dt_pmx[, .N, slfidfcn]

    ## only use PMs that are used in dt_pmyear
    dt_slfidfcn_rcd <- dt_pmx[dt_pmyear[, .(ID = funique(ID))], on = "ID"] %>% copy %>% # only use those in pmyear
        .[, .(ID, slfidfcn)] %>% 
        .[slfidfcn %in% slfids_to_yeet, slfidfcn := "None"] %>% # rename
        .[, .N, slfidfcn] %>% # aggregate, then recode
        .[, slfidfcn_rcd := fifelse(slfidfcn %in% c("Museum", "Foundation", "Collection"), slfidfcn, "Other")]

    dt_slfidfcn <- merge(dt_slfidfcn_rcd, dt_examples, by = "slfidfcn") %>%
        .[order(-N)]


    c_colnames <- gc_colnames(col_names = names(dt_slfidfcn),
                              col_lbls = c("grp_filler" = "", "slfidfcn" = "Self-identification",
                                           "N" = "N", "slfidfcn_rcd" = "Self-ID (recoded)",
                                           "example" = "examples"))
    c_atr <- list(
        pos = list(-1),
        command = c_colnames)
    
    list(dt_fmtd = dt_slfidfcn,
         align_cfg = c("l", "l", "r", "l", "l"),
         hline_after = c(-1, nrow(dt_slfidfcn)),
         number_cols = c(F, F, T, F, F),
         add_to_row = c_atr)
    
    
    ## .[slfidfcn == "Park / garden", name]
    

    ## .[, .(.N, example = paste0(head(name, 2), collapse = ", ")), slfidfcn] %>% .[order(-N)]

    
}


## ** numbers

gn_mow_prop_museum <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generates info on how many MOWs museums have "museum" in their name
    
    l_form_strs <- list(
        museum = c("museum", "museo", "museu", "musee", "muzej", "musée", "muuseum", "muzeum", "musei",
                   "muséum", "muzium", "muséyé", "museale", "muzeu", "muzeul", "múzeum", "muzeal", "musèe", "museè",
                   "muziejus", "musem", "muzeyi", "muzei", "muséo", "muziejos", "muzieus", "muzeý", "muze ",
                   "museística", "müzesi"), # found by going through the list
        collection = c("collection", "sammlung", "coleccion", "colección", "colecção"),
        foundation = c("foundation", "stiftung", "funda", "fundação", "fondation"))
    
    dt_mow_name <- gd_mow_info() %>% .[, .(MOW_ID, name = str_to_lower(name))] %>% 
        .[, c("str_muem", "str_clcn", "str_fndn") := # string matching
                map(l_form_strs, ~str_count(name, paste0(.x, collapse = "|")))]
    ## dt_mow_name[str_muem == 0][sample(1:.N, 30)] %>% print(n=30)

    ## dt_mow_name %>% copy %>% .[, nbr_match := rowSums(.SD), .SDcols = patterns("^str_")] %>%
    ##     .[nbr_match == 0] %>% .[sample(1:.N, 30)] %>% print(n=30)

    ## dt_mow_name[, map(.SD, ~mean(.x)), .SDcols = patterns("^str_")] %>% print()
    ## ~70% of MOWs have museum in their name

    return(list(nbr_name = "mow_prop_name_museum",
         nbr_fmt = format(mean(dt_mow_name$str_muem)*100,  digits= 2),
         grp = "descs") %>% adt)

}
