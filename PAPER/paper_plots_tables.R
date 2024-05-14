source("header.R")
source("PROCESS_DATA/aux_orphanhood.R")
source("PROCESS_DATA/PAPER/aux_paper.R")

type.input <- "Department" # c("Municipality", "Department", "Region", "National")
per_n_children <- ifelse(type.input == "Municipality", 1000, 100000)
should_round <- FALSE

geo_info <- read_csv(file = "DATA/geo_info.csv", col_types = "fcfcfcfc"); muns <- unique(geo_info$mun)
population <- read_csv(file = "DATA/Population from Census/pop_years_list_municipality.csv", col_types = cols())
population <- population %>% filter(Year <= 2021) %>% mutate(gender = Sex, loc = factor(Mun), year = Year, age = Age, population = Population) %>% select(gender, loc, year, age, population) %>% arrange(gender, loc, year, age)
population <- population %>% filter(loc %in% muns)

COL.REG <- c("#34B4EB", "#F4C470", "#D36167", "#B9DB7D" , "#43A278", "#FFB0B3")
names(COL.REG) <- c("Caribe", "Eje cafetero y Antioquia", "Pacífica", "Central", "Llanos", "Amazonía")

COL.PDET <- c("#D36167","#43A278")
CAT_PDET <- c("PDET", "No PDET")
names(COL.PDET) <- CAT_PDET

COL.TYPE <- c("#476314", "#71a800", "#feb502", "#ffe77e", "#0083a8", "#7ab5f5", "#bdf0ff")
CAT_LEVELS <- c("Strongly affected and persistent", "Mildly affected and persistent", "Strongly affected and disrupted", "Mildly affected and disrupted", "Strongly affected and finished", "Midly affected and finished", "With no conflicts")
names(COL.TYPE) <- CAT_LEVELS

COL.MPM <- rev(pal_frontiers()(5)) 
CAT_MPM <- geo_info %>% select(MPM_cat, MPM_name) %>% distinct() %>% arrange(MPM_cat) %>% select(MPM_name) %>% c() %>% unlist() %>% unname() 
names(COL.MPM) <- CAT_MPM

# Compute municipalities with small population #

if (TRUE) { # (type.input == "Municipality") {
  exclude_low_density <- TRUE
  n_excluded_muns <- 29
  
  original_pop <- read_csv("DATA/Population from Census/backup_cp_pop_years_list_municipality.csv")
  original_pop <- original_pop %>% filter(Year == 2018)
  original_pop <- original_pop %>% group_by(Mun) %>% summarise(total_pop = sum(Population)) %>% ungroup() %>% rename(mun = Mun) %>% mutate(mun = factor(mun))

  col_tmp <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry) %>% rename(mun = Code) %>% mutate(mun = factor(mun))
  col_tmp <- col_tmp %>% mutate(area = st_area(geometry))
  col_tmp <- col_tmp %>% left_join(y = original_pop, by = "mun")
  col_tmp <- col_tmp %>% mutate(pop_density = (total_pop / area)) %>% arrange(desc(pop_density)) %>% select(-geometry)
  col_tmp <- col_tmp %>% left_join(y = geo_info[, c("mun", "mun_name", "dep_name")], by = "mun") %>% select(mun, mun_name, dep_name, total_pop, area, pop_density) %>% rename(population = total_pop)
  
  less_dense_mun <- col_tmp %>% tail(n_excluded_muns) %>% select(mun) %>% c() %>% unlist() %>% unname() %>% as.character() %>% as.numeric()

  # Order the municipalities based on `something`
  # POVERTY (`MPM`)
  
  MPM <- geo_info[, c("mun", "MPM")]
  MPM <- MPM  %>% arrange(MPM)
  if (exclude_low_density) { MPM <- MPM %>% filter(!(as.numeric(as.character(mun)) %in% less_dense_mun)) }
  
  n_mun <- MPM %>% nrow()
  n_per <- floor(n_mun / 5)
  
  MPM$quint_MPM <- c(rep(1:5, each = n_per), rep(5, n_mun - (n_per * 5)))
  MPM <- MPM %>% mutate(quint_MPM = factor(x = quint_MPM, labels = CAT_MPM, levels = 1:5))
  MPM <- MPM %>% select(mun, quint_MPM) %>% rename(MPM = quint_MPM)
}

# Aggregate population (and children proportion)

valid_muns     <- unname(unlist(c(MPM$mun))); saveRDS(object = valid_muns, file = "PROCESS_DATA/valid_muns.RDS")
tmp_geo_info   <- geo_info %>% filter(mun %in% valid_muns)
tmp_population <- population %>% filter(loc %in% valid_muns)

if (TRUE) { # Check excluded rows
  excluded_rows <- anti_join(x = geo_info, y = tmp_geo_info, by = c("mun", "dep"))
  flagged_dep <- excluded_rows$dep_name %>% unique()
  flagged_reg <- excluded_rows$reg_name %>% unique()
}

tmp <- convert_resolution_pop(type.input = "Municipality", population = tmp_population, geo_info = tmp_geo_info, valid_muns = valid_muns)
population <- tmp$population
prop_15_17 <- tmp$prop_15_17

# Compute incidence and prevalence (already considering excluded municipalities) #

inc_tab <- generate_incidence_table(type.input = "Municipality", per_n_children = per_n_children, geo_info = tmp_geo_info, should_round = FALSE)

if (type.input != "Municipality") {
  tmp_orphans <- inc_tab$orphans
  tmp_orp_per <- inc_tab$orphans
  tmp_pop_chl <- inc_tab$tmp_population; tmp_pop_chl <- tmp_pop_chl %>% rename(mun = loc)
  
  if (type.input == "Department") {
    tmp_orphans <- tmp_orphans %>% left_join(tmp_geo_info[, c("mun", "dep", "dep_name")], by = "mun") %>% select(dep, dep_name, gender, year, n_orp)
    tmp_orphans <- tmp_orphans %>% group_by(dep, dep_name, gender, year) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(dep, gender, year)
    tmp_pop_chl <- tmp_pop_chl %>% left_join(tmp_geo_info[, c("mun", "dep")], by = "mun") %>% select(dep, year, children)
    tmp_pop_chl <- tmp_pop_chl %>% group_by(dep, year) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(dep, year)
    tmp_orp_per <- tmp_orphans
    tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("dep", "year")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% select(-children)
    if (should_round) { tmp_orp_per$n_orp <- ceiling(tmp_orp_per$n_orp) }
  } else if (type.input == "Region") {
    tmp_orphans <- tmp_orphans %>% left_join(tmp_geo_info[, c("mun", "reg", "reg_name")], by = "mun") %>% select(reg, reg_name, gender, year, n_orp)
    tmp_orphans <- tmp_orphans %>% group_by(reg, reg_name, gender, year) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(reg, gender, year)
    tmp_pop_chl <- tmp_pop_chl %>% left_join(tmp_geo_info[, c("mun", "reg")], by = "mun") %>% select(reg, year, children)
    tmp_pop_chl <- tmp_pop_chl %>% group_by(reg, year) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(reg, year)
    tmp_orp_per <- tmp_orphans
    tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("reg", "year")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% select(-children)
  } else if (type.input == "National") {
    tmp_orphans <- tmp_orphans %>% left_join(tmp_geo_info[, c("mun", "nat", "nat_name")], by = "mun") %>% select(nat, nat_name, gender, year, n_orp)
    tmp_orphans <- tmp_orphans %>% group_by(nat, nat_name, gender, year) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(nat, gender, year)
    tmp_pop_chl <- tmp_pop_chl %>% left_join(tmp_geo_info[, c("mun", "nat")], by = "mun") %>% select(nat, year, children)
    tmp_pop_chl <- tmp_pop_chl %>% group_by(nat, year) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(nat, year)
    tmp_orp_per <- tmp_orphans
    tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("nat", "year")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% select(-children)
  } else { stop("Invalid `type.input`") }
  
  inc_tab$orphans <- tmp_orphans
  inc_tab$orphans_per_child <- tmp_orp_per
}

if (should_round) { inc_tab$orphans_per_child$n_orp <- ceiling(inc_tab$orphans_per_child$n_orp) }

write_csv(x = inc_tab$orphans          , file = paste("PROCESS_DATA/PAPER/TABLES/", tolower(type.input), "_inc_orphans_abs.csv", sep = ""))
write_csv(x = inc_tab$orphans_per_child, file = paste("PROCESS_DATA/PAPER/TABLES/", tolower(type.input), "_inc_orphans_per_", as.integer(per_n_children), ".csv", sep = ""))

if (type.input != "Municipality") {
  pre_tab <- generate_prevalence_table(type.input = "Department", per_n_children = per_n_children, geo_info = tmp_geo_info, should_round = FALSE)
  
  if (type.input != "Department") {
    tmp_orphans <- pre_tab$orphans
    tmp_orp_per <- pre_tab$orphans
    tmp_pop_chl <- pre_tab$nb_children
    
    if (type.input == "Region") {
      tmp_orphans <- tmp_orphans %>% left_join(distinct(tmp_geo_info[, c("dep", "reg", "reg_name")]), by = "dep") %>% select(reg, reg_name, gender, n_orp)
      tmp_orphans <- tmp_orphans %>% group_by(reg, reg_name, gender) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(reg, gender)
      tmp_pop_chl <- tmp_pop_chl %>% left_join(distinct(tmp_geo_info[, c("dep", "reg")]), by = "dep") %>% select(reg, gender, children)
      tmp_pop_chl <- tmp_pop_chl %>% group_by(reg, gender) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(reg, gender)
      tmp_orp_per <- tmp_orphans
      tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("reg", "gender")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% select(-children)
    } else if (type.input == "National") {
      tmp_orphans <- tmp_orphans %>% left_join(distinct(tmp_geo_info[, c("dep", "nat", "nat_name")]), by = "dep") %>% select(nat, nat_name, gender, n_orp)
      tmp_orphans <- tmp_orphans %>% group_by(nat, nat_name, gender) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% arrange(nat, gender)
      
      tmp_pop_chl <- tmp_pop_chl %>% left_join(distinct(tmp_geo_info[, c("dep", "nat")]), by = "dep") %>% select(nat, gender, children)
      tmp_pop_chl <- tmp_pop_chl %>% group_by(nat, gender) %>% summarise(children = sum(children)) %>% ungroup() %>% arrange(nat, gender)
      tmp_orp_per <- tmp_orphans
      tmp_orp_per <- tmp_orp_per %>% left_join(y = tmp_pop_chl, by = c("nat", "gender")) %>% mutate(n_orp = n_orp * per_n_children / children) %>% select(-children)
    } else { stop("Invalid `type.input`") }
    
    pre_tab$orphans <- tmp_orphans
    pre_tab$orphans_per_child <- tmp_orp_per
  }
  
  if (should_round) { pre_tab$orphans_per_child$n_orp <- ceiling(pre_tab$orphans_per_child$n_orp) }

  write_csv(x = pre_tab$orphans          , file = paste("PROCESS_DATA/PAPER/TABLES/", tolower(type.input), "_pre_orphans_abs.csv", sep = ""))
  write_csv(x = pre_tab$orphans_per_child, file = paste("PROCESS_DATA/PAPER/TABLES/", tolower(type.input), "_pre_orphans_per_", as.integer(per_n_children), ".csv", sep = ""))
}

##################################################
##################################################
# PLOT
##################################################
##################################################

plot_prev <- function (data, type.input, per_n_children, g = NA, prop = FALSE, ...) {
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  if (type.input == "Department") {
    geo_info
    
    tmp_geo_info <- geo_info[, c("dep", "reg", "reg_name")] %>% distinct() 
    data <- left_join(x = data, y = tmp_geo_info, by = c("dep"))
  }
  
  if (prop == FALSE) { 
    txt_legend <- "Total" 
  } else { 
    txt_legend <- "% of children aged 0-17 years"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  if (!is.na(g)) { data <- data %>% filter(gender == g) } else { data <- data %>% group_by(across(-c(gender, n_orp))) %>% summarise(n_orp = sum(n_orp)) }
  data <- data %>% arrange(desc(n_orp))
  data[, tmp_loc] <- as.character(unname(unlist(c(data[, tmp_loc]))))
  data[, tmp_loc] <- factor(unname(unlist(c(data[, tmp_loc]))), labels = unname(unlist(c(data[, paste(tmp_loc, "_name", sep = "")]))), levels = unname(unlist(c(data[, tmp_loc]))))
  # factor(unname(unlist(c(data[, tmp_loc]))), labels = unname(unlist(c(data[, paste(tmp_loc, "_name", sep = "")]))), levels = unname(unlist(c(data[, tmp_loc]))[order(-data$n_orp)])) 
  
  levels_dep <- levels(data$dep)
  levels_dep <- sapply(X = levels_dep, FUN = function(x) { ifelse(x %in% flagged_dep, paste(x, "*", sep = ""), x) })
  levels(data$dep) <- levels_dep
  
  pp <- ggplot(data = data) +
    { if (type.input == "Department") geom_bar(aes(x = dep, y = n_orp, fill = reg_name), stat = "identity") } + 
    { if (type.input != "Department") geom_bar(aes(x = dep, y = n_orp), stat = "identity") } + 
    labs(x = "", y = paste("Orphanhood prevalence in 2021 by ", type.input, "\n(", txt_legend, ")", sep = "")) + 
    # labs(x = "", y = paste("Estimated prevalence (", txt_legend, ")", sep = ""), title = ifelse(is.na(g), "", paste(toupper(g), " DATA", sep = "")), subtitle = ifelse(is.na(g), "", "Estimated prevalence of orphanhood in 2021")) + 
    { if ( prop) scale_y_continuous(limits = c(0, ceiling(max(data$n_orp))), breaks = 0:ceiling(max(data$n_orp)), labels = paste(format((0:ceiling(max(data$n_orp)) / 1), 1), "%", sep = ""), expand = expansion(mult = c(0, 0))) } +
    { if (!prop) scale_y_continuous(label = comma, expand = expansion(mult = c(0, 0.05))) }+ 
    # { if (type.input == "Department") scale_fill_viridis_d(name = "Region") } +
    { if (type.input == "Department") scale_fill_manual(name = "Region", values = COL.REG) } +
    theme_bw() +
    theme(text = element_text(size = 12, family = "LM Roman 10"),
          axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(size = 14),
          panel.grid.major.x = element_blank())
  
  pp 
}

pre_tab$orphans           <- pre_tab$orphans           %>% mutate(dep_name = ifelse(dep == 88, "Arch. de SA, Prov. y St. Cat.", dep_name))
pre_tab$orphans_per_child <- pre_tab$orphans_per_child %>% mutate(dep_name = ifelse(dep == 88, "Arch. de SA, Prov. y St. Cat.", dep_name))
p_abs <- plot_prev(data = pre_tab$orphans,           type.input = type.input, per_n_children = per_n_children, g = NA, prop = FALSE)
p_rel <- plot_prev(data = pre_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, g = NA, prop = TRUE )
p_tot <- (p_abs + p_rel + plot_layout(guides = "collect")) # + plot_annotation(title = "Estimated prevalence of orphanhood in 2021", subtitle = "Children aged 0-17 years who lost their mother, father or both") & theme(text = element_text(family = "LM Roman 10"))
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/prevalence_2021.jpeg", sep = ""), plot = p_tot, width = 4000, height = 1790, units = c("px"), dpi = 300, bg = "white")

plot_prev(data = pre_tab$orphans, type.input = type.input, per_n_children = per_n_children, g = "Female", prop = FALSE)
plot_prev(data = pre_tab$orphans, type.input = type.input, per_n_children = per_n_children, g = "Male"  , prop = FALSE)
plot_prev(data = pre_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, g = "Female", prop = TRUE)
plot_prev(data = pre_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, g = "Male"  , prop = TRUE)

##################################################
##################################################

plot_prev_mun <- function (data, type.input, per_n_children, prop = FALSE, PDET = FALSE, MPM_flag = FALSE, yy = 2021, ...) {
  if (type.input == "Municipality") { tmp_loc <- "mun" } else { stop("Only implemented for Municipality.") }
  
  if (prop == FALSE) { 
    txt_legend <- "Total" 
  } else { 
    txt_legend <- "% of children aged 0-17 years"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  tmp_geo_info <- geo_info[, c("mun", "dep", "dep_name", "cat", "cat_name", "PDET", "MPM_cat")]
  deps <- sort(as.numeric(as.character(unique(tmp_geo_info$dep))))
  
  data <- left_join(x = data, y = tmp_geo_info, by = c("mun"))
  data <- data %>% filter(year == yy)
  
  pps <- list()
  for (pos_dep in 1:length(deps)) {
    cp_data <- data
    data <- data %>% filter(dep == deps[pos_dep])
    data <- data %>% group_by(across(-c(gender, n_orp))) %>% summarise(n_orp = sum(n_orp)) %>% ungroup()
    data <- data %>% arrange(desc(n_orp))
    data[, tmp_loc] <- as.numeric(as.character(unname(unlist(c(data[, tmp_loc])))))
    data[, tmp_loc] <- as.character(unname(unlist(c(data[, tmp_loc]))))
    
    # Include `MPM` by quintile
    data <- data %>% left_join(y = MPM, by = "mun")
    
    data[, tmp_loc] <- factor(unname(unlist(c(data[, tmp_loc]))), labels = unname(unlist(c(data[, paste(tmp_loc, "_name", sep = "")]))), levels = unname(unlist(c(data[, tmp_loc]))))
    data[, "PDET"]  <- factor(unname(unlist(c(data[, "PDET"]))),  labels = rev(CAT_PDET), levels = 0:1)
    data[, "cat"]   <- factor(unname(unlist(c(data[, "cat"]))),   labels = CAT_LEVELS, levels = 1:7)
    data[, "MPM_cat"] <- factor(unname(unlist(c(data[, "MPM_cat"]))), labels = CAT_MPM, levels = 1:5)
    
    # Filter out municipalities with low density (based on `MPM`)
    data[which(data$MPM %>% is.na()), c("PDET", "cat", "MPM_cat")] <- NA
    data <- data %>% rename(MPM_quint = MPM)
    
    pps[[pos_dep]] <- ggplot(data = data) +
      { if ( PDET & !MPM_flag) geom_bar(aes(x = mun, y = n_orp, fill = PDET), stat = "identity") } + 
      { if (!PDET & !MPM_flag) geom_bar(aes(x = mun, y = n_orp, fill = cat ), stat = "identity") } + 
      { if (         MPM_flag) geom_bar(aes(x = mun, y = n_orp, fill = MPM_quint), stat = "identity")} +
      labs(x = "", y = paste("Orphanhood incidence in 2021 by ", type.input, "\n(", txt_legend, ")", sep = ""), title = tmp_geo_info[tmp_geo_info$dep == deps[pos_dep], "dep_name"][1, ]) + 
      { if ( prop) scale_y_continuous(limits = c(0, ceiling(max(data$n_orp, na.rm = TRUE))), breaks = 0:ceiling(max(data$n_orp, na.rm = TRUE)), labels = paste(format((0:ceiling(max(data$n_orp, na.rm = TRUE)) / 1), 1), "%", sep = ""), expand = expansion(mult = c(0, 0))) } +
      { if (!prop) scale_y_continuous(label = comma, expand = expansion(mult = c(0, 0.05))) }+ 
      { if ( PDET & !MPM_flag) scale_fill_manual(name = "PDET",             values = COL.PDET, drop = FALSE) } + 
      { if (!PDET & !MPM_flag) scale_fill_manual(name = "Type of conflict", values = COL.TYPE, drop = FALSE) } + 
      { if (         MPM_flag) scale_fill_manual(name = "MPM quintiles\n(as classification from 2018)", values = COL.MPM, drop = FALSE) } + 
      theme_bw() +
      theme(legend.position = "right",
            text = element_text(size = 12, family = "LM Roman 10"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
            axis.title.y = element_text(size = 8),
            plot.title = element_text(size = 14),
            panel.grid.major.x = element_blank())
    
    data <- cp_data
  }
  
  pps
}

pps_number_CERAC <- plot_prev_mun(data = inc_tab$orphans,           type.input = type.input, per_n_children = inc_tab$per_n_children, prop = FALSE, PDET = FALSE)
pps_percen_CERAC <- plot_prev_mun(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = inc_tab$per_n_children, prop = TRUE , PDET = FALSE)

pps_number_PDET  <- plot_prev_mun(data = inc_tab$orphans,           type.input = type.input, per_n_children = inc_tab$per_n_children, prop = FALSE, PDET = TRUE)
pps_percen_PDET  <- plot_prev_mun(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = inc_tab$per_n_children, prop = TRUE , PDET = TRUE)

pps_number_MPM  <- plot_prev_mun(data = inc_tab$orphans,           type.input = type.input, per_n_children = inc_tab$per_n_children, prop = FALSE, MPM_flag = TRUE)
pps_percen_MPM  <- plot_prev_mun(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = inc_tab$per_n_children, prop = TRUE , MPM_flag = TRUE)

pps_number_total <- pps_percen_total <- list()
for (i in 1:length(pps_number_PDET)) { pps_number_total[[i]] <- (pps_number_PDET[[i]] + pps_number_MPM[[i]]) }
for (i in 1:length(pps_number_PDET)) { pps_percen_total[[i]] <- (pps_percen_PDET[[i]] + pps_percen_MPM[[i]]) }
deps <- sort(as.numeric(as.character(unique(geo_info$dep))))
deps_name <- geo_info %>% select(dep, dep_name) %>% mutate(dep = as.numeric(as.character(dep))) %>% arrange(dep) %>% distinct() %>% select(dep_name) %>% c() %>% unlist() %>% unname()
for (i in 1:length(deps)) {
  ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/MUNICIPALITY/orphans_rate_by_mun_", deps_name[i],".jpeg", sep = ""), plot = pps_percen_total[[i]], width = 6000, height = 1500, units = c("px"), dpi = 300, bg = "white")
  # ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/MUNICIPALITY/orphans_rate_by_mun_", deps_name[i],".jpeg", sep = ""), plot = pps_percen_PDET[[i]] , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")
}

##################################################
##################################################

plot_inc_dep <- function (data, type.input, g, per_n_children, prop = FALSE, ...) {
  
  data <- data %>% filter(gender == g)
  if (prop == FALSE) { 
    txt_legend <- "absolute number" 
  } else { 
    txt_legend <- "proportion %"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  tmp_geo_info <- geo_info[, c("dep", "reg", "reg_name")] %>% distinct() 
  data <- left_join(x = data, y = tmp_geo_info, by = c("dep"))
  
  pp <- ggplot(data) +
    geom_line(aes(x = year, y = n_orp, color = dep)) + 
    facet_wrap(~reg_name, ncol = 3) +
    labs(x = "Year", y = paste("Estimated incidence (", txt_legend ,")", sep = ""), title = paste(toupper(g), " DATA", sep = ""), 
         subtitle = "Estimated incidence of orphanhood from 2015 to 2021") + 
    scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021) +
    scale_color_viridis_d(name = "Department") +
    theme_bw() +
    theme(text = element_text(size = 12, family = "LM Roman 10"), 
          plot.title = element_text(size = 14),
          axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
  
  pp
}

plot_inc_dep(data = inc_tab$orphans, type.input = type.input, g = "Female", per_n_children = per_n_children, prop = FALSE)
plot_inc_dep(data = inc_tab$orphans, type.input = type.input, g = "Male"  , per_n_children = per_n_children, prop = FALSE)
plot_inc_dep(data = inc_tab$orphans_per_child, type.input = type.input, g = "Female", per_n_children = per_n_children, prop = TRUE)
plot_inc_dep(data = inc_tab$orphans_per_child, type.input = type.input, g = "Male"  , per_n_children = per_n_children, prop = TRUE)

##################################################
##################################################

plot_inc_dep_all <- function (data, type.input, per_n_children, prop = FALSE, ...) {
  
  data <- data %>% mutate(gender = ifelse(gender == "Female", "Mother", "Father"))
  
  if (prop == FALSE) { 
    txt_legend <- "absolute number" 
  } else { 
    txt_legend <- "proportion %"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  data_fem <- data %>% filter(gender == "Mother")
  data_mal <- data %>% filter(gender == "Father")
  data_tot <- data_fem
  data_tot$gender <- "Both"
  data_tot$n_orp <- data_fem$n_orp + data_mal$n_orp
  
  data <- bind_rows(data_tot, data_fem, data_mal)
  data$gender <- factor(data$gender, levels = c("Both", "Mother", "Father"))
  
  tmp_geo_info <- geo_info[, c("dep", "reg", "reg_name")] %>% distinct() 
  tmp_geo_info <- tmp_geo_info %>% mutate(reg_name = ifelse(reg_name == "Eje cafetero y Antioquia", "Eje cafetero y\nAntioquia", reg_name))
  names(COL.REG)[which(names(COL.REG) == "Eje cafetero y Antioquia")] <- "Eje cafetero y\nAntioquia"
  data <- left_join(x = data, y = tmp_geo_info, by = c("dep"))
  data <- data %>% arrange(gender, year, reg)
  
  levels_as_char <- sprintf("%02s", as.character(data$dep))
  data <- data %>% mutate(dep = factor(levels_as_char, levels = levels_as_char[1:33]))
  
  # Set colors to repeat per region
  table_deps <- data %>% filter(gender == "Both", year == 2015) %>% select(reg) %>% table() %>% c()
  n_max <- max(table_deps)
  possible_colors <- c(pal_d3()(10), "#B9DB7D") # pal_ucscgb()(n_max) # viridis_pal()(n_max)
  all_comb_colors <- c(possible_colors[1:(table_deps[1])],
                       possible_colors[1:(table_deps[2])],
                       possible_colors[1:(table_deps[3])],
                       possible_colors[1:(table_deps[4])],
                       possible_colors[1:(table_deps[5])],
                       possible_colors[1:(table_deps[6])])
  data$my_colors <- rep(all_comb_colors, 21) # 21 = 7 years * 3 genders (female, male, and total)
  
  generic_theme <- theme(text = element_text(size = 12, family = "LM Roman 10"), 
                         plot.title = element_text(size = 10),
                         axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
                         strip.background = element_rect(fill = NA, color = NA),
                         axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))
  
  levels(data$gender) <- c("Children who lost\ntheir mother or father", 
                           "Children who lost\ntheir mother",
                           "Children who lost\ntheir father")
  data <- data %>% mutate(dep_name = ifelse(dep == 88, "Arch. de SA, Prov. y St. Cat.", dep_name))
  
  nat_avg <- data %>% group_by(gender, year) %>% summarise(nat_avg = mean(n_orp)) %>% ungroup() %>% mutate(description = "National average")
  
  pp <- ggplot(data) +
    geom_line(aes(x = year, y = n_orp, color = dep), linewidth = 0.5) + 
    geom_point(aes(x = year, y = n_orp, color = dep), size = 0, shape = 15, show.legend = FALSE) +
    geom_line(data = nat_avg, aes(x = year, y = nat_avg), color = "black", linetype = 2, linewidth = 0.5) + 
    facet_grid(reg_name ~ gender) +
    labs(x = "", y = "Orphanhood incidence from 2015 to 2021 by Department\n(% of children aged 0-17 years)") + # paste("Estimated incidence (", txt_legend ,")", sep = "") + 
    scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021) +
    { if (prop) scale_y_continuous(limits = c(0, 1.025), breaks = seq(0, 1, 0.25), labels = paste(format(seq(0, 1, 0.25), nsmall = 2), "%", sep = "")) } +
    # scale_color_viridis_d(name = "Department") +
    scale_color_manual(values = data$my_colors, name = "Department") +
    theme_bw() +
    guides(colour = guide_legend(override.aes = list(size = 5, linetype = 0))) + generic_theme + theme(legend.position = "none")
  
  # for_1_legend <- data %>% filter(reg == 1) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 1, ]$my_colors, name = "Pacífico ") + generic_theme + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))
  # for_2_legend <- data %>% filter(reg == 2) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 2, ]$my_colors, name = "Caribe   ") + generic_theme + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))
  # for_3_legend <- data %>% filter(reg == 3) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 3, ]$my_colors, name = "Andina   ") + generic_theme + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))
  # for_4_legend <- data %>% filter(reg == 4) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 4, ]$my_colors, name = "Orinoquía") + generic_theme + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))
  # for_5_legend <- data %>% filter(reg == 5) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 5, ]$my_colors, name = "Amazónica") + generic_theme + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))
  # for_6_legend <- data %>% filter(reg == 6) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 6, ]$my_colors, name = "Insular  ") + generic_theme + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))
  
  for_0_legend <- nat_avg %>% ggplot() + geom_line(data = nat_avg, aes(x = year, y = nat_avg, color = description), linetype = 2, linewidth = 0.5) + scale_color_manual(values = "black", name = "") + theme_bw() + generic_theme + theme(legend.position = "right", legend.title = element_text(size = 0))
  
  for_1_legend <- data %>% filter(reg == 1) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep_name), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 1, ]$my_colors, name = "Pacífico ") + theme_bw() + generic_theme + theme(legend.position = "right", legend.text = element_text(size = 8), legend.title = element_text(size = 10)) + guides(colour = guide_legend(ncol = 2))
  for_2_legend <- data %>% filter(reg == 2) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep_name), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 2, ]$my_colors, name = "Caribe   ") + theme_bw() + generic_theme + theme(legend.position = "right", legend.text = element_text(size = 8), legend.title = element_text(size = 10)) + guides(colour = guide_legend(ncol = 2))
  for_3_legend <- data %>% filter(reg == 3) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep_name), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 3, ]$my_colors, name = "Andina   ") + theme_bw() + generic_theme + theme(legend.position = "right", legend.text = element_text(size = 8), legend.title = element_text(size = 10)) + guides(colour = guide_legend(ncol = 2))
  for_4_legend <- data %>% filter(reg == 4) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep_name), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 4, ]$my_colors, name = "Orinoquía") + theme_bw() + generic_theme + theme(legend.position = "right", legend.text = element_text(size = 8), legend.title = element_text(size = 10)) + guides(colour = guide_legend(ncol = 2))
  for_5_legend <- data %>% filter(reg == 5) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep_name), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 5, ]$my_colors, name = "Amazónica") + theme_bw() + generic_theme + theme(legend.position = "right", legend.text = element_text(size = 8), legend.title = element_text(size = 10)) + guides(colour = guide_legend(ncol = 2))
  for_6_legend <- data %>% filter(reg == 6) %>% ggplot() + geom_point(aes(x = year, y = n_orp, color = dep_name), size = 5, shape = 15) + scale_color_manual(values = data[data$reg == 6, ]$my_colors, name = "Insular  ") + theme_bw() + generic_theme + theme(legend.position = "right", legend.text = element_text(size = 8), legend.title = element_text(size = 10)) + guides(colour = guide_legend(ncol = 2))
  
  nat_lagends <- get_legend(for_0_legend)
  all_legends <- plot_grid(get_legend(for_5_legend + theme(legend.justification = c("left", "top"))),
                           get_legend(for_3_legend + theme(legend.justification = c("left", "top"))),
                           get_legend(for_2_legend + theme(legend.justification = c("left", "top"))),
                           get_legend(for_6_legend + theme(legend.justification = c("left", "top"))),
                           get_legend(for_4_legend + theme(legend.justification = c("left", "top"))),
                           get_legend(for_1_legend + theme(legend.justification = c("left", "top"))), 
                           ncol = 3, rel_heights = c(0.5, 1), rel_widths = c(0.675, 1, 1.18))
  
  final_plot <- plot_grid(pp, all_legends, ncol = 1, rel_heights = c(10, 2.25))
  final_plot + inset_element(nat_lagends, left = 0.78, bottom = 0.925, right = 0.8715, top = 0.9509, align_to = "full") 
  
}

p_inc_abs <- plot_inc_dep_all(data = inc_tab$orphans, type.input = type.input , per_n_children = per_n_children, prop = FALSE)
p_inc_rel <- plot_inc_dep_all(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE)

ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_abs.jpeg", sep = ""), plot = p_inc_abs, width = 2150, height = 3300, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_rel.jpeg", sep = ""), plot = p_inc_rel, width = 2150, height = 3300, units = c("px"), dpi = 300, bg = "white")

##################
##################

plot_inc_reg_all <- function (data, type.input, per_n_children, prop = FALSE, ...) {
  
  data <- data %>% mutate(gender = ifelse(gender == "Female", "Mother", "Father"))
  
  if (prop == FALSE) { 
    txt_legend <- "absolute number" 
  } else { 
    txt_legend <- "proportion %"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  data_fem <- data %>% filter(gender == "Mother")
  data_mal <- data %>% filter(gender == "Father")
  data_tot <- data_fem
  data_tot$gender <- "Both"
  data_tot$n_orp <- data_fem$n_orp + data_mal$n_orp
  
  data <- bind_rows(data_tot, data_fem, data_mal)
  data$gender <- factor(data$gender, levels = c("Both", "Mother", "Father"))
  
  tmp_geo_info <- geo_info[, c("reg", "reg_name")] %>% distinct() 
  # tmp_geo_info <- tmp_geo_info %>% mutate(reg_name = ifelse(reg_name == "Eje cafetero y Antioquia", "Eje cafetero y\nAntioquia", reg_name))
  # names(COL.REG)[which(names(COL.REG) == "Eje cafetero y Antioquia")] <- "Eje cafetero y\nAntioquia"
  data <- left_join(x = dplyr::select(data, -reg_name), y = tmp_geo_info, by = c("reg"))
  data <- data %>% dplyr::select(reg, reg_name, gender, year, n_orp)
  data <- data %>% arrange(reg, gender, year)
  
  generic_theme <- theme(text = element_text(size = 10, family = "LM Roman 10"), 
                         plot.title = element_text(size = 10),
                         axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
                         strip.background = element_rect(fill = NA, color = NA),
                         axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))
  
  levels(data$gender) <- c("Children who lost\ntheir mother or father", 
                           "Children who lost\ntheir mother",
                           "Children who lost\ntheir father")
  
  data <- data %>% left_join(y = as_tibble(data.frame(reg_name = names(COL.REG), my_colors = COL.REG)), by = "reg_name")
  
  nat_avg <- data %>% group_by(gender, year) %>% summarise(nat_avg = mean(n_orp)) %>% ungroup() %>% mutate(description = "National average")
  
  pp <- ggplot(data) +
    geom_line(aes(x = year, y = n_orp, color = reg), linewidth = 0.75) + 
    geom_line(data = nat_avg, aes(x = year, y = nat_avg), color = "black", linetype = 2, linewidth = 0.75) + 
    facet_grid(~ gender) +
    labs(x = "", y = "Orphanhood incidence from 2015 to 2021 by Region\n(% of children aged 0-17 years)") + 
    scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021) +
    { if (prop) scale_y_continuous(limits = c(0.025, 0.725), breaks = seq(0, 0.75, 0.25), labels = paste(format(seq(0, 0.75, 0.25), nsmall = 2), "%", sep = "")) } +
    scale_color_manual(values = unique(data$my_colors), labels = unique(data$reg_name), name = "Region") +
    theme_bw() +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE)) + generic_theme + theme(legend.position = "bottom")
  
  for_0_legend <- nat_avg %>% ggplot() + geom_line(data = nat_avg, aes(x = year, y = nat_avg, color = description), linetype = 2, linewidth = 0.5) + scale_color_manual(values = "black", name = "") + theme_bw() + generic_theme + theme(legend.position = "right", legend.title = element_text(size = 0))
  nat_lagends <- get_legend(for_0_legend)
  
  final_plot <- pp 
  # final_plot <- final_plot + inset_element(nat_lagends, left = 1.80, bottom = 0.05, right = 1, top = 0.12, align_to = "full") 
  final_plot
}

p_inc_reg_abs <- plot_inc_reg_all(data = inc_tab$orphans, type.input = type.input , per_n_children = per_n_children, prop = FALSE)
p_inc_reg_rel <- plot_inc_reg_all(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE)

ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_reg_abs.jpeg", sep = ""), plot = p_inc_reg_abs, width = 3000, height = 1250, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_reg_rel.jpeg", sep = ""), plot = p_inc_reg_rel, width = 3000, height = 1250, units = c("px"), dpi = 300, bg = "white")

##################
##################


# plot_inc_mun_all <- function (data, type.input, per_n_children, prop = FALSE, ...) {
#   
#   CAT_LEVELS_adapted <- c("Strongly affected\nand persistent", "Mildly affected\nand persistent", "Strongly affected\nand disrupted", "Mildly affected\nand disrupted", "Strongly affected\nand finished", "Midly affected\nand finished", "With no conflicts")
#   
#   data <- data %>% mutate(gender = ifelse(gender == "Female", "Mother", "Father"))
#   
#   if (prop == FALSE) { 
#     txt_legend <- "absolute number" 
#   } else { 
#     txt_legend <- "proportion %"
#     data$n_orp <- data$n_orp / (per_n_children / 100)
#   }
#   
#   data_fem <- data %>% filter(gender == "Mother") %>% arrange(mun, year)
#   data_mal <- data %>% filter(gender == "Father") %>% arrange(mun, year)
#   data_tot <- data_fem
#   data_tot$gender <- "Both"
#   data_tot$n_orp <- data_fem$n_orp + data_mal$n_orp
#   
#   data <- bind_rows(data_tot, data_fem, data_mal)
#   data$gender <- factor(data$gender, levels = c("Both", "Mother", "Father"))
#   
#   tmp_geo_info <- geo_info[, c("mun", "cat", "cat_name")] %>% distinct() 
#   data <- left_join(x = data, y = tmp_geo_info, by = c("mun"))
#   data <- data %>% arrange(gender, year, cat)
#   
#   levels_as_char <- sprintf("%05s", as.character(data$mun))
#   data <- data %>% mutate(mun = factor(levels_as_char, levels = levels_as_char[1:(length(unique(levels_as_char)))]))
#   data <- data %>% mutate(cat = factor(cat, labels = CAT_LEVELS_adapted, levels = 1:7))
#   
#   # Set colors to repeat per region
#   table_muns <- data %>% filter(gender == "Both", year == 2015) %>% select(cat) %>% table() %>% c()
#   n_max <- max(table_muns)
#   set.seed(1);possible_colors <- sample(rainbow(n_max))
#   all_comb_colors <- c(possible_colors[1:(table_muns[1])],
#                        possible_colors[1:(table_muns[2])],
#                        possible_colors[1:(table_muns[3])],
#                        possible_colors[1:(table_muns[4])],
#                        possible_colors[0:(table_muns[5])], # As there is no `5`
#                        possible_colors[1:(table_muns[6])],
#                        possible_colors[1:(table_muns[7])])
#   data$my_colors <- rep(all_comb_colors, 21) # 21 = 7 years * 3 genders (female, male, and total)
#   
#   generic_theme <- theme(text = element_text(size = 12, family = "LM Roman 10"), 
#                          plot.title = element_text(size = 10),
#                          axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
#                          strip.background = element_rect(fill = NA, color = NA),
#                          axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))
#   
#   levels(data$gender) <- c("Children who lost\ntheir mother or father", 
#                            "Children who lost\ntheir mother",
#                            "Children who lost\ntheir father")
#   
#   nat_avg <- data %>% group_by(gender, year) %>% summarise(nat_avg = mean(n_orp)) %>% ungroup() %>% mutate(description = "National average")
#   
#   pp <- ggplot(data) +
#     geom_line(aes(x = year, y = n_orp, color = mun), linewidth = 0.5, alpha = 1) + 
#     geom_point(aes(x = year, y = n_orp, color = mun), size = 0, shape = 15, show.legend = FALSE) +
#     geom_line(data = nat_avg, aes(x = year, y = nat_avg), color = "black", linetype = 2, linewidth = 0.5) + 
#     facet_grid(cat ~ gender, drop = FALSE) +
#     labs(x = "", y = "Orphanhood incidence from 2015 to 2021 by Municipality\n(% of children aged 0-17 years)") + 
#     scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021) +
#     { if (prop) scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5), labels = paste(format(seq(0, 3, 0.5), nsmall = 2), "%", sep = "")) } +
#     # scale_color_viridis_d(name = "Municipality") +
#     scale_color_manual(values = data$my_colors, name = "Department") +
#     theme_bw() +
#     guides(colour = guide_legend(override.aes = list(size = 5, linetype = 0))) + generic_theme + theme(legend.position = "none")
#   
#   for_0_legend <- nat_avg %>% ggplot() + geom_line(data = nat_avg, aes(x = year, y = nat_avg, color = description), linetype = 2, linewidth = 0.5) + scale_color_manual(values = "black", name = "") + theme_bw() + generic_theme + theme(legend.position = "right", legend.title = element_text(size = 0))
#   nat_lagends  <- get_legend(for_0_legend)
#   
#   pp <- pp + inset_element(nat_lagends, left = 0.8080, bottom = 0.927, right = 0.8415, top = 0.9509, align_to = "full") 
#   
#   pp
# }
# 
# p_inc_mun_abs <- plot_inc_mun_all(data = inc_tab$orphans,           type.input = type.input, per_n_children = per_n_children, prop = FALSE)
# p_inc_mun_rel <- plot_inc_mun_all(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE )
# 
# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_mun_abs.jpeg", sep = ""), plot = p_inc_mun_abs, width = 2150, height = 3300, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_mun_rel.jpeg", sep = ""), plot = p_inc_mun_rel, width = 2150, height = 3300, units = c("px"), dpi = 300, bg = "white")


plot_inc_mun_all <- function (data, type.input, per_n_children, prop = FALSE, ...) {
  
  CAT_MPM_adapted <-  c("[00%, 20%)", "[20%, 40%)", "[40%, 60%)", "[60%, 80%)", "[80%, 100%]")
    
  data <- data %>% mutate(gender = ifelse(gender == "Female", "Mother", "Father"))
  
  if (prop == FALSE) { 
    txt_legend <- "absolute number" 
  } else { 
    txt_legend <- "proportion %"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  data_fem <- data %>% filter(gender == "Mother") %>% arrange(mun, year)
  data_mal <- data %>% filter(gender == "Father") %>% arrange(mun, year)
  data_tot <- data_fem
  data_tot$gender <- "Both"
  data_tot$n_orp <- data_fem$n_orp + data_mal$n_orp
  
  data <- bind_rows(data_tot, data_fem, data_mal)
  data$gender <- factor(data$gender, levels = c("Both", "Mother", "Father"))
  
  tmp_geo_info <- geo_info[, c("mun", "MPM_cat", "MPM_name")] %>% distinct() 
  data <- left_join(x = data, y = tmp_geo_info, by = c("mun"))
  data <- data %>% arrange(gender, year, MPM_cat)
  
  #####
  #####
  #####
  # Include `MPM` by quintile
  data <- data %>% left_join(y = MPM, by = "mun")
  
  # Filter out municipalities with low density (based on `MPM`)
  data[which(data$MPM %>% is.na()), c("n_orp")] <- NA
  data <- data %>% rename(MPM_quint = MPM)
  #####
  #####
  #####
  
  levels_as_char <- sprintf("%05s", as.character(data$mun))
  data <- data %>% mutate(mun = factor(levels_as_char, levels = levels_as_char[1:(length(unique(levels_as_char)))]))
  data <- data %>% mutate(MPM_cat = factor(MPM_cat, labels = CAT_MPM_adapted, levels = 1:5))
  
  # Set colors to repeat per region
  table_muns <- data %>% filter(gender == "Both", year == 2015) %>% select(MPM_cat) %>% table() %>% c()
  n_max <- max(table_muns)
  set.seed(1);possible_colors <- sample(rainbow(n_max))
  all_comb_colors <- c(possible_colors[1:(table_muns[1])],
                       possible_colors[1:(table_muns[2])],
                       possible_colors[1:(table_muns[3])],
                       possible_colors[1:(table_muns[4])],
                       possible_colors[1:(table_muns[5])])
  data$my_colors <- rep(all_comb_colors, 21) # 21 = 7 years * 3 genders (female, male, and total)
  
  generic_theme <- theme(text = element_text(size = 12, family = "LM Roman 10"), 
                         plot.title = element_text(size = 10),
                         axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
                         strip.background = element_rect(fill = NA, color = NA),
                         axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))
  
  levels(data$gender) <- c("Children who lost\ntheir mother or father", 
                           "Children who lost\ntheir mother",
                           "Children who lost\ntheir father")
  
  nat_avg <- data %>% group_by(gender, year) %>% summarise(nat_avg = mean(n_orp, na.rm = TRUE)) %>% ungroup() %>% mutate(description = "National average")
  
  pp <- ggplot(na.omit(data)) +
    geom_line(aes(x = year, y = n_orp, color = mun), linewidth = 0.5, alpha = 1) + 
    geom_point(aes(x = year, y = n_orp, color = mun), size = 0, shape = 15, show.legend = FALSE) +
    geom_line(data = nat_avg, aes(x = year, y = nat_avg), color = "black", linetype = 2, linewidth = 0.5) + 
    facet_grid(MPM_quint ~ gender, drop = FALSE) +
    labs(x = "", y = "Orphanhood incidence from 2015 to 2021 by Municipality\n(% of children aged 0-17 years)") + 
    scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021) +
    { if (prop) scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5), labels = paste(format(seq(0, 3, 0.5), nsmall = 2), "%", sep = "")) } +
    # scale_color_viridis_d(name = "Municipality") +
    scale_color_manual(values = data$my_colors, name = "Department") +
    theme_bw() +
    guides(colour = guide_legend(override.aes = list(size = 5, linetype = 0))) + generic_theme + theme(legend.position = "none")
  
  for_0_legend <- nat_avg %>% ggplot() + geom_line(data = nat_avg, aes(x = year, y = nat_avg, color = description), linetype = 2, linewidth = 0.5) + scale_color_manual(values = "black", name = "") + theme_bw() + generic_theme + theme(legend.position = "right", legend.title = element_text(size = 0))
  nat_lagends  <- get_legend(for_0_legend)
  
  pp <- pp + inset_element(nat_lagends, left = 0.818, bottom = 0.923, right = 0.8615, top = 0.9469, align_to = "full") 
  
  pp
}

p_inc_mun_abs <- plot_inc_mun_all(data = inc_tab$orphans,           type.input = type.input, per_n_children = per_n_children, prop = FALSE)
p_inc_mun_rel <- plot_inc_mun_all(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE )

# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_mun_abs.jpeg", sep = ""), plot = p_inc_mun_abs, width = 2150, height = 3300, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_mun_rel.jpeg", sep = ""), plot = p_inc_mun_rel, width = 2150, height = 3300, units = c("px"), dpi = 300, bg = "white")

##################
##################

# THIS IS A COPY FROM THE LAST FUNCTION, BUT FOR THE CLASSIFICATION BASED ON THE `PDET`
plot_inc_mun_all_PDET <- function (data, type.input, per_n_children, prop = FALSE, ...) {
  
  CAT_PDET_adapted <- rev(CAT_PDET)
  
  data <- data %>% mutate(gender = ifelse(gender == "Female", "Mother", "Father"))
  
  if (prop == FALSE) { 
    txt_legend <- "absolute number" 
  } else { 
    txt_legend <- "proportion %"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  data_fem <- data %>% filter(gender == "Mother") %>% arrange(mun, year)
  data_mal <- data %>% filter(gender == "Father") %>% arrange(mun, year)
  data_tot <- data_fem
  data_tot$gender <- "Both"
  data_tot$n_orp <- data_fem$n_orp + data_mal$n_orp
  
  data <- bind_rows(data_tot, data_fem, data_mal)
  data$gender <- factor(data$gender, levels = c("Both", "Mother", "Father"))
  
  tmp_geo_info <- geo_info[, c("mun", "PDET")] %>% distinct() 
  data <- left_join(x = data, y = tmp_geo_info, by = c("mun"))
  data <- data %>% arrange(gender, year, PDET)
  
  #####
  #####
  #####
  # Include `MPM` by quintile
  data <- data %>% left_join(y = MPM, by = "mun")
  
  # Filter out municipalities with low density (based on `MPM`)
  data[which(data$MPM %>% is.na()), c("n_orp")] <- NA
  data <- data %>% rename(MPM_quint = MPM)
  #####
  #####
  #####
  
  levels_as_char <- sprintf("%05s", as.character(data$mun))
  data <- data %>% mutate(mun = factor(levels_as_char, levels = levels_as_char[1:(length(unique(levels_as_char)))]))
  data <- data %>% mutate(PDET_fact = factor(PDET, labels = CAT_PDET_adapted, levels = 0:1))
  
  # Set colors to repeat per region
  table_muns <- data %>% filter(gender == "Both", year == 2015) %>% select(PDET_fact) %>% table() %>% c()
  n_max <- max(table_muns)
  set.seed(1);possible_colors <- sample(rainbow(n_max))
  all_comb_colors <- c(possible_colors[1:(table_muns[1])],
                       possible_colors[1:(table_muns[2])])
  data$my_colors <- rep(all_comb_colors, 21) # 21 = 7 years * 3 genders (female, male, and total)
  
  generic_theme <- theme(text = element_text(size = 12, family = "LM Roman 10"), 
                         plot.title = element_text(size = 10),
                         axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
                         strip.background = element_rect(fill = NA, color = NA),
                         axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))
  
  levels(data$gender) <- c("Children who lost\ntheir mother or father", 
                           "Children who lost\ntheir mother",
                           "Children who lost\ntheir father")
  
  nat_avg <- data %>% group_by(gender, year) %>% summarise(nat_avg = mean(n_orp, na.rm = TRUE)) %>% ungroup() %>% mutate(description = "National average")
  
  pp <- ggplot(na.omit(data)) +
    geom_line(aes(x = year, y = n_orp, color = mun), linewidth = 0.5, alpha = 1) + 
    geom_point(aes(x = year, y = n_orp, color = mun), size = 0, shape = 15, show.legend = FALSE) +
    geom_line(data = nat_avg, aes(x = year, y = nat_avg), color = "black", linetype = 2, linewidth = 0.5) + 
    facet_grid(PDET_fact ~ gender, drop = FALSE) +
    labs(x = "", y = "Orphanhood incidence from 2015 to 2021 by Municipality\n(% of children aged 0-17 years)") + 
    scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021) +
    { if (prop) scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5), labels = paste(format(seq(0, 3, 0.5), nsmall = 2), "%", sep = "")) } +
    # scale_color_viridis_d(name = "Municipality") +
    scale_color_manual(values = data$my_colors, name = "Department") +
    theme_bw() +
    guides(colour = guide_legend(override.aes = list(size = 5, linetype = 0))) + generic_theme + theme(legend.position = "none")
  
  for_0_legend <- nat_avg %>% ggplot() + geom_line(data = nat_avg, aes(x = year, y = nat_avg, color = description), linetype = 2, linewidth = 0.5) + scale_color_manual(values = "black", name = "") + theme_bw() + generic_theme + theme(legend.position = "right", legend.title = element_text(size = 0))
  nat_lagends  <- get_legend(for_0_legend)
  
  pp <- pp + inset_element(nat_lagends, left = 0.830, bottom = 0.841, right = 0.8635, top = 0.8649, align_to = "full") 
  
  pp
}

p_inc_mun_abs_PDET <- plot_inc_mun_all_PDET(data = inc_tab$orphans,           type.input = type.input, per_n_children = per_n_children, prop = FALSE)
p_inc_mun_rel_PDET <- plot_inc_mun_all_PDET(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE )

# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_mun_abs_PDET.jpeg", sep = ""), plot = p_inc_mun_abs_PDET, width = 2150, height = 1400, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/incidence_mun_rel_PDET.jpeg", sep = ""), plot = p_inc_mun_rel_PDET, width = 2150, height = 1400, units = c("px"), dpi = 300, bg = "white")

##################
##################

plot_box_mun <- function (data, type.input, per_n_children, prop = FALSE, yy = 2015:2021, show_factor = FALSE, errorbar = FALSE, ...) {
  
  CAT_MPM_adapted <- c("[00%, 20%)", "[20%, 40%)", "[40%, 60%)", "[60%, 80%)", "[80%, 100%]")
    
  data <- data %>% mutate(gender = ifelse(gender == "Female", "Mother", "Father"))
  
  if (prop == FALSE) { 
    txt_legend <- "absolute number" 
  } else { 
    txt_legend <- "proportion %"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  data_fem <- data %>% filter(gender == "Mother") %>% arrange(mun, year)
  data_mal <- data %>% filter(gender == "Father") %>% arrange(mun, year)
  data_tot <- data_fem
  data_tot$gender <- "Both"
  data_tot$n_orp <- data_fem$n_orp + data_mal$n_orp
  
  data <- bind_rows(data_tot, data_fem, data_mal)
  data$gender <- factor(data$gender, levels = c("Both", "Mother", "Father"))
  
  tmp_geo_info <- geo_info[, c("mun", "MPM_cat", "MPM_name")] %>% distinct() 
  data <- left_join(x = data, y = tmp_geo_info, by = c("mun"))
  data <- data %>% arrange(gender, year, MPM_cat)
  
  #####
  #####
  #####
  # Include `MPM` by quintile
  data <- data %>% left_join(y = MPM, by = "mun")
  
  # Filter out municipalities with low density (based on `MPM`)
  data[which(data$MPM %>% is.na()), c("n_orp")] <- NA
  data <- data %>% rename(MPM_quint = MPM)
  #####
  #####
  #####
  
  levels_as_char <- sprintf("%05s", as.character(data$mun))
  data <- data %>% mutate(mun = factor(levels_as_char, levels = levels_as_char[1:(length(unique(levels_as_char)))]))
  levels(data$gender) <- c("Children who lost\ntheir mother or father", "Children who lost\ntheir mother", "Children who lost\ntheir father")
  
  ###
  
  ys <- ifelse(length(yy) == 1, yy, paste(range(yy)[1], "-", range(yy)[2], sep = ""))
  
  data_yy <- data %>% filter(year %in% yy)
  data_yy <- na.omit(data_yy)
  
  ns <- data_yy %>% group_by(gender, MPM_quint) %>% summarise(ns = n()) %>% ungroup() %>% select(ns) %>% c() %>% unlist() %>% unname()
  ns <- ns[1:5]
  # ns <- c(ns[1:4], 0, ns[5:6]) # Manually include `0` for `cat = 5`
  ns <- gsub(" ", "", format(ns, big.mark = ","))
  MPM_quint_temp <- as.character(data_yy$MPM_quint)
  # for (i in 1:length(CAT_MPM_adapted)) { CAT_MPM_adapted[i] <- paste(CAT_MPM_adapted[i], "\n(n = ", ns[i], ")", sep = "")  }
  
  # Include number of points
  if (FALSE) {
    for (i in 1:length(MPM_quint_temp)) { 
           if (MPM_quint_temp[i] == CAT_MPM[1]) { MPM_quint_temp[i] <- paste(MPM_quint_temp[i], "\n(n = ", ns[1], ")", sep = "") }
      else if (MPM_quint_temp[i] == CAT_MPM[2]) { MPM_quint_temp[i] <- paste(MPM_quint_temp[i], "\n(n = ", ns[2], ")", sep = "") }
      else if (MPM_quint_temp[i] == CAT_MPM[3]) { MPM_quint_temp[i] <- paste(MPM_quint_temp[i], "\n(n = ", ns[3], ")", sep = "") }
      else if (MPM_quint_temp[i] == CAT_MPM[4]) { MPM_quint_temp[i] <- paste(MPM_quint_temp[i], "\n(n = ", ns[4], ")", sep = "") }
      else if (MPM_quint_temp[i] == CAT_MPM[5]) { MPM_quint_temp[i] <- paste(MPM_quint_temp[i], "\n(n = ", ns[5], ")", sep = "") }
    } 
  }
  
  data_yy$MPM_quint <- factor(x = MPM_quint_temp)
  # data_yy <- data_yy %>% mutate(MPM_cat = factor(MPM_cat, labels = CAT_MPM_adapted, levels = 1:5))
  data_yy <- na.omit(data_yy) %>% group_by(gender, MPM_quint) %>% mutate(outlier_lwr = n_orp < quantile(n_orp, probs = 0.25) - IQR(n_orp) * 1.5,
                                                                         outlier_upr = n_orp > quantile(n_orp, probs = 0.75) + IQR(n_orp) * 1.5) %>% ungroup()
  
  comp_median <- data_yy %>% group_by(gender, MPM_quint) %>% summarise(comp_median = median(n_orp)) %>% ungroup
  comp_median_both <- format(round(exp(diff(log((unname(unlist(c(comp_median[ 1:5 , "comp_median"]))))))), 2), digits = 2)
  comp_median_moth <- format(round(exp(diff(log((unname(unlist(c(comp_median[ 6:10, "comp_median"]))))))), 2), digits = 2)
  comp_median_fath <- format(round(exp(diff(log((unname(unlist(c(comp_median[11:15, "comp_median"]))))))), 2), digits = 2)
  comp_median <- as_tibble(data.frame(x = rep(c(1.5, 2.5, 3.5, 4.5), 3), y = 2.5, mm = paste("\u00D7", c(comp_median_both, comp_median_moth, comp_median_fath), sep = ""), gender = rep(levels(data$gender), each = 4)))
  comp_median$gender <- factor(comp_median$gender, levels = levels(data$gender))
  
  if (errorbar) {
  
    data_errorbar <- data_yy %>% group_by(MPM_quint, gender) %>% summarise(as_tibble_row(quantile(n_orp, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
    tmp_mm <- data_yy %>% group_by(MPM_quint, gender) %>% summarise(mm = mean(n_orp, na.rm = TRUE))
    data_errorbar$mm <- tmp_mm$mm
    
    # pp <- ggplot(data = data_errorbar, mapping = aes(x = MPM_quint)) +
    #   geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), color = "blue", width = 0.5) +
    #   geom_errorbar(aes(ymin = `25%`, ymax = `75%`), color = "red", width = 0.5) +
    #   geom_point(aes(y = mm), color = "black", shape = 4, size = 3) +
    #   facet_wrap(~ gender, ncol = 1, strip.position = "right") +
    #   labs(x = "", y = paste("Orphanhood incidence in ", ys, " by Municipality\n(% of children aged 0-17 years)", sep = "")) +
    #   { if (prop) scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.25), labels = paste(format(seq(0, 1.5, 0.25), nsmall = 2), "%", sep = "")) } +
    #   scale_x_discrete(drop = FALSE) +
    #   theme_bw() +
    #   theme(text = element_text(size = 12, family = "LM Roman 10"),
    #         plot.title = element_text(size = 10),
    #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    #         strip.background = element_rect(fill = NA, color = NA),
    #         axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0), size = 11))
    
    pp <- ggplot(data = data_errorbar, mapping = aes(x = MPM_quint)) +
      geom_boxplot(aes(ymin = `2.5%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `97.5%`), stat = "identity", width = 0.75) +
      geom_point(aes(y = mm), color = "red", shape = 19, size = 2) +
      facet_wrap(~ gender, ncol = 1, strip.position = "right") +
      { if (prop & show_factor) geom_text(data = comp_median, mapping = aes(x = x, y = y, label = mm), family = "LM Roman 10", size = 3.5) } +
      facet_wrap(~ gender, ncol = 1, strip.position = "right") +
      labs(x = "", y = paste("Orphanhood incidence in ", ys, " by Municipality\n(% of children aged 0-17 years)", sep = "")) +
      { if (prop) scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25), labels = paste(format(seq(0, 1.25, 0.25), nsmall = 2), "%", sep = "")) } +
      scale_x_discrete(drop = FALSE) + 
      theme_bw() + 
      theme(text = element_text(size = 12, family = "LM Roman 10"), 
            plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.background = element_rect(fill = NA, color = NA),
            axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0), size = 11))
  
    
  } else {
    
    pp <- ggplot(data_yy) +
      aes(x = MPM_quint, y = n_orp) + 
      geom_boxplot(outlier.shape = NA, width = 0.5) +
      geom_point(data = function(x) subset(x, outlier_lwr | outlier_upr), position = position_jitter(w = 0.175, h = 0), size = 0.5) + # Outliers
      { if (prop & show_factor) geom_text(data = comp_median, mapping = aes(x = x, y = y, label = mm), family = "LM Roman 10", size = 3.5) } +
      facet_wrap(~ gender, ncol = 1, strip.position = "right") +
      labs(x = "", y = paste("Orphanhood incidence in ", ys, " by Municipality\n(% of children aged 0-17 years)", sep = "")) +
      { if (prop) scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5), labels = paste(format(seq(0, 3, 0.5), nsmall = 2), "%", sep = "")) } +
      scale_x_discrete(drop = FALSE) + 
      theme_bw() + 
      theme(text = element_text(size = 12, family = "LM Roman 10"), 
            plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.background = element_rect(fill = NA, color = NA),
            axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0), size = 11))
    
  }
  
  pp
}

errorbar <- TRUE

ys <- 2015:2021
box_mun_conf_abs <- plot_box_mun(data = inc_tab$orphans          , type.input = type.input, per_n_children = per_n_children, prop = FALSE, yy = ys, errorbar = errorbar)
box_mun_conf_rel <- plot_box_mun(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE,  yy = ys, errorbar = errorbar)
# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/boxplot_mun_conflict_abs_2015-2021", ifelse(errorbar, "_errorbar", ""), ".jpeg", sep = ""), plot = box_mun_conf_abs, width = 2000, height = 1650, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/boxplot_mun_conflict_rel_2015-2021", ifelse(errorbar, "_errorbar", ""), ".jpeg", sep = ""), plot = box_mun_conf_rel, width = 2000, height = 1650, units = c("px"), dpi = 300, bg = "white")

ys <- 2021
box_mun_conf_abs <- plot_box_mun(data = inc_tab$orphans          , type.input = type.input, per_n_children = per_n_children, prop = FALSE, yy = ys, errorbar = errorbar)
box_mun_conf_rel <- plot_box_mun(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE,  yy = ys, errorbar = errorbar)
# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/boxplot_mun_conflict_abs_2021", ifelse(errorbar, "_errorbar", ""), ".jpeg", sep = ""), plot = box_mun_conf_abs, width = 2000, height = 1650, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/boxplot_mun_conflict_rel_2021", ifelse(errorbar, "_errorbar", ""), ".jpeg", sep = ""), plot = box_mun_conf_rel, width = 2000, height = 1650, units = c("px"), dpi = 300, bg = "white")

##################
##################

# THIS IS A COPY FROM THE LAST FUNCTION, BUT FOR THE CLASSIFICATION BASED ON THE `PDET`
plot_box_mun_PDET <- function (data, type.input, per_n_children, prop = FALSE, yy = 2015:2021, show_factor = FALSE, errorbar = FALSE, ...) {
  
  CAT_PDET_adapted <- c("No PDET", "PDET")
  
  data <- data %>% mutate(gender = ifelse(gender == "Female", "Mother", "Father"))
  
  if (prop == FALSE) { 
    txt_legend <- "absolute number" 
  } else { 
    txt_legend <- "proportion %"
    data$n_orp <- data$n_orp / (per_n_children / 100)
  }
  
  data_fem <- data %>% filter(gender == "Mother") %>% arrange(mun, year)
  data_mal <- data %>% filter(gender == "Father") %>% arrange(mun, year)
  data_tot <- data_fem
  data_tot$gender <- "Both"
  data_tot$n_orp <- data_fem$n_orp + data_mal$n_orp
  
  data <- bind_rows(data_tot, data_fem, data_mal)
  data$gender <- factor(data$gender, levels = c("Both", "Mother", "Father"))
  
  tmp_geo_info <- geo_info[, c("mun", "PDET")] %>% distinct() 
  data <- left_join(x = data, y = tmp_geo_info, by = c("mun"))
  data <- data %>% arrange(gender, year, PDET)
  
  #####
  #####
  #####
  # Include `MPM` by quintile
  data <- data %>% left_join(y = MPM, by = "mun")
  
  # Filter out municipalities with low density (based on `MPM`)
  data[which(data$MPM %>% is.na()), c("n_orp")] <- NA
  data <- data %>% rename(MPM_quint = MPM)
  #####
  #####
  #####
  
  levels_as_char <- sprintf("%05s", as.character(data$mun))
  data <- data %>% mutate(mun = factor(levels_as_char, levels = levels_as_char[1:(length(unique(levels_as_char)))]))
  levels(data$gender) <- c("Children who lost\ntheir mother or father", "Children who lost\ntheir mother", "Children who lost\ntheir father")
  
  ###
  
  ys <- ifelse(length(yy) == 1, yy, paste(range(yy)[1], "-", range(yy)[2], sep = ""))
  
  data_yy <- data %>% filter(year %in% yy)
  data_yy <- na.omit(data_yy)
  
  ns <- data_yy %>% group_by(gender, PDET) %>% summarise(ns = n()) %>% ungroup() %>% select(ns) %>% c() %>% unlist() %>% unname() %>% unique()
  # ns <- c(ns[1:4], 0, ns[5:6]) # Manually include `0` for `cat = 5`
  ns <- gsub(" ", "", format(ns, big.mark = ","))
  if (TRUE) {
    for (i in 1:length(CAT_PDET_adapted)) { 
      CAT_PDET_adapted[i] <- paste(CAT_PDET_adapted[i], "\n(n = ", ns[i], ")", sep = "")  
      }
  }
  
  data_yy <- data_yy %>% mutate(PDET_fact = factor(PDET, labels = CAT_PDET_adapted, levels = 0:1))
  data_yy <- data_yy %>% group_by(gender, PDET_fact) %>% mutate(outlier_lwr = n_orp < quantile(n_orp, probs = 0.25) - IQR(n_orp) * 1.5,
                                                                outlier_upr = n_orp > quantile(n_orp, probs = 0.75) + IQR(n_orp) * 1.5) %>% ungroup()
  
  comp_median <- data_yy %>% group_by(gender, PDET_fact) %>% summarise(comp_median = median(n_orp)) %>% ungroup
  comp_median_both <- format(round(exp(diff(log((unname(unlist(c(comp_median[1:2, "comp_median"]))))))), 2), digits = 2)
  comp_median_moth <- format(round(exp(diff(log((unname(unlist(c(comp_median[3:4, "comp_median"]))))))), 2), digits = 2)
  comp_median_fath <- format(round(exp(diff(log((unname(unlist(c(comp_median[5:6, "comp_median"]))))))), 2), digits = 2)
  comp_median <- as_tibble(data.frame(x = rep(c(1.5), 3), y = 2.5, mm = paste("\u00D7", c(comp_median_both, comp_median_moth, comp_median_fath), sep = ""), gender = rep(levels(data$gender), each = 4)))
  comp_median$gender <- factor(comp_median$gender, levels = levels(data$gender))
  
  if (errorbar) {
    
    data_errorbar <- data_yy %>% group_by(PDET_fact, gender) %>% summarise(as_tibble_row(quantile(n_orp, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
    tmp_mm <- data_yy %>% group_by(PDET_fact, gender) %>% summarise(mm = mean(n_orp, na.rm = TRUE))
    data_errorbar$mm <- tmp_mm$mm
    
    # pp <- ggplot(data = data_errorbar, mapping = aes(x = PDET_fact)) +
    #   geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), color = "blue", width = 0.5) +
    #   geom_errorbar(aes(ymin = `25%`, ymax = `75%`), color = "red", width = 0.5) +
    #   geom_point(aes(y = mm), color = "black", shape = 4, size = 3) +
    #   facet_wrap(~ gender, ncol = 3, strip.position = "top") +
    #   labs(x = "", y = paste("Orphanhood incidence in ", ys, " by Municipality\n(% of children aged 0-17 years)", sep = "")) +
    #   { if (prop) scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.25), labels = paste(format(seq(0, 1.5, 0.25), nsmall = 2), "%", sep = "")) } +
    #   scale_x_discrete(drop = FALSE) + 
    #   theme_bw() + 
    #   theme(text = element_text(size = 12, family = "LM Roman 10"), 
    #         plot.title = element_text(size = 10),
    #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    #         strip.background = element_rect(fill = NA, color = NA),
    #         axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0), size = 11))
    
    pp <- ggplot(data = data_errorbar, mapping = aes(x = PDET_fact)) +
      geom_boxplot(aes(ymin = `2.5%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `97.5%`), stat = "identity", width = 0.75) +
      geom_point(aes(y = mm), color = "red", shape = 19, size = 2) +
      facet_wrap(~ gender, ncol = 3, strip.position = "top") +
      labs(x = "", y = paste("Orphanhood incidence in ", ys, " by\nMunicipality(% of children aged 0-17 years)", sep = "")) +
      { if (prop) scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.25), labels = paste(format(seq(0, 1.5, 0.25), nsmall = 2), "%", sep = "")) } +
      scale_x_discrete(drop = FALSE) + 
      theme_bw() + 
      theme(text = element_text(size = 12, family = "LM Roman 10"), 
            plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.background = element_rect(fill = NA, color = NA),
            axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0), size = 11))
  
  } else {
    
    pp <- ggplot(data_yy) +
      aes(x = PDET_fact, y = n_orp) + 
      geom_boxplot(outlier.shape = NA, width = 0.5) +
      geom_point(data = function(x) subset(x, outlier_lwr | outlier_upr), position = position_jitter(w = 0.175, h = 0), size = 0.5) + # Outliers
      { if (prop & show_factor) geom_text(data = comp_median, mapping = aes(x = x, y = y, label = mm), family = "LM Roman 10", size = 3.5) } +
      facet_wrap(~ gender, ncol = 3, strip.position = "top") +
      labs(x = "", y = paste("Orphanhood incidence in ", ys, " by\nMunicipality(% of children aged 0-17 years)", sep = "")) +
      { if (prop) scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5), labels = paste(format(seq(0, 3, 0.5), nsmall = 2), "%", sep = "")) } +
      scale_x_discrete(drop = FALSE) + 
      theme_bw() + 
      theme(text = element_text(size = 12, family = "LM Roman 10"), 
            plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.background = element_rect(fill = NA, color = NA),
            axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0), size = 11))
    
  }
  pp
}

errorbar <- TRUE

ys <- 2015:2021
box_mun_conf_abs_PDET <- plot_box_mun_PDET(data = inc_tab$orphans          , type.input = type.input, per_n_children = per_n_children, prop = FALSE, yy = ys, errorbar = errorbar)
box_mun_conf_rel_PDET <- plot_box_mun_PDET(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE,  yy = ys, errorbar = errorbar)
# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/boxplot_mun_conflict_abs_PDET_2015-2021", ifelse(errorbar, "_errorbar", ""), ".jpeg", sep = ""), plot = box_mun_conf_abs_PDET, width = 2500, height = 1100, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/boxplot_mun_conflict_rel_PDET_2015-2021", ifelse(errorbar, "_errorbar", ""), ".jpeg", sep = ""), plot = box_mun_conf_rel_PDET, width = 2500, height = 1100, units = c("px"), dpi = 300, bg = "white")

ys <- 2021
box_mun_conf_abs_PDET <- plot_box_mun_PDET(data = inc_tab$orphans          , type.input = type.input, per_n_children = per_n_children, prop = FALSE, yy = ys, errorbar = errorbar)
box_mun_conf_rel_PDET <- plot_box_mun_PDET(data = inc_tab$orphans_per_child, type.input = type.input, per_n_children = per_n_children, prop = TRUE,  yy = ys, errorbar = errorbar)
# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/boxplot_mun_conflict_abs_PDET_2021", ifelse(errorbar, "_errorbar", ""), ".jpeg", sep = ""), plot = box_mun_conf_abs_PDET, width = 2500, height = 1100, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/boxplot_mun_conflict_rel_PDET_2021", ifelse(errorbar, "_errorbar", ""), ".jpeg", sep = ""), plot = box_mun_conf_rel_PDET, width = 2500, height = 1100, units = c("px"), dpi = 300, bg = "white")

##################
##################
##################
##################
##################
##################
##################

colombia <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry) %>% rename(mun = Code) %>% mutate(mun = factor(mun))
high_res_islands <- readRDS(file = "DATA/high_res_islands.RDS")

# COUNTS
orphans_count_fem <- inc_tab$orphans %>% arrange(mun) %>% filter(gender == "Female") %>% select(-gender)
orphans_count_mal <- inc_tab$orphans %>% arrange(mun) %>% filter(gender == "Male")   %>% select(-gender)
orphans_count <- full_join(x = orphans_count_fem, y = orphans_count_mal, by = c("mun", "mun_name", "year"))
orphans_count$n_orp <- rowSums(cbind(orphans_count$n_orp.x,  orphans_count$n_orp.y), na.rm = TRUE) 
orphans_count <- orphans_count %>% select(-c("n_orp.x", "n_orp.y"))

# RATES
orphans_rates_fem <- inc_tab$orphans_per_child %>% arrange(mun) %>% filter(gender == "Female") %>% select(-gender)
orphans_rates_mal <- inc_tab$orphans_per_child %>% arrange(mun) %>% filter(gender == "Male")   %>% select(-gender)
orphans_rates <- full_join(x = orphans_rates_fem, y = orphans_rates_mal, by = c("mun", "mun_name", "year"))
orphans_rates$n_orp <- rowSums(cbind(orphans_rates$n_orp.x, orphans_rates$n_orp.y), na.rm = TRUE) 
orphans_rates <- orphans_rates %>% select(-c("n_orp.x", "n_orp.y"))

orphans_count <- right_join(x = orphans_count, y = colombia, by = "mun")
orphans_rates <- right_join(x = orphans_rates, y = colombia, by = "mun")

for (yy in 2015:2021) {
  
  tmp_data <- orphans_rates %>% filter(year == yy | is.na(year))
  # tmp_data <- tmp_data %>% mutate_if(is.numeric, coalesce, 0) # Replace NA by 0
  
  #####
  #####
  #####
  # Include `MPM` by quintile
  tmp_data <- tmp_data %>% left_join(y = MPM, by = "mun")
  
  # Filter out municipalities with low density (based on `MPM`)
  tmp_data[which(tmp_data$MPM %>% is.na()), c("n_orp")] <- NA
  tmp_data <- tmp_data %>% rename(MPM_quint = MPM)
  #####
  #####
  #####
  
  mn <- min(tmp_data$n_orp, na.rm = T)
  mx <- max(tmp_data$n_orp, na.rm = T)
  
  is.even <- function (x, ...) { ifelse (x %% 2 == 0, TRUE, FALSE) }
  tmp_mx <- 30 # ifelse(is.even(mx), mx + 0, mx + 1)
  
  rescale <- function (x, a, b, e = 1, f = 100, ...) {
    # Re-scale a value x from the interval [e, f] to the interval [a, b]
    ((x - e) * (b - a) / (f - e)) + a
  }

  breaks <- c(0, 2, 4, 6, 8, 10, 30) # seq(0, tmp_mx, 5)
  # labels <- paste0(seq(0, (tmp_mx - 1), 2), "-", c(seq(1, (tmp_mx - 1), 2), tmp_mx))
  seq_tmp <- paste(format(breaks / 10, nsmall = 1), "%", sep = "")
  labels <- paste("[", seq_tmp[1:(length(seq_tmp) - 1)] , ", ", seq_tmp[-1], ")", sep = "")
  n_labs <- length(labels)
  #chosen_colors <- rev(viridis(n_labs + ceiling(n_labs * 4), begin = 0.2, end = 1, option = "A")) # 15 = n_labs
  #chosen_colors <- rev(viridis(n_labs + ceiling(n_labs * 4), begin = 0.2, end = 1, option = "A")) # 15 = n_labs
  chosen_colors <- plot3D::jet.col(n = (n_labs + ceiling(n_labs * 4)) + 4, alpha = 1)
  chosen_colors <- chosen_colors[3:(length(chosen_colors) - 2)]
  alt_seq <- c(1, 15, 30, 40, 50, 60)[1:n_labs]
  #alt_seq <- c(1, 10, 35, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105)[1:n_labs]
  #alt_seq <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 70, 85, 100)[1:n_labs]
  alt_seq <- round(rescale(alt_seq, a = 1, b = length(chosen_colors), e = min(alt_seq), f = max(alt_seq)), 0)
  chosen_colors <- chosen_colors[alt_seq][1:n_labs]

  # tmp_data <- tmp_data %>% mutate(n_orp = ifelse(n_orp == mx, n_orp - 0.1, n_orp))
  tmp_data$category <- cut(tmp_data$n_orp, breaks = breaks, labels = labels, right = FALSE)
  
  tmp_data <- tmp_data %>% left_join(y = tmp_geo_info[, c("mun", "PDET")], by = "mun")
  tmp_data <- tmp_data %>% mutate(PDET = ifelse(PDET == 0, "NO", "PDET"))
  
  tmp_isl1 <- tmp_data %>% filter( (mun %in% c(88001)))        # Select islands ind.
  tmp_isl2 <- tmp_data %>% filter( (mun %in% c(88564)))        # Select islands ind.
  tmp_isla <- tmp_data %>% filter( (mun %in% c(88001, 88564))) # Select islands
  tmp_data <- tmp_data %>% filter(!(mun %in% c(88001, 88564))) # Remove islands
  
  tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
  tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry
  
  # Municipality `88001`
  col_88001 <- chosen_colors[which(tmp_isl1$category == labels)]
  i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
    geom_sf(fill = col_88001, color = "black") +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  # Municipality `88564`
  col_88564 <- chosen_colors[which(tmp_isl2$category == labels)]
  i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
    geom_sf(fill = col_88564, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  filtered_tmp_data <- tmp_data %>% filter(PDET == "PDET")
  pp <- ggplot() + 
    geom_sf(data = tmp_data, aes(geometry = geometry, fill = category), color = "black") + 
    geom_sf_pattern(data = filtered_tmp_data, aes(geometry = geometry, pattern_type = as.factor(PDET)), 
                    pattern = "crosshatch", 
                    pattern_fill = NA, fill = NA, 
                    pattern_density = 1, 
                    pattern_spacing = 0.005, 
                    pattern_colour = "black",
                    colour = NA, pattern_size = 0.1) + 
    scale_fill_manual(values = chosen_colors, breaks = labels, drop = FALSE, name = paste("Orphanhood incidence in ", yy, "\nby Municipality\n(% of children aged 0-17 years)", sep = ""), na.translate = TRUE) + # name = "Number of children") +
    scale_pattern_type_manual(values = c('crosshatch'), name = "") +
    labs(fill = "Number of orphans", pattern = "") + 
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) + 
    guides(fill = guide_legend(order = 1))
  
  # annotation_custom(ggplotGrob(i1),
  #                   xmin = -78.5, xmax = -77.5,
  #                   ymin =  10.5, ymax =  11.5)
  
  pp <- pp + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  pp <- pp + inset_element(i1, left = 0.05, bottom = 0.8, right = 0.1, top = 0.9, align_to = "full") # Municipality `88001`
  pp <- pp + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  pp <- pp + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/orphans_rate_", yy,".jpeg", sep = ""), plot = pp, width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

############################
############################
############################
# PLOTTING NUMBERS INSTEAD
############################
############################
############################

for (yy in 2015:2021) {
  
  tmp_data <- orphans_count %>% filter(year == yy | is.na(year))
  # tmp_data <- tmp_data %>% mutate_if(is.numeric, coalesce, 0) # Replace NA by 0
  
  #####
  #####
  #####
  # Include `MPM` by quintile
  tmp_data <- tmp_data %>% left_join(y = MPM, by = "mun")
  
  # Filter out municipalities with low density (based on `MPM`)
  tmp_data[which(tmp_data$MPM %>% is.na()), c("n_orp")] <- NA
  tmp_data <- tmp_data %>% rename(MPM_quint = MPM)
  #####
  #####
  #####
  
  mn <- min(tmp_data$n_orp, na.rm = T)
  mx <- max(tmp_data$n_orp, na.rm = T)
  tmp_mx <- 10000
  
  breaks <- c(0, 5, 10, 50, 100, 1000, tmp_mx)
  seq_tmp <- breaks
  labels <- paste("[", seq_tmp[1:(length(seq_tmp) - 1)] , ", ", seq_tmp[-1], ")", sep = "")
  n_labs <- length(labels)
  # chosen_colors <- rev(viridis(n_labs + ceiling(n_labs * 4), begin = 0, end = 1, option = "A")) # 15 = n_labs
  chosen_colors <- plot3D::jet.col(n = (n_labs + ceiling(n_labs * 4)) + 4, alpha = 1)
  chosen_colors <- chosen_colors[3:(length(chosen_colors) - 2)]
  alt_seq <- c(1, 15, 30, 40, 50, 60)[1:n_labs]
  alt_seq <- round(rescale(alt_seq, a = 1, b = length(chosen_colors), e = min(alt_seq), f = max(alt_seq)), 0)
  chosen_colors <- chosen_colors[alt_seq][1:n_labs]
  
  # tmp_data <- tmp_data %>% mutate(n_orp = ifelse(n_orp == mx, n_orp - 0.1, n_orp))
  tmp_data$category <- cut(tmp_data$n_orp, breaks = breaks, labels = labels, right = FALSE)
  
  tmp_data <- tmp_data %>% left_join(y = tmp_geo_info[, c("mun", "PDET")], by = "mun")
  tmp_data <- tmp_data %>% mutate(PDET = ifelse(PDET == 0, "NO", "PDET"))
  
  tmp_isl1 <- tmp_data %>% filter( (mun %in% c(88001)))        # Select islands ind.
  tmp_isl2 <- tmp_data %>% filter( (mun %in% c(88564)))        # Select islands ind.
  tmp_isla <- tmp_data %>% filter( (mun %in% c(88001, 88564))) # Select islands
  tmp_data <- tmp_data %>% filter(!(mun %in% c(88001, 88564))) # Remove islands
  
  tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
  tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry
  
  # Municipality `88001`
  col_88001 <- chosen_colors[which(tmp_isl1$category == labels)]
  i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
    geom_sf(fill = col_88001, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  # Municipality `88564`
  col_88564 <- chosen_colors[which(tmp_isl2$category == labels)]
  i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
    geom_sf(fill = col_88564, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  filtered_tmp_data <- tmp_data %>% filter(PDET == "PDET")
  pp <- ggplot() + 
    geom_sf(data = tmp_data, aes(geometry = geometry, fill = category), color = "black") + 
    geom_sf_pattern(data = filtered_tmp_data, aes(geometry = geometry, pattern_type = as.factor(PDET)), 
                    pattern = "crosshatch", 
                    pattern_fill = NA, fill = NA, 
                    pattern_density = 1, 
                    pattern_spacing = 0.005, 
                    pattern_colour = "black",
                    colour = NA, pattern_size = 0.1) + 
    scale_fill_manual(values = chosen_colors, breaks = labels, drop = FALSE, name = paste("Orphanhood incidence in ", yy, "\nby Municipality\n(Number of children aged 0-17 years)", sep = ""), na.translate = TRUE) + # name = "Number of children") +
    scale_pattern_type_manual(values = c('crosshatch'), name = "") +
    labs(fill = "Number of orphans", pattern = "") + 
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  pp <- pp + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  pp <- pp + inset_element(i1, left = 0.05, bottom = 0.8, right = 0.1, top = 0.9, align_to = "full") # Municipality `88001`
  pp <- pp + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  pp <- pp + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/orphans_count_", yy,".jpeg", sep = ""), plot = pp, width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

