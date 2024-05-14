source("header.R")

geo_info <- read_csv(file = "DATA/geo_info.csv", col_types = "fcfcfcfcfcf") %>% rename(CERAC = cat)

COL.PDET <- c("#43A278", "#D36167")
PDET_LEVELS <- c("No PDET", "PDET")
names(COL.PDET) <- PDET_LEVELS

COL.TYPE <- c("#476314", "#71a800", "#feb502", "#ffe77e", "#0083a8", "#7ab5f5", "#bdf0ff")
CAT_LEVELS <- c("Strongly affected and persistent", "Mildly affected and persistent", "Strongly affected and disrupted", "Mildly affected and disrupted", "Strongly affected and finished", "Midly affected and finished", "With no conflicts")

COL.MPM <- rev(pal_frontiers()(5)) 
CAT_MPM <- geo_info %>% select(MPM_cat, MPM_name) %>% distinct() %>% arrange(MPM_cat) %>% select(MPM_name) %>% c() %>% unlist() %>% unname() 
names(COL.MPM) <- CAT_MPM

##### Compute municipalities with small population

exclude_low_density <- TRUE
n_excluded_muns <- 29

original_pop <- read_csv("DATA/Population from Census/backup_cp_pop_years_list_municipality.csv")
original_pop <- original_pop %>% filter(Year == 2018)
original_pop <- original_pop %>% group_by(Mun) %>% summarise(total_pop = sum(Population)) %>% ungroup() %>% rename(mun = Mun) %>% mutate(mun = factor(mun))

col_tmp <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry) %>% rename(mun = Code) %>% mutate(mun = factor(mun))
col_tmp <- col_tmp %>% mutate(area = units::set_units(st_area(geometry), km^2))
col_tmp <- col_tmp %>% left_join(y = original_pop, by = "mun")
col_tmp <- col_tmp %>% mutate(pop_density = (total_pop / area)) %>% arrange(desc(pop_density)) %>% select(-geometry)
col_tmp <- col_tmp %>% left_join(y = geo_info[, c("mun", "mun_name", "dep_name")], by = "mun") %>% select(mun, mun_name, dep_name, total_pop, area, pop_density) %>% rename(population = total_pop)

less_dense_mun <- col_tmp %>% tail(n_excluded_muns) %>% select(mun) %>% c() %>% unlist() %>% unname() %>% as.character() %>% as.numeric()

#####

colombia <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry) %>% rename(mun = Code) %>% mutate(mun = factor(mun))
high_res_islands <- readRDS(file = "DATA/high_res_islands.RDS") %>% rename(mun = code) %>% mutate(mun = factor(mun))

colombia <- left_join(x = colombia, y = geo_info[, c("mun", "CERAC", "PDET", "MPM_cat")], by = "mun")
high_res_islands <- left_join(x = high_res_islands, y = geo_info[, c("mun", "CERAC", "PDET", "MPM_cat")], by = "mun")

##########
##########

# Order the municipalities based on `something`
# `POVERTY`

MPM <- geo_info[, c("mun", "MPM")]
MPM <- MPM  %>% arrange(MPM)
if (exclude_low_density) { MPM <- MPM %>% filter(!(as.numeric(as.character(mun)) %in% less_dense_mun)) }

n_mun <- MPM %>% nrow()
n_per <- floor(n_mun / 5)

MPM$quint_MPM <- c(rep(1:5, each = n_per), rep(5, n_mun - (n_per * 5)))
MPM <- MPM %>% mutate(quint_MPM = factor(x = quint_MPM, labels = CAT_MPM, levels = 1:5))
MPM <- MPM %>% select(mun, quint_MPM) %>% rename(MPM = quint_MPM)

colombia <- colombia %>% left_join(y = MPM, by = "mun")
colombia <- colombia %>% left_join(y = col_tmp[, c("mun", "area", "pop_density")], by = "mun")

##########
##########

colombia[, "CERAC"]   <- factor(unname(unlist(c(colombia[, "CERAC"]))), labels = CAT_LEVELS, levels = 1:7)
high_res_islands[, "CERAC"]   <- factor(unname(unlist(c(high_res_islands[, "CERAC"]))), labels = CAT_LEVELS, levels = 1:7)
colombia[, "PDET"]   <- factor(unname(unlist(c(colombia[, "PDET"]))), labels = PDET_LEVELS, levels = 0:1)
high_res_islands[, "PDET"]   <- factor(unname(unlist(c(high_res_islands[, "PDET"]))), labels = PDET_LEVELS, levels = 0:1)
colombia[, "MPM_cat"]   <- factor(unname(unlist(c(colombia[, "MPM_cat"]))), labels = CAT_MPM, levels = 1:5)
high_res_islands[, "MPM_cat"]   <- factor(unname(unlist(c(high_res_islands[, "MPM_cat"]))), labels = CAT_MPM, levels = 1:5)

# Filter out municipalities with low density (based on `MPM`)
colombia[which(colombia$MPM %>% is.na()), c("CERAC", "PDET", "MPM_cat")] <- NA

isl1 <- colombia %>% filter( (mun %in% c(88001)))        # Select islands ind.
isl2 <- colombia %>% filter( (mun %in% c(88564)))        # Select islands ind.
isla <- colombia %>% filter( (mun %in% c(88001, 88564))) # Select islands
colombia <- colombia %>% filter(!(mun %in% c(88001, 88564))) # Remove islands

isl1$geometry <- high_res_islands[high_res_islands$mun == 88001, ]$geometry
isl2$geometry <- high_res_islands[high_res_islands$mun == 88564, ]$geometry

#####

plot_maps <- function (my_colors, my_var, my_labels, ...) {
  # Municipality `88001`
  col_88001 <- my_colors[which(as.character(unlist(c((isl1[, my_var])))) == my_labels)]
  i1 <- ggplot(data = isl1, aes(geometry = geometry)) + 
    geom_sf(fill = col_88001, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  # Municipality `88564`
  col_88564 <- my_colors[which(as.character(unlist(c((isl2[, my_var])))) == my_labels)]
  i2 <- ggplot(data = isl2, aes(geometry = geometry)) + 
    geom_sf(fill = col_88564, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  pp <- ggplot(data = colombia, aes(geometry = geometry)) + 
    geom_sf(aes(fill = .data[[my_var]]), color = "black") + 
    scale_fill_manual(values = my_colors, drop = FALSE, name = "") +
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
  
  pp <- pp + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  pp <- pp + inset_element(i1, left = 0.05, bottom = 0.8, right = 0.1, top = 0.9, align_to = "full") # Municipality `88001`
  pp <- pp + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  pp <- pp + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  pp
}

map_PDET  <- plot_maps(my_colors = COL.PDET, my_var = "PDET" , my_labels = PDET_LEVELS)
map_CERAC <- plot_maps(my_colors = COL.TYPE, my_var = "CERAC", my_labels = CAT_LEVELS )
map_MPM   <- plot_maps(my_colors = COL.MPM,  my_var = "MPM",   my_labels = CAT_MPM    )
# map_MPM   <- plot_maps(my_colors = COL.MPM,  my_var = "MPM_cat", my_labels = CAT_MPM)

ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/map_PDET.jpeg" , sep = ""), plot = map_PDET , width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/map_CERAC.jpeg", sep = ""), plot = map_CERAC, width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/map_MPM.jpeg"  , sep = ""), plot = map_MPM  , width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")



################################################################################
################################################################################
# PLOT MAPS WITH CONTINUOUS VARIABLES (e.g., `area` and `population_density`)
################################################################################
################################################################################

plot_continuous_map <- function (my_var = "pop_density", ...) {
  nn <- ifelse(my_var == "pop_density", "Population density\nper square kilometre\n(log-scale)", "Area\nin square kilometre\n(log-scale)")
  
  pp <- ggplot(data = colombia, aes(geometry = geometry)) + 
    geom_sf(aes(fill = as.numeric(.data[[my_var]])), color = "black") + 
    # scale_fill_manual(values = my_colors, drop = FALSE, name = "") +
    scale_fill_gradientn(name = nn, trans = "log", colours = rainbow(999, start = 0.1, end = 0.9)) +
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
  
  pp
}

pp_dens <- plot_continuous_map(my_var = "pop_density")
pp_area <- plot_continuous_map(my_var = "area")

ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/map_pop_dens.jpeg" , sep = ""), plot = pp_dens , width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/map_pop_area.jpeg" , sep = ""), plot = pp_area , width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")

