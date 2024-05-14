source("header.R")
source("PROCESS_DATA/aux_process.R")
source("PROCESS_DATA/aux_orphanhood.R")

##################################################
# Set common parameters and read data
##################################################

standardized <- TRUE
exclude_ages <- FALSE
gender <- "ALL" # "F", "M", "ALL"
yys <- 2005

exclude_low_density <- TRUE
n_excluded_muns <- 29

per1K <- TRUE
type.input <- "Municipality"

rates <- readRDS(file = "PROCESS_DATA/EQUALIZED_RATES.RDS")

mortality_rates <- rates$mortality_rates
fertility_rates <- rates$fertility_rates

# Rates
mortality_rates$population <- ceiling(mortality_rates$population)
fertility_rates$population <- ceiling(fertility_rates$population)
mortality_rates$death_rate <- compute_rate(count = mortality_rates$deaths, pop = mortality_rates$population)
fertility_rates$fertility_rate <- compute_rate(count = fertility_rates$births, pop = fertility_rates$population)

# Original death count
death_count <- read_csv(file = "DATA/Deaths/col_83-21_deaths_municipality.csv")
death_count$mun <- factor(death_count$mun)

# Assuming that the `pop_years_list_municipality.csv` file was correctly updated
population <- read_csv(file = "DATA/Population from Census/pop_years_list_municipality.csv")
population <- population %>% filter(Year <= 2021) %>% mutate(gender = Sex, loc = factor(Mun), year = Year, age = Age, population = Population) %>% select(gender, loc, year, age, population) %>% arrange(gender, loc, year, age)

# Municipality `27086` (Belén de Bajirá, only created in 2022)  missing
geo_info <- read_csv(file = "DATA/geo_info.csv")
geo_info$mun <- factor(geo_info$mun)
geo_info$dep <- factor(geo_info$dep)
geo_info$reg <- factor(geo_info$reg)
geo_info$nat <- factor(geo_info$nat)

muns <- unique(geo_info$mun)

mortality_rates <- mortality_rates %>% filter(loc %in% muns) %>% arrange(gender, loc, year, age)
fertility_rates <- fertility_rates %>% filter(loc %in% muns) %>% arrange(gender, loc, year, age)
population <- population %>% filter(loc %in% muns) %>% arrange(gender, loc, year, age)

##################################################
# Convert it to the desired resolution
##################################################

tmp_data <- convert_resolution(geo_info = geo_info, mortality_rates = mortality_rates, fertility_rates = fertility_rates, population = population, death_count = death_count, type.input = type.input)

mortality_rates <- tmp_data$mortality_rates
fertility_rates <- tmp_data$fertility_rates
population      <- tmp_data$population

##################################################
# Summarize quantities
##################################################

original_pop <- read_csv("DATA/Population from Census/backup_cp_pop_years_list_municipality.csv")
original_pop <- original_pop %>% filter(Year == 2018)
cp_original_pop <- original_pop %>% dplyr::select(-Year) %>% rename(loc = Mun, age = Age, gender = Sex, population = Population) %>% mutate(loc = factor(loc))
original_pop <- original_pop %>% group_by(Mun) %>% summarise(total_pop = sum(Population)) %>% ungroup() %>% rename(mun = Mun) %>% mutate(mun = factor(mun))

col_tmp <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry) %>% rename(mun = Code) %>% mutate(mun = factor(mun))
col_tmp <- col_tmp %>% mutate(area = st_area(geometry))
col_tmp <- col_tmp %>% left_join(y = original_pop, by = "mun")
col_tmp <- col_tmp %>% mutate(pop_density = (total_pop / area)) %>% arrange(desc(pop_density)) %>% select(-geometry)
col_tmp <- col_tmp %>% left_join(y = geo_info[, c("mun", "mun_name", "dep_name")], by = "mun") %>% select(mun, mun_name, dep_name, total_pop, area, pop_density) %>% rename(population = total_pop)

less_dense_mun <- col_tmp %>% tail(n_excluded_muns) %>% select(mun) %>% c() %>% unlist() %>% unname() %>% as.character() %>% as.numeric()

##################################################
# Standardize quantities
##################################################

ref_population <- population %>% filter(year == 2018, age != "0-9", !loc %in% less_dense_mun) %>% select(-year)
ref_mort_rates <- mortality_rates %>% filter(!loc %in% less_dense_mun)
ref_fert_rates <- fertility_rates %>% filter(!loc %in% less_dense_mun)

ref_population_fem <- ref_population %>% filter(gender == "Female")
ref_population_mal <- ref_population %>% filter(gender == "Male")
ref_mort_rates_fem <- ref_mort_rates %>% filter(gender == "Female")
ref_mort_rates_mal <- ref_mort_rates %>% filter(gender == "Male")
ref_fert_rates_fem <- ref_fert_rates %>% filter(gender == "Female")
ref_fert_rates_mal <- ref_fert_rates %>% filter(gender == "Male")

if (exclude_ages) {
  ref_mort_rates_fem <- ref_mort_rates_fem %>% filter(!age %in% c("10-14", "50+"))
  ref_mort_rates_mal <- ref_mort_rates_mal %>% filter(!age %in% c("10-14", "55+"))
  ref_fert_rates_fem <- ref_fert_rates_fem %>% filter(!age %in% c("10-14"))
  ref_fert_rates_mal <- ref_fert_rates_mal %>% filter(!age %in% c("10-14", "55+"))
}

data_mort_fem <- ref_mort_rates_fem %>% filter(year == 2018) %>% select(-c(gender, year, population, death_rate, mod)) %>% group_by(loc) %>% summarise(ref_deaths = sum(deaths)) %>% ungroup
ref_mort_rates_fem <- ref_mort_rates_fem %>% left_join(y = data_mort_fem, by = "loc")
ref_mort_rates_fem <- ref_mort_rates_fem %>% left_join(y = rename(ref_population_fem, pop_2018 = population), by = c("gender", "loc", "age"))
ref_mort_rates_fem <- ref_mort_rates_fem %>% mutate(exp_count = death_rate * pop_2018)
ref_mort_rates_fem <- ref_mort_rates_fem %>% select(loc, year, age, pop_2018, exp_count) %>% group_by(loc, year) %>% summarise(pop_2018 = sum(pop_2018), exp_count = sum(exp_count)) %>% ungroup()
ref_mort_rates_fem <- ref_mort_rates_fem %>% mutate(exp_rate = exp_count / pop_2018) %>% select(-c(pop_2018, exp_count))
ref_mort_rates_fem <- ref_mort_rates_fem %>% mutate(gender = "Female") %>% rename(death_rate = exp_rate) %>% select(gender, loc, year, death_rate)

data_mort_mal <- ref_mort_rates_mal %>% filter(year == 2018) %>% select(-c(gender, year, population, death_rate, mod)) %>% group_by(loc) %>% summarise(ref_deaths = sum(deaths)) %>% ungroup
ref_mort_rates_mal <- ref_mort_rates_mal %>% left_join(y = data_mort_fem, by = "loc")
ref_mort_rates_mal <- ref_mort_rates_mal %>% left_join(y = rename(ref_population_mal, pop_2018 = population), by = c("gender", "loc", "age"))
ref_mort_rates_mal <- ref_mort_rates_mal %>% mutate(exp_count = death_rate * pop_2018)
ref_mort_rates_mal <- ref_mort_rates_mal %>% select(loc, year, age, pop_2018, exp_count) %>% group_by(loc, year) %>% summarise(pop_2018 = sum(pop_2018), exp_count = sum(exp_count)) %>% ungroup()
ref_mort_rates_mal <- ref_mort_rates_mal %>% mutate(exp_rate = exp_count / pop_2018) %>% select(-c(pop_2018, exp_count))
ref_mort_rates_mal <- ref_mort_rates_mal %>% mutate(gender = "Male") %>% rename(death_rate = exp_rate) %>% select(gender, loc, year, death_rate)

data_fert_fem <- ref_fert_rates_fem %>% filter(year == 2018) %>% select(-c(gender, year, population, fertility_rate, mod)) %>% group_by(loc) %>% summarise(ref_births = sum(births)) %>% ungroup
ref_fert_rates_fem <- ref_fert_rates_fem %>% left_join(y = data_fert_fem, by = "loc")
ref_fert_rates_fem <- ref_fert_rates_fem %>% left_join(y = rename(ref_population_fem, pop_2018 = population), by = c("gender", "loc", "age"))
ref_fert_rates_fem <- ref_fert_rates_fem %>% mutate(exp_count = fertility_rate * pop_2018)
ref_fert_rates_fem <- ref_fert_rates_fem %>% select(loc, year, age, pop_2018, exp_count) %>% group_by(loc, year) %>% summarise(pop_2018 = sum(pop_2018), exp_count = sum(exp_count)) %>% ungroup()
ref_fert_rates_fem <- ref_fert_rates_fem %>% mutate(exp_rate = exp_count / pop_2018) %>% select(-c(pop_2018, exp_count))
ref_fert_rates_fem <- ref_fert_rates_fem %>% mutate(gender = "Female") %>% rename(fertility_rate = exp_rate) %>% select(gender, loc, year, fertility_rate)

data_fert_mal <- ref_fert_rates_mal %>% filter(year == 2018) %>% select(-c(gender, year, population, fertility_rate, mod)) %>% group_by(loc) %>% summarise(ref_births = sum(births)) %>% ungroup
ref_fert_rates_mal <- ref_fert_rates_mal %>% left_join(y = data_fert_fem, by = "loc")
ref_fert_rates_mal <- ref_fert_rates_mal %>% left_join(y = rename(ref_population_mal, pop_2018 = population), by = c("gender", "loc", "age"))
ref_fert_rates_mal <- ref_fert_rates_mal %>% mutate(exp_count = fertility_rate * pop_2018)
ref_fert_rates_mal <- ref_fert_rates_mal %>% select(loc, year, age, pop_2018, exp_count) %>% group_by(loc, year) %>% summarise(pop_2018 = sum(pop_2018), exp_count = sum(exp_count)) %>% ungroup()
ref_fert_rates_mal <- ref_fert_rates_mal %>% mutate(exp_rate = exp_count / pop_2018) %>% select(-c(pop_2018, exp_count))
ref_fert_rates_mal <- ref_fert_rates_mal %>% mutate(gender = "Male") %>% rename(fertility_rate = exp_rate) %>% select(gender, loc, year, fertility_rate)

if (gender == "M") {
  ref_mort_rates <- ref_mort_rates_mal
  ref_fert_rates <- ref_fert_rates_mal
} else if (gender == "F") {
  ref_mort_rates <- ref_mort_rates_fem
  ref_fert_rates <- ref_fert_rates_fem
} else {
  ref_mort_rates <- bind_rows(ref_mort_rates_fem, ref_mort_rates_mal)
  ref_fert_rates <- bind_rows(ref_fert_rates_fem, ref_fert_rates_mal)
}

  ##############################
  # Summarize quantities
  ##############################
  
  # yys <- 2021 # 1998:2021
  
  ref_mort_rates_summ <- ref_mort_rates %>% filter(year %in% yys) %>% select(gender, loc, year, death_rate    ) %>% group_by(loc) %>% summarize(death_rate     = mean(death_rate))     %>% arrange(death_rate)
  ref_fert_rates_summ <- ref_fert_rates %>% filter(year %in% yys) %>% select(gender, loc, year, fertility_rate) %>% group_by(loc) %>% summarize(fertility_rate = mean(fertility_rate)) %>% arrange(fertility_rate)
  
  n_mun <- ref_mort_rates_summ %>% nrow()
  n_per <- floor(n_mun / 5)
  
  ref_mort_rates_summ$quint_mort <- c(rep(1:5, each = n_per), rep(5, n_mun - (n_per * 5)))
  ref_fert_rates_summ$quint_fert <- c(rep(1:5, each = n_per), rep(5, n_mun - (n_per * 5)))
  
##################################################
# Summarize quantities
##################################################

# yys <- 2021 # 1998:2021

mortality_rates_summ <- mortality_rates %>% filter(year %in% yys) %>% select(gender, loc, year, age, death_rate    ) %>% group_by(loc) %>% summarize(death_rate     = mean(death_rate))     %>% arrange(death_rate)
fertility_rates_summ <- fertility_rates %>% filter(year %in% yys) %>% select(gender, loc, year, age, fertility_rate) %>% group_by(loc) %>% summarize(fertility_rate = mean(fertility_rate)) %>% arrange(fertility_rate)

if (exclude_low_density) {
  mortality_rates_summ <- mortality_rates_summ %>% filter(!(as.numeric(as.character(loc)) %in% less_dense_mun))
  fertility_rates_summ <- fertility_rates_summ %>% filter(!(as.numeric(as.character(loc)) %in% less_dense_mun))
}

n_mun <- mortality_rates_summ %>% nrow()
n_per <- floor(n_mun / 5)

mortality_rates_summ$quint_mort <- c(rep(1:5, each = n_per), rep(5, n_mun - (n_per * 5)))
fertility_rates_summ$quint_fert <- c(rep(1:5, each = n_per), rep(5, n_mun - (n_per * 5)))


  ##############################
  # Select standardized
  ##############################

  if (standardized) {
    mortality_rates_summ <- ref_mort_rates_summ
    fertility_rates_summ <- ref_fert_rates_summ
  }

##################################################
# Plot quantities
##################################################

colombia <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry) %>% rename(mun = Code) %>% mutate(mun = factor(mun))
high_res_islands <- readRDS(file = "DATA/high_res_islands.RDS") %>% rename(mun = code) %>% mutate(mun = factor(mun))

COL.RATE <- rev(pal_frontiers()(5)) 
CAT_RATE <- c("[00%,  20%)", "[20%,  40%)", "[40%,  60%)", "[60%,  80%)", "[80%, 100%]")
names(COL.RATE) <- CAT_RATE

geo_info <- read_csv(file = "DATA/geo_info.csv", col_types = "fcfcfcfc")
geo_info <- geo_info %>% select(mun) %>% rename(loc = mun)
# `NAs` come from the non-utilized municipalities
geo_info <- geo_info %>% left_join(y = mortality_rates_summ, by = "loc")
geo_info <- geo_info %>% left_join(y = fertility_rates_summ, by = "loc")

geo_info <- geo_info %>% rename(mun = loc)
zero_one <- function (x, na.rm = TRUE, ...) { ((x - min(x, na.rm = na.rm)) / diff(range(x, na.rm = na.rm))) }
geo_info <- geo_info %>% mutate(death_rate = zero_one(death_rate), fertility_rate = zero_one(fertility_rate))
geo_info <- geo_info %>% mutate(death_rate_cat = ifelse(death_rate < 0.2, 1, ifelse(death_rate < 0.4, 2, ifelse(death_rate < 0.6, 3, ifelse(death_rate < 0.8, 4, 5)))))
geo_info <- geo_info %>% mutate(fertility_rate_cat = ifelse(fertility_rate < 0.2, 1, ifelse(fertility_rate < 0.4, 2, ifelse(fertility_rate < 0.6, 3, ifelse(fertility_rate < 0.8, 4, 5)))))
geo_info <- geo_info %>% mutate(death_rate_cat = factor(x = death_rate_cat, labels = CAT_RATE, levels = 1:5))
geo_info <- geo_info %>% mutate(fertility_rate_cat = factor(x = fertility_rate_cat, labels = CAT_RATE, levels = 1:5))
geo_info <- geo_info %>% mutate(quint_fert = factor(x = quint_fert, labels = CAT_RATE, levels = 1:5))
geo_info <- geo_info %>% mutate(quint_mort = factor(x = quint_mort, labels = CAT_RATE, levels = 1:5))

colombia <- left_join(x = colombia, y = geo_info, by = "mun")
high_res_islands <- left_join(x = high_res_islands, y = geo_info, by = "mun")

# PLOT MAP WITH SUMMARIZED DEATH OF FERTILITY RATES (PER QUINTILE)
plot_rates_mun <- function (my_colors, my_var, my_labels, ...) {
  # Process islands
  isl1 <- colombia %>% filter((mun %in% c(88001)))        
  isl2 <- colombia %>% filter((mun %in% c(88564)))        
  isla <- colombia %>% filter((mun %in% c(88001, 88564))) 
  colombia <- colombia %>% filter(!(mun %in% c(88001, 88564))) 
  isl1$geometry <- high_res_islands[high_res_islands$mun == 88001, ]$geometry
  isl2$geometry <- high_res_islands[high_res_islands$mun == 88564, ]$geometry
  
  # Plot map
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
    scale_fill_manual(values = my_colors, drop = FALSE, name = ifelse((my_var == "death_rate_cat") | (my_var == "quint_mort"), "Death rate (quintiles)\nAvg. over gender and age group", "Fertility rate (quintiles)\nAvg. over gender and age group")) +
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 12),
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
  pp <- pp
  
  pp
}

my_var_1 <- "quint_mort" # "death_rate_cat"
my_var_2 <- "quint_fert" # "fertility_rate_cat"
pp_death     <- plot_rates_mun(my_colors = COL.RATE, my_var = my_var_1, my_labels = CAT_RATE)
pp_fertility <- plot_rates_mun(my_colors = COL.RATE, my_var = my_var_2, my_labels = CAT_RATE)
pp_total     <- pp_death + pp_fertility

# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/summ_death_rate_cat.jpeg", sep = ""), plot = pp_death, width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/summ_fertility_rate_cat.jpeg", sep = ""), plot = pp_fertility, width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/", as.character(yys), "/summ_death_fertility_rate_cat_", as.character(standardized), "_", gender, ".jpeg", sep = ""), plot = pp_total, width = 5500, height = 2600, units = c("px"), dpi = 300, bg = "white")



