suppressMessages(library(tidyverse))
suppressMessages(library(sf))
suppressMessages(library(rcartocolor))
suppressMessages(library(patchwork))

compute_rate <- function (count, pop, ...) { 
  r <- (count / pop) 
  r[is.nan(r)] <- 0 # 0/0 
  r
}

##############################
# Read data
##############################

home.dir <- "/Users/or105/Library/CloudStorage/OneDrive-ImperialCollegeLondon/OR_Work/OCAY_Program"
home.dir <- "/Users/avramaral/Library/CloudStorage/OneDrive-SharedLibraries-ImperialCollegeLondon/Ratmann, Oliver - OCAY_Program"
data.dir <- file.path(home.dir, "Colombia_primary_data")
out.dir <- file.path(home.dir, "Colombia_analysis_reporting_bias_in_deaths")
stamp <- gsub("-", "", Sys.Date())

data <- readRDS(paste(data.dir, "/summarised_data/raw_rates.RDS", sep = ""))
fert <- data$fertility_rates %>% dplyr::select(-fertility_rate)
mort <- data$mortality_rates %>% dplyr::select(-death_rate)

total_pop <- readRDS(paste(data.dir, "/summarised_data/pop_all.RDS", sep = ""))
gdp       <- readRDS(paste(data.dir, "/summarised_data/gbp.RDS", sep = ""))
geo_info  <- read_csv(paste(data.dir, "/summarised_data/geo_info.csv", sep = ""), col_types = cols())

# Check capital of "Cundinamarca"
geo_info <- geo_info %>% dplyr::select(dep, dep_name, mun, mun_name) %>% mutate(capital = ifelse(mun == (dep * 1e3 + 1), 1, 0)) %>% arrange(dep)
geo_info$mun <- factor(geo_info$mun)

# colombia <- readRDS(paste(data.dir, "/summarised_data/colombia_map_municipality.RDS", sep = ""))
official_colombia_file <- paste(data.dir, "/summarised_data/official_colombia_map_municipality.RDS", sep = "")
if (!file.exists(official_colombia_file)) {
  colombia <- read_sf(dsn = paste(data.dir, "/MGN2018_00_COLOMBIA/ADMINISTRATIVO/", sep = ""), layer = "MGN_MPIO_POLITICO")
  colombia <- colombia %>% mutate(mun = factor(as.numeric(paste(DPTO_CCDGO, MPIO_CCDGO, sep = "")))) %>% dplyr::select(mun, geometry)
  # Merge `94343` and `94663` (`BARRANCOMINAS` and `MAPIRIPANA`), since July 2019
  tmp_data <- colombia %>% filter( (mun %in% c(94343, 94663))) %>% summarise(mun = 94343, geometry = st_union(geometry))
  colombia <- colombia %>% filter(!(mun %in% c(94343, 94663)))
  colombia <- bind_rows(tmp_data, mutate(colombia, mun = as.numeric(as.character(mun)))) %>% arrange(mun) %>% mutate(mun = factor(as.numeric(as.character(mun))))
  colombia <- colombia %>% right_join(y = geo_info[, "mun"], by = "mun") %>% dplyr::select(mun, geometry)
  saveRDS(object = colombia, file = official_colombia_file)
} else {
  colombia <- readRDS(file = official_colombia_file)
}

# colombia <- colombia %>% rename(mun = Code) %>% mutate(mun = factor(mun)) %>% dplyr::select(mun, geometry)

# Proportion of individuals within 5-year windows
prop_65_66_fem  <- read_csv(paste(data.dir, "/summarised_data/pop_65_66_mun_fem.csv", sep = ""), col_types = cols())
prop_75_76_fem  <- read_csv(paste(data.dir, "/summarised_data/pop_75_76_mun_mal.csv", sep = ""), col_types = cols())

##############################
# Analyse raw data
##############################

# Reference population (computed based on `mort`)
pop_ref <- total_pop %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()
pop_ref <- mort %>% dplyr::select(loc, year, gender, age, population) %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()

# `gender` & `age` death rates
mort <- mort %>% mutate(death_rate = compute_rate(deaths, population))
# Fix `x / 0`, such that `x > 0`; i.e., death with no population
mort <- mort %>% mutate(deaths = ifelse(is.infinite(death_rate), 0, deaths))
mort <- mort %>% mutate(death_rate = compute_rate(deaths, population))

mort <- mort %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_mort <- filter(mort, year == 2018) %>% group_by(loc, year) %>% summarise(pop_n = sum(population),
                                                                               deaths = sum(deaths),
                                                                               deaths_r = (sum(deaths) / sum(population)),
                                                                               deaths_r_std = (sum(death_rate * national_pop) / national_pop_total)) %>% ungroup()

# Add `dep`
stand_mort <- stand_mort %>% rename(mun = loc) %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `gdp` (missing 1998-2004 and 2020-2021)
stand_mort <- stand_mort %>% left_join(y = gdp, by = c("year", "dep")) 
# stand_mort <- stand_mort %>% filter(year == 2018)

stand_mort <- stand_mort %>% mutate(deaths_r_std_disc = ifelse(deaths_r_std == 0, " 0",
                                                        ifelse(deaths_r_std <= 1e-3, "<=1 per 1000",
                                                        ifelse(deaths_r_std <= 2e-3, "<=2 per 1000",
                                                        ifelse(deaths_r_std <= 3e-3, "<=3 per 1000",
                                                        ifelse(deaths_r_std <= 4e-3, "<=4 per 1000", ">4 per 1000"))))))

tmp <- colombia %>% left_join(y = stand_mort[, c("mun", "deaths_r_std_disc")], by = "mun") %>% rename(value = deaths_r_std_disc) %>% dplyr::select(mun, value, geometry)

m1 <- ggplot() +
  geom_sf(data = tmp, aes(geometry = geometry, fill = value), color = "black") +
  scale_fill_carto_d(name = "Standardized mortality rate in 2018", palette = "Burg", direction = 1) +
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  guides(fill = guide_legend(order = 1))

ggsave(m1, file = file.path(out.dir, paste("Colombia_mortality_rate_2018_", stamp, ".png", sep = "")), width = 10, height = 10)

##############################
# Per department (and capital)
##############################

stand_mort <- stand_mort %>% left_join(y = geo_info[, c("mun", "dep_name", "capital")], by = "mun")

tmp <- stand_mort %>% arrange(gdp_per_capita)
sorted_deps <- tmp$dep_name %>% unique()
tmp <- tmp %>% mutate(dep_name_gdp = factor(match(dep_name, sorted_deps), levels = match(dep_name, sorted_deps), labels = dep_name))

b1 <- ggplot(data = tmp, aes(x = dep_name_gdp, y = deaths_r_std)) +
  geom_boxplot() +
  geom_point(data = dplyr::filter(tmp, capital == 1), col = "red") +
  coord_flip() + 
  theme_bw() +
  ylim(0, 0.01) + ylab("Standardized\nmortality rate") +  xlab("Departments ordered by GBP")

ggsave(b1, file = file.path(out.dir, paste("Colombia_mortality_rate_2018_capital_vs_mun_ordered_by_gdp_", stamp, ".png", sep = "")), width = 10, height = 10)

##############################
# Adjust non-capital rates
##############################

shift_to_percentile <- function (others, capital, percentile = 0.66, ...) {
  target_value <- quantile(others, probs = percentile)
  shift_value <- capital - target_value
  # Shifted values
  others + shift_value
}

shifted_stand_mort <- stand_mort %>% dplyr::select(mun, dep, capital, deaths_r_std)
unique_deps <- sort(unique(shifted_stand_mort$dep))
for (i in 1:length(unique_deps)) {
  tmp_data <- shifted_stand_mort %>% filter(dep == unique_deps[i])
  if (nrow(tmp_data) != 1) {
    tmp_data <- tmp_data %>% arrange(desc(capital))
    o <- tail(tmp_data$deaths_r_std, (nrow(tmp_data) - 1))
    k <- tmp_data$deaths_r_std[1]
    tmp_shifted <- shift_to_percentile(others = o, capital = k, percentile = 0.66)
    tmp_data$deaths_r_std[2:nrow(tmp_data)] <- tmp_shifted
  }
  if (i == 1) {
    cp_shifted_stand_mort <- tmp_data
  } else {
    cp_shifted_stand_mort <- bind_rows(cp_shifted_stand_mort, tmp_data)
  }
}
cp_shifted_stand_mort <- cp_shifted_stand_mort %>% mutate(deaths_r_std = ifelse(deaths_r_std < 0, 0, deaths_r_std))
shifted_stand_mort <- cp_shifted_stand_mort %>% arrange(dep)

# shifted_stand_mort <- shifted_stand_mort %>% 
#                         group_by(dep) %>%  
#                         mutate(deaths_r_std = ifelse(capital == 0, 
#                                                      shift_to_percentile(deaths_r_std[capital == 0], deaths_r_std[capital == 1], 0.66), 
#                                                      deaths_r_std)) %>% ungroup()

# Comment: there is no need to manually fix `Bogotá`, as it is a capital

shifted_stand_mort <- dplyr::select(stand_mort, -deaths_r_std) %>% left_join(y = shifted_stand_mort[, c("mun", "deaths_r_std")], by = "mun")

shifted_stand_mort <- shifted_stand_mort %>% mutate(deaths_r_std_disc = ifelse(deaths_r_std == 0, " 0",
                                                                        ifelse(deaths_r_std <= 1e-3, "<=1 per 1000",
                                                                        ifelse(deaths_r_std <= 2e-3, "<=2 per 1000",
                                                                        ifelse(deaths_r_std <= 3e-3, "<=3 per 1000",
                                                                        ifelse(deaths_r_std <= 4e-3, "<=4 per 1000", ">4 per 1000"))))))
### REPEATING THE SAME AS BEFORE ###

tmp <- colombia %>% left_join(y = shifted_stand_mort[, c("mun", "deaths_r_std_disc")], by = "mun") %>% rename(value = deaths_r_std_disc) %>% dplyr::select(mun, value, geometry)

m2 <- ggplot() +
  geom_sf(data = tmp, aes(geometry = geometry, fill = value), color = "black") +
  scale_fill_carto_d(name = "Standardized shifted mortality rate in 2018", palette = "Burg", direction = 1) +
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  guides(fill = guide_legend(order = 1))

ggsave(m2, file = file.path(out.dir, paste("Colombia_shifted_mortality_rate_2018_", stamp, ".png", sep = "")), width = 10, height = 10)

##############################
# Per department (and capital)
##############################

tmp <- shifted_stand_mort %>% arrange(gdp_per_capita)
sorted_deps <- tmp$dep_name %>% unique()
tmp <- tmp %>% mutate(dep_name_gdp = factor(match(dep_name, sorted_deps), levels = match(dep_name, sorted_deps), labels = dep_name))

b2 <- ggplot(data = tmp, aes(x = dep_name_gdp, y = deaths_r_std)) +
  geom_boxplot() +
  geom_point(data = dplyr::filter(tmp, capital == 1), col = "red") +
  coord_flip() + 
  theme_bw() +
  ylim(0, 0.01) + ylab("Standardized shifted\nmortality rate") +  xlab("Departments ordered by GBP")

ggsave(b2, file = file.path(out.dir, paste("Colombia_shifted_mortality_rate_2018_capital_vs_mun_ordered_by_gdp_", stamp, ".png", sep = "")), width = 10, height = 10)

####################
# Plots combined
####################

ggsave((m1 + m2), file = file.path(out.dir, paste("Colombia_all_mortality_rate_2018_", stamp, ".png", sep = "")), width = 20, height = 10)
ggsave((b1 + b2), file = file.path(out.dir, paste("Colombia_all_mortality_rate_2018_capital_vs_mun_ordered_by_gdp_", stamp, ".png", sep = "")), width = 20, height = 10)

##############################
##############################

##############################
# Standardized mortality by age group and gender
##############################

stand_mort_age_gender <- filter(mort, year == 2018) %>% group_by(gender, loc, year, age) %>% summarise(pop_n = sum(population),
                                                                                                       deaths = sum(deaths),
                                                                                                       deaths_r = (sum(deaths) / sum(population)),
                                                                                                       deaths_r_std = (sum(death_rate * national_pop) / national_pop_total)) %>% ungroup()

stand_mort_age_gender <- stand_mort_age_gender %>% rename(mun = loc) %>% left_join(y = geo_info[, c("mun", "dep", "capital")], by = "mun") %>% select(dep, mun, capital, gender, year, age, pop_n, deaths, deaths_r, deaths_r_std)


gender_age <- stand_mort_age_gender %>% select(gender, age) %>% distinct() # 19 possible combinations
gender_age$percentile <- 0

##########
##########

bx_before <- list()
for (i in 1:nrow(gender_age)) {
  tmp_gender <- gender_age$gender[i]
  tmp_age <- gender_age$age[i]
  
  tmp <- stand_mort_age_gender %>% filter(gender == tmp_gender, age == tmp_age) 
  tmp <- tmp %>% left_join(y = gdp, by = c("year", "dep")) %>% arrange(gdp_per_capita)
  tmp <- tmp %>% left_join(y = distinct(geo_info[, c("dep", "dep_name")]), by = "dep")
  sorted_deps <- tmp$dep_name %>% unique()
  tmp <- tmp %>% mutate(dep_name_gdp = factor(match(dep_name, sorted_deps), levels = match(dep_name, sorted_deps), labels = dep_name))
  
  best_deps <- tail(sorted_deps, 16)
  tmp_2 <- tmp %>% filter(dep_name %in% best_deps)
  tmp_2 <- tmp_2 %>% select(dep, mun, capital, deaths_r_std)
  tmp_2 <- tmp_2 %>% select(-mun)
  tmp_2 <- tmp_2 %>% group_by(dep) %>% mutate(percentile = percent_rank(deaths_r_std))
  tmp_2 <- tmp_2 %>% filter(capital == 1) %>% select(dep, percentile)
  tmp_2 <- tmp_2$percentile %>% mean(na.rm = T)
  gender_age$percentile[i] <- tmp_2
  
  bx <- ggplot(data = tmp, aes(x = dep_name_gdp, y = deaths_r_std)) +
    geom_boxplot() +
    geom_point(data = dplyr::filter(tmp, capital == 1), col = "red") +
    coord_flip() + 
    theme_bw() + 
    labs(title = paste(tmp_gender, " - ", tmp_age, sep = "")) + ylab("Standardized\nmortality rate") +  xlab("Departments ordered by GBP")
  
  bx_before[[i]] <- bx
  ggsave(bx, file = file.path(out.dir, "GENDER_AGE", "PRE", paste(tmp_gender, "_", tmp_age, "_Colombia_mortality_rate_2018_capital_vs_mun_ordered_by_gdp_", stamp, ".png", sep = "")), width = 10, height = 10)
}

percentiles_pp <- ggplot(gender_age %>% mutate(x = paste(gender, " ", age, sep = ""))) +
  geom_point(mapping = aes(x = x, y = percentile), size = 3, pch = 9) +
  labs(x = "", y = "Percentile") + 
  theme_bw() +
  ylim(0.5, 0.8) +
  theme(text = element_text(size = 12, family = "LM Roman 10"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

ggsave(percentiles_pp, file = file.path(out.dir, "GENDER_AGE", paste("percentile_capital_vs_mun_ordered_by_gdp_", stamp, ".png", sep = "")), width = 5, height = 3.5)

##############################
# Adjust non-capital rates
##############################

shifted_stand_mort_age_gender <- stand_mort_age_gender %>% dplyr::select(dep, mun, capital, gender, age, deaths_r_std)
unique_deps <- sort(unique(shifted_stand_mort_age_gender$dep))

pb <- txtProgressBar(min = 1, max = length(unique_deps), initial = 1) 
for (i in 1:length(unique_deps)) {
  tmp_data <- shifted_stand_mort_age_gender %>% filter(dep == unique_deps[i])
  
  if (length(unique(tmp_data$mun)) != 1) {
    
    tmp_data_fem <- tmp_data %>% filter(gender == "Female")
    tmp_data_mal <- tmp_data %>% filter(gender == "Male")
    
    age_fem <- unique(tmp_data_fem$age)
    age_mal <- unique(tmp_data_mal$age)
    
    # Female
    for (j in 1:length(age_fem)) {
      tmp_data_fem_cp <- tmp_data_fem %>% filter(age == age_fem[j]) %>% arrange(desc(capital))
      o <- tail(tmp_data_fem_cp$deaths_r_std, (nrow(tmp_data_fem_cp) - 1))
      k <- tmp_data_fem_cp$deaths_r_std[1]
      p <- gender_age %>% filter(gender == "Female", age == age_fem[j]) %>% select(percentile) %>% c() %>% unlist() %>% unname()
      tmp_shifted_fem <- shift_to_percentile(others = o, capital = k, percentile = p)
      tmp_data_fem_cp$deaths_r_std[2:nrow(tmp_data_fem_cp)] <- tmp_shifted_fem
      
      if (j == 1) {
        tmp_data_fem_mun <- tmp_data_fem_cp
      } else {
        tmp_data_fem_mun <- bind_rows(tmp_data_fem_mun, tmp_data_fem_cp)
      }
    }
    
    # Male
    for (j in 1:length(age_mal)) {
      tmp_data_mal_cp <- tmp_data_mal %>% filter(age == age_mal[j]) %>% arrange(desc(capital))
      o <- tail(tmp_data_mal_cp$deaths_r_std, (nrow(tmp_data_mal_cp) - 1))
      k <- tmp_data_mal_cp$deaths_r_std[1]
      p <- gender_age %>% filter(gender == "Male", age == age_mal[j]) %>% select(percentile) %>% c() %>% unlist() %>% unname()
      tmp_shifted_mal <- shift_to_percentile(others = o, capital = k, percentile = p)
      tmp_data_mal_cp$deaths_r_std[2:nrow(tmp_data_mal_cp)] <- tmp_shifted_mal
      
      if (j == 1) {
        tmp_data_mal_mun <- tmp_data_mal_cp
      } else {
        tmp_data_mal_mun <- bind_rows(tmp_data_mal_mun, tmp_data_mal_cp)
      }
    }
    
    tmp_data_mun <- bind_rows(tmp_data_fem_mun, tmp_data_mal_mun)
    
  } else { 
    tmp_data_mun <- tmp_data
  }
  
  if (i == 1) {
    cp_shifted_stand_mort_age_gender <- tmp_data_mun
  } else {
    cp_shifted_stand_mort_age_gender <- bind_rows(cp_shifted_stand_mort_age_gender, tmp_data_mun)
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

shifted_stand_mort_age_gender <- cp_shifted_stand_mort_age_gender %>% mutate(deaths_r_std = ifelse(deaths_r_std < 0, 0, deaths_r_std)) %>% arrange(dep, mun, desc(capital), gender, age)

shifted_stand_mort_age_gender <- dplyr::select(stand_mort_age_gender, -deaths_r_std) %>% left_join(y = shifted_stand_mort_age_gender[, c("mun", "gender", "age", "deaths_r_std")], by = c("mun", "gender", "age"))

##########
##########

bx_after <- list()
for (i in 1:nrow(gender_age)) {
  tmp_gender <- gender_age$gender[i]
  tmp_age <- gender_age$age[i]
  
  tmp <- shifted_stand_mort_age_gender %>% filter(gender == tmp_gender, age == tmp_age) 
  tmp <- tmp %>% left_join(y = gdp, by = c("year", "dep")) %>% arrange(gdp_per_capita)
  tmp <- tmp %>% left_join(y = distinct(geo_info[, c("dep", "dep_name")]), by = "dep")
  sorted_deps <- tmp$dep_name %>% unique()
  tmp <- tmp %>% mutate(dep_name_gdp = factor(match(dep_name, sorted_deps), levels = match(dep_name, sorted_deps), labels = dep_name))
  
  bx <- ggplot(data = tmp, aes(x = dep_name_gdp, y = deaths_r_std)) +
    geom_boxplot() +
    geom_point(data = dplyr::filter(tmp, capital == 1), col = "red") +
    coord_flip() + 
    theme_bw() + 
    labs(title = paste(tmp_gender, " - ", tmp_age, sep = "")) + ylab("Standardized shifted\nmortality rate") +  xlab("Departments ordered by GBP")
  
  bx_after[[i]] <- bx
  ggsave(bx, file = file.path(out.dir, "GENDER_AGE", "POST", paste(tmp_gender, "_", tmp_age, "_Colombia_shifted_mortality_rate_2018_capital_vs_mun_ordered_by_gdp_", stamp, ".png", sep = "")), width = 10, height = 10)
}

##########
##########

for (i in 1:nrow(gender_age)) {
  tmp_gender <- gender_age$gender[i]
  tmp_age <- gender_age$age[i]
  
  ggsave((bx_before[[i]] + bx_after[[i]]), file = file.path(out.dir, "GENDER_AGE", paste(tmp_gender, "_", tmp_age, "_Colombia_all_mortality_rate_2018_capital_vs_mun_ordered_by_gdp_", stamp, ".png", sep = "")), width = 20, height = 10)
}

##############################
# Compute back death count
##############################

tmp_mort <- mort %>% select(-national_pop)
tmp_mort <- dplyr::select(rename(shifted_stand_mort_age_gender, loc = mun), c("gender", "loc", "age", "deaths_r_std")) %>% right_join(y = tmp_mort, by = c("gender", "loc", "age"))
national_pop_all <- mort %>% dplyr::select(loc, year, gender, age, population) %>% group_by(year, gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()
# total_pop %>% group_by(year, gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()
tmp_mort <- tmp_mort %>% left_join(y = national_pop_all, by = c("year", "gender", "age"))
tmp_mort <- tmp_mort %>% mutate(death_rate = (deaths_r_std * national_pop_total) / national_pop)
tmp_mort <- tmp_mort %>% mutate(deaths = death_rate * population) # ~ 266,218 new deaths (total: 2,839,484)
tmp_mort <- tmp_mort %>% dplyr::select(gender, loc, year, age, deaths, population, death_rate)
tmp_mort <- tmp_mort %>% mutate(death_rate = compute_rate(deaths, population))

saveRDS(object = tmp_mort, file = file.path(data.dir, "summarised_data/new_death_rate_correction_quantiles.RDS"))

### LAST THING
### Plotting standardized death map
tmp_mort <- tmp_mort %>% left_join(y = pop_ref, by = c("gender", "age"))
stand_mort <- filter(tmp_mort, year == 2018) %>% group_by(loc, year) %>% summarise(pop_n = sum(population),
                                                                                   deaths = sum(deaths),
                                                                                   deaths_r = (sum(deaths) / sum(population)),
                                                                                   deaths_r_std = (sum(death_rate * national_pop) / national_pop_total)) %>% ungroup()

# Add `dep`
stand_mort <- stand_mort %>% rename(mun = loc) %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `gdp` (missing 1998-2004 and 2020-2021)
stand_mort <- stand_mort %>% left_join(y = gdp, by = c("year", "dep")) 

stand_mort <- stand_mort %>% mutate(deaths_r_std_disc = ifelse(deaths_r_std == 0, " 0",
                                                        ifelse(deaths_r_std <= 1e-3, "<=1 per 1000",
                                                        ifelse(deaths_r_std <= 2e-3, "<=2 per 1000",
                                                        ifelse(deaths_r_std <= 3e-3, "<=3 per 1000",
                                                        ifelse(deaths_r_std <= 4e-3, "<=4 per 1000", ">4 per 1000"))))))

tmp <- colombia %>% left_join(y = stand_mort[, c("mun", "deaths_r_std_disc")], by = "mun") %>% rename(value = deaths_r_std_disc) %>% dplyr::select(mun, value, geometry)

m3 <- ggplot() +
  geom_sf(data = tmp, aes(geometry = geometry, fill = value), color = "black") +
  scale_fill_carto_d(name = "Standardized shifted (per stratum)\nmortality rate in 2018", palette = "Burg", direction = 1) +
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  guides(fill = guide_legend(order = 1))

ggsave((m1 + m3), file = file.path(out.dir, paste("Colombia_all_stratum_mortality_rate_2018_", stamp, ".png", sep = "")), width = 20, height = 10)







