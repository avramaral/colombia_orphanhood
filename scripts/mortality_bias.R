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
national_pop_total <- total_pop %>% filter(year == 2018) %>% select(population) %>% sum() %>% c() %>% unlist() %>% unname()
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
### REPEAT THE SAME AS BEFORE!!! ###

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
####################

####################
# Plots combined
####################
ggsave((m1 + m2), file = file.path(out.dir, paste("Colombia_all_mortality_rate_2018_", stamp, ".png", sep = "")), width = 20, height = 10)
ggsave((b1 + b2), file = file.path(out.dir, paste("Colombia_all_mortality_rate_2018_capital_vs_mun_ordered_by_gdp_", stamp, ".png", sep = "")), width = 20, height = 10)






