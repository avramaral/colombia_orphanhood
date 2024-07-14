suppressMessages(library(tidyverse))
suppressMessages(library(sf))
suppressMessages(library(mgcv))
suppressMessages(library(rcartocolor))
suppressMessages(library(patchwork))
suppressMessages(library(data.table))
suppressMessages(library(fitdistrplus))

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

# Proportion of individuals within 5-year windows
prop_65_66_fem  <- read_csv(paste(data.dir, "/summarised_data/pop_65_66_mun_fem.csv", sep = ""), col_types = cols())
prop_75_76_fem  <- read_csv(paste(data.dir, "/summarised_data/pop_75_76_mun_mal.csv", sep = ""), col_types = cols())

# MPI by municipality
dmpi <- read.csv(file.path(data.dir, "COLOMBIA COVID and POP Datasets", "multidimensional poverty", "CNPV-2018_PobrezaMultidimensional_Municipal_IPM.csv"),
                 skip = 11,
                 nrow = 1122,
                 header = TRUE)[, 1:6]
dmpi <- as_tibble(dmpi)
colnames(dmpi) <- c("dep", "mun", "mun_name", "mpi", "mpi_cabeceras", "mpi_centros")
dmpi <- dmpi %>% mutate(mun = factor(mun))

# Valor Aggregado by municipality
dgbpm <- read.csv(file.path(data.dir, "Files_Colombia", "anexo-2018-provisional-valor-agregado-municipio-2011-2018_sheet9.csv"),
                  nrow = 1122,
                  header = TRUE)
dgbpm <- as.data.table(dgbpm)
setnames(dgbpm, 1:4, c("mun", "mun_name", "dep", "dep_name"))
setnames(dgbpm, "X.Valor.agregado..", "va")
set(dgbpm, NULL, c(5L:7L,9L), NULL)
set(dgbpm, NULL, "va", dgbpm[, 1e3 * as.integer(gsub(",", "", va))])
dgbpm <- as_tibble(dgbpm)
dgbpm <- dgbpm %>% mutate(mun = factor(mun))

# GBP by municipality
dgbp <- read.csv(file.path(data.dir, "Files_Colombia/anexo-2019-preliminar-PIB-total-por-departamento.csv"),
                 skip = 8, 
                 nrow = 34,
                 header = TRUE)
dgbp <- as.data.table(dgbp)
setnames(dgbp, 1:2, c("dep", "dep_name"))
set(dgbp, NULL, c("X", "dep_name"), NULL)
dgbp <- melt(dgbp, id.vars = c("dep"), value.name = "gdp_per_capita", variable.name = "year")
dgbp <- subset(dgbp, !is.na(dep))
set(dgbp, NULL, "gdp_per_capita", dgbp[, as.integer(gsub(",", "", gdp_per_capita))])
set(dgbp, NULL, "year", dgbp[, as.integer(gsub("X|pr|p", "",year))])
dgbp <- dgbp %>% as_tibble()

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

# Add `DEP`
stand_mort <- stand_mort %>% rename(mun = loc) %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP` (missing 1998-2004 and 2020-2021)
stand_mort <- stand_mort %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
stand_mort <- stand_mort %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
stand_mort <- stand_mort %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
stand_mort <- stand_mort %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
stand_mort <- stand_mort %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

stand_mort <- stand_mort %>% mutate(deaths_r_std_disc = ifelse(deaths_r_std == 0, " 0",
                                                        ifelse(deaths_r_std <= 1e-3, "<=1 per 1000",
                                                        ifelse(deaths_r_std <= 2e-3, "<=2 per 1000",
                                                        ifelse(deaths_r_std <= 3e-3, "<=3 per 1000",
                                                        ifelse(deaths_r_std <= 4e-3, "<=4 per 1000", ">4 per 1000"))))))

#########################
#########################
########## NEW ##########
#########################
#########################

pop_ref <- mort %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(natl_pop_n = sum(population)) %>% ungroup()
pop_ref <- pop_ref %>% mutate(natl_pop_t = sum(natl_pop_n))
pop_ref <- pop_ref %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t)

stand_mort <- mort %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(deaths = mean(deaths)) %>% ungroup()
tmp <- mort %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
stand_mort <- stand_mort %>% left_join(y = tmp, by = c("loc", "gender", "age"))
stand_mort <- stand_mort %>% mutate(mun_death_r_2018 = deaths / population)
stand_mort <- stand_mort %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
stand_mort <- stand_mort %>% filter(age != "0-9")
stand_mort <- stand_mort %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))

stand_mort <- stand_mort %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates (AGAIN) based on the average of years 2017-2019
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_mort <- stand_mort %>% rename(mun = loc) %>% group_by(mun, year) %>% summarise(pop_n = sum(population),
                                                                                     deaths = sum(deaths),
                                                                                     death_r = (sum(deaths) / sum(population)),
                                                                                     death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

stand_mort <- stand_mort %>% mutate(deaths_r_std_disc = ifelse(death_r_std == 0, " 0",
                                                        ifelse(death_r_std <= 1e-3, "<=1 per 1000",
                                                        ifelse(death_r_std <= 2e-3, "<=2 per 1000",
                                                        ifelse(death_r_std <= 3e-3, "<=3 per 1000",
                                                        ifelse(death_r_std <= 4e-3, "<=4 per 1000", ">4 per 1000"))))))


# Add `DEP`
stand_mort <- stand_mort %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP` (missing 1998-2004 and 2020-2021)
stand_mort <- stand_mort %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
stand_mort <- stand_mort %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
stand_mort <- stand_mort %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
stand_mort <- stand_mort %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
stand_mort <- stand_mort %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

# Based on VA
p_VA <- ggplot(stand_mort, aes(x = va, y = death_r_std)) +
  geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_vline(xintercept = 1e+06, lty = "dashed") + 
  scale_x_log10() +
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.007)) +
  labs(x = "VA", y = "Mean standardized death rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Original") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(stand_mort,  capital == 1), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(stand_mort, (capital == 0) & (va <  1e6)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(stand_mort, (capital == 0) & (va >= 1e6)), colour = "cyan",    lty = "dotted", method = "lm")
p_VA

# Based on MPI
p_MPI <- ggplot(stand_mort, aes(x = mpi, y = death_r_std)) +
  geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_vline(xintercept = 25, lty = "dashed") + 
  scale_x_log10() +
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.007)) +
  labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized death rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Original") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(stand_mort, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(stand_mort, (capital == 0) & (mpi <  25)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(stand_mort, (capital == 0) & (mpi >= 25)), colour = "cyan",    lty = "dotted", method = "lm")
p_MPI

##################
### PROCESSING ###
##################

max_MPI <- max(stand_mort$mpi)
ref_MPI <- 25 # Changing point (cp)

# Fit `lm()` for capitals
lm_capital <- lm(formula = death_r_std ~ mpi, data = subset(stand_mort, capital == 1))
# Coefficients for capitals
inter_capital <- coef(lm_capital)["(Intercept)"]
slope_capital <- coef(lm_capital)["mpi"]

# Fit `lm()` for non-capitals
lm_non_capital_poor <- lm(formula = death_r_std ~ mpi, data = subset(stand_mort, (capital == 0) & (mpi >  ref_MPI)))
lm_non_capital_rich <- lm(formula = death_r_std ~ mpi, data = subset(stand_mort, (capital == 0) & (mpi <= ref_MPI)))
# Coefficients for poor non-capitals
inter_non_capital_poor <- coef(lm_non_capital_poor)["(Intercept)"]
slope_non_capital_poor <- coef(lm_non_capital_poor)["mpi"]
# Coefficients for rich non-capitals
inter_non_capital_rich <- coef(lm_non_capital_rich)["(Intercept)"]
slope_non_capital_rich <- coef(lm_non_capital_rich)["mpi"]

# Calculate the value of `death_r_std` at `mpi = ref_MPI` for non-capitals with `mpi <= ref_MPI`
death_r_std_at_cp_rich <- inter_non_capital_rich + slope_non_capital_rich * ref_MPI

# Calculate the intercept for non-capitals with `mpi > ref_MPI` to ensure continuity at `mpi = ref_MPI
inter_adjusted <- death_r_std_at_cp_rich - slope_capital * ref_MPI

# Adjust the fitted values for non-capitals with `mpi > ref_MPI` to use the calculated intercept and slope from capitals
stand_mort <- stand_mort %>%
  mutate(fitted_death_r_std = ifelse(
    (capital == 0) & (mpi > ref_MPI),
    inter_adjusted + slope_capital * mpi + (death_r_std - (inter_non_capital_poor + slope_non_capital_poor * mpi)),
    death_r_std
  ))
stand_mort <- stand_mort %>% mutate(fitted_death_r_std = ifelse(fitted_death_r_std <= 0, 0, fitted_death_r_std))

new_p_MPI <- ggplot(stand_mort, aes(x = mpi, y = fitted_death_r_std)) +
  geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_vline(xintercept = ref_MPI, lty = "dashed") + 
  scale_x_log10() +
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.007)) +
  labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized death rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Adjusted") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(stand_mort, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(stand_mort, (capital == 0) & (mpi <  ref_MPI)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(stand_mort, (capital == 0) & (mpi >= ref_MPI)), colour = "cyan",    lty = "dotted", method = "lm")

(p_total_MPI <- p_MPI + new_p_MPI)
ggsave(p_total_MPI, file = "/Users/avramaral/Desktop/plots_fert/plot_MPI.png", width = 12, height = 6)

##################
# Computations per age group and gender
##################

stand_mort_age_gender <- mort %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(deaths = mean(deaths)) %>% ungroup()
tmp <- mort %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = tmp, by = c("loc", "gender", "age"))
stand_mort_age_gender <- stand_mort_age_gender %>% mutate(mun_death_r_2018 = deaths / population)
stand_mort_age_gender <- stand_mort_age_gender %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
stand_mort_age_gender <- stand_mort_age_gender %>% filter(age != "0-9")
stand_mort_age_gender <- stand_mort_age_gender %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))


stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = pop_ref, by = c("gender", "age"))


# Calculate standardized death rates (AGAIN) based on the average of years 2017-2019, but now stratified by age group and gender
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_mort_age_gender <- stand_mort_age_gender %>% rename(mun = loc) %>% group_by(gender, mun, year, age) %>% summarise(pop_n = sum(population),
                                                                                                                        deaths = sum(deaths),
                                                                                                                        death_r = (sum(deaths) / sum(population)),
                                                                                                                        death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

stand_mort_age_gender <- stand_mort_age_gender %>% mutate(deaths_r_std_disc = ifelse(death_r_std == 0, " 0",
                                                                              ifelse(death_r_std <= 1e-3, "<=1 per 1000",
                                                                              ifelse(death_r_std <= 2e-3, "<=2 per 1000",
                                                                              ifelse(death_r_std <= 3e-3, "<=3 per 1000",
                                                                              ifelse(death_r_std <= 4e-3, "<=4 per 1000", ">4 per 1000"))))))

# Add `DEP`
stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP` (missing 1998-2004 and 2020-2021)
stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
stand_mort_age_gender <- stand_mort_age_gender %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

# Rearrange rows based on `gender`, `mun`, `year`, and `age`
stand_mort_age_gender <- stand_mort_age_gender %>% arrange(gender, mun, year, age)

gender_age <- stand_mort_age_gender %>% dplyr::select(gender, age) %>% distinct() # 19 possible combinations

##########
##########

p_total <- list()
boxplots_total <- list()
for (i in 1:nrow(gender_age)) {
  tmp_gender <- gender_age$gender[i]
  tmp_age <- gender_age$age[i]
  ref_mpi <- 25
  # print(paste(tmp_gender, " ", tmp_age, sep = ""))
  
  tmp <- stand_mort_age_gender %>% filter(gender == tmp_gender, age == tmp_age) 
  max_death_std <- max(tmp$death_r_std) * 1.1 # + 10%
  
  tmp_p_MPI <- ggplot(tmp, aes(x = mpi, y = death_r_std)) +
    geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
    geom_vline(xintercept = ref_MPI, lty = "dashed") + 
    scale_x_log10() +
    scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(-1e-05, max_death_std)) +
    labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized death rates 2017-2019", colour = "Type", size = "", pch = "") +
    theme_bw() +
    theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = paste("Original (", tmp_gender, " ", tmp_age, ")", sep = "")) +
    geom_smooth(colour = "yellow") +
    geom_smooth(data = subset(tmp, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
    geom_smooth(data = subset(tmp, (capital == 0) & (mpi <  ref_MPI)), colour = "magenta", lty = "dotted", method = "lm") +
    geom_smooth(data = subset(tmp, (capital == 0) & (mpi >= ref_MPI)), colour = "cyan",    lty = "dotted", method = "lm")
  
  # Fit `lm()` for capitals
  lm_capital <- lm(formula = death_r_std ~ mpi, data = subset(tmp, capital == 1))
  # Coefficients for capitals
  inter_capital <- coef(lm_capital)["(Intercept)"]
  slope_capital <- coef(lm_capital)["mpi"]
  
  # Fit `lm()` for non-capitals
  lm_non_capital_poor <- lm(formula = death_r_std ~ mpi, data = subset(tmp, (capital == 0) & (mpi >  ref_mpi)))
  lm_non_capital_rich <- lm(formula = death_r_std ~ mpi, data = subset(tmp, (capital == 0) & (mpi <= ref_mpi)))
  # Coefficients for poor non-capitals
  inter_non_capital_poor <- coef(lm_non_capital_poor)["(Intercept)"]
  slope_non_capital_poor <- coef(lm_non_capital_poor)["mpi"]
  # Coefficients for rich non-capitals
  inter_non_capital_rich <- coef(lm_non_capital_rich)["(Intercept)"]
  slope_non_capital_rich <- coef(lm_non_capital_rich)["mpi"]
  
  # Calculate the value of `death_r_std` at `mpi = ref_mpi` for non-capitals with `mpi <= ref_mpi`
  death_r_std_at_cp_rich <- inter_non_capital_rich + slope_non_capital_rich * ref_mpi
  
  # Calculate the intercept for non-capitals with `mpi > ref_mpi` to ensure continuity at `mpi = ref_mpi`
  inter_adjusted <- death_r_std_at_cp_rich - slope_capital * ref_mpi
  
  # Adjust the fitted values for non-capitals with `mpi > ref_mpi` to use the calculated intercept and slope from capitals
  tmp <- tmp %>%
    mutate(fitted_death_r_std = ifelse(
      (capital == 0) & (mpi > ref_mpi),
      inter_adjusted + slope_capital * mpi + (death_r_std - (inter_non_capital_poor + slope_non_capital_poor * mpi)),
      death_r_std
    ))
  tmp <- tmp %>% mutate(fitted_death_r_std = ifelse(fitted_death_r_std <= 0, 0, fitted_death_r_std))
  
  new_tmp_p_MPI <- ggplot(tmp, aes(x = mpi, y = fitted_death_r_std)) +
    geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
    geom_vline(xintercept = ref_MPI, lty = "dashed") + 
    scale_x_log10() +
    scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, max_death_std)) +
    labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized death rates 2017-2019", colour = "Type", size = "", pch = "") +
    theme_bw() +
    theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = paste("Adjusted (", tmp_gender, " ", tmp_age, ")", sep = "")) +
    geom_smooth(colour = "yellow") +
    geom_smooth(data = subset(tmp, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
    geom_smooth(data = subset(tmp, (capital == 0) & (mpi <  ref_MPI)), colour = "magenta", lty = "dotted", method = "lm") +
    geom_smooth(data = subset(tmp, (capital == 0) & (mpi >= ref_MPI)), colour = "cyan",    lty = "dotted", method = "lm")
  
  p_total[[i]] <- tmp_p_MPI + new_tmp_p_MPI
  ggsave(p_total[[i]], file = paste("/Users/avramaral/Desktop/plots_fert/OTHERS/plot_", i, ".png", sep = ""), width = 12, height = 6)
  
  if (i == 1) { new_stand_mort_age_gender <- tmp } else { new_stand_mort_age_gender <- bind_rows(new_stand_mort_age_gender, tmp)}
}

new_stand_mort_age_gender <- new_stand_mort_age_gender %>% arrange(gender, mun, year, age)

##################
# Boxplots  
##################

##################
# Based on VA
##################

new_stand_mort_age_gender <- new_stand_mort_age_gender %>% mutate(va_disc = ifelse(va < 1e5, "(1) <100,000",
                                                                            ifelse(va < 5e5, "(2) 100,000-500,000",
                                                                            ifelse(va < 1e6, "(3) 500,000-1,000,000", "(4) >1,000,000"))))


new_stand_mort_age_gender <- new_stand_mort_age_gender %>% mutate(class = paste(gender, "_", age, sep = "")) %>% mutate(class = factor(class))

tmp_max_box <- max(c(new_stand_mort_age_gender$death_r_std, new_stand_mort_age_gender$fitted_death_r_std))

# National mean
mean_tmp_box     <- new_stand_mort_age_gender %>% group_by(age) %>% summarize(mean_death_r_std = mean(death_r_std)) %>% ungroup() 
new_mean_tmp_box <- new_stand_mort_age_gender %>% group_by(age) %>% summarize(mean_death_r_std = mean(fitted_death_r_std)) %>% ungroup() 

tmp_box_VA <- ggplot(new_stand_mort_age_gender, aes(x = age)) +
  geom_boxplot(aes(x = age, y = death_r_std)) +
  geom_line(data = mean_tmp_box,     aes(x = age, y = mean_death_r_std, group = 1, color = "Original"), lwd = 0.75) +
  geom_line(data = new_mean_tmp_box, aes(x = age, y = mean_death_r_std, group = 1, color = "Adjusted"), lwd = 0.75) +
  scale_y_sqrt(limits = c(0, tmp_max_box)) +
  scale_color_manual(values = c("Original" = "red", "Adjusted" = "blue")) +
  facet_grid(va_disc ~ gender) +
  theme_bw() +
  labs(title = "Original", x = "Age", y = "Mean standardized death rates 2017-2019", color = "Mean") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5))

new_tmp_box_VA <- ggplot(new_stand_mort_age_gender, aes(x = age)) +
  geom_boxplot(aes(x = age, y = fitted_death_r_std)) +
  geom_line(data = mean_tmp_box,     aes(x = age, y = mean_death_r_std, group = 1, color = "Original"), lwd = 0.75) +
  geom_line(data = new_mean_tmp_box, aes(x = age, y = mean_death_r_std, group = 1, color = "Adjusted"), lwd = 0.75) +
  scale_y_sqrt(limits = c(0, tmp_max_box)) +
  scale_color_manual(values = c("Original" = "red", "Adjusted" = "blue")) +
  facet_grid(va_disc ~ gender) +
  theme_bw() +
  labs(title = "Adjusted", x = "Age", y = "Mean standardized death rates 2017-2019", color = "Mean") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5))

(total_box_VA <- tmp_box_VA + new_tmp_box_VA)
ggsave(total_box_VA, file = "/Users/avramaral/Desktop/plots_fert/boxplot_VA.png", width = 12, height = 6)

##################
# Based on MPI
##################

new_stand_mort_age_gender <- new_stand_mort_age_gender %>% mutate(mpi_disc = ifelse(mpi < 25, "(4) MPI:  0-25",
                                                                             ifelse(mpi < 50, "(3) MPI: 25-50",
                                                                             ifelse(mpi < 75, "(2) MPI: 50-75", "(1) MPI: 75-100"))))

tmp_box_MPI <- ggplot(new_stand_mort_age_gender, aes(x = age)) +
  geom_boxplot(aes(x = age, y = death_r_std)) +
  geom_line(data = mean_tmp_box,     aes(x = age, y = mean_death_r_std, group = 1, color = "Original"), lwd = 0.75) +
  geom_line(data = new_mean_tmp_box, aes(x = age, y = mean_death_r_std, group = 1, color = "Adjusted"), lwd = 0.75) +
  scale_y_sqrt(limits = c(0, tmp_max_box)) +
  scale_color_manual(values = c("Original" = "red", "Adjusted" = "blue")) +
  facet_grid(mpi_disc ~ gender) +
  theme_bw() +
  labs(title = "Original", x = "Age", y = "Mean standardized death rates 2017-2019", color = "Mean") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5))

new_tmp_box_MPI <- ggplot(new_stand_mort_age_gender, aes(x = age)) +
  geom_boxplot(aes(x = age, y = fitted_death_r_std)) +
  geom_line(data = mean_tmp_box,     aes(x = age, y = mean_death_r_std, group = 1, color = "Original"), lwd = 0.75) +
  geom_line(data = new_mean_tmp_box, aes(x = age, y = mean_death_r_std, group = 1, color = "Adjusted"), lwd = 0.75) +
  scale_y_sqrt(limits = c(0, tmp_max_box)) +
  scale_color_manual(values = c("Original" = "red", "Adjusted" = "blue")) +
  facet_grid(mpi_disc ~ gender) +
  theme_bw() +
  labs(title = "Adjusted", x = "Age", y = "Mean standardized death rates 2017-2019", color = "Mean") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5))

(total_box_MPI <- tmp_box_MPI + new_tmp_box_MPI)
ggsave(total_box_MPI, file = "/Users/avramaral/Desktop/plots_fert/boxplot_MPI.png", width = 12, height = 6)

##################
# Compute back death count
##################

tmp_mort <- mort %>% dplyr::select(-national_pop)
tmp_mort <- dplyr::select(rename(new_stand_mort_age_gender, loc = mun), c("gender", "loc", "age", "fitted_death_r_std")) %>% rename(death_r_std = fitted_death_r_std) %>% right_join(y = tmp_mort, by = c("gender", "loc", "age"))
national_pop_all <- mort %>% dplyr::select(loc, year, gender, age, population) %>% group_by(year, gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()
tmp_mort <- tmp_mort %>% left_join(y = national_pop_all, by = c("year", "gender", "age"))
tmp_mort <- tmp_mort %>% mutate(death_rate = (death_r_std * national_pop_total) / national_pop)
tmp_mort <- tmp_mort %>% mutate(deaths = death_rate * population) # ~ 235,386 new deaths (total: 2,808,652)
tmp_mort <- tmp_mort %>% dplyr::select(gender, loc, year, age, deaths, population, death_rate)
tmp_mort <- tmp_mort %>% mutate(death_rate = compute_rate(deaths, population))

##################
##################







