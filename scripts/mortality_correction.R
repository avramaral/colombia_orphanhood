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

rescale_f <- function (x, a, b, e = 1, f = 100, ...) {
  # Re-scale a value x from the interval [e, f] to the interval [a, b]
  ((x - e) * (b - a) / (f - e)) + a
}

high_res_islands <- readRDS(file = "~/Documents/Colombia Orphanhood/colombia_project/DATA/high_res_islands.RDS")

##############################
# Read data
##############################

home.dir <- "/Users/or105/Library/CloudStorage/OneDrive-ImperialCollegeLondon/OR_Work/OCAY_Program"
home.dir <- "/Users/avramaral/Library/CloudStorage/OneDrive-SharedLibraries-ImperialCollegeLondon/Ratmann, Oliver - OCAY_Program"
data.dir <- file.path(home.dir, "Colombia_primary_data")
out.dir <- file.path(home.dir, "Colombia_underreporting")
stamp <- gsub("-", "", Sys.Date())

data <- readRDS(paste(data.dir, "/summarised_data/raw_rates.RDS", sep = ""))
fert <- data$fertility_rates %>% dplyr::select(-fertility_rate)
mort <- data$mortality_rates %>% dplyr::select(-death_rate)
mort_cp <- mort

total_pop <- readRDS(paste(data.dir, "/summarised_data/pop_all.RDS", sep = ""))
gdp       <- readRDS(paste(data.dir, "/summarised_data/gbp.RDS", sep = ""))
geo_info  <- read_csv(paste(data.dir, "/summarised_data/geo_info.csv", sep = ""), col_types = cols())

###################################
########## Raw mortality ##########
###################################

death_count <- read_csv(file = "~/Documents/Colombia Orphanhood/colombia_project/DATA/Deaths/col_83-21_deaths_municipality.csv")
death_count <- death_count %>% filter(year >= 1998, year <= 2021, mun %in% unique(mort$loc)) %>% rename(loc = mun) %>% mutate(loc = factor(loc))
death_count <- death_count %>% mutate(age = ifelse((age == "80-84") | (age == "85+"), "80+", age))
death_count <- death_count %>% group_by(gender, loc, year, age) %>% summarise(deaths = sum(deaths)) %>% ungroup() %>% dplyr::select(year, loc, gender, age, deaths)

all_y <- unique(death_count$year)
all_l <- unique(death_count$loc)
all_g <- unique(death_count$gender)
all_a <- unique(death_count$age)

all_combinations <- expand.grid(all_y, all_l, all_g, all_a) %>% as_tibble() %>% rename(year = Var1, loc = Var2, gender = Var3, age = Var4) %>% mutate(loc = factor(loc), gender = as.character(gender), age = as.character(age))
death_count <- all_combinations %>% left_join(y = death_count, by = c("year", "loc", "gender", "age"))
death_count <- death_count %>% mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>% arrange(year, loc, gender, age)
death_count <- death_count %>% left_join(y = rename(total_pop, loc = mun), by = c("year", "loc", "gender", "age"))

mort <- death_count

###################################
###################################

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

# MPI by municipality
dmpi <- read.csv(file.path(data.dir, "COLOMBIA COVID and POP Datasets", "multidimensional poverty", "CNPV-2018_PobrezaMultidimensional_Municipal_IPM.csv"),
                 skip = 11,
                 nrow = 1122,
                 header = TRUE)[, 1:6]
dmpi <- as_tibble(dmpi)
colnames(dmpi) <- c("dep", "mun", "mun_name", "mpi", "mpi_cabeceras", "mpi_centros")
dmpi <- dmpi %>% mutate(mun = factor(mun))

# VA by municipality
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

pop_ref <- mort %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(natl_pop_n = sum(population), natl_death_n = sum(deaths)) %>% ungroup()
pop_ref <- pop_ref %>% mutate(natl_pop_t = sum(natl_pop_n))
pop_ref <- pop_ref %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t)
pop_ref <- pop_ref %>% mutate(natl_death_r = natl_death_n / natl_pop_n)
pop_ref <- pop_ref %>% mutate(natl_death_r_log = log(natl_death_n / natl_pop_n))

# # Determine age midpoints
# tmp <- pop_ref %>% dplyr::select(gender, age) %>% distinct()
# tmp <- tmp %>% mutate(age_from = as.integer(gsub("([0-9]+)-([0-9]+)", "\\1", age)))
# tmp <- tmp %>% mutate(age_to   = as.integer(gsub("([0-9]+)-([0-9]+)", "\\2", age)))
# tmp <- tmp %>% mutate(age_mid  = (age_from + age_to + 1L) / 2)
# pop_ref <- pop_ref %>% left_join(y = tmp[, c("age", "gender", "age_from", "age_to", "age_mid")], by = c("age", "gender"))
# 
# # Fit spline to age trends in death rates over continuous age mid points
# pop_ref <- pop_ref %>% as.data.table()
# tmp <- pop_ref[, {
#   d <- list(natl_death_r = natl_death_r, age_mid = age_mid)
#   f <- gam(natl_death_r ~ s(age_mid, bs = "cs", k = 7), data = d, method = "REML")
#   z <- mgcv:::predict.gam(f, d, type = "response", se.fit = TRUE)
#   list(natl_death_r_fitted = as.numeric(z$fit), natl_death_r_fitted_se = as.numeric(z$se.fit), age_mid = age_mid)
# }, by = c("gender")] %>% as_tibble()
# pop_ref <- pop_ref %>% as_tibble()
# pop_ref <- pop_ref %>% left_join(y = tmp, by = c("gender", "age_mid"))

yys <- 1998:2021
all_death_r <- list()
all_death_r_std <- list()
count <- 0
for (yy in yys) {
  count <- count + 1
  print(yy)
  
  tmp_pop_ref <- mort %>% filter(year == yy) %>% group_by(gender, age) %>% summarise(natl_pop_n = sum(population), natl_death_n = sum(deaths)) %>% ungroup()
  tmp_pop_ref <- tmp_pop_ref %>% mutate(natl_pop_t = sum(natl_pop_n))
  tmp_pop_ref <- tmp_pop_ref %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t)
  tmp_pop_ref <- tmp_pop_ref %>% mutate(natl_death_r = natl_death_n / natl_pop_n)
  tmp_pop_ref <- tmp_pop_ref %>% mutate(natl_death_r_log = log(natl_death_n / natl_pop_n))
  
  tmp <- mort %>% filter(year == yy) %>% dplyr::select(loc, gender, age, year, population)
  tmp_nat_stand_mort <- mort %>% filter(year == yy) %>% group_by(loc, gender, age) %>% summarise(deaths = mean(deaths)) %>% ungroup()
  tmp_nat_stand_mort <- tmp_nat_stand_mort %>% left_join(y = tmp, by = c("loc", "gender", "age"))
  tmp_nat_stand_mort <- tmp_nat_stand_mort %>% mutate(mun_death_r_yy = deaths / population)
  tmp_nat_stand_mort <- tmp_nat_stand_mort %>% mutate(mun_death_r_yy = ifelse(population == 0, 0, mun_death_r_yy))
  tmp_nat_stand_mort <- tmp_nat_stand_mort %>% left_join(y = tmp_pop_ref, by = c("gender", "age"))
  
  national_pop_total <- total_pop %>% filter(year == yy) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
  tmp_nat_stand_mort <- tmp_nat_stand_mort %>% group_by(gender, age, year) %>% summarise(pop_n = sum(population), deaths = sum(deaths), death_r = (sum(deaths) / sum(population)), death_r_std = (sum(mun_death_r_yy * natl_pop_n) / national_pop_total)) %>% ungroup()
  tmp_nat_stand_mort <- tmp_nat_stand_mort %>% arrange(gender, age)
  
  all_death_r[[as.character(yy)]]     <- tmp_nat_stand_mort$death_r
  all_death_r_std[[as.character(yy)]] <- tmp_nat_stand_mort$death_r_std
  
}

multiplier_yy <- list()
count <- 0
for (yy in yys) {
  count <- count + 1
  multiplier_yy[[as.character(yy)]] <- (all_death_r_std[[as.character(yy)]] / all_death_r_std[[as.character(2018)]])
}

len_age <- pop_ref$age %>% unique() %>% length() * 2 # Female and male
multiplier_yy <- as_tibble(data.frame(year = rep(yys, each = len_age), gender = rep(rep(c("Female", "Male"), each = (len_age / 2)), length(yys)), age = rep(pop_ref$age, length(yys)), multiplier = (multiplier_yy %>% unlist() %>% unname())))

##########

nat_stand_mort <- mort %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(deaths = mean(deaths)) %>% ungroup()
tmp <- mort %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
nat_stand_mort <- nat_stand_mort %>% left_join(y = tmp, by = c("loc", "gender", "age"))
nat_stand_mort <- nat_stand_mort %>% mutate(mun_death_r_2018 = deaths / population)
nat_stand_mort <- nat_stand_mort %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
nat_stand_mort <- nat_stand_mort %>% filter(age != "0-9")
nat_stand_mort <- nat_stand_mort %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))

nat_stand_mort <- nat_stand_mort %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates based on the average of years 2017-2019 (National)
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
nat_stand_mort <- nat_stand_mort %>% group_by(gender, age, year) %>% summarise(pop_n = sum(population),
                                                                               deaths = sum(deaths),
                                                                               death_r = (sum(deaths) / sum(population)),
                                                                               death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

#########################
#########################

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
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.01)) +
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
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.01)) +
  labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized death rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Original") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(stand_mort, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(stand_mort, (capital == 0) & (mpi <  25)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(stand_mort, (capital == 0) & (mpi >= 25)), colour = "cyan",    lty = "dotted", method = "lm")
p_MPI


# Compute the weighted average of the standardized death rates based on the capitals
weighted_death_r_std <- stand_mort %>% filter(capital == 1) %>% dplyr::select(pop_n, death_r_std) %>% group_by() %>% summarise(weigthed_avg = weighted.mean(x = death_r_std, w = pop_n)) %>% ungroup() %>% c() %>% unlist() %>% unname()

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
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.01)) +
  labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized death rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Adjusted") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(stand_mort, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(stand_mort, (capital == 0) & (mpi <  ref_MPI)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(stand_mort, (capital == 0) & (mpi >= ref_MPI)), colour = "cyan",    lty = "dotted", method = "lm")

(p_total_MPI <- p_MPI + new_p_MPI)
# ggsave(p_total_MPI, file = "/Users/avramaral/Desktop/plots_fert/plot_MPI.png", width = 12, height = 6)

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
stand_mort_age_gender_cp <- stand_mort_age_gender

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
  # ggsave(p_total[[i]], file = paste("/Users/avramaral/Desktop/plots_fert/OTHERS/plot_", i, ".png", sep = ""), width = 12, height = 6)
  
  if (i == 1) { new_stand_mort_age_gender <- tmp } else { new_stand_mort_age_gender <- bind_rows(new_stand_mort_age_gender, tmp)}
}

new_stand_mort_age_gender <- new_stand_mort_age_gender %>% arrange(gender, mun, year, age)

##################
# Analysis based on the "multiplier" approach (computations per gender)
##################

# stand_mort_gender <- mort %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender) %>% summarise(deaths = mean(deaths)) %>% ungroup()
# tmp <- mort %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population) %>% group_by(loc, gender, year) %>% summarise(population = sum(population)) %>% ungroup()
# stand_mort_gender <- stand_mort_gender %>% left_join(y = tmp, by = c("loc", "gender"))
# stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018 = deaths / population)
# stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
# stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))
# 
# stand_mort_gender <- stand_mort_gender %>% left_join(y = (pop_ref %>% group_by(gender, natl_pop_t) %>% summarise(natl_pop_n = sum(natl_pop_n)) %>% ungroup() %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t) %>% dplyr::select(gender, natl_pop_n, natl_pop_t, natl_pop_p)), by = c("gender"))
# 
# # Calculate standardized death rates (AGAIN) based on the average of years 2017-2019, but now stratified by gender only
# national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
# stand_mort_gender <- stand_mort_gender %>% rename(mun = loc) %>% group_by(gender, mun, year) %>% summarise(pop_n = sum(population),
#                                                                                                            deaths = sum(deaths),
#                                                                                                            death_r = (sum(deaths) / sum(population)),
#                                                                                                            death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

stand_mort_gender <- mort %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(deaths = mean(deaths)) %>% ungroup()
tmp <- mort %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
stand_mort_gender <- stand_mort_gender %>% left_join(y = tmp, by = c("loc", "gender", "age"))
stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018 = deaths / population)
stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
stand_mort_gender <- stand_mort_gender %>% filter(age != "0-9")
stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))

stand_mort_gender <- stand_mort_gender %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates (AGAIN) based on the average of years 2017-2019, but now stratified by gender only
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_mort_gender <- stand_mort_gender %>% rename(mun = loc) %>% group_by(gender, mun, year) %>% summarise(pop_n = sum(population),
                                                                                                           deaths = sum(deaths),
                                                                                                           death_r = (sum(deaths) / sum(population)),
                                                                                                           death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

# Unnecessary
stand_mort_gender <- stand_mort_gender %>% mutate(deaths_r_std_disc = ifelse(death_r_std == 0, " 0",
                                                                      ifelse(death_r_std <= 1e-3, "<=1 per 1000",
                                                                      ifelse(death_r_std <= 2e-3, "<=2 per 1000",
                                                                      ifelse(death_r_std <= 3e-3, "<=3 per 1000",
                                                                      ifelse(death_r_std <= 4e-3, "<=4 per 1000", ">4 per 1000"))))))

# Add `DEP`
stand_mort_gender <- stand_mort_gender %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP` (missing 1998-2004 and 2020-2021)
stand_mort_gender <- stand_mort_gender %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
stand_mort_gender <- stand_mort_gender %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
stand_mort_gender <- stand_mort_gender %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
stand_mort_gender <- stand_mort_gender %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
stand_mort_gender <- stand_mort_gender %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

# Rearrange rows based on `gender`, `mun`, `year`, and `age`
stand_mort_gender <- stand_mort_gender %>% arrange(gender, mun, year)

# Compute the weighted average of the standardized death rates based on the capitals (for each gender)
weighted_death_r_std_men <- stand_mort_gender %>% filter(capital == 1, gender == "Male")   %>% dplyr::select(pop_n, death_r_std) %>% group_by() %>% summarise(weigthed_avg = weighted.mean(x = death_r_std, w = pop_n)) %>% ungroup() %>% c() %>% unlist() %>% unname()
weighted_death_r_std_fem <- stand_mort_gender %>% filter(capital == 1, gender == "Female") %>% dplyr::select(pop_n, death_r_std) %>% group_by() %>% summarise(weigthed_avg = weighted.mean(x = death_r_std, w = pop_n)) %>% ungroup() %>% c() %>% unlist() %>% unname()

# Compute multiplicative factors for each poor (defined according to VA or MPI) municipality
ref_mpi <- 25
stand_mort_gender <- stand_mort_gender %>% mutate(mult_factor = 1)
stand_mort_gender <- stand_mort_gender %>% mutate(mult_factor = ifelse((capital == 0) & (gender == "Male")   & (mpi > ref_mpi), (weighted_death_r_std_men / death_r_std), mult_factor))
stand_mort_gender <- stand_mort_gender %>% mutate(mult_factor = ifelse((capital == 0) & (gender == "Female") & (mpi > ref_mpi), (weighted_death_r_std_fem / death_r_std), mult_factor))
stand_mort_gender <- stand_mort_gender %>% mutate(mult_factor = ifelse(is.infinite(mult_factor), 1, mult_factor))
stand_mort_gender <- stand_mort_gender %>% mutate(mult_factor = ifelse(mult_factor < 1, 1, mult_factor))

{
  plot(stand_mort_gender$mult_factor, ylab = "", main = "Multiplicative factor", col = (as.numeric(stand_mort_gender$gender == "Male") + 2), ylim = c(0, 12))
  abline(h = 1, lwd = 2, col = "blue")
}

##########
##########

nat_mean_mal <- stand_mort_age_gender_cp %>% filter(gender == "Male")   %>% group_by(age) %>% summarize(mean_death_r_std = mean(death_r_std)) %>% ungroup() 
nat_mean_fem <- stand_mort_age_gender_cp %>% filter(gender == "Female") %>% group_by(age) %>% summarize(mean_death_r_std = mean(death_r_std)) %>% ungroup() 

##########
##########

muns <- stand_mort_gender$mun %>% unique()
muns_to_change <- stand_mort_gender %>% filter(capital == 0, mpi > ref_mpi) %>% dplyr::select(mun) %>% unique() %>% unname() %>% unlist() %>% c()
n_muns <- muns %>% length()

pb <- txtProgressBar(min = 1, max = n_muns, initial = 1) 
for (i in 1:n_muns) {
  
  # Male
  tmp_mult_factor_mal <- stand_mort_gender %>% filter(mun == muns[i], gender == "Male"  ) %>% dplyr::select(mult_factor) %>% c() %>% unlist() %>% unname()
  tmp_mal <- stand_mort_age_gender_cp %>% filter(mun == muns[i], gender == "Male"  )
  if (muns[i] %in% muns_to_change) { # Set as the `max`
    tmp_death_r_std_mal <- unname(unlist(c(nat_mean_mal$mean_death_r_std))) * tmp_mult_factor_mal
    tmp_mal <- tmp_mal %>% mutate(death_r_std = ifelse(tmp_death_r_std_mal > death_r_std, tmp_death_r_std_mal, death_r_std))
  } else {
    tmp_mal <- tmp_mal %>% mutate(death_r_std = death_r_std * tmp_mult_factor_mal)
  }
  
  # Female
  tmp_mult_factor_fem <- stand_mort_gender %>% filter(mun == muns[i], gender == "Female") %>% dplyr::select(mult_factor) %>% c() %>% unlist() %>% unname()
  tmp_fem <- stand_mort_age_gender_cp %>% filter(mun == muns[i], gender == "Female")
  if (muns[i] %in% muns_to_change) { # Set as the `max`
    tmp_death_r_std_fem <- unname(unlist(c(nat_mean_fem$mean_death_r_std))) * tmp_mult_factor_fem
    tmp_fem <- tmp_fem %>% mutate(death_r_std = ifelse(tmp_death_r_std_fem > death_r_std, tmp_death_r_std_fem, death_r_std))
  } else {
    tmp_fem <- tmp_fem %>% mutate(death_r_std = death_r_std * tmp_mult_factor_fem)
  }
  
  tmp <- bind_rows(tmp_mal, tmp_fem)
  
  if (i == 1) {
    tmp_stand_mort_age_gender <- tmp
  } else {
    tmp_stand_mort_age_gender <- bind_rows(tmp_stand_mort_age_gender, tmp)
  }
  setTxtProgressBar(pb, i)
}
close(pb)
tmp_stand_mort_age_gender <- tmp_stand_mort_age_gender %>% arrange(gender, mun, year, age)
stand_mort_age_gender_cp  <- stand_mort_age_gender_cp  %>% arrange(gender, mun, year, age)

stand_mort_age_gender_cp$fitted_death_r_std <- tmp_stand_mort_age_gender$death_r_std

# Set `new_stand_mort_age_gender` as the result of this approach (overwriting previous analyses)
new_stand_mort_age_gender <- stand_mort_age_gender_cp

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
# ggsave(total_box_VA, file = paste(out.dir, "/Deaths/boxplot_VA_mort.png", sep = ""), width = 12, height = 6)

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
ggsave(total_box_MPI, file = paste(out.dir, "/Deaths/boxplot_MPI_mort.png", sep = ""), width = 12, height = 6)

##################
# Compute back death count
##################

tmp_mort <- mort %>% dplyr::select(-national_pop)
tmp_mort <- dplyr::select(rename(new_stand_mort_age_gender, loc = mun), c("gender", "loc", "age", "fitted_death_r_std")) %>% rename(death_r_std = fitted_death_r_std) %>% right_join(y = tmp_mort, by = c("gender", "loc", "age"))
national_pop_all <- mort %>% dplyr::select(loc, year, gender, age, population) %>% group_by(year, gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()
tmp_mort <- tmp_mort %>% left_join(y = national_pop_all, by = c("year", "gender", "age"))
# national_pop_all <- mort %>% dplyr::select(loc, year, gender, age, population) %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()
# tmp_mort <- tmp_mort %>% left_join(y = national_pop_all, by = c("gender", "age"))
tmp_mort <- tmp_mort %>% left_join(y = multiplier_yy, by = c("year", "gender", "age"))
tmp_mort <- tmp_mort %>% mutate(death_rate = (death_r_std * national_pop_total) / national_pop * multiplier)
tmp_mort <- tmp_mort %>% mutate(deaths = death_rate * population)
tmp_mort <- tmp_mort %>% dplyr::select(gender, loc, year, age, deaths, population, death_rate)
tmp_mort <- tmp_mort %>% mutate(death_rate = compute_rate(deaths, population))

# Save `death count`
death_count <- tmp_mort 
death_count_cp <- death_count
death_count <- death_count %>% dplyr::select(-c(population, death_rate))
# Create `tmp_mort` based on the selected age groups
tmp_mort_fem <- tmp_mort %>% filter(gender == "Female", age %in% c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69"))
tmp_mort_mal <- tmp_mort %>% filter(gender == "Male"  , age %in% c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")) 
# Correct age group `65-69` (so it is `65-66`) for women, and group `75-79` (so it is `75-76`) for men
prop_65_66_mun_fem <- read_csv("~/Documents/Colombia Orphanhood/colombia_project/DATA/Population from Census/pop_65_66_mun_fem.csv", col_types = cols()) %>% rename(loc = mun) %>% mutate(loc = factor(loc))
prop_75_76_mun_mal <- read_csv("~/Documents/Colombia Orphanhood/colombia_project/DATA/Population from Census/pop_75_76_mun_mal.csv", col_types = cols()) %>% rename(loc = mun) %>% mutate(loc = factor(loc))
tmp_mort_fem <- tmp_mort_fem %>% left_join(y = prop_65_66_mun_fem, by = "loc") %>% mutate(pop_65_66_prop_female = ifelse(is.na(pop_65_66_prop_female), mean(prop_65_66_mun_fem$pop_65_66_prop_female, na.rm = TRUE), pop_65_66_prop_female))
tmp_mort_mal <- tmp_mort_mal %>% left_join(y = prop_75_76_mun_mal, by = "loc") %>% mutate(pop_75_76_prop_male   = ifelse(is.na(pop_75_76_prop_male  ), mean(prop_75_76_mun_mal$pop_75_76_prop_male  , na.rm = TRUE), pop_75_76_prop_male  ))
tmp_mort_fem <- tmp_mort_fem %>% mutate(deaths = ifelse(age == "65-69", deaths * pop_65_66_prop_female, deaths)) %>% mutate(population = ifelse(age == "65-69", population * pop_65_66_prop_female, population)) %>% dplyr::select(-pop_65_66_prop_female)
tmp_mort_mal <- tmp_mort_mal %>% mutate(deaths = ifelse(age == "75-79", deaths * pop_75_76_prop_male  , deaths)) %>% mutate(population = ifelse(age == "75-79", population * pop_75_76_prop_male  , population)) %>% dplyr::select(-pop_75_76_prop_male  )
# Aggregate old women to `50-66` and old men to `55-79`
tmp_mort_fem <- tmp_mort_fem %>% mutate(age = ifelse(age %in% c("50-54", "55-59", "60-64", "65-69"), "50-66", age))
tmp_mort_mal <- tmp_mort_mal %>% mutate(age = ifelse(age %in% c("55-59", "60-64", "65-69", "70-74", "75-79"), "55-79", age))
tmp_mort_fem <- tmp_mort_fem %>% group_by(gender, loc, year, age) %>% summarise(deaths = sum(deaths), population = sum(population)) %>% ungroup() %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
tmp_mort_mal <- tmp_mort_mal %>% group_by(gender, loc, year, age) %>% summarise(deaths = sum(deaths), population = sum(population)) %>% ungroup() %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
# Aggregate women and men
tmp_mort_cp <- bind_rows(tmp_mort_fem, tmp_mort_mal) %>% arrange(gender, loc, year, age)
print(paste("We have new ", round(sum(tmp_mort_cp$deaths) - sum(mort_cp$deaths), 2), " deaths, which is ", round((sum(tmp_mort_cp$deaths) - sum(mort_cp$deaths)) / sum(mort_cp$deaths), 4) * 100, "% of the initial count.", sep = ""))

tmp_mort <- tmp_mort_cp
saveRDS(object = death_count, file = file.path(data.dir, "summarised_data/new_death_count_correction_death_curves.RDS"))
saveRDS(object = tmp_mort, file = file.path(data.dir, "summarised_data/new_death_rate_correction_death_curves.RDS"))
tmp_mort_parental <- tmp_mort
tmp_mort <- death_count_cp

##################
##################

##################
# Compute standardized death rates based on the newly computed death counts
##################

last_stand_mort <- tmp_mort %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(deaths = mean(deaths)) %>% ungroup()
tmp <- mort %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
last_stand_mort <- last_stand_mort %>% left_join(y = tmp, by = c("loc", "gender", "age"))
last_stand_mort <- last_stand_mort %>% mutate(mun_death_r_2018 = deaths / population)
last_stand_mort <- last_stand_mort %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
last_stand_mort <- last_stand_mort %>% filter(age != "0-9")
last_stand_mort <- last_stand_mort %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))

last_stand_mort <- last_stand_mort %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates (last time) based on the average of years 2017-2019
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
last_stand_mort <- last_stand_mort %>% rename(mun = loc) %>% group_by(mun, year) %>% summarise(pop_n = sum(population),
                                                                                               deaths = sum(deaths),
                                                                                               death_r = (sum(deaths) / sum(population)),
                                                                                               death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

# Add `DEP`
last_stand_mort <- last_stand_mort %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP` (missing 1998-2004 and 2020-2021)
last_stand_mort <- last_stand_mort %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
last_stand_mort <- last_stand_mort %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
last_stand_mort <- last_stand_mort %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
last_stand_mort <- last_stand_mort %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
last_stand_mort <- last_stand_mort %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

# Based on MPI
last_p_MPI <- ggplot(last_stand_mort, aes(x = mpi, y = death_r_std)) +
  geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_vline(xintercept = 25, lty = "dashed") + 
  scale_x_log10() +
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.01)) +
  labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized death rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Adjusted") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(last_stand_mort, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(last_stand_mort, (capital == 0) & (mpi <  25)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(last_stand_mort, (capital == 0) & (mpi >= 25)), colour = "cyan",    lty = "dotted", method = "lm")

(total_MPI <- p_MPI + last_p_MPI)

ggsave(total_MPI, file = paste(out.dir, "/Deaths/plot_MPI_mort.png", sep = ""), width = 12, height = 6)

##################
##################

# Compute the standardized death rates for all years and plot the map
yys <- 1998:2021

all_stand_mort <- list()
all_m_death_r_std <- list()
for (yy in 2021) {
  
  stand_mort_yy <- tmp_mort_parental %>% filter(year == yy) %>% group_by(loc, gender, age) %>% summarise(deaths = mean(deaths)) %>% ungroup()
  tmp <- tmp_mort_parental %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
  stand_mort_yy <- stand_mort_yy %>% left_join(y = tmp, by = c("loc", "gender", "age"))
  stand_mort_yy <- stand_mort_yy %>% mutate(mun_death_r_2018 = deaths / population)
  stand_mort_yy <- stand_mort_yy %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
  stand_mort_yy <- stand_mort_yy %>% filter(age != "0-9")
  stand_mort_yy <- stand_mort_yy %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))
  
  # Needed, as age structure is different
  pop_ref_tmp <- tmp_mort_parental %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(natl_pop_n = sum(population), natl_death_n = sum(deaths)) %>% ungroup()
  pop_ref_tmp <- pop_ref_tmp %>% mutate(natl_pop_t = sum(natl_pop_n))
  pop_ref_tmp <- pop_ref_tmp %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t)
  pop_ref_tmp <- pop_ref_tmp %>% mutate(natl_death_r = natl_death_n / natl_pop_n)
  pop_ref_tmp <- pop_ref_tmp %>% mutate(natl_death_r_log = log(natl_death_n / natl_pop_n))
  
  stand_mort_yy <- stand_mort_yy %>% left_join(y = pop_ref_tmp, by = c("gender", "age"))
  
  # Compute standardized death rates
  national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
  stand_mort_yy <- stand_mort_yy %>% rename(mun = loc) %>% group_by(mun, year) %>% summarise(pop_n = sum(population),
                                                                                             deaths = sum(deaths),
                                                                                             death_r = (sum(deaths) / sum(population)),
                                                                                             death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()
  
  # Add `DEP`
  stand_mort_yy <- stand_mort_yy %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
  # Add `GDP` 
  stand_mort_yy <- stand_mort_yy %>% left_join(y = gdp, by = c("year", "dep")) 
  # Add `MPI`
  stand_mort_yy <- stand_mort_yy %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
  # Add `VA`
  stand_mort_yy <- stand_mort_yy %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
  # Add `capital`
  stand_mort_yy <- stand_mort_yy %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")
  
  stand_mort_yy <- stand_mort_yy %>% mutate(deaths_r_std_disc = ifelse(death_r_std == 0, " 0",
                                                                ifelse(death_r_std < 2e-3, "<2 per 1,000",
                                                                ifelse(death_r_std < 3e-3, "<3 per 1,000",
                                                                ifelse(death_r_std < 4e-3, "<4 per 1,000",
                                                                ifelse(death_r_std < 5e-3, "<5 per 1,000", "5+ per 1,000"))))))
  levels <- c(" 0", "<2 per 1,000", "<3 per 1,000", "<4 per 1,000", "<5 per 1,000", "5+ per 1,000")
  stand_mort_yy <- stand_mort_yy %>% mutate(deaths_r_std_disc = factor(deaths_r_std_disc, levels = levels))
  
  all_stand_mort[[as.character(yy)]] <- stand_mort_yy
  
  ####################
  # Plotting the maps
  ####################

  n_labs <- 6
  chosen_colors <- plot3D::jet.col(n = 34, alpha = 1)
  chosen_colors <- chosen_colors[3:32]
  alt_seq <- c(1, 15, 30, 40, 50, 60)
  alt_seq <- round(rescale_f(alt_seq, a = 1, b = length(chosen_colors), e = min(alt_seq), f = max(alt_seq)), 0)
  chosen_colors <- chosen_colors[alt_seq][1:n_labs]
  
  tmp <- colombia %>% left_join(y = all_stand_mort[[as.character(yy)]][, c("mun", "deaths_r_std_disc")], by = "mun") %>% rename(value = deaths_r_std_disc) %>% dplyr::select(mun, value, geometry)
  
  tmp_isl1 <- tmp %>% filter( (mun %in% c(88001)))        # Select islands ind.
  tmp_isl2 <- tmp %>% filter( (mun %in% c(88564)))        # Select islands ind.
  tmp_isla <- tmp %>% filter( (mun %in% c(88001, 88564))) # Select islands
  tmp_data <- tmp %>% filter(!(mun %in% c(88001, 88564))) # Remove islands
  
  tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
  tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry
  
  # Municipality `88001`
  col_88001 <- chosen_colors[which(tmp_isl1$value == levels)]
  i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
    geom_sf(fill = col_88001, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  # Municipality `88564`
  col_88564 <- chosen_colors[which(tmp_isl2$value == levels)]
  i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
    geom_sf(fill = col_88564, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  # Main area
  m_death_r_std <- ggplot() +
    geom_sf(data = tmp_data, aes(geometry = geometry, fill = value), color = "black") +
    scale_fill_manual(values = chosen_colors, breaks = levels, drop = FALSE, name = paste("Under-reporting adjusted parental\ndeath rates by municipality (", yy, ")", sep = ""), na.translate = TRUE) + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  m_death_r_std <- m_death_r_std + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  m_death_r_std <- m_death_r_std + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
  m_death_r_std <- m_death_r_std + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  m_death_r_std <- m_death_r_std + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  all_m_death_r_std[[as.character(yy)]] <- m_death_r_std
  
  ggsave(all_m_death_r_std[[as.character(yy)]], file = paste(out.dir, "/Deaths/death_r_std_", yy, ".png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

##################
# Other maps
##################

#######
# MPI #
#######

mpi_data <- last_stand_mort %>% dplyr::select(mun, mpi)
mpi_data <- mpi_data %>% mutate(mpi_disc = ifelse(mpi < 25, "0-24",
                                           ifelse(mpi < 50, "25-49",
                                           ifelse(mpi < 75, "50-74", "75-100")))) %>% dplyr::select(-mpi)
mpi_data <- colombia %>% left_join(y = mpi_data, by = "mun") %>% rename(value = mpi_disc) %>% dplyr::select(mun, value, geometry)
levels <- c("0-24", "25-49", "50-74", "75-100")
mpi_data <- mpi_data %>% mutate(value = factor(value, levels = levels))

teal_palette <- c("#219C90", "#FFF455", "#FFC700", "#EE4E4E")

tmp_isl1 <- mpi_data %>% filter( (mun %in% c(88001)))        
tmp_isl2 <- mpi_data %>% filter( (mun %in% c(88564)))        
mpi_data <- mpi_data %>% filter(!(mun %in% c(88001, 88564)))

tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry

# Municipality `88001`
col_88001 <- teal_palette[which(tmp_isl1$value == levels)]
i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
  geom_sf(fill = col_88001, color = "black") + 
  theme_bw() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# Municipality `88564`
col_88564 <- teal_palette[which(tmp_isl2$value == levels)]
i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
  geom_sf(fill = col_88564, color = "black") + 
  theme_bw() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

m_mpi <- ggplot() +
  geom_sf(data = mpi_data, aes(geometry = geometry, fill = value), color = "black") +
  scale_fill_manual(values = teal_palette, breaks = levels, drop = FALSE, name = "MPI", na.translate = TRUE) + 
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  guides(fill = guide_legend(order = 1))

m_mpi <- m_mpi + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
m_mpi <- m_mpi + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
m_mpi <- m_mpi + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
m_mpi <- m_mpi + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

ggsave(m_mpi, file = paste(out.dir, "/mpi.png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")

##############
# Population #
##############

tmp_pop <- total_pop %>% group_by(mun, year) %>% summarise(population = sum(population)) %>% ungroup()
tmp_pop <- tmp_pop %>% filter(year == 2021)
tmp_pop <- tmp_pop %>% mutate(pop_disc = ifelse(population < 6500,  "0-6,500",
                                         ifelse(population < 13000, "6,501-13,000",
                                         ifelse(population < 25000, "13,001-25,000", "25,001-8,000,000")))) %>% dplyr::select(-population)

tmp_pop <- colombia %>% left_join(y = tmp_pop, by = "mun") %>% rename(value = pop_disc) %>% dplyr::select(mun, value, geometry)
levels <- c("0-6,500", "6,501-13,000", "13,001-25,000", "25,001-8,000,000")
tmp_pop <- tmp_pop %>% mutate(value = factor(value, levels = levels))

teal_palette <- c("#219C90", "#FFF455", "#FFC700", "#EE4E4E")

tmp_isl1 <- tmp_pop %>% filter( (mun %in% c(88001)))        
tmp_isl2 <- tmp_pop %>% filter( (mun %in% c(88564)))        
tmp_pop <- tmp_pop %>% filter(!(mun %in% c(88001, 88564)))

tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry

# Municipality `88001`
col_88001 <- teal_palette[which(tmp_isl1$value == levels)]
i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
  geom_sf(fill = col_88001, color = "black") + 
  theme_bw() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# Municipality `88564`
col_88564 <- teal_palette[which(tmp_isl2$value == levels)]
i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
  geom_sf(fill = col_88564, color = "black") + 
  theme_bw() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

m_pop <- ggplot() +
  geom_sf(data = tmp_pop, aes(geometry = geometry, fill = value), color = "black") +
  scale_fill_manual(values = teal_palette, breaks = levels, drop = FALSE, name = "Population size", na.translate = TRUE) + 
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  guides(fill = guide_legend(order = 1))

m_pop <- m_pop + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
m_pop <- m_pop + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
m_pop <- m_pop + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
m_pop <- m_pop + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

ggsave(m_pop, file = paste(out.dir, "/pop.png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")

######################
# Population density #
######################

tmp_dens <- total_pop %>% group_by(mun, year) %>% summarise(population = sum(population)) %>% ungroup()
tmp_dens <- tmp_dens %>% filter(year == 2021)
tmp_dens <- colombia %>% left_join(y = tmp_dens, by = "mun") %>% dplyr::select(mun, population, geometry) %>% arrange(mun)
tmp_dens <- tmp_dens %>% mutate(area = as.double(units::set_units(st_area(geometry), km^2)))
tmp_dens <- tmp_dens %>% mutate(density = population / area)

tmp_dens <- tmp_dens %>% mutate(value = ifelse(density < 10,  "0-10",
                                        ifelse(density < 50,  "11-50",
                                        ifelse(density < 100, "51-100", "101-15,000")))) %>% dplyr::select(-c(population, area, density))

levels <- c("0-10", "11-50", "51-100", "101-15,000")
tmp_dens <- tmp_dens %>% mutate(value = factor(value, levels = levels))

teal_palette <- c("#219C90", "#FFF455", "#FFC700", "#EE4E4E")

tmp_isl1 <- tmp_dens %>% filter( (mun %in% c(88001)))        
tmp_isl2 <- tmp_dens %>% filter( (mun %in% c(88564)))        
tmp_dens <- tmp_dens %>% filter(!(mun %in% c(88001, 88564)))

tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry

# Municipality `88001`
col_88001 <- teal_palette[which(tmp_isl1$value == levels)]
i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
  geom_sf(fill = col_88001, color = "black") + 
  theme_bw() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# Municipality `88564`
col_88564 <- teal_palette[which(tmp_isl2$value == levels)]
i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
  geom_sf(fill = col_88564, color = "black") + 
  theme_bw() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

m_dens <- ggplot() +
  geom_sf(data = tmp_dens, aes(geometry = geometry, fill = value), color = "black") +
  scale_fill_manual(values = teal_palette, breaks = levels, drop = FALSE, name = "Population density\n(inhabitants per square kilometer)", na.translate = TRUE) + 
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  guides(fill = guide_legend(order = 1))

m_dens <- m_dens + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
m_dens <- m_dens + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
m_dens <- m_dens + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
m_dens <- m_dens + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

ggsave(m_dens, file = paste(out.dir, "/pop_dens.png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")

#########################
# Empirical death rates #
#########################

# Raw

# `reduced_mortality` excludes elderly (i.e., non-parental)
red_mort_fem <- mort %>% filter(gender == "Female", age %in% c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69"))
red_mort_mal <- mort %>% filter(gender == "Male"  , age %in% c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")) 
# Correct age group `65-69` (so it is `65-66`) for women, and group `75-79` (so it is `75-76`) for men
prop_65_66_mun_fem <- read_csv("~/Documents/Colombia Orphanhood/colombia_project/DATA/Population from Census/pop_65_66_mun_fem.csv", col_types = cols()) %>% rename(loc = mun) %>% mutate(loc = factor(loc))
prop_75_76_mun_mal <- read_csv("~/Documents/Colombia Orphanhood/colombia_project/DATA/Population from Census/pop_75_76_mun_mal.csv", col_types = cols()) %>% rename(loc = mun) %>% mutate(loc = factor(loc))
red_mort_fem <- red_mort_fem %>% left_join(y = prop_65_66_mun_fem, by = "loc") %>% mutate(pop_65_66_prop_female = ifelse(is.na(pop_65_66_prop_female), mean(prop_65_66_mun_fem$pop_65_66_prop_female, na.rm = TRUE), pop_65_66_prop_female))
red_mort_mal <- red_mort_mal %>% left_join(y = prop_75_76_mun_mal, by = "loc") %>% mutate(pop_75_76_prop_male   = ifelse(is.na(pop_75_76_prop_male  ), mean(prop_75_76_mun_mal$pop_75_76_prop_male  , na.rm = TRUE), pop_75_76_prop_male  ))
red_mort_fem <- red_mort_fem %>% mutate(deaths = ifelse(age == "65-69", deaths * pop_65_66_prop_female, deaths)) %>% mutate(population = ifelse(age == "65-69", population * pop_65_66_prop_female, population)) %>% dplyr::select(-pop_65_66_prop_female)
red_mort_mal <- red_mort_mal %>% mutate(deaths = ifelse(age == "75-79", deaths * pop_75_76_prop_male  , deaths)) %>% mutate(population = ifelse(age == "75-79", population * pop_75_76_prop_male  , population)) %>% dplyr::select(-pop_75_76_prop_male  )
# Aggregate old women to `50-66` and old men to `55-79`
red_mort_fem <- red_mort_fem %>% mutate(age = ifelse(age %in% c("50-54", "55-59", "60-64", "65-69"), "50-66", age))
red_mort_mal <- red_mort_mal %>% mutate(age = ifelse(age %in% c("55-59", "60-64", "65-69", "70-74", "75-79"), "55-79", age))
red_mort_fem <- red_mort_fem %>% group_by(gender, loc, year, age) %>% summarise(deaths = sum(deaths), population = sum(population)) %>% ungroup() %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
red_mort_mal <- red_mort_mal %>% group_by(gender, loc, year, age) %>% summarise(deaths = sum(deaths), population = sum(population)) %>% ungroup() %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
# Aggregate women and men
red_mort <- bind_rows(red_mort_fem, red_mort_mal) %>% arrange(gender, loc, year, age)

raw_empirical_death <- red_mort %>% rename(mun = loc) %>% dplyr::select(mun, year, population, deaths) %>% group_by(mun, year) %>% summarise(population = sum(population), deaths = sum(deaths)) %>% ungroup()
raw_empirical_death <- raw_empirical_death %>% mutate(death_rate = compute_rate(count = deaths, pop = population) * 1e3)
raw_empirical_death <- raw_empirical_death %>% mutate(death_rate_disc = ifelse(death_rate < 2,  "<2  per 1,000",
                                                                        ifelse(death_rate < 3,  "<3  per 1,000",
                                                                        ifelse(death_rate < 4,  "<4  per 1,000",
                                                                        ifelse(death_rate < 5,  "<5  per 1,000",
                                                                        ifelse(death_rate < 10, "<10 per 1,000", "10+ per 1,000"))))))

n_labs <- 6
chosen_colors <- plot3D::jet.col(n = 34, alpha = 1)
chosen_colors <- chosen_colors[3:32]
alt_seq <- c(1, 15, 30, 40, 50, 60)
alt_seq <- round(rescale_f(alt_seq, a = 1, b = length(chosen_colors), e = min(alt_seq), f = max(alt_seq)), 0)
chosen_colors <- chosen_colors[alt_seq][1:n_labs]

levels <- c("<2  per 1,000", "<3  per 1,000", "<4  per 1,000", "<5  per 1,000", "<10 per 1,000", "10+ per 1,000")

yys <- 2021:1998

for (yy in 2021) {
  
  tmp_raw_empirical_death <- raw_empirical_death %>% filter(year == yy)
  tmp_raw_empirical_death <- colombia %>% left_join(y = tmp_raw_empirical_death, by = "mun") %>% rename(value = death_rate_disc) %>% dplyr::select(mun, value, geometry)
  tmp_raw_empirical_death <- tmp_raw_empirical_death %>% mutate(value = factor(value, levels = levels))
  
  tmp_isl1 <- tmp_raw_empirical_death %>% filter((mun %in% c(88001)))        
  tmp_isl2 <- tmp_raw_empirical_death %>% filter((mun %in% c(88564)))        
  tmp_raw_empirical_death <- tmp_raw_empirical_death %>% filter(!(mun %in% c(88001, 88564)))
  
  tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
  tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry
  
  # Municipality `88001`
  col_88001 <- chosen_colors[which(tmp_isl1$value == levels)]
  i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
    geom_sf(fill = col_88001, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  # Municipality `88564`
  col_88564 <- chosen_colors[which(tmp_isl2$value == levels)]
  i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
    geom_sf(fill = col_88564, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  m_raw_emp_death <- ggplot() +
    geom_sf(data = tmp_raw_empirical_death, aes(geometry = geometry, fill = value), color = "black") +
    scale_fill_manual(values = chosen_colors, breaks = levels, drop = FALSE, name = paste("Unadjusted empirical parental\ndeath rate by municipality (", yy, ")", sep = ""), na.translate = TRUE) + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  m_raw_emp_death <- m_raw_emp_death + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  m_raw_emp_death <- m_raw_emp_death + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
  m_raw_emp_death <- m_raw_emp_death + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  m_raw_emp_death <- m_raw_emp_death + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  ggsave(m_raw_emp_death, file = paste(out.dir, "/Deaths/unadjusted_emp_death_", yy, ".png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")

}

# Adjusted

empirical_death <- tmp_mort_parental %>% rename(mun = loc) %>% dplyr::select(mun, year, population, deaths) %>% group_by(mun, year) %>% summarise(population = sum(population), deaths = sum(deaths)) %>% ungroup()
empirical_death <- empirical_death %>% mutate(death_rate = compute_rate(count = deaths, pop = population) * 1e3)
empirical_death <- empirical_death %>% mutate(death_rate_disc = ifelse(death_rate < 2,  "<2  per 1,000",
                                                                ifelse(death_rate < 3,  "<3  per 1,000",
                                                                ifelse(death_rate < 4,  "<4  per 1,000",
                                                                ifelse(death_rate < 5,  "<5  per 1,000",
                                                                ifelse(death_rate < 10, "<10 per 1,000", "10+ per 1,000"))))))

n_labs <- 6
chosen_colors <- plot3D::jet.col(n = 34, alpha = 1)
chosen_colors <- chosen_colors[3:32]
alt_seq <- c(1, 15, 30, 40, 50, 60)
alt_seq <- round(rescale_f(alt_seq, a = 1, b = length(chosen_colors), e = min(alt_seq), f = max(alt_seq)), 0)
chosen_colors <- chosen_colors[alt_seq][1:n_labs]

levels <- c("<2  per 1,000", "<3  per 1,000", "<4  per 1,000", "<5  per 1,000", "<10 per 1,000", "10+ per 1,000")

yys <- 2021:1998

for (yy in 2021) {
  
  tmp_empirical_death <- empirical_death %>% filter(year == yy)
  tmp_empirical_death <- colombia %>% left_join(y = tmp_empirical_death, by = "mun") %>% rename(value = death_rate_disc) %>% dplyr::select(mun, value, geometry)
  tmp_empirical_death <- tmp_empirical_death %>% mutate(value = factor(value, levels = levels))
  

  tmp_isl1 <- tmp_empirical_death %>% filter((mun %in% c(88001)))        
  tmp_isl2 <- tmp_empirical_death %>% filter((mun %in% c(88564)))        
  tmp_empirical_death <- tmp_empirical_death %>% filter(!(mun %in% c(88001, 88564)))
  
  tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
  tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry
  
  # Municipality `88001`
  col_88001 <- chosen_colors[which(tmp_isl1$value == levels)]
  i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
    geom_sf(fill = col_88001, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  # Municipality `88564`
  col_88564 <- chosen_colors[which(tmp_isl2$value == levels)]
  i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
    geom_sf(fill = col_88564, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  m_emp_death <- ggplot() +
    geom_sf(data = tmp_empirical_death, aes(geometry = geometry, fill = value), color = "black") +
    scale_fill_manual(values = chosen_colors, breaks = levels, drop = FALSE, name = paste("Under-reporting adjusted empirical parental\ndeath rate by municipality (", yy, ")", sep = ""), na.translate = TRUE) + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  m_emp_death <- m_emp_death + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  m_emp_death <- m_emp_death + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
  m_emp_death <- m_emp_death + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  m_emp_death <- m_emp_death + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

  ggsave(m_emp_death, file = paste(out.dir, "/Deaths/adjusted_emp_death_", yy, ".png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

###############################
# Death total in parental age #
###############################

death_total <- tmp_mort_parental %>% rename(mun = loc) %>% dplyr::select(mun, year, population, deaths) %>% group_by(mun, year) %>% summarise(population = sum(population), deaths = sum(deaths)) %>% ungroup()
death_total <- death_total %>% mutate(deaths_disc = ifelse(deaths == 0,   "0",
                                                    ifelse(deaths < 2,    "0-2",
                                                    ifelse(deaths < 5,    "3-5",
                                                    ifelse(deaths < 10,   "6-10",
                                                    ifelse(deaths < 25,   "11-25",
                                                    ifelse(deaths < 50,   "26-50",
                                                    ifelse(deaths < 100,  "51-100",
                                                    ifelse(deaths < 250,  "101-200",
                                                    ifelse(deaths < 500,  "201-500", "501-15,000"))))))))))

chosen_colors <- plot3D::jet.col(n = 10, alpha = 1)
levels <- c("0", "0-2", "3-5", "6-10", "11-25", "26-50", "51-100", "101-200", "201-500", "501-15,000")

yys <- 2021:1998

for (yy in 2021) {
  
  tmp_death_total <- death_total %>% filter(year == yy)
  tmp_death_total <- colombia %>% left_join(y = tmp_death_total, by = "mun") %>% rename(value = deaths_disc) %>% dplyr::select(mun, value, geometry)
  tmp_death_total <- tmp_death_total %>% mutate(value = factor(value, levels = levels))

  
  tmp_isl1 <- tmp_death_total %>% filter((mun %in% c(88001)))        
  tmp_isl2 <- tmp_death_total %>% filter((mun %in% c(88564)))        
  tmp_death_total <- tmp_death_total %>% filter(!(mun %in% c(88001, 88564)))
  
  tmp_isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
  tmp_isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry
  
  # Municipality `88001`
  col_88001 <- chosen_colors[which(tmp_isl1$value == levels)]
  i1 <- ggplot(data = tmp_isl1, aes(geometry = geometry)) + 
    geom_sf(fill = col_88001, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  # Municipality `88564`
  col_88564 <- chosen_colors[which(tmp_isl2$value == levels)]
  i2 <- ggplot(data = tmp_isl2, aes(geometry = geometry)) + 
    geom_sf(fill = col_88564, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  m_death_total <- ggplot() +
    geom_sf(data = tmp_death_total, aes(geometry = geometry, fill = value), color = "black") +
    scale_fill_manual(values = chosen_colors, breaks = levels, drop = FALSE, name = paste("Total number of deaths\nin parental ages in ", yy, sep = ""), na.translate = TRUE) + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  m_death_total <- m_death_total + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  m_death_total <- m_death_total + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
  m_death_total <- m_death_total + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  m_death_total <- m_death_total + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

  ggsave(m_death_total, file = paste(out.dir, "/Deaths/death_parental_", yy, ".png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

##################
##################
