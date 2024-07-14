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

total_pop <- readRDS(paste(data.dir, "/summarised_data/pop_all.RDS", sep = ""))
valid_mun <- unique(total_pop$mun)
gdp       <- readRDS(paste(data.dir, "/summarised_data/gbp.RDS", sep = ""))
geo_info  <- read_csv(paste(data.dir, "/summarised_data/geo_info.csv", sep = ""), col_types = cols())

######################################
# Fix missing `population` in `fert` #
######################################

fert <- fert %>% filter(loc %in% valid_mun) %>% mutate(loc = factor(loc))

######################################
######################################

# Check capital of "Cundinamarca"
geo_info <- geo_info %>% dplyr::select(dep, dep_name, mun, mun_name) %>% mutate(capital = ifelse(mun == (dep * 1e3 + 1), 1, 0)) %>% arrange(dep)
geo_info$mun <- factor(geo_info$mun)

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

#########################
#########################
########## NEW ##########
#########################
#########################

pop_ref <- fert %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(natl_pop_n = sum(population), natl_birth_n = sum(births)) %>% ungroup()
pop_ref <- pop_ref %>% mutate(natl_pop_t = sum(natl_pop_n))
pop_ref <- pop_ref %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t)
pop_ref <- pop_ref %>% mutate(natl_birth_r = natl_birth_n / natl_pop_n)
pop_ref <- pop_ref %>% mutate(natl_birth_r_log = log(natl_birth_n / natl_pop_n))

#########################
#########################

yys <- 1998:2021
all_birth_r <- list()
all_birth_r_std <- list()
count <- 0
for (yy in yys) {
  count <- count + 1
  print(yy)
  
  tmp_pop_ref <- fert %>% filter(year == yy) %>% group_by(gender, age) %>% summarise(natl_pop_n = sum(population), natl_birth_n = sum(deaths)) %>% ungroup()
  tmp_pop_ref <- tmp_pop_ref %>% mutate(natl_pop_t = sum(natl_pop_n))
  tmp_pop_ref <- tmp_pop_ref %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t)
  tmp_pop_ref <- tmp_pop_ref %>% mutate(natl_death_r = natl_birth_n / natl_pop_n)
  tmp_pop_ref <- tmp_pop_ref %>% mutate(natl_death_r_log = log(natl_birth_n / natl_pop_n))
  
  tmp <- fert %>% filter(year == yy) %>% dplyr::select(loc, gender, age, year, population)
  tmp_nat_stand_fert <- fert %>% filter(year == yy) %>% group_by(loc, gender, age) %>% summarise(births = mean(births)) %>% ungroup()
  tmp_nat_stand_fert <- tmp_nat_stand_fert %>% left_join(y = tmp, by = c("loc", "gender", "age"))
  tmp_nat_stand_fert <- tmp_nat_stand_fert %>% mutate(mun_birth_r_yy = births / population)
  tmp_nat_stand_fert <- tmp_nat_stand_fert %>% mutate(mun_birth_r_yy = ifelse(population == 0, 0, mun_birth_r_yy))
  tmp_nat_stand_fert <- tmp_nat_stand_fert %>% left_join(y = tmp_pop_ref, by = c("gender", "age"))
  
  national_pop_total <- total_pop %>% filter(year == yy) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
  tmp_nat_stand_fert <- tmp_nat_stand_fert %>% group_by(gender, age, year) %>% summarise(pop_n = sum(population), births = sum(births), birth_r = (sum(births) / sum(population)), birth_r_std = (sum(mun_birth_r_yy * natl_pop_n) / national_pop_total)) %>% ungroup()
  tmp_nat_stand_fert <- tmp_nat_stand_fert %>% arrange(gender, age)
  
  all_birth_r[[as.character(yy)]]     <- tmp_nat_stand_fert$birth_r
  all_birth_r_std[[as.character(yy)]] <- tmp_nat_stand_fert$birth_r_std
  
}

multiplier_yy <- list()
count <- 0
for (yy in yys) {
  count <- count + 1
  multiplier_yy[[as.character(yy)]] <- (all_birth_r_std[[as.character(yy)]] / all_birth_r_std[[as.character(2018)]])
}

age_fem <- pop_ref %>% filter(gender == "Female") %>% dplyr::select(age) %>% c() %>% unlist() %>% unname()
age_mal <- pop_ref %>% filter(gender == "Male"  ) %>% dplyr::select(age) %>% c() %>% unlist() %>% unname()
len_age <- length(age_fem) + length(age_mal)
multiplier_yy <- as_tibble(data.frame(year = rep(yys, each = len_age), gender = rep(rep(c("Female", "Male"), c(length(age_fem), length(age_mal))), length(yys)), age = rep(pop_ref$age, length(yys)), multiplier = (multiplier_yy %>% unlist() %>% unname())))

#########################
#########################

stand_fert <- fert %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(births = mean(births)) %>% ungroup()
tmp <- fert %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
stand_fert <- stand_fert %>% left_join(y = tmp, by = c("loc", "gender", "age"))
stand_fert <- stand_fert %>% mutate(mun_birth_r_2018 = births / population)
stand_fert <- stand_fert %>% mutate(mun_birth_r_2018 = ifelse(population == 0, 0, mun_birth_r_2018))
stand_fert <- stand_fert %>% filter(age != "0-9")
stand_fert <- stand_fert %>% mutate(mun_birth_r_2018_log = log(mun_birth_r_2018))

stand_fert <- stand_fert %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates based on the average of years 2017-2019
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_fert <- stand_fert %>% rename(mun = loc) %>% group_by(mun, year) %>% summarise(pop_n = sum(population),
                                                                                     births = sum(births),
                                                                                     birth_r = (sum(births) / sum(population)),
                                                                                     birth_r_std = (sum(mun_birth_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

stand_fert <- stand_fert %>% mutate(births_r_std_disc = ifelse(birth_r_std == 0, " 0",
                                                        ifelse(birth_r_std <= 5e-3,  "<=5  per 1000",
                                                        ifelse(birth_r_std <= 10e-3, "<=10 per 1000",
                                                        ifelse(birth_r_std <= 20e-3, "<=20 per 1000",
                                                        ifelse(birth_r_std <= 30e-3, "<=30 per 1000", ">30 per 1000"))))))


# Add `DEP`
stand_fert <- stand_fert %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP`
stand_fert <- stand_fert %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
stand_fert <- stand_fert %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
stand_fert <- stand_fert %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
stand_fert <- stand_fert %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
stand_fert <- stand_fert %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

# Based on VA
p_VA <- ggplot(stand_fert, aes(x = va, y = birth_r_std)) +
  geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_vline(xintercept = 1e+06, lty = "dashed") + 
  scale_x_log10() +
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.1)) +
  labs(x = "VA", y = "Mean standardized fertility rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Original") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(stand_fert,  capital == 1), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(stand_fert, (capital == 0) & (va <  1e6)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(stand_fert, (capital == 0) & (va >= 1e6)), colour = "cyan",    lty = "dotted", method = "lm")
p_VA

# Based on MPI
p_MPI <- ggplot(stand_fert, aes(x = mpi, y = birth_r_std)) +
  geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_vline(xintercept = 25, lty = "dashed") + 
  scale_x_log10() +
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.1)) +
  labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized fertility rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Original") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(stand_fert, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(stand_fert, (capital == 0) & (mpi <  25)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(stand_fert, (capital == 0) & (mpi >= 25)), colour = "cyan",    lty = "dotted", method = "lm")
p_MPI


# Compute the weighted average of the standardized death rates based on the capitals
weighted_birth_r_std <- stand_fert %>% filter(capital == 1) %>% dplyr::select(pop_n, birth_r_std) %>% group_by() %>% summarise(weigthed_avg = weighted.mean(x = birth_r_std, w = pop_n)) %>% ungroup() %>% c() %>% unlist() %>% unname()

##################
### PROCESSING ###
##################

##################
# Computations per age group and gender
##################

tmp <- fert %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
stand_fert_age_gender <- fert %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(births = mean(births)) %>% ungroup()
stand_fert_age_gender <- stand_fert_age_gender %>% left_join(y = tmp, by = c("loc", "gender", "age"))
stand_fert_age_gender <- stand_fert_age_gender %>% mutate(mun_birth_r_2018 = births / population)
stand_fert_age_gender <- stand_fert_age_gender %>% mutate(mun_birth_r_2018 = ifelse(population == 0, 0, mun_birth_r_2018))
stand_fert_age_gender <- stand_fert_age_gender %>% filter(age != "0-9")
stand_fert_age_gender <- stand_fert_age_gender %>% mutate(mun_birth_r_2018_log = log(mun_birth_r_2018))
stand_fert_age_gender <- stand_fert_age_gender %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates based on the average of years 2017-2019, but stratified by age group and gender
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_fert_age_gender <- stand_fert_age_gender %>% rename(mun = loc) %>% group_by(gender, mun, year, age) %>% summarise(pop_n = sum(population),
                                                                                                                        births = sum(births),
                                                                                                                        birth_r = (sum(births) / sum(population)),
                                                                                                                        birth_r_std = (sum(mun_birth_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

# Add `DEP`
stand_fert_age_gender <- stand_fert_age_gender %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP`
stand_fert_age_gender <- stand_fert_age_gender %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
stand_fert_age_gender <- stand_fert_age_gender %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
stand_fert_age_gender <- stand_fert_age_gender %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
stand_fert_age_gender <- stand_fert_age_gender %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
stand_fert_age_gender <- stand_fert_age_gender %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

# Rearrange rows based on `gender`, `mun`, `year`, and `age`
stand_fert_age_gender <- stand_fert_age_gender %>% arrange(gender, mun, year, age)
stand_fert_age_gender_cp <- stand_fert_age_gender

gender_age <- stand_fert_age_gender %>% dplyr::select(gender, age) %>% distinct() # all possible combinations

##################
# Analysis based on the "multiplier" approach (computations per gender)
##################

stand_fert_gender <- fert %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(births = mean(births)) %>% ungroup()
tmp <- fert %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
stand_fert_gender <- stand_fert_gender %>% left_join(y = tmp, by = c("loc", "gender", "age"))
stand_fert_gender <- stand_fert_gender %>% mutate(mun_birth_r_2018 = births / population)
stand_fert_gender <- stand_fert_gender %>% mutate(mun_birth_r_2018 = ifelse(population == 0, 0, mun_birth_r_2018))
stand_fert_gender <- stand_fert_gender %>% filter(age != "0-9")
stand_fert_gender <- stand_fert_gender %>% mutate(mun_birth_r_2018_log = log(mun_birth_r_2018))

stand_fert_gender <- stand_fert_gender %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates based on the average of years 2017-2019, but stratified by gender only
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_fert_gender <- stand_fert_gender %>% rename(mun = loc) %>% group_by(gender, mun, year) %>% summarise(pop_n = sum(population),
                                                                                                           births = sum(births),
                                                                                                           birth_r = (sum(births) / sum(population)),
                                                                                                           birth_r_std = (sum(mun_birth_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

stand_fert_gender <- stand_fert_gender %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP`
stand_fert_gender <- stand_fert_gender %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
stand_fert_gender <- stand_fert_gender %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
stand_fert_gender <- stand_fert_gender %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
stand_fert_gender <- stand_fert_gender %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
stand_fert_gender <- stand_fert_gender %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

# Rearrange rows based on `gender`, `mun`, `year`, and `age`
stand_fert_gender <- stand_fert_gender %>% arrange(gender, mun, year)

# Compute the weighted average of the standardized death rates based on the capitals (for each gender)
weighted_birth_r_std_men <- stand_fert_gender %>% filter(capital == 1, gender == "Male")   %>% dplyr::select(pop_n, birth_r_std) %>% group_by() %>% summarise(weigthed_avg = weighted.mean(x = birth_r_std, w = pop_n)) %>% ungroup() %>% c() %>% unlist() %>% unname()
weighted_birth_r_std_fem <- stand_fert_gender %>% filter(capital == 1, gender == "Female") %>% dplyr::select(pop_n, birth_r_std) %>% group_by() %>% summarise(weigthed_avg = weighted.mean(x = birth_r_std, w = pop_n)) %>% ungroup() %>% c() %>% unlist() %>% unname()

# Compute multiplicative factors for each poor (defined according to VA or MPI) municipality
ref_mpi <- 25
stand_fert_gender <- stand_fert_gender %>% mutate(mult_factor = 1)
stand_fert_gender <- stand_fert_gender %>% mutate(mult_factor = ifelse((capital == 0) & (gender == "Male")   & (mpi > ref_mpi), (weighted_birth_r_std_men / birth_r_std), mult_factor))
stand_fert_gender <- stand_fert_gender %>% mutate(mult_factor = ifelse((capital == 0) & (gender == "Female") & (mpi > ref_mpi), (weighted_birth_r_std_fem / birth_r_std), mult_factor))
stand_fert_gender <- stand_fert_gender %>% mutate(mult_factor = ifelse(is.infinite(mult_factor), 1, mult_factor))
stand_fert_gender <- stand_fert_gender %>% mutate(mult_factor = ifelse(mult_factor < 1, 1, mult_factor))

{
  plot(stand_fert_gender$mult_factor, ylab = "", main = "Multiplicative factor", col = (as.numeric(stand_fert_gender$gender == "Male") + 2), ylim = c(1, 6))
  abline(h = 1, lwd = 2, col = "blue")
}

##########
##########

nat_mean_mal <- stand_fert_age_gender_cp %>% filter(gender == "Male")   %>% group_by(age) %>% summarize(mean_birth_r_std = mean(birth_r_std)) %>% ungroup() 
nat_mean_fem <- stand_fert_age_gender_cp %>% filter(gender == "Female") %>% group_by(age) %>% summarize(mean_birth_r_std = mean(birth_r_std)) %>% ungroup() 

##########
##########

muns <- stand_fert_gender$mun %>% unique()
muns_to_change <- stand_fert_gender %>% filter(capital == 0, mpi > ref_mpi) %>% dplyr::select(mun) %>% unique() %>% unname() %>% unlist() %>% c()
n_muns <- muns %>% length()

pb <- txtProgressBar(min = 1, max = n_muns, initial = 1) 
for (i in 1:n_muns) {
  
  # Male
  tmp_mult_factor_mal <- stand_fert_gender %>% filter(mun == muns[i], gender == "Male"  ) %>% dplyr::select(mult_factor) %>% c() %>% unlist() %>% unname()
  tmp_mal <- stand_fert_age_gender_cp %>% filter(mun == muns[i], gender == "Male"  )
  if (muns[i] %in% muns_to_change) { # Set as the `max`
    tmp_birth_r_std_mal <- unname(unlist(c(nat_mean_mal$mean_birth_r_std))) * tmp_mult_factor_mal
    tmp_mal <- tmp_mal %>% mutate(birth_r_std = ifelse(tmp_birth_r_std_mal > birth_r_std, tmp_birth_r_std_mal, birth_r_std))
  } else {
    tmp_mal <- tmp_mal %>% mutate(birth_r_std = birth_r_std * tmp_mult_factor_mal)
  }
  
  # Female
  tmp_mult_factor_fem <- stand_fert_gender %>% filter(mun == muns[i], gender == "Female") %>% dplyr::select(mult_factor) %>% c() %>% unlist() %>% unname()
  tmp_fem <- stand_fert_age_gender_cp %>% filter(mun == muns[i], gender == "Female")
  if (muns[i] %in% muns_to_change) { # Set as the `max`
    tmp_birth_r_std_fem <- unname(unlist(c(nat_mean_fem$mean_birth_r_std))) * tmp_mult_factor_fem
    tmp_fem <- tmp_fem %>% mutate(birth_r_std = ifelse(tmp_birth_r_std_fem > birth_r_std, tmp_birth_r_std_fem, birth_r_std))
  } else {
    tmp_fem <- tmp_fem %>% mutate(birth_r_std = birth_r_std * tmp_mult_factor_fem)
  }
  
  tmp <- bind_rows(tmp_mal, tmp_fem)
  
  if (i == 1) {
    tmp_stand_fert_age_gender <- tmp
  } else {
    tmp_stand_fert_age_gender <- bind_rows(tmp_stand_fert_age_gender, tmp)
  }
  setTxtProgressBar(pb, i)
}
close(pb)
tmp_stand_fert_age_gender <- tmp_stand_fert_age_gender %>% arrange(gender, mun, year, age)
stand_fert_age_gender_cp  <- stand_fert_age_gender_cp  %>% arrange(gender, mun, year, age)

stand_fert_age_gender_cp$fitted_birth_r_std <- tmp_stand_fert_age_gender$birth_r_std

# Set `new_stand_fert_age_gender` as the result of this approach (overwriting previous analyses)
new_stand_fert_age_gender <- stand_fert_age_gender_cp

##################
# Boxplots  
##################

##################
# Based on VA
##################

new_stand_fert_age_gender <- new_stand_fert_age_gender %>% mutate(va_disc = ifelse(va < 1e5, "(1) <100,000",
                                                                            ifelse(va < 5e5, "(2) 100,000-500,000",
                                                                            ifelse(va < 1e6, "(3) 500,000-1,000,000", "(4) >1,000,000"))))


new_stand_fert_age_gender <- new_stand_fert_age_gender %>% mutate(class = paste(gender, "_", age, sep = "")) %>% mutate(class = factor(class))

tmp_max_box <- max(c(new_stand_fert_age_gender$birth_r_std, new_stand_fert_age_gender$fitted_birth_r_std))

# National mean
mean_tmp_box     <- new_stand_fert_age_gender %>% group_by(age) %>% summarize(mean_birth_r_std = mean(birth_r_std)) %>% ungroup() 
new_mean_tmp_box <- new_stand_fert_age_gender %>% group_by(age) %>% summarize(mean_birth_r_std = mean(fitted_birth_r_std)) %>% ungroup() 

tmp_box_VA <- ggplot(new_stand_fert_age_gender, aes(x = age)) +
  geom_boxplot(aes(x = age, y = birth_r_std)) +
  geom_line(data = mean_tmp_box,     aes(x = age, y = mean_birth_r_std, group = 1, color = "Original"), lwd = 0.75) +
  geom_line(data = new_mean_tmp_box, aes(x = age, y = mean_birth_r_std, group = 1, color = "Adjusted"), lwd = 0.75) +
  scale_y_sqrt(limits = c(0, tmp_max_box)) +
  scale_color_manual(values = c("Original" = "red", "Adjusted" = "blue")) +
  facet_grid(va_disc ~ gender) +
  theme_bw() +
  labs(title = "Original", x = "Age", y = "Mean standardized fertility rates 2017-2019", color = "Mean") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5))

new_tmp_box_VA <- ggplot(new_stand_fert_age_gender, aes(x = age)) +
  geom_boxplot(aes(x = age, y = fitted_birth_r_std)) +
  geom_line(data = mean_tmp_box,     aes(x = age, y = mean_birth_r_std, group = 1, color = "Original"), lwd = 0.75) +
  geom_line(data = new_mean_tmp_box, aes(x = age, y = mean_birth_r_std, group = 1, color = "Adjusted"), lwd = 0.75) +
  scale_y_sqrt(limits = c(0, tmp_max_box)) +
  scale_color_manual(values = c("Original" = "red", "Adjusted" = "blue")) +
  facet_grid(va_disc ~ gender) +
  theme_bw() +
  labs(title = "Adjusted", x = "Age", y = "Mean standardized fertility rates 2017-2019", color = "Mean") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5))

(total_box_VA <- tmp_box_VA + new_tmp_box_VA)
# ggsave(total_box_VA, file = paste(out.dir, "/Births/boxplot_VA_fert.png", sep = ""), width = 12, height = 6)

##################
# Based on MPI
##################

new_stand_fert_age_gender <- new_stand_fert_age_gender %>% mutate(mpi_disc = ifelse(mpi < 25, "(4) MPI:  0-25",
                                                                             ifelse(mpi < 50, "(3) MPI: 25-50",
                                                                             ifelse(mpi < 75, "(2) MPI: 50-75", "(1) MPI: 75-100"))))

tmp_box_MPI <- ggplot(new_stand_fert_age_gender, aes(x = age)) +
  geom_boxplot(aes(x = age, y = birth_r_std)) +
  geom_line(data = mean_tmp_box,     aes(x = age, y = mean_birth_r_std, group = 1, color = "Original"), lwd = 0.75) +
  geom_line(data = new_mean_tmp_box, aes(x = age, y = mean_birth_r_std, group = 1, color = "Adjusted"), lwd = 0.75) +
  scale_y_sqrt(limits = c(0, tmp_max_box)) +
  scale_color_manual(values = c("Original" = "red", "Adjusted" = "blue")) +
  facet_grid(mpi_disc ~ gender) +
  theme_bw() +
  labs(title = "Original", x = "Age", y = "Mean standardized fertility rates 2017-2019", color = "Mean") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5))

new_tmp_box_MPI <- ggplot(new_stand_fert_age_gender, aes(x = age)) +
  geom_boxplot(aes(x = age, y = fitted_birth_r_std)) +
  geom_line(data = mean_tmp_box,     aes(x = age, y = mean_birth_r_std, group = 1, color = "Original"), lwd = 0.75) +
  geom_line(data = new_mean_tmp_box, aes(x = age, y = mean_birth_r_std, group = 1, color = "Adjusted"), lwd = 0.75) +
  scale_y_sqrt(limits = c(0, tmp_max_box)) +
  scale_color_manual(values = c("Original" = "red", "Adjusted" = "blue")) +
  facet_grid(mpi_disc ~ gender) +
  theme_bw() +
  labs(title = "Adjusted", x = "Age", y = "Mean standardized fertility rates 2017-2019", color = "Mean") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5))

(total_box_MPI <- tmp_box_MPI + new_tmp_box_MPI)
ggsave(total_box_VA, file = paste(out.dir, "/Births/boxplot_MPI_fert.png", sep = ""), width = 12, height = 6)

##################
# Compute back birth count
##################

national_pop_all <- fert %>% dplyr::select(loc, year, gender, age, population) %>% group_by(year, gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()
tmp_fert <- fert 
tmp_fert <- dplyr::select(rename(new_stand_fert_age_gender, loc = mun), c("gender", "loc", "age", "fitted_birth_r_std")) %>% rename(birth_r_std = fitted_birth_r_std) %>% right_join(y = tmp_fert, by = c("gender", "loc", "age"))
tmp_fert <- tmp_fert %>% left_join(y = national_pop_all, by = c("year", "gender", "age"))
tmp_fert <- tmp_fert %>% left_join(y = multiplier_yy, by = c("year", "gender", "age"))
tmp_fert <- tmp_fert %>% mutate(birth_rate = (birth_r_std * national_pop_total) / national_pop * multiplier)
tmp_fert <- tmp_fert %>% mutate(births = birth_rate * population) 
tmp_fert <- tmp_fert %>% dplyr::select(gender, loc, year, age, births, population, birth_rate)
tmp_fert <- tmp_fert %>% mutate(fertility_rate = compute_rate(births, population))

print(paste("We have new ", round(sum(tmp_fert$births) - sum(fert$births), 2), " births, which is ", round((sum(tmp_fert$births) - sum(fert$births)) / sum(fert$births), 4) * 100, "% of the initial count.", sep = ""))

saveRDS(object = tmp_fert, file = file.path(data.dir, "summarised_data/new_birth_rate_correction_death_curves.RDS"))

##################
##################

##################
# Compute standardized birth rates based on the newly computed death counts
##################

last_stand_fert <- tmp_fert %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(births = mean(births)) %>% ungroup()
tmp <- fert %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
last_stand_fert <- last_stand_fert %>% left_join(y = tmp, by = c("loc", "gender", "age"))
last_stand_fert <- last_stand_fert %>% mutate(mun_birth_r_2018 = births / population)
last_stand_fert <- last_stand_fert %>% mutate(mun_birth_r_2018 = ifelse(population == 0, 0, mun_birth_r_2018))
last_stand_fert <- last_stand_fert %>% filter(age != "0-9")
last_stand_fert <- last_stand_fert %>% mutate(mun_birth_r_2018_log = log(mun_birth_r_2018))
last_stand_fert <- last_stand_fert %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized fertility rates based on the average of years 2017-2019
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
last_stand_fert <- last_stand_fert %>% rename(mun = loc) %>% group_by(mun, year) %>% summarise(pop_n = sum(population),
                                                                                               births = sum(births),
                                                                                               birth_r = (sum(births) / sum(population)),
                                                                                               birth_r_std = (sum(mun_birth_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

# Add `DEP`
last_stand_fert <- last_stand_fert %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP`
last_stand_fert <- last_stand_fert %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
last_stand_fert <- last_stand_fert %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
last_stand_fert <- last_stand_fert %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
last_stand_fert <- last_stand_fert %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
last_stand_fert <- last_stand_fert %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

# Based on MPI
last_p_MPI <- ggplot(last_stand_fert, aes(x = mpi, y = birth_r_std)) +
  geom_point(aes(color = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_vline(xintercept = 25, lty = "dashed") + 
  scale_x_log10() +
  scale_color_manual(values = c("red", "blue")) + scale_y_continuous(limits = c(0, 0.1)) +
  labs(x = "Multidimensional poverty index (MPI)", y = "Mean standardized fertility rates 2017-2019", colour = "Type", size = "", pch = "") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Adjusted") +
  geom_smooth(colour = "yellow") +
  geom_smooth(data = subset(last_stand_fert, (capital == 1)), colour = "black", lty = "dotted", method = "lm") + 
  geom_smooth(data = subset(last_stand_fert, (capital == 0) & (mpi <  25)), colour = "magenta", lty = "dotted", method = "lm") +
  geom_smooth(data = subset(last_stand_fert, (capital == 0) & (mpi >= 25)), colour = "cyan",    lty = "dotted", method = "lm")

(total_MPI <- p_MPI + last_p_MPI)

ggsave(total_MPI, file = paste(out.dir, "/Births/plot_MPI_fer.png", sep = ""), width = 12, height = 6)

##################
##################

# Compute the standardized fertility rates for all years and plot the map
yys <- 1998:2021

all_stand_fert <- list()
all_m_birth_r_std <- list()
for (yy in 2021) {
  
  stand_fert_yy <- tmp_fert %>% filter(year == yy) %>% group_by(loc, gender, age) %>% summarise(births = mean(births)) %>% ungroup()
  tmp <- fert %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
  stand_fert_yy <- stand_fert_yy %>% left_join(y = tmp, by = c("loc", "gender", "age"))
  stand_fert_yy <- stand_fert_yy %>% mutate(mun_birth_r_2018 = births / population)
  stand_fert_yy <- stand_fert_yy %>% mutate(mun_birth_r_2018 = ifelse(population == 0, 0, mun_birth_r_2018))
  stand_fert_yy <- stand_fert_yy %>% filter(age != "0-9")
  stand_fert_yy <- stand_fert_yy %>% mutate(mun_birth_r_2018_log = log(mun_birth_r_2018))
  stand_fert_yy <- stand_fert_yy %>% left_join(y = pop_ref, by = c("gender", "age"))
  
  # Compute standardized fertility rates
  national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
  stand_fert_yy <- stand_fert_yy %>% rename(mun = loc) %>% group_by(mun, year) %>% summarise(pop_n = sum(population),
                                                                                             births = sum(births),
                                                                                             birth_r = (sum(births) / sum(population)),
                                                                                             birth_r_std = (sum(mun_birth_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()
  
  # Add `DEP`
  stand_fert_yy <- stand_fert_yy %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
  # Add `GDP` 
  stand_fert_yy <- stand_fert_yy %>% left_join(y = gdp, by = c("year", "dep")) 
  # Add `MPI`
  stand_fert_yy <- stand_fert_yy %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
  # Add `VA`
  stand_fert_yy <- stand_fert_yy %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
  # Add `capital`
  stand_fert_yy <- stand_fert_yy %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")
  
  stand_fert_yy <- stand_fert_yy %>% mutate(births_r_std_disc = ifelse(birth_r_std == 0, " 0",
                                                                ifelse(birth_r_std <  10e-3, "<10 per 1,000",
                                                                ifelse(birth_r_std <  25e-3, "<25 per 1,000",
                                                                ifelse(birth_r_std <  30e-3, "<30 per 1,000",
                                                                ifelse(birth_r_std <  35e-3, "<35 per 1,000", "35+ per 1,000"))))))
  
  levels <- c(" 0", "<10 per 1,000", "<25 per 1,000", "<30 per 1,000", "<35 per 1,000", "35+ per 1,000")
  stand_fert_yy <- stand_fert_yy %>% mutate(births_r_std_disc = factor(births_r_std_disc, levels = levels))
  
  all_stand_fert[[as.character(yy)]] <- stand_fert_yy
  
  ####################
  # Plotting the maps
  ####################

  n_labs <- 6
  chosen_colors <- plot3D::jet.col(n = 34, alpha = 1)
  chosen_colors <- chosen_colors[3:32]
  alt_seq <- c(1, 15, 30, 40, 50, 60)
  alt_seq <- round(rescale_f(alt_seq, a = 1, b = length(chosen_colors), e = min(alt_seq), f = max(alt_seq)), 0)
  chosen_colors <- chosen_colors[alt_seq][1:n_labs]
  
  tmp <- colombia %>% left_join(y = all_stand_fert[[as.character(yy)]][, c("mun", "births_r_std_disc")], by = "mun") %>% rename(value = births_r_std_disc) %>% dplyr::select(mun, value, geometry)
  
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
  m_birth_r_std <- ggplot() +
    geom_sf(data = tmp_data, aes(geometry = geometry, fill = value), color = "black") +
    scale_fill_manual(values = chosen_colors, breaks = levels, drop = FALSE, name = paste("Under-reporting adjusted parental\nfertility rates by municipality (", yy, ")", sep = ""), na.translate = TRUE) + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  m_birth_r_std <- m_birth_r_std + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  m_birth_r_std <- m_birth_r_std + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
  m_birth_r_std <- m_birth_r_std + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  m_birth_r_std <- m_birth_r_std + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  all_m_birth_r_std[[as.character(yy)]] <- m_birth_r_std
  
  ggsave(all_m_birth_r_std[[as.character(yy)]], file = paste(out.dir, "/Births/birth_r_std_", yy, ".png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

#########################
# Empirical birth rates #
#########################

# Raw

valid_muns <- geo_info$mun %>% unique()

raw_empirical_birth <- fert %>% rename(mun = loc) %>% filter(mun %in% valid_muns) %>% mutate(mun = factor(mun)) %>% dplyr::select(mun, year, population, births) %>% group_by(mun, year) %>% summarise(population = sum(population), births = sum(births)) %>% ungroup()
raw_empirical_birth <- raw_empirical_birth %>% mutate(births = ifelse(population == 0, 0, births))
raw_empirical_birth <- raw_empirical_birth %>% mutate(fertility_rate = compute_rate(count = births, pop = population) * 1e3)
raw_empirical_birth <- raw_empirical_birth %>% mutate(fertility_rate_disc = ifelse(fertility_rate < 20,  "<20  per 1,000",
                                                                            ifelse(fertility_rate < 40,  "<40  per 1,000",
                                                                            ifelse(fertility_rate < 60,  "<60  per 1,000",
                                                                            ifelse(fertility_rate < 80,  "<80  per 1,000",
                                                                            ifelse(fertility_rate < 100, "<100 per 1,000", "100+ per 1,000"))))))

n_labs <- 6
chosen_colors <- plot3D::jet.col(n = 34, alpha = 1)
chosen_colors <- chosen_colors[3:32]
alt_seq <- c(1, 15, 30, 40, 50, 60)
alt_seq <- round(rescale_f(alt_seq, a = 1, b = length(chosen_colors), e = min(alt_seq), f = max(alt_seq)), 0)
chosen_colors <- chosen_colors[alt_seq][1:n_labs]

levels <- c("<20  per 1,000", "<40  per 1,000", "<60  per 1,000", "<80  per 1,000", "<100 per 1,000", "100+ per 1,000")

yys <- 2021:1998

for (yy in 2021) {
  
  tmp_raw_empirical_birth <- raw_empirical_birth %>% filter(year == yy)
  tmp_raw_empirical_birth <- colombia %>% left_join(y = tmp_raw_empirical_birth, by = "mun") %>% rename(value = fertility_rate_disc) %>% dplyr::select(mun, value, geometry)
  tmp_raw_empirical_birth <- tmp_raw_empirical_birth %>% mutate(value = factor(value, levels = levels))
  
  tmp_isl1 <- tmp_raw_empirical_birth %>% filter((mun %in% c(88001)))        
  tmp_isl2 <- tmp_raw_empirical_birth %>% filter((mun %in% c(88564)))        
  tmp_raw_empirical_birth <- tmp_raw_empirical_birth %>% filter(!(mun %in% c(88001, 88564)))
  
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
  
  m_raw_emp_birth <- ggplot() +
    geom_sf(data = tmp_raw_empirical_birth, aes(geometry = geometry, fill = value), color = "black") +
    scale_fill_manual(values = chosen_colors, breaks = levels, drop = FALSE, name = paste("Unadjusted empirical parental\nbirth rate by municipality (", yy, ")", sep = ""), na.translate = TRUE) + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  m_raw_emp_birth <- m_raw_emp_birth + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  m_raw_emp_birth <- m_raw_emp_birth + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
  m_raw_emp_birth <- m_raw_emp_birth + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  m_raw_emp_birth <- m_raw_emp_birth + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  ggsave(m_raw_emp_birth, file = paste(out.dir, "/Births/unadjusted_emp_birth_", yy, ".png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

# Adjusted

empirical_birth <- tmp_fert %>% rename(mun = loc) %>% dplyr::select(mun, year, population, births) %>% group_by(mun, year) %>% summarise(population = sum(population), births = sum(births)) %>% ungroup()
empirical_birth <- empirical_birth %>% mutate(fertility_rate = compute_rate(count = births, pop = population) * 1e3)
empirical_birth <- empirical_birth %>% mutate(birth_rate_disc = ifelse(fertility_rate < 20,  "<20  per 1,000",
                                                                ifelse(fertility_rate < 40,  "<40  per 1,000",
                                                                ifelse(fertility_rate < 60,  "<60  per 1,000",
                                                                ifelse(fertility_rate < 80,  "<80  per 1,000",
                                                                ifelse(fertility_rate < 100, "<100 per 1,000", "100+ per 1,000"))))))

n_labs <- 6
chosen_colors <- plot3D::jet.col(n = 34, alpha = 1)
chosen_colors <- chosen_colors[3:32]
alt_seq <- c(1, 15, 30, 40, 50, 60)
alt_seq <- round(rescale_f(alt_seq, a = 1, b = length(chosen_colors), e = min(alt_seq), f = max(alt_seq)), 0)
chosen_colors <- chosen_colors[alt_seq][1:n_labs]

levels <- c("<20  per 1,000", "<40  per 1,000", "<60  per 1,000", "<80  per 1,000", "<100 per 1,000", "100+ per 1,000")

yys <- 2021:1998

for (yy in 2021) {
  
  tmp_empirical_birth <- empirical_birth %>% filter(year == yy)
  tmp_empirical_birth <- colombia %>% left_join(y = tmp_empirical_birth, by = "mun") %>% rename(value = birth_rate_disc) %>% dplyr::select(mun, value, geometry)
  tmp_empirical_birth <- tmp_empirical_birth %>% mutate(value = factor(value, levels = levels))
  
  tmp_isl1 <- tmp_empirical_birth %>% filter((mun %in% c(88001)))        
  tmp_isl2 <- tmp_empirical_birth %>% filter((mun %in% c(88564)))        
  tmp_empirical_birth <- tmp_empirical_birth %>% filter(!(mun %in% c(88001, 88564)))
  
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
  
  m_emp_birth <- ggplot() +
    geom_sf(data = tmp_empirical_birth, aes(geometry = geometry, fill = value), color = "black") +
    scale_fill_manual(values = chosen_colors, breaks = levels, drop = FALSE, name = paste("Under-reporting adjusted empirical parental\nfertility rate by municipality (", yy, ")", sep = ""), na.translate = TRUE) + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  m_emp_birth <- m_emp_birth + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  m_emp_birth <- m_emp_birth + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
  m_emp_birth <- m_emp_birth + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  m_emp_birth <- m_emp_birth + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  ggsave(m_emp_birth, file = paste(out.dir, "/Births/adjusted_emp_birth_", yy, ".png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

###############################
# Birth total in parental age #
###############################

birth_total <- tmp_fert %>% rename(mun = loc) %>% dplyr::select(mun, year, population, births) %>% group_by(mun, year) %>% summarise(population = sum(population), births = sum(births)) %>% ungroup()
birth_total <- birth_total %>% mutate(births_disc = ifelse(births == 0,   "0",
                                                    ifelse(births < 100,  "0-100",
                                                    ifelse(births < 200,  "101-200",
                                                    ifelse(births < 300,  "201-300",
                                                    ifelse(births < 400,  "301-400",
                                                    ifelse(births < 500,  "401-500",
                                                    ifelse(births < 750,  "501-750",
                                                    ifelse(births < 1000, "751-1,000",
                                                    ifelse(births < 1500, "1,001-1,500", "1,501-200,000"))))))))))

chosen_colors <- plot3D::jet.col(n = 10, alpha = 1)
levels <- c("0", "0-100", "101-200", "201-300", "301-400", "401-500", "501-750", "751-1,000", "1,001-1,500", "1,501-200,000")

yys <- 2021:1998

for (yy in 2021) {
  
  tmp_birth_total <- birth_total %>% filter(year == yy)
  tmp_birth_total <- colombia %>% left_join(y = tmp_birth_total, by = "mun") %>% rename(value = births_disc) %>% dplyr::select(mun, value, geometry)
  tmp_birth_total <- tmp_birth_total %>% mutate(value = factor(value, levels = levels))

  
  tmp_isl1 <- tmp_birth_total %>% filter((mun %in% c(88001)))        
  tmp_isl2 <- tmp_birth_total %>% filter((mun %in% c(88564)))        
  tmp_birth_total <- tmp_birth_total %>% filter(!(mun %in% c(88001, 88564)))
  
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
  
  m_birth_total <- ggplot() +
    geom_sf(data = tmp_birth_total, aes(geometry = geometry, fill = value), color = "black") +
    scale_fill_manual(values = chosen_colors, breaks = levels, drop = FALSE, name = paste("Total number of births\nin parental ages in ", yy, sep = ""), na.translate = TRUE) + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10"), plot.title = element_text(size = 16), legend.title = element_text(size = 11), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(order = 1))
  
  m_birth_total <- m_birth_total + annotate("text", x = -79, y = 11.8, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  m_birth_total <- m_birth_total + inset_element(i1, left = 0.05, bottom = 0.80, right = 0.100, top = 0.90, align_to = "full") # Municipality `88001`
  m_birth_total <- m_birth_total + inset_element(i2, left = 0.05, bottom = 0.83, right = 0.175, top = 0.88, align_to = "full") # Municipality `88564`
  m_birth_total <- m_birth_total + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  ggsave(m_birth_total, file = paste(out.dir, "/Births/birth_parental_", yy, ".png", sep = ""), width = 2750, height = 2600, units = c("px"), dpi = 300, bg = "white")
}

##################
##################
