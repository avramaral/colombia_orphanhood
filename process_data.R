source("header.R")
source("PROCESS_DATA/aux_process.R")

##################################################
# Set common parameters
##################################################

per1K <- TRUE # It sets (or not) `rates per 1,000 individuals`

type.input <- "Municipality" # c("Municipality", "Department")

save_plots <- TRUE
pseudo_log <- FALSE

##################################################
# Read data
##################################################

##############################
# Paths
##############################

fertility_rate_f_file <- "DATA/Fertility/backup_cp_municipality_fertility_f_complete.csv"
fertility_rate_m_file <- "DATA/Fertility/backup_cp_municipality_fertility_m_complete.csv"

population_locs_file <- "DATA/Population from Census/backup_cp_pop_years_list_municipality.csv"

##############################

if (type.input == "Municipality") {
  
  colombia <- readRDS(file = "DATA/colombia_map_municipality.RDS")
  spatial_obj <- readRDS(file = "DATA/spatial_obj_colombia_mun.RDS")
  
  mortality_count <- read_csv(file = "DATA/Deaths/col_83-21_deaths_municipality.csv")
  fertility_rat_f <- read_csv(file = fertility_rate_f_file) 
  fertility_rat_m <- read_csv(file = fertility_rate_m_file) 
  {
    fertility_rat_f_cp <- fertility_rat_f
    fertility_rat_m_cp <- fertility_rat_m
  }
  fertility_rates <- rbind(fertility_rat_f, fertility_rat_m)
  fertility_rates$fertility_rate <- fertility_rates$fertility_rate / 1000
  population_locs <- read_csv(file = population_locs_file)
  
  ls <- population_locs$Mun %>% unique()
  
  mortality_count <- mortality_count %>% rename(loc = mun)
  fertility_rates <- fertility_rates %>% rename(loc = mun)
  population_locs <- population_locs %>% rename(Loc = Mun)
  
} else if (type.input == "Department") {
  
  colombia <- readRDS(file = "DATA/colombia_map_department.RDS")
  
  mortality_count <- read_csv(file = "../colombia_original_code/Colombia OCAY datasets and files/Deaths/col_83-21_deaths_department.csv")
  fertility_rat_f <- read_csv(file = "../colombia_original_code/Colombia OCAY datasets and files/Fertility/department_fertility_f_complete.csv") 
  fertility_rat_m <- read_csv(file = "../colombia_original_code/Colombia OCAY datasets and files/Fertility/department_fertility_m_complete.csv") 
  fertility_rates <- rbind(fertility_rat_f, fertility_rat_m)
  fertility_rates$fertility_rate <- fertility_rates$fertility_rate / 1000
  population_locs <- read_csv(file = "../colombia_original_code/Colombia OCAY datasets and files/Population from Census/pop_years_list_department.csv")
  
  ls <- population_locs$Dep %>% unique()
  
  mortality_count <- mortality_count %>% rename(loc = dep)
  fertility_rates <- fertility_rates %>% rename(loc = dep)
  population_locs <- population_locs %>% rename(Loc = Dep)
  
} else {
  stop("Invalid `type.input`.")
}

fertility_rates <- fertility_rates %>% mutate(fertility_rate = compute_rate(births, population))

##############################
# Colombia geographic info
##############################

colnames(colombia) <- c("loc", "mun", "geometry")
colombia$loc <- factor(colombia$loc)

if (type.input == "Municipality") { 
  possible_locs <- read_csv(file = "DATA/municipalities_code.csv")
  possible_locs$code <- as.integer(possible_locs$code)
  names(possible_locs) <- c("code", "loc")
  possible_locs <- possible_locs %>% arrange(code)
} else if (type.input == "Department") {
  possible_locs <- as_tibble(data.frame(code = c(5, 8, 11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73, 76, 81, 85, 86, 88, 91, 94, 95, 97, 99), loc = c("Antioquia", "Atl\u00E1ntico", "Bogot\u00E1", "Bol\u00EDvar", "Boyac\u00E1", "Caldas", "Caquet\u00E1", "Cauca", "Cesar", "C\u00F3rdoba", "Cundinamarca", "Choc\u00F3", "Huila", "La Guajira", "Magdalena", "Meta", "Nari\u00F1o", "Norte de Santander", "Quind\u00EDo", "Risaralda", "Santander", "Sucre", "Tolima", "Valle del Cauca", "Arauca", "Casanare", "Putumayo", "Archipelago de San Andr\u00E9s, Providencia y Santa Catalina", "Amazonas", "Guain\u00EDa", "Guaviare", "Vaup\u00E9s", "Vichada")))
} else {
  stop("Invalid `type.input`.")
}

##################################################
# Filter data
##################################################

# From (2015 - 17) to 2021
year_mn <- 1998 # 1998 or 2006
year_mx <- 2021 # 2005 or 2021
mortality_count <- mortality_count %>% filter(year >= year_mn, year <= year_mx)
fertility_rates <- fertility_rates %>% filter(year >= year_mn, year <= year_mx)
population_locs <- population_locs %>% filter(Year >= year_mn, Year <= year_mx)

##########
# Exclude Men 77+ and Women 67+
##########
prop_65_66_mun_fem <- read_csv("DATA/Population from Census/pop_65_66_mun_fem.csv")
prop_75_76_mun_mal <- read_csv("DATA/Population from Census/pop_75_76_mun_mal.csv")
mortality_count_cp_f <- mortality_count %>% filter(gender == "Female")
mortality_count_cp_m <- mortality_count %>% filter(gender == "Male"  )

mortality_count_cp_f <- mortality_count_cp_f %>% filter(!(age %in% c("70-74", "75-79", "80-84", "85+")))
mortality_count_cp_m <- mortality_count_cp_m %>% filter(!(age %in% c("80-84", "85+")))

mortality_count_cp_f <- mortality_count_cp_f %>% left_join(y = rename(prop_65_66_mun_fem, loc = mun), by = "loc")
mortality_count_cp_m <- mortality_count_cp_m %>% left_join(y = rename(prop_75_76_mun_mal, loc = mun), by = "loc")

mortality_count_cp_f <- mortality_count_cp_f %>% mutate(deaths = ifelse(age == "65-69", ceiling(deaths * pop_65_66_prop_female), deaths)) %>% select(-pop_65_66_prop_female)
mortality_count_cp_m <- mortality_count_cp_m %>% mutate(deaths = ifelse(age == "75-79", ceiling(deaths * pop_75_76_prop_male  ), deaths)) %>% select(-pop_75_76_prop_male  )

mortality_count_cp <- bind_rows(mortality_count_cp_f, mortality_count_cp_m) %>% arrange(gender, loc, year)
mortality_count <- mortality_count_cp
##########
##########
mortality_count <- mortality_count %>% mutate(age = ifelse(gender == "Male"   & age %in% c("55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"), "55+", age))
mortality_count <- mortality_count %>% mutate(age = ifelse(gender == "Female" & age %in% c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"), "50+", age))
mortality_count <- mortality_count %>% group_by(gender, loc, year, age) %>% summarize(deaths = sum(deaths)) %>% ungroup()
mortality_count <- mortality_count %>% filter(loc %in% ls)
mortality_count <- mortality_count %>% arrange(gender, loc, year, age)

population_locs <- population_locs %>% filter(Age %in% unique(mortality_count$age))
population_locs <- population_locs %>% select(gender = Sex, loc = Loc, year = Year, age = Age, population = Population)
population_locs <- population_locs %>% arrange(gender, loc, year, age)

if (type.input == "Municipality") { # Include back the impossible negative population values, so we can treat them later
  neg_pop <- read_csv(file = "PROCESS_DATA/AUX_DATA/removing_negative.csv")
  neg_pop <- neg_pop %>% mutate(gender = Sex, loc = Code, year = Year, age = Age, population = `Old Population`) %>%  select(gender, loc, year, age, population)
  tmp_pop <- left_join(x = population_locs, y = neg_pop, by = c("gender", "loc", "year", "age"))
  tmp_pop <- tmp_pop %>% mutate(population.x = ifelse(!is.na(population.y), population.y, population.x)) %>% mutate(population = population.x) %>% select(-c(population.x, population.y))
  population_locs <- tmp_pop
}
population_locs$loc <- factor(population_locs$loc)

# Compute rates

mortality_count$loc <- factor(mortality_count$loc) # Locations should be a factor
mortality_rates <- right_join(mortality_count, population_locs, by = c("gender", "loc", "year", "age")) %>% mutate_if(is.numeric, coalesce, 0) # Replace NA by 0
mortality_rates <- mortality_rates %>% mutate(death_rate = compute_rate(deaths, population))

fertility_rates$loc <- factor(fertility_rates$loc) # Locations should be a factor
fertility_rates <- fertility_rates %>% select(gender, loc, year, age, births, fertility_rate) %>% arrange(gender, loc, year, age)
fertility_rates <- left_join(fertility_rates, population_locs, by = c("gender", "loc", "year", "age")) 
fertility_rates <- fertility_rates %>% mutate(fertility_rate = compute_rate(births, population))

if (per1K) {
  mortality_rates$death_rate <- mortality_rates$death_rate * 1000
  fertility_rates$fertility_rate <- fertility_rates$fertility_rate * 1000
}

##################################################
# Data pre-processing
##################################################

fertility_rates$mod <- 0
mortality_rates$mod <- 0

##################################################
saveRDS(object = list(mortality_rates = mortality_rates, fertility_rates = fertility_rates), file = "PROCESS_DATA/ORIGINAL_RATES.RDS")
saveRDS(object = population_locs, file = "PROCESS_DATA/OLD_POPULATION.RDS")
##################################################

##############################
# Negative populations
##############################

if (type.input == "Municipality") {
  # Exclude the inputs with negative population that were corrected before
  # This file was originally generated for the report, and it was located in `REPORT/data/`
  
  neg_pop <- read_csv(file = "PROCESS_DATA/AUX_DATA/removing_negative.csv")
  neg_pop <- neg_pop %>% mutate(gender = Sex, loc = Code, year = Year, age = Age) 
  
  fertility_rates <- set_to_NA(data = fertility_rates, tbexcluded = neg_pop, rate = "fertility_rate", mod = 1)
  mortality_rates <- set_to_NA(data = mortality_rates, tbexcluded = neg_pop, rate = "death_rate", mod = 1)

}
    
    ##################################################
    # Imputation for missing data 
    ##################################################

    ##############################
    # Negative populations
    ##############################
    
    # Mortality
    mor_imp_neg_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/negative_population_mortality.RDS", sep = "")
    if (file.exists(mor_imp_neg_pop_file)) {
      mortality_rates <- readRDS(file = mor_imp_neg_pop_file)
    } else {
      # ~5 minutes
      mortality_rates <- imputation_negative_population(data = mortality_rates, per1K = per1K)
      saveRDS(object = mortality_rates, file = mor_imp_neg_pop_file)
    }
    
    # Fertility
    fer_imp_neg_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/negative_population_fertility.RDS", sep = "")
    if (file.exists(fer_imp_neg_pop_file)) {
      fertility_rates <- readRDS(file = fer_imp_neg_pop_file)
    } else {
      # ~5 minutes
      fertility_rates <- imputation_negative_population(data = fertility_rates, per1K = per1K)
      saveRDS(object = fertility_rates, file = fer_imp_neg_pop_file)
    }
    
    # Update population data
    population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)
    saveRDS(object = population_locs, file = "DATA/Population from Census/updated_negative_pop_list_municipality.csv")

##############################
# IMPOSSIBLE rates
##############################

###############
# Mortality
###############

imp_mor <- identify_impossible_mortality_rates(mortality_rates, per1K)

mortality_rates <- set_to_NA(data = mortality_rates, tbexcluded = imp_mor, rate = "death_rate", mod = 2)

###############
# Fertility
###############

imp_fer <- identify_impossible_fertility_rates(fertility_rates)

fertility_rates <- set_to_NA(data = fertility_rates, tbexcluded = imp_fer, rate = "fertility_rate", mod = 2)

    ##################################################
    # Imputation for missing data 
    ##################################################
    
    ##############################
    # Impossible rates
    ##############################
    
    mortality_rates <- imputation_impossible_mortality_rates(mortality_rates = mortality_rates, population_locs = population_locs, per1K = per1K, mods = c(2))
    fertility_rates <- imputation_impossible_fertility_rates(fertility_rates = fertility_rates, population_locs = population_locs, per1K = per1K, mods = c(2), max_children = 999)
    
    # Update population data
    population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)
    
    ##################################################
    saveRDS(object = list(mortality_rates = mortality_rates, fertility_rates = fertility_rates), file = "PROCESS_DATA/AFTER_IMPOSSIBLE_RATES.RDS")
    ##################################################


##############################
# UNLIKELY rates
##############################

initial_year <- 2005 
final_year   <- 2018 
n_sd <- 3

use_limits <- TRUE # For imputation

###############
# Mortality
###############

unl_mor_tmp_file <- paste("PROCESS_DATA/AUX_DATA/unlikely_mortality_", initial_year, "_", final_year, "_", n_sd, ".RDS", sep = "")
if (file.exists(unl_mor_tmp_file)) {
  unl_mor_tmp <- readRDS(file = unl_mor_tmp_file)
} else {
  # ~20 minutes
  unl_mor_tmp <- identify_unlikely_mortality_rates(mortality_rates, initial_year, final_year, n_sd)
  saveRDS(object = unl_mor_tmp, file = unl_mor_tmp_file)
}

unl_mor <- unl_mor_tmp$unl_mor

mortality_rates <- set_to_NA(data = mortality_rates, tbexcluded = unl_mor, rate = "death_rate", mod = 3)

###############
# Fertility
###############

unl_fer_tmp_file <- paste("PROCESS_DATA/AUX_DATA/unlikely_fertility_", initial_year, "_", final_year, "_", n_sd, ".RDS", sep = "")
if (file.exists(unl_fer_tmp_file)) {
  unl_fer_tmp <- readRDS(file = unl_fer_tmp_file)
} else {
  # ~20 minutes
  unl_fer_tmp <- identify_unlikely_fertility_rates(fertility_rates, initial_year, final_year, n_sd)
  saveRDS(object = unl_fer_tmp, file = unl_fer_tmp_file)
}

unl_fer <- unl_fer_tmp$unl_fer

fertility_rates <- set_to_NA(data = fertility_rates, tbexcluded = unl_fer, rate = "fertility_rate", mod = 3)

    ##################################################
    # Imputation for missing data 
    ##################################################
    
    ##############################
    # Unlikely rates
    ##############################
    
    mortality_ref <- unl_mor_tmp$mortality_ref
    fertility_ref <- unl_fer_tmp$fertility_ref
    
    # Mortality
    mor_unl_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/unlikely_mortality_use_limits_", use_limits, ".RDS", sep = "")
    if (file.exists(mor_unl_pop_file)) {
      mortality_rates <- readRDS(file = mor_unl_pop_file)
    } else {
      # ~15 minutes
      mortality_rates <- imputation_unlikely_mortality_rates(mortality_rates = mortality_rates, mortality_ref = mortality_ref, population_locs = population_locs, per1K = per1K, use_limits = use_limits)
      saveRDS(object = mortality_rates, file = mor_unl_pop_file)
    }
    
    # Fertility
    fer_unl_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/unlikely_fertility_use_limits_", use_limits, ".RDS", sep = "")
    if (file.exists(fer_unl_pop_file)) {
      fertility_rates <- readRDS(file = fer_unl_pop_file)
    } else {
      # ~15 minutes
      fertility_rates <- imputation_unlikely_fertility_rates(fertility_rates = fertility_rates, fertility_ref = fertility_ref, population_locs = population_locs, per1K = per1K, use_limits = use_limits)
      saveRDS(object = fertility_rates, file = fer_unl_pop_file)
    }
    
    # Update population data
    population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)

##############################
# Spatial outliers
##############################

n_sd <- 3
mx_lag <- 1 # only applies if `identify_spatial_local = TRUE`

identify_spatial_with_summary <- TRUE # If `TRUE`, identify outliers based on the `X_rate_mean` from summary for all years; if `FALSE`, identify outliers based on the summary for each year
identify_spatial_local <- FALSE # Local (or global) analysis; only implemented if `identify_spatial_with_summary = TRUE`. The standard is `TRUE`, i.e., locally

####################
# Summary
####################

# Mortality
mortality_rates_summ <- mortality_rates %>% 
                          select(-population) %>% 
                          group_by(gender, loc, age) %>% 
                          summarize(deaths_mean = mean(deaths, na.rm = TRUE), deaths_var = var(deaths, na.rm = TRUE), death_rate_mean = mean(death_rate, na.rm = TRUE), death_rate_var = var(death_rate, na.rm = TRUE)) %>% 
                          ungroup()

mortality_rates_summ <- mortality_rates_summ %>% mutate(deaths_sd = sqrt(deaths_var), death_rate_sd = sqrt(death_rate_var)) %>% select(c(1:5, 8, 6:7, 9))
mortality_rates_summ <- mortality_rates_summ %>% mutate(age = ifelse(age %in% c("50+", "50-54"), "50+/50-54", age))

# Fertility
fertility_rates_summ <- fertility_rates %>% 
                          select(-population) %>% 
                          group_by(gender, loc, age) %>% 
                          summarize(births_mean = mean(births, na.rm = TRUE), births_var = var(births, na.rm = TRUE), fertility_rate_mean = mean(fertility_rate, na.rm = TRUE), fertility_rate_var = var(fertility_rate, na.rm = TRUE)) %>% 
                          ungroup()

fertility_rates_summ <- fertility_rates_summ %>% mutate(births_sd = sqrt(births_var), fertility_rate_sd = sqrt(fertility_rate_var)) %>% select(c(1:5, 8, 6:7, 9))

###############
# Mortality
###############

spa_mor_tmp_file <- paste("PROCESS_DATA/AUX_DATA/spatial_mortality_", initial_year, "_", final_year, "_", n_sd, "_summary_", identify_spatial_with_summary, "_local_", identify_spatial_local, ".RDS", sep = "")
if (file.exists(spa_mor_tmp_file)) {
  spa_mor_tmp <- readRDS(file = spa_mor_tmp_file)
} else {
  # ~10 minutes, if not `summary`
  if (identify_spatial_with_summary) {
    spa_mor_tmp <- identify_spatial_outliers_summary(data = mortality_rates_summ, sub_data = mortality_rates, variable = "death_rate_mean", sub_variable = "death_rate", sub_count = "deaths", n_sd = n_sd, mx_lag = mx_lag, local = identify_spatial_local)
  } else {
    spa_mor_tmp <- identify_spatial_outliers_independent(data = mortality_rates, variable = "death_rate", n_sd = n_sd, mx_lag = mx_lag, local = identify_spatial_local)
  }
  saveRDS(object = spa_mor_tmp, file = spa_mor_tmp_file)
}

if (identify_spatial_with_summary) {
  tmp_mortality_rates <- mortality_rates %>% filter(is.na(death_rate)) %>% select(gender, loc, year, age) %>% mutate(excluded = TRUE)
  spa_mor <- spa_mor_tmp$spa_out
  spa_mor <- left_join(x = spa_mor, y = tmp_mortality_rates, by = c("gender", "loc", "year", "age"))
  spa_mor <- spa_mor %>% filter(is.na(excluded)) %>% select(-excluded)
} else {
  spa_mor <- spa_mor_tmp$spa_out
}

mortality_rates <- set_to_NA(data = mortality_rates, tbexcluded = spa_mor, rate = "death_rate", mod = 4)

###############
# Fertility
###############

spa_fer_tmp_file <- paste("PROCESS_DATA/AUX_DATA/spatial_fertility_", initial_year, "_", final_year, "_", n_sd, "_summary_", identify_spatial_with_summary, "_local_", identify_spatial_local, ".RDS", sep = "")
if (file.exists(spa_fer_tmp_file)) {
  spa_fer_tmp <- readRDS(file = spa_fer_tmp_file)
} else {
  # ~10 minutes, if not `summary`
  if (identify_spatial_with_summary) {
    spa_fer_tmp <- identify_spatial_outliers_summary(data = fertility_rates_summ, sub_data = fertility_rates, variable = "fertility_rate_mean", sub_variable = "fertility_rate", sub_count = "births", n_sd = n_sd, mx_lag = mx_lag, local = identify_spatial_local)
  } else {
    spa_fer_tmp <- identify_spatial_outliers_independent(data = fertility_rates, variable = "fertility_rate", n_sd = n_sd, mx_lag = mx_lag, local = identify_spatial_local)
  }
  saveRDS(object = spa_fer_tmp, file = spa_fer_tmp_file)
}

if (identify_spatial_with_summary) {
  tmp_fertility_rates <- fertility_rates %>% filter(is.na(fertility_rate)) %>% select(gender, loc, year, age) %>% mutate(excluded = TRUE)
  spa_fer <- spa_fer_tmp$spa_out
  spa_fer <- left_join(x = spa_fer, y = tmp_fertility_rates, by = c("gender", "loc", "year", "age"))
  spa_fer <- spa_fer %>% filter(is.na(excluded)) %>% select(-excluded)
} else {
  spa_fer <- spa_fer_tmp$spa_out
}

fertility_rates <- set_to_NA(data = fertility_rates, tbexcluded = spa_fer, rate = "fertility_rate", mod = 4)

    ##################################################
    # Imputation for missing data 
    ##################################################
    
    ##############################
    # Spatial outliers
    ##############################

    mx_lag <- 1
    
    # Mortality
    mor_spt_out_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/spatial_outliers_mortality.RDS", sep = "")
    if (file.exists(mor_spt_out_file)) {
      mortality_rates <- readRDS(file = mor_spt_out_file)
    } else {
      # ~10 minutes
      mortality_rates <- imputation_spatial_outliers(data = mortality_rates, variable = "death_rate", per1K = per1K, mx_lag = mx_lag, mm = 4)
      saveRDS(object = mortality_rates, file = mor_spt_out_file)
    }
    
    # Fertility
    fer_spt_out_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/spatial_outliers_fertility.RDS", sep = "")
    if (file.exists(fer_spt_out_file)) {
      fertility_rates <- readRDS(file = fer_spt_out_file)
    } else {
      # ~10 minutes
      fertility_rates <- imputation_spatial_outliers(data = fertility_rates, variable = "fertility_rate", per1K = per1K, mx_lag = mx_lag, mm = 4)
      saveRDS(object = fertility_rates, file = fer_spt_out_file)
    }
    
    # Update population data
    population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)

# TEMPORARY #
##################################################
saveRDS(object = list(mortality_rates = mortality_rates, fertility_rates = fertility_rates), file = "PROCESS_DATA/FINAL_RATES.RDS")
saveRDS(object = population_locs, file = "PROCESS_DATA/FINAL_POPULATION.RDS")
##################################################
    
##############################
# Equalize populations
##############################
  
if (TRUE) { # TRUE
  mortality_rates <- left_join(x = mortality_rates, y = population_locs, by = c("gender", "loc", "year", "age"))
  new_idx <- which(mortality_rates$population.x != mortality_rates$population.y)
  mortality_rates[new_idx, "mod"] <- 5 
  mortality_rates <- mortality_rates %>% mutate(population = population.y) %>% select(-c(population.x, population.y)) %>% select(gender, loc, year, age, deaths, population, death_rate, mod)
  mortality_rates <- mortality_rates %>% mutate(death_rate = compute_rate(count = mortality_rates$deaths, pop = mortality_rates$population) * ifelse(per1K, 1000, 1))
  
  fertility_rates <- left_join(x = fertility_rates, y = population_locs, by = c("gender", "loc", "year", "age"))
  new_idx <- which(fertility_rates$population.x != fertility_rates$population.y)
  fertility_rates[new_idx, "mod"] <- 5 
  fertility_rates <- fertility_rates %>% mutate(population = population.y) %>% select(-c(population.x, population.y)) %>% select(gender, loc, year, age, births, population, fertility_rate, mod)
  fertility_rates <- fertility_rates %>% mutate(fertility_rate = compute_rate(count = fertility_rates$births, pop = fertility_rates$population) * ifelse(per1K, 1000, 1))
}

saveRDS(object = list(mortality_rates = mortality_rates, fertility_rates = fertility_rates), file = "PROCESS_DATA/EQUALIZED_RATES.RDS")
saveRDS(object = population_locs, file = "PROCESS_DATA/EQUALIZED_POPULATION.RDS")

# ####################
# # Make `mortality` and `fertility` comparable
# ####################
# 
# mor_NA <- mortality_rates %>% filter(is.na(population)) %>% select(gender, loc, year, age) %>% mutate(category = "mortality")
# fer_NA <- fertility_rates %>% filter(is.na(population)) %>% select(gender, loc, year, age) %>% mutate(category = "fertility")
# 
# all_NA <- full_join(x = mor_NA, y = fer_NA, by = c("gender", "loc", "year", "age"))
# 
# mor_NA <- all_NA %>% filter(is.na(category.x)) %>% select(-c(category.x, category.y))
# fer_NA <- all_NA %>% filter(is.na(category.y)) %>% select(-c(category.x, category.y))
# 
# mortality_rates <- set_to_NA(data = mortality_rates, tbexcluded = mor_NA, rate = "death_rate",     mod = 5)
# fertility_rates <- set_to_NA(data = fertility_rates, tbexcluded = fer_NA, rate = "fertility_rate", mod = 5)
# 
# ##################################################
# saveRDS(object = list(mortality_rates = mortality_rates, fertility_rates = fertility_rates), file = "PROCESS_DATA/EMPTY_RATES.RDS")
# ##################################################



##################################################
# Imputation for missing data 
##################################################

##############################
# Negative populations
##############################

# # Mortality
# mor_imp_neg_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/negative_population_mortality.RDS", sep = "")
# if (file.exists(mor_imp_neg_pop_file)) {
#   mortality_rates <- readRDS(file = mor_imp_neg_pop_file)
# } else {
#   # ~5 minutes
#   mortality_rates <- imputation_negative_population(data = mortality_rates, per1K = per1K)
#   saveRDS(object = mortality_rates, file = mor_imp_neg_pop_file)
# }
# 
# # Fertility
# fer_imp_neg_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/negative_population_fertility.RDS", sep = "")
# if (file.exists(fer_imp_neg_pop_file)) {
#   fertility_rates <- readRDS(file = fer_imp_neg_pop_file)
# } else {
#   # ~5 minutes
#   fertility_rates <- imputation_negative_population(data = fertility_rates, per1K = per1K)
#   saveRDS(object = fertility_rates, file = fer_imp_neg_pop_file)
# }
# 
# # Update population data
# population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)

##############################
# IMPOSSIBLE rates
##############################

# mortality_rates <- imputation_impossible_mortality_rates(mortality_rates = mortality_rates, population_locs = population_locs, per1K = per1K, mods = c(2))
# fertility_rates <- imputation_impossible_fertility_rates(fertility_rates = fertility_rates, population_locs = population_locs, per1K = per1K, mods = c(2), max_children = 999)
# 
# # Update population data
# population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)

# ##############################
# # UNLIKELY rates
# ##############################
# 
# mortality_ref <- unl_mor_tmp$mortality_ref
# fertility_ref <- unl_fer_tmp$fertility_ref
# 
# # Mortality
# mor_unl_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/unlikely_mortality.RDS", sep = "")
# if (file.exists(mor_unl_pop_file)) {
#   mortality_rates <- readRDS(file = mor_unl_pop_file)
# } else {
#   # ~15 minutes
#   mortality_rates <- imputation_unlikely_mortality_rates(mortality_rates = mortality_rates, mortality_ref = mortality_ref, per1K = per1K)
#   saveRDS(object = mortality_rates, file = mor_unl_pop_file)
# }
# 
# # Fertility
# fer_unl_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/unlikely_fertility.RDS", sep = "")
# if (file.exists(fer_unl_pop_file)) {
#   fertility_rates <- readRDS(file = fer_unl_pop_file)
# } else {
#   # ~15 minutes
#   fertility_rates <- imputation_unlikely_fertility_rates(fertility_rates = fertility_rates, fertility_ref = fertility_ref, per1K = per1K)
#   saveRDS(object = fertility_rates, file = fer_unl_pop_file)
# }
# 
# # Update population data
# population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)
# 
# ##############################
# # Spatial outliers
# ##############################
# 
# mx_lag <- 2
# 
# # Mortality
# mor_spt_out_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/spatial_outliers_mortality.RDS", sep = "")
# if (file.exists(mor_spt_out_file)) {
#   mortality_rates <- readRDS(file = mor_spt_out_file)
# } else {
#   # ~10 minutes
#   mortality_rates <- imputation_spatial_outliers(data = mortality_rates, variable = "death_rate", per1K = per1K, mx_lag = mx_lag, mm = 4)
#   saveRDS(object = mortality_rates, file = mor_spt_out_file)
# }
# 
# # Fertility
# fer_spt_out_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/spatial_outliers_fertility.RDS", sep = "")
# if (file.exists(fer_spt_out_file)) {
#   fertility_rates <- readRDS(file = fer_spt_out_file)
# } else {
#   # ~10 minutes
#   fertility_rates <- imputation_spatial_outliers(data = fertility_rates, variable = "fertility_rate", per1K = per1K, mx_lag = mx_lag, mm = 4)
#   saveRDS(object = fertility_rates, file = fer_spt_out_file)
# }
# 
# # Update population data
# population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)
# 
# ##############################
# # Common population
# ##############################
# 
# common_pop_file <- paste("PROCESS_DATA/AUX_DATA/IMPUTATION/common_population.RDS", sep = "")
# if (file.exists(common_pop_file)) {
#   common_population <- readRDS(file = common_pop_file)
# } else {
#   # ~40 minutes
#   common_population <- imputation_common_population(mortality_rates, fertility_rates, population_locs, per1K)
#   saveRDS(object = common_population, file = common_pop_file)
# }
# 
# mortality_rates <- common_population$mortality_rates
# fertility_rates <- common_population$fertility_rates
# population_locs <- common_population$population_locs
# 
# # Final adjusts (Manual)
# 
# mortality_rates <- mortality_rates %>% mutate(population = ifelse((deaths >= 1) & (population == 0), deaths, population))
# mortality_rates <- mortality_rates %>% mutate(population = ceiling(population))
# fertility_rates <- fertility_rates %>% mutate(population = ifelse((births >= 1) & (population == 0), births, population))
# fertility_rates <- fertility_rates %>% mutate(population = ceiling(population))
# mortality_rates <- mortality_rates %>% mutate(death_rate     = compute_rate(count = deaths, pop = population) * ifelse(per1K, 1000, 1))
# fertility_rates <- fertility_rates %>% mutate(fertility_rate = compute_rate(count = births, pop = population) * ifelse(per1K, 1000, 1))
# population_locs <- update_pop(mortality_rates, fertility_rates, population_locs, per1K)
# 
# ##################################################
# saveRDS(object = list(mortality_rates = mortality_rates, fertility_rates = fertility_rates), file = "PROCESS_DATA/FINAL_RATES.RDS")
# saveRDS(object = population_locs, file = "PROCESS_DATA/FINAL_POPULATION.RDS")
# ##################################################
# final_rates     <- readRDS(file = "PROCESS_DATA/FINAL_RATES.RDS")
# population_locs <- readRDS(file = "PROCESS_DATA/FINAL_POPULATION.RDS")
# mortality_rates <- final_rates$mortality_rates
# fertility_rates <- final_rates$fertility_rates
# ##################################################

  

# ##################################################
# ##################################################
# ##################################################
# # PLOTS
# ##################################################
# ##################################################
# ##################################################
# 
# extension_name <- ""
# if (FALSE) { # Plot original data
#   extension_name <- "_original_data"
#   orig_data <- readRDS(file = "PROCESS_DATA/ORIGINAL_RATES.RDS")
#   fertility_rates <- orig_data$fertility_rates
#   mortality_rates <- orig_data$mortality_rates
#   population_locs <- readRDS(file = "PROCESS_DATA/OLD_POPULATION.RDS")
# }
# 
# ##################################################
# # Aggregated by gender and age group
# ##################################################
# 
# mortality_rates_agg <- mortality_rates %>% select(-death_rate) %>% group_by(loc, year) %>% summarize(total_death = sum(deaths), total_pop = sum(population)) %>% ungroup()
# mortality_rates_agg <- mortality_rates_agg %>% mutate(death_rate = compute_rate(total_death, total_pop))
# if (per1K) { mortality_rates_agg$death_rate <- mortality_rates_agg$death_rate * 1000 }
# 
# fertility_rates_agg <- fertility_rates %>% select(-fertility_rate) %>% group_by(loc, year) %>% summarize(total_birth = sum(births), total_pop = sum(population)) %>% ungroup()
# fertility_rates_agg <- fertility_rates_agg %>% mutate(fertility_rate = compute_rate(total_birth, total_pop))
# if (per1K) { fertility_rates_agg$fertility_rate <- fertility_rates_agg$fertility_rate * 1000 }
# 
# ss <- 40
# selected_ls <- sample(ls, size = min(length(ls), ss))
# if (FALSE) { saveRDS(object = selected_ls, file = "PROCESS_DATA/locs_line_plots.RDS") } else { selected_ls <- readRDS(file = "PROCESS_DATA/locs_line_plots.RDS") }
# 
# filtered_mortality_rates_agg <- mortality_rates_agg %>% filter(loc %in% selected_ls)
# filtered_fertility_rates_agg <- fertility_rates_agg %>% filter(loc %in% selected_ls)
# 
# plot_time_series <- function (data, count, rate, tt = "", per1K = FALSE, ...) {
#   
#   type <- ifelse(grepl("death", count, ignore.case = TRUE), "Death", "Fertility") 
#   
#   scaling_txt <- ifelse(per1K, " (per 1,000 individuals)", "")
#   
#   p1 <- ggplot(data = data) +
#     geom_line(mapping = aes(x = year, y = .data[[count]], color = loc)) +
#     labs(color = "Location", x = "Year", y = paste(type, " count", sep = "")) + 
#     theme_bw() +
#     theme(text = element_text(size = 12, family = "LM Roman 10"))
#   
#   p2 <- ggplot(data = data) +
#     geom_line(mapping = aes(x = year, y = .data[[rate]], color = loc)) +
#     labs(color = "Location", x = "Year", y = paste(type, " rate", scaling_txt, sep = "")) + 
#     theme_bw() +
#     theme(text = element_text(size = 12, family = "LM Roman 10"))
#   
#   p_total <- p1 / p2 + plot_layout(guides = "collect") & theme(legend.position = "right")
#   p_total <- p_total + plot_annotation(title = tt) & theme(plot.title = element_text(size = 12, hjust = 0.5, family = "LM Roman 10"))
#   
#   p_total
# }
# 
# ts_death <- plot_time_series(filtered_mortality_rates_agg, "total_death", "death_rate", type.input, per1K)
# ts_birth <- plot_time_series(filtered_fertility_rates_agg, "total_birth", "fertility_rate", type.input, per1K)
# 
# if (save_plots) {
#   ggsave(filename = paste("PROCESS_DATA/IMAGES/agg_time_series_death_", type.input, "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = ts_death, width = 2000, height = 2500, units = c("px"), dpi = 300, bg = "white")
#   ggsave(filename = paste("PROCESS_DATA/IMAGES/agg_time_series_birth_", type.input, "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = ts_birth, width = 2000, height = 2500, units = c("px"), dpi = 300, bg = "white")
# }
# 
# ##################################################
# # Stratified analyses
# ##################################################
# 
# poss_genders <- c("Male", "Female"); i <- 1
# selected_age <- "25-29"
# 
# for (i in 1:length(poss_genders)) {
#   
#   selected_gender <- poss_genders[i]
#   
#   filtered_mortality_rates <- mortality_rates %>% filter(loc %in% selected_ls)
#   filtered_mortality_rates <- filtered_mortality_rates %>% filter(gender %in% selected_gender, age %in% selected_age)
#   
#   filtered_fertility_rates <- fertility_rates %>% filter(loc %in% selected_ls)
#   filtered_fertility_rates <- filtered_fertility_rates %>% filter(gender %in% selected_gender, age %in% selected_age)
#   
#   ts_death_particular <- plot_time_series(filtered_mortality_rates, "deaths", "death_rate", paste(selected_gender, " (", selected_age, "), ", type.input, sep = ""), per1K)
#   ts_birth_particular <- plot_time_series(filtered_fertility_rates, "births", "fertility_rate", paste(selected_gender, " (", selected_age, "), ", type.input, sep = ""), per1K)
#   
#   if (save_plots) {
#     ggsave(filename = paste("PROCESS_DATA/IMAGES/time_series_death_", selected_gender, "_", selected_age, "_", type.input, "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = ts_death_particular, width = 2000, height = 2500, units = c("px"), dpi = 300, bg = "white") 
#     ggsave(filename = paste("PROCESS_DATA/IMAGES/time_series_birth_", selected_gender, "_", selected_age, "_", type.input, "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = ts_birth_particular, width = 2000, height = 2500, units = c("px"), dpi = 300, bg = "white") 
#   }
# }
# 
# ##################################################
# # Compute mean and variance
# ##################################################
# 
# # Mortality
# mortality_rates_summ <- mortality_rates %>% select(-population) %>% group_by(gender, loc, age) %>% summarize(deaths_mean = mean(deaths, na.rm = TRUE), deaths_var = var(deaths, na.rm = TRUE), death_rate_mean = mean(death_rate, na.rm = TRUE), death_rate_var = var(death_rate, na.rm = TRUE)) %>% ungroup()
# mort_rates_summ_filt <- mortality_rates %>% filter(population > 10) %>% 
#                                               select(-population) %>% 
#                                               group_by(gender, loc, age) %>% 
#                                               summarize(deaths_mean = mean(deaths, na.rm = TRUE), deaths_var = var(deaths, na.rm = TRUE), death_rate_mean = mean(death_rate, na.rm = TRUE), death_rate_var = var(death_rate, na.rm = TRUE)) %>% 
#                                               ungroup()
# mortality_rates_summ <- left_join(x = mortality_rates_summ, y = mort_rates_summ_filt, by = c("gender", "loc", "age")) %>% mutate(deaths_mean = deaths_mean.y, deaths_var = deaths_var.y, death_rate_mean = death_rate_mean.y, death_rate_var = death_rate_var.y) %>% select(gender, loc, age, deaths_mean, deaths_var, death_rate_mean, death_rate_var) %>% mutate_if(is.numeric, coalesce, 0)
# 
# mortality_rates_summ <- mortality_rates_summ %>% mutate(deaths_sd = sqrt(deaths_var), death_rate_sd = sqrt(death_rate_var)) %>% select(c(1:5, 8, 6:7, 9))
# mortality_rates_summ <- mortality_rates_summ %>% mutate(age = ifelse(age %in% c("50+", "50-54"), "50+/50-54", age))
# 
# # Fertility
# fertility_rates_summ <- fertility_rates %>% select(-population) %>% group_by(gender, loc, age) %>% summarize(births_mean = mean(births, na.rm = TRUE), births_var = var(births, na.rm = TRUE), fertility_rate_mean = mean(fertility_rate, na.rm = TRUE), fertility_rate_var = var(fertility_rate, na.rm = TRUE)) %>% ungroup()
# fert_rates_summ_filt <- fertility_rates %>% filter(population > 10) %>% 
#                                               select(-population) %>% 
#                                               group_by(gender, loc, age) %>% 
#                                               summarize(births_mean = mean(births, na.rm = TRUE), births_var = var(births, na.rm = TRUE), fertility_rate_mean = mean(fertility_rate, na.rm = TRUE), fertility_rate_var = var(fertility_rate, na.rm = TRUE)) %>% 
#                                               ungroup()
# fertility_rates_summ <- left_join(x = fertility_rates_summ, y = fert_rates_summ_filt, by = c("gender", "loc", "age")) %>% mutate(births_mean = births_mean.y, births_var = births_var.y, fertility_rate_mean = fertility_rate_mean.y, fertility_rate_var = fertility_rate_var.y) %>% select(gender, loc, age, births_mean, births_var, fertility_rate_mean, fertility_rate_var) %>% mutate_if(is.numeric, coalesce, 0)
# 
# 
# fertility_rates_summ <- fertility_rates_summ %>% mutate(births_sd = sqrt(births_var), fertility_rate_sd = sqrt(fertility_rate_var)) %>% select(c(1:5, 8, 6:7, 9))
# 
# ##################################################
# # Plotting maps
# ##################################################
# 
# possible_strata <- function (d, ...) {
#   
#   genders <- unique(d$gender)
#   ages <- unique(d$age)
#   
#   opts <- expand.grid(genders, ages)
#   colnames(opts) <- c("Gender", "Age")
#   
#   opts
# }
# 
# # https://win-vector.com/2012/03/01/modeling-trick-the-signed-pseudo-logarithm/
# pseudo_log10_trans <- function (x) { scales::pseudo_log_trans(sigma = 1, base = 10) } # asinh(x / (2 * sigma)) / log(base)
# 
# plot_map_colombia <- function (data, var, nn = "", tt = "", ll = NA, pseudo_log = FALSE, per1K = FALSE, ...) {
#   
#   if (sum(is.na(data))) { m <- ggplot() + theme_bw() } else { # If there is NA, it means there is no data.
#     
#     if (is.na(ll)) {
#       if (per1K) {
#         mn <- round(min(data[[var]]) - 0.50, 0) 
#         mx <- round(max(data[[var]]) + 0.50, 0) 
#       } else {
#         mn <- round(min(data[[var]]) - 0.01, 2)
#         mx <- round(max(data[[var]]) + 0.01, 2)
#       }
#       mn <- max(mn, 0)
#     } else { mn <- ll[1]; mx <- ll[2] }
#     
#     pal <- jet.col(n = 100, alpha = 1)
#     labs <- seq(mn, mx, length.out = 6)
#     
#     if (pseudo_log) { nn <- paste(nn, "\n(pseudo-log)", sep = "") }
#     
#     m <- ggplot(data = data, aes(geometry = geometry)) + 
#       geom_sf(aes(fill = .data[[var]]), color = "black") + 
#       { if ( pseudo_log) scale_fill_viridis(trans = "pseudo_log10", limits = c(0, mx)) } + # breaks = labs, labels = labs, limits = c(labs[1], tail(labs, 1))) + 
#       { if (!pseudo_log) scale_fill_viridis(breaks = labs, labels = labs, limits = c(labs[1], tail(labs, 1))) } + 
#       labs(x = "Longitude", y = "Latitude", title = tt, fill = nn) + 
#       theme_bw() +
#       theme(legend.position = "right", 
#             text = element_text(size = 10, family = "LM Roman 10"), 
#             plot.title = element_text(size = 10, hjust = 0.5)) +
#       guides(fill = guide_colorbar(barheight = unit(215, "pt"), title.position = "top"))
#   }
#   
#   m
# }
# 
# generate_maps <- function (data, var, summ = "", ll = NA, pseudo_log = FALSE, per1K = FALSE, plot_count = FALSE, ...) {
#   
#   if (plot_count) { per1K <- FALSE }
#   scaling_txt <- ifelse(per1K, " (per 1,000 ind.)", "")
#   if (plot_count) { count_rate <- "count" } else { count_rate <- "rate"}
#   
#   opts <- possible_strata(data); # print(opts)
#   maps_cat <- list()
#   
#   for (i in 1:nrow(opts)) {
#     data_tmp <- data %>% filter(gender == opts$Gender[i], age == opts$Age[i])
#     data_tmp <- right_join(x = data_tmp, y = colombia, by = "loc")
#     
#     fert_mort <- ifelse(grepl("death", var, ignore.case = TRUE), paste("Death ", count_rate, scaling_txt, sep = ""), paste("Fertility ", count_rate, scaling_txt, sep = "")) 
#     
#     maps_cat[[i]] <- plot_map_colombia(data_tmp, var, summ, paste(fert_mort, "—", opts$Gender[i], ", ", opts$Age[i], sep = ""), ll = ll, pseudo_log = pseudo_log, per1K = per1K)
#   }
#   
#   maps_cat
# }
# 
# ###############
# # Mortality 
# ###############
# 
# maps_cat_mortality_mean <- generate_maps(data = mortality_rates_summ, var = "death_rate_mean", summ = "Mean", pseudo_log = pseudo_log, per1K = per1K)
# maps_cat_mortality_sd   <- generate_maps(data = mortality_rates_summ, var = "death_rate_sd", summ = "Std. dev.", pseudo_log = pseudo_log, per1K = per1K)
# # These two following might be unnecessary.
# maps_cat_mortality_count_mean <- generate_maps(data = mortality_rates_summ, var = "deaths_mean", summ = "Mean", pseudo_log = pseudo_log, per1K = per1K, plot_count = TRUE)
# maps_cat_mortality_count_sd   <- generate_maps(data = mortality_rates_summ, var = "deaths_sd", summ = "Std. dev.", pseudo_log = pseudo_log, per1K = per1K, plot_count = TRUE)
# 
# combine_maps <- function (maps_mean, maps_sd, gender = "F", ...) {
#   if (gender == "F") { i <- 1 } else if (gender == "M") { i <- 2 } else { stop("Undefined.") }
#   
#   total_map <- (maps_mean[[i]]      + maps_sd[[i]])      / 
#     (maps_mean[[i + 2]]  + maps_sd[[i + 2]])  / 
#     (maps_mean[[i + 4]]  + maps_sd[[i + 4]])  /
#     (maps_mean[[i + 6]]  + maps_sd[[i + 6]])  /
#     (maps_mean[[i + 8]]  + maps_sd[[i + 8]])  /
#     (maps_mean[[i + 10]] + maps_sd[[i + 10]]) /
#     (maps_mean[[i + 12]] + maps_sd[[i + 12]]) /
#     (maps_mean[[i + 14]] + maps_sd[[i + 14]]) /
#     (maps_mean[[i + 16]] + maps_sd[[i + 16]]) /
#     (maps_mean[[i + 18]] + maps_sd[[i + 18]])
#   
#   total_map
# }
# 
# maps_cat_mortality_female <- combine_maps(maps_cat_mortality_mean, maps_cat_mortality_sd, gender = "F")
# maps_cat_mortality_male   <- combine_maps(maps_cat_mortality_mean, maps_cat_mortality_sd, gender = "M")
# maps_cat_mortality_count_female <- combine_maps(maps_cat_mortality_count_mean, maps_cat_mortality_count_sd, gender = "F")
# maps_cat_mortality_count_male   <- combine_maps(maps_cat_mortality_count_mean, maps_cat_mortality_count_sd, gender = "M")
# 
# ggsave(filename = paste("PROCESS_DATA/IMAGES/MAPS/maps_mortality_female_", type.input, "_pseudoLog_", as.character(pseudo_log), "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = maps_cat_mortality_female, width = 3000, height = 12000, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("PROCESS_DATA/IMAGES/MAPS/maps_mortality_male_",   type.input, "_pseudoLog_", as.character(pseudo_log), "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = maps_cat_mortality_male,   width = 3000, height = 12000, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("PROCESS_DATA/IMAGES/MAPS/maps_mortality_count_female_", type.input, "_pseudoLog_", as.character(pseudo_log), "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = maps_cat_mortality_count_female, width = 3000, height = 12000, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("PROCESS_DATA/IMAGES/MAPS/maps_mortality_count_male_",   type.input, "_pseudoLog_", as.character(pseudo_log), "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = maps_cat_mortality_count_male,   width = 3000, height = 12000, units = c("px"), dpi = 300, bg = "white")
# 
# ###############
# # Fertility
# ###############
# 
# maps_cat_fertility_mean <- generate_maps(data = fertility_rates_summ, var = "fertility_rate_mean", summ = "Mean", pseudo_log = pseudo_log, per1K = per1K)
# maps_cat_fertility_sd   <- generate_maps(data = fertility_rates_summ, var = "fertility_rate_sd", summ = "Std. dev.", pseudo_log = pseudo_log, per1K = per1K)
# ## The two following might be unnecessary.
# maps_cat_fertility_count_mean <- generate_maps(data = fertility_rates_summ, var = "births_mean", summ = "Mean", pseudo_log = pseudo_log, per1K = per1K, plot_count = TRUE)
# maps_cat_fertility_count_sd   <- generate_maps(data = fertility_rates_summ, var = "births_sd", summ = "Std. dev.", pseudo_log = pseudo_log, per1K = per1K, plot_count = TRUE)
# 
# maps_cat_fertility_female <- combine_maps(maps_cat_fertility_mean, maps_cat_fertility_sd, gender = "F")
# maps_cat_fertility_male   <- combine_maps(maps_cat_fertility_mean, maps_cat_fertility_sd, gender = "M")  
# maps_cat_fertility_count_female <- combine_maps(maps_cat_fertility_count_mean, maps_cat_fertility_count_sd, gender = "F")
# maps_cat_fertility_count_male   <- combine_maps(maps_cat_fertility_count_mean, maps_cat_fertility_count_sd, gender = "M")  
# 
# ggsave(filename = paste("PROCESS_DATA/IMAGES/MAPS/maps_fertility_female_", type.input, "_pseudoLog_", as.character(pseudo_log), "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = maps_cat_fertility_female, width = 3000, height = 12000, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("PROCESS_DATA/IMAGES/MAPS/maps_fertility_male_",   type.input, "_pseudoLog_", as.character(pseudo_log), "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = maps_cat_fertility_male,   width = 3000, height = 12000, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("PROCESS_DATA/IMAGES/MAPS/maps_fertility_count_female_", type.input, "_pseudoLog_", as.character(pseudo_log), "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = maps_cat_fertility_count_female, width = 3000, height = 12000, units = c("px"), dpi = 300, bg = "white")
# ggsave(filename = paste("PROCESS_DATA/IMAGES/MAPS/maps_fertility_count_male_",   type.input, "_pseudoLog_", as.character(pseudo_log), "_per1K_", as.character(per1K), extension_name, ".jpeg", sep = ""), plot = maps_cat_fertility_count_male,   width = 3000, height = 12000, units = c("px"), dpi = 300, bg = "white")
# 
# ##################################################



##################################################
##################################################
##################################################
# Update files to compute orphanhood
##################################################
##################################################
##################################################

# ###############
# # Fertility
# ###############
# fertility_rates <- fertility_rates %>% select(-mod)
# fertility_rat_f <- fertility_rates %>% filter(gender == "Female") %>% select(year, loc, age, gender, births, population, fertility_rate) %>% arrange(year)
# fertility_rat_m <- fertility_rates %>% filter(gender == "Male")   %>% select(year, loc, age, gender, births, population, fertility_rate) %>% arrange(year)
# colnames(fertility_rat_f) <- colnames(fertility_rat_m) <- c("year", "mun", "age", "gender", "births", "population", "fertility_rate")
# fertility_rat_f$mun <- as.double(as.character(fertility_rat_f$mun))
# fertility_rat_m$mun <- as.double(as.character(fertility_rat_m$mun))
# 
# yys <- 2015:2021
# 
# count <- 0
# for (y in yys) {
#   tmp_fert_rat_f <- fertility_rat_f %>% filter(year %in% (1998 + count):y)
#   tmp_fert_rat_m <- fertility_rat_m %>% filter(year %in% (1998 + count):y)
#   write_csv(x = tmp_fert_rat_f, file = paste("DATA/Fertility/municipality_fertility_f_", y, ".csv", sep = ""))
#   write_csv(x = tmp_fert_rat_m, file = paste("DATA/Fertility/municipality_fertility_m_", y, ".csv", sep = ""))
#   count <- count + 1
# }
# write_csv(x = fertility_rat_f, file = "DATA/Fertility/municipality_fertility_f_complete.csv")
# write_csv(x = fertility_rat_m, file = "DATA/Fertility/municipality_fertility_m_complete.csv")
# 

# ###############
# # Population
# ###############
old_pop <- read_csv(file = "DATA/Population from Census/backup_cp_pop_years_list_municipality.csv")
order_mun <- old_pop$Mun %>% unique
old_pop_2022 <- old_pop %>% filter(Year == 2022)
colnames(old_pop_2022) <- c("year", "loc", "age", "gender", "population")
old_pop_2022 <- old_pop_2022 %>% select(gender, loc, year, age, population)

new_pop <- population_locs
new_pop <- new_pop %>% select(gender, loc, year, age, population)
if (TRUE) { # Retrieve age group `0-9`
  original_pop <- read_csv("DATA/Population from Census/backup_cp_pop_years_list_municipality.csv")
  original_pop <- original_pop %>% filter(Age == "0-9", Year >= 1998, Year <= 2021) %>% select(Sex, Mun, Year, Age, Population) %>% arrange(Sex, Mun, Year, Age)
  colnames(original_pop) <- c("gender", "loc", "year", "age", "population")
  original_pop$loc <- as.double(as.character(original_pop$loc))
}
new_pop$loc <- as.double(as.character(new_pop$loc))
new_pop <- bind_rows(new_pop, original_pop, old_pop_2022)
colnames(new_pop) <- c("Sex", "Mun", "Year", "Age", "Population")
new_pop <- new_pop %>% select("Year", "Mun", "Age", "Sex", "Population")
new_pop$Mun <- factor(x = new_pop$Mun, levels = order_mun)
new_pop <- new_pop %>% arrange(Year, Mun, Age, Sex)
new_pop$Mun <- as.integer(as.character(new_pop$Mun))
new_pop <- new_pop %>% arrange(Year, Mun, Age, Sex)

write_csv(x = new_pop, file = "DATA/Population from Census/pop_years_list_municipality.csv")

