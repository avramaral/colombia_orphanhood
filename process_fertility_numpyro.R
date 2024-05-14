source("header.R")

compute_rate_tmp <- function (count, pop, ...) { 
  r <- (count / pop) 
  r[is.nan(r)] <- 0
  r
}

############
# OPTION 1 #
############

rates <- readRDS(file = "PROCESS_DATA/EQUALIZED_RATES.RDS")

mortality_rates <- rates$mortality_rates
fertility_rates <- rates$fertility_rates

# Process fertility rates, and assume it is constant over the years
fertility_rates <- fertility_rates %>% mutate(fertility_rate = compute_rate_tmp(count = births, pop = population))
fertility_rates <- fertility_rates %>% mutate(fertility_rate = ifelse(mod != 0, NA, fertility_rate), population = ifelse(mod != 0, NA, population)) %>% select(-mod)
# Use a better way to deal with zeros
fertility_rates <- fertility_rates %>% mutate(fertility_rate = ifelse(fertility_rate == 0, .Machine$double.eps, fertility_rate)) %>% mutate(log_fertility_rate = log(fertility_rate))

# Create dummy variables
fertility_rates$loc  <- as.numeric(fertility_rates$loc)
fertility_rates$year <- fertility_rates$year - 1997 # Set it from 1 to 24
fertility_rates <- fertility_rates %>% mutate(female = ifelse(gender == "Female", 1, 0), male = ifelse(gender == "Male", 1, 0))
fertility_rates$age <- as.numeric(factor(fertility_rates$age)) # Age maps to `10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55+`, such that `female` goes up to `45-49`

fertility_rates <- fertility_rates %>% select(loc, year, age, female, male, births, population, fertility_rate)

write_csv(x = fertility_rates, file = "PROCESS_DATA/DATA/raw_fertility_rates_1.csv")

############
# OPTION 2 #
############

population <- readRDS(file = "DATA/Population from Census/updated_negative_pop_list_municipality.csv")
fertility_rates <- readRDS(file = "PROCESS_DATA/AUX_DATA/IMPUTATION/negative_population_fertility.RDS")
fertility_rates <- dplyr::select(fertility_rates, -population) %>% left_join(y = population, by = c("gender", "loc", "year", "age"))
fertility_rates <- fertility_rates %>% mutate(fertility_rate = compute_rate_tmp(count = births, pop = population)) %>% select(-mod)
fertility_rates <- fertility_rates %>% mutate(population = ifelse(year %in% c(2005, 2018), population, NA), fertility_rate = ifelse(year %in% c(2005, 2018), fertility_rate, NA))

gs <- unique(fertility_rates$gender)
ls <- unique(fertility_rates$loc)
ys <- unique(fertility_rates$year)
as_fem <- unique(filter(fertility_rates, gender == "Female")$age)
as_mal <- unique(filter(fertility_rates, gender == "Male")$age)

# Check this approach for "constant fertility rate"; i.e., is it correct if I simply average the fertility rates for the 2 census years and compute the population accordingly?
for (g in gs) {
  print(g)
  fert_1 <- fertility_rates %>% filter(gender == g)
  
  pb <- txtProgressBar(min = 1, max = length(ls), initial = 1) 
  cc <- 1
  cc_create <- 1
  for (l in ls) {
    fert_2 <- fert_1 %>% filter(loc == l)
    if (g == "Female") {
      for (a in as_fem) {
        fert_3 <- fert_2 %>% filter(age == a)
        
        mm_fer <- mean(fert_3$fertility_rate, na.rm = TRUE)
        fert_3$fertility_rate <- mm_fer
        fert_3$population <- fert_3$births / fert_3$fertility_rate
        if (cc_create == 1) {
          fert_fem <- fert_3
        } else {
          fert_fem <- bind_rows(fert_3, fert_fem)
        }
        cc_create <- cc_create + 1
      }
    } else {
      for (a in as_mal) {
        fert_3 <- fert_2 %>% filter(age == a)
        
        mm_fer <- mean(fert_3$fertility_rate, na.rm = TRUE)
        fert_3$fertility_rate <- mm_fer
        fert_3$population <- fert_3$births / fert_3$fertility_rate
        if (cc_create == 1) {
          fert_mal <- fert_3
        } else {
          fert_mal <- bind_rows(fert_3, fert_mal)
        }
        cc_create <- cc_create + 1
      }
    }
    cc <- cc + 1
    setTxtProgressBar(pb, cc)
  }
  close(pb)
}

new_fertility_rates <- bind_rows(fert_fem, fert_mal)
new_fertility_rates <- new_fertility_rates %>% mutate(population = ifelse(is.infinite(population), births, population))
new_fertility_rates <- new_fertility_rates %>% mutate(population = ifelse(is.na(population), NA, population))

# Fix entries with no population
idxs_to_fix <- new_fertility_rates %>% rownames_to_column() %>% filter(is.na(population)) %>% select(rowname) %>% c() %>% unlist() %>% unname() %>% as.numeric()

# VERY SLOW
pb <- txtProgressBar(min = 1, max = length(idxs_to_fix), initial = 1) 
cc <- 1
for (i in idxs_to_fix) {
  rr <- new_fertility_rates[i, ]
  gg <- rr$gender
  ll <- rr$loc
  yy <- rr$year
  aa <- rr$age
  
  new_fertility_rates[i, "population"] <- mean((new_fertility_rates %>% filter(gender == gg, loc == ll, age == aa))$population, na.rm = TRUE)

  cc <- cc + 1
  setTxtProgressBar(pb, cc)
}
close(pb)
# Correct it to `0` if there is no population in any year
new_fertility_rates <- new_fertility_rates %>% mutate(population = ifelse(is.na(population), 0, population))

new_fertility_rates <- new_fertility_rates %>% mutate(fertility_rate = compute_rate_tmp(count = births, pop = population)) %>% arrange(gender, loc, year, age)
new_fertility_rates -> fertility_rates

# Create dummy variables
fertility_rates$loc  <- as.numeric(fertility_rates$loc)
fertility_rates$year <- fertility_rates$year - 1997 # Set it from 1 to 24
fertility_rates <- fertility_rates %>% mutate(female = ifelse(gender == "Female", 1, 0), male = ifelse(gender == "Male", 1, 0))
fertility_rates$age <- as.numeric(factor(fertility_rates$age)) # Age maps to `10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55+`, such that `female` goes up to `45-49`

fertility_rates <- fertility_rates %>% select(loc, year, age, female, male, births, population, fertility_rate)
# Final fix
fertility_rates <- fertility_rates %>% mutate(population = ifelse(is.infinite(fertility_rate), births, population))
fertility_rates <- fertility_rates %>% mutate(fertility_rate = compute_rate_tmp(count = births, pop = population))

write_csv(x = fertility_rates, file = "PROCESS_DATA/DATA/raw_fertility_rates_2.csv")


raw_fertility_rates_2_v2$population <- ceiling(raw_fertility_rates_2_v2$population)

write_csv(x = raw_fertility_rates_2_v2, file = "PROCESS_DATA/DATA/raw_fertility_rates_2_v2.csv")
