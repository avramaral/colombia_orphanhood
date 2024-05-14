source("header.R")
source("PROCESS_DATA/aux_process.R")
source("PROCESS_DATA/aux_orphanhood.R")

##################################################
# Set common parameters and read data
##################################################

per1K <- TRUE
type.input <- "Municipality" # c("Municipality", "Department", "Region", "National")
compute_prevalence <- ifelse(type.input != "Municipality", TRUE, FALSE)
per_n_children <- ifelse(type.input == "Municipality", 1000, 100000)

yys <- seq(ifelse(compute_prevalence, 2004, 2015), 2021)

rates <- readRDS(file = "PROCESS_DATA/EQUALIZED_RATES.RDS")

mortality_rates <- rates$mortality_rates
fertility_rates <- rates$fertility_rates

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

if (type.input != "Municipality") {
  valid_muns          <- readRDS(file = "PROCESS_DATA/valid_muns.RDS")
  tmp_geo_info        <- geo_info %>% filter(mun %in% valid_muns)
  tmp_mortality_rates <- mortality_rates %>% filter(loc %in% valid_muns)
  tmp_fertility_rates <- fertility_rates %>% filter(loc %in% valid_muns)
  tmp_population      <- population %>% filter(loc %in% valid_muns)
  tmp_death_count     <- death_count %>% filter(mun %in% valid_muns)
  
  tmp_data <- convert_resolution(geo_info = tmp_geo_info, mortality_rates = tmp_mortality_rates, fertility_rates = tmp_fertility_rates, population = tmp_population, death_count = tmp_death_count, type.input = type.input)
} else {
  tmp_data <- convert_resolution(geo_info = geo_info, mortality_rates = mortality_rates, fertility_rates = fertility_rates, population = population, death_count = death_count, type.input = type.input)
}

mortality_rates <- tmp_data$mortality_rates
fertility_rates <- tmp_data$fertility_rates
population      <- tmp_data$population
death_count     <- tmp_data$death_count; colnames(death_count)[2] <- "loc"
prop_15_17      <- tmp_data$prop_15_17

if (compute_prevalence) {
  
  prev_yys <- rev(2004:2014 - 17)
  
  tmp_mortality_rates <- mortality_rates %>% filter(year == (2015 - 17))
  tmp_fertility_rates <- fertility_rates %>% filter(year == (2015 - 17))
  tmp_population      <- population      %>% filter(year == (2015 - 17))
  
  for (prev_yy in prev_yys) {
    
    tmp_mortality_rates$year <- prev_yy
    tmp_fertility_rates$year <- prev_yy
    tmp_population$year      <- prev_yy
    
    # tmp_mortality_rates$deaths <- death_count[death_count$year == prev_yy, ]$deaths
    
    mortality_rates <- bind_rows(tmp_mortality_rates, mortality_rates)
    fertility_rates <- bind_rows(tmp_fertility_rates, fertility_rates)
    population      <- bind_rows(tmp_population     , population     )
  }
  
  mortality_rates <- mortality_rates %>% arrange(gender, loc, year, age)
  fertility_rates <- fertility_rates %>% arrange(gender, loc, year, age)
  population      <- population      %>% arrange(gender, loc, year, age)
}

##################################################
# Compute orphanhood
##################################################

for (yy in yys) {
  print(paste("Year: ", yy, " (until ", yys[length(yys)], ")", sep = ""))

  ##############################
  # Process no. children
  ##############################
  
  print("Processing number of children.")
  process_number_children_year(yy = yy, type.input = type.input, fertility_rates = fertility_rates, per1K = per1K)
  
  # Process number of orphans.
  print("Processing number of orphans.")
  process_nb_orphans_table_dep_national_year(yy = yy, type.input = type.input, death_count = death_count)
  
  print(paste("Done for year ", yy, ".", sep = ""))
}

if (type.input != "Municipality") {
  orphan_table(type.input = type.input, population = population, prop_15_17 = prop_15_17, geo_info = tmp_geo_info, per_n_children = per_n_children)
} else {
  orphan_table(type.input = type.input, population = population, prop_15_17 = prop_15_17, geo_info =     geo_info, per_n_children = per_n_children)
}





