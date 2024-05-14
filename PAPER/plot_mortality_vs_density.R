source("header.R")
source("PROCESS_DATA/aux_orphanhood.R")
source("PROCESS_DATA/PAPER/aux_paper.R")

type.input <- "Municipality" # c("Municipality", "Department", "Region", "National")
geo_info <- read_csv(file = "DATA/geo_info.csv", col_types = "fcfcfcfc"); muns <- unique(geo_info$mun)

########################################################
##### Compute municipalities with small population #####
########################################################

if (type.input == "Municipality") {
  original_pop <- read_csv("DATA/Population from Census/backup_cp_pop_years_list_municipality.csv")
  original_pop <- original_pop %>% filter(Year == 2018)
  original_pop <- original_pop %>% group_by(Mun) %>% summarise(total_pop = sum(Population)) %>% ungroup() %>% rename(mun = Mun) %>% mutate(mun = factor(mun))
  
  col_tmp <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry) %>% rename(mun = Code) %>% mutate(mun = factor(mun))
  col_tmp <- col_tmp %>% mutate(area = st_area(geometry))
  col_tmp <- col_tmp %>% left_join(y = original_pop, by = "mun")
  col_tmp <- col_tmp %>% mutate(pop_density = (total_pop / area)) %>% arrange(desc(pop_density)) %>% select(-geometry)
  col_tmp <- col_tmp %>% left_join(y = geo_info[, c("mun", "mun_name", "dep_name")], by = "mun") %>% select(mun, mun_name, dep_name, total_pop, area, pop_density) %>% rename(population = total_pop)
  
  col_tmp <- col_tmp %>% rename(loc = mun)
}

##############################
##### Retrieve mortality #####
##############################

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

# Convert it to the desired resolution
tmp_data <- convert_resolution(geo_info = geo_info, mortality_rates = mortality_rates, fertility_rates = fertility_rates, population = population, death_count = death_count, type.input = type.input)

mortality_rates <- tmp_data$mortality_rates
fertility_rates <- tmp_data$fertility_rates
population      <- tmp_data$population
death_count     <- tmp_data$death_count; colnames(death_count)[2] <- "loc"
prop_15_17      <- tmp_data$prop_15_17

##################################
##### Combine all quantities #####
##################################

data <- col_tmp %>% left_join(y = mortality_rates[, c("loc", "death_rate")], by = "loc")
data <- data %>% group_by(loc, pop_density) %>% summarise(mean_death_rate = mean(death_rate))
data$pop_density <- as.numeric(data$pop_density) * 1000

pp <- ggplot(data = data, mapping = aes(x = pop_density, y = mean_death_rate)) +
  geom_point(size = 2, shape = 1) +
  geom_smooth(color = "blue", method = "loess", span = 0.5) +
  labs(title = "Death rate versus population density", subtitle = "With local polynomial regression fitting (span = 0.5)") + 
  theme_bw() +
  geom_vline(xintercept = 1e-03, colour = "red", linewidth = 1) +
  scale_x_log10() +
  #xlim(c(0, 0.00025))+
  labs(x =  "Population density", y = "Average death rate") +
  theme(text = element_text(size = 12, family = "LM Roman 10"), panel.grid.major.x = element_blank())

ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/pop_density_vs_death_rate.jpeg", sep = ""), plot = pp, width = 2000, height = 2000, units = c("px"), dpi = 300, bg = "white")

