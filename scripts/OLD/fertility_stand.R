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
fert <- fert %>% mutate(population = ifelse(is.na(population), 0, population))

total_pop <- readRDS(paste(data.dir, "/summarised_data/pop_all.RDS", sep = ""))
gdp       <- readRDS(paste(data.dir, "/summarised_data/gbp.RDS", sep = ""))
geo_info  <- read_csv(paste(data.dir, "/summarised_data/geo_info.csv", sep = ""), col_types = cols())

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

##############################
# Analyse raw data
##############################

# Reference population (computed based on `fert`)
pop_ref <- total_pop %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()
pop_ref <- fert %>% dplyr::select(loc, year, gender, age, population) %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(national_pop = sum(population)) %>% ungroup()

# `gender` & `age` fertility rates
fert <- fert %>% mutate(fertility_rate = compute_rate(births, population))
# Fix `x / 0`, such that `x > 0`; i.e., births with no population
fert <- fert %>% mutate(births = ifelse(is.infinite(fertility_rate), 0, births))
fert <- fert %>% mutate(fertility_rate = compute_rate(births, population))

fert <- fert %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_fert <- filter(fert, year == 2018) %>% group_by(loc, year) %>% summarise(pop_n = sum(population),
                                                                               births = sum(births),
                                                                               births_r = (sum(births) / sum(population)),
                                                                               births_r_std = (sum(fertility_rate * national_pop) / national_pop_total)) %>% ungroup()

# Add `dep`
stand_fert <- stand_fert %>% mutate(loc = factor(loc))
stand_fert <- stand_fert %>% rename(mun = loc) %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `gdp` (missing 1998-2004 and 2020-2021)
stand_fert <- stand_fert %>% left_join(y = gdp, by = c("year", "dep")) 

stand_fert <- stand_fert %>% mutate(births_r_std_disc = ifelse(births_r_std == 0, " 0",
                                                        ifelse(births_r_std <= 5e-3,  "<= 5 per 1000",
                                                        ifelse(births_r_std <= 10e-3, "<=10 per 1000",
                                                        ifelse(births_r_std <= 15e-3, "<=15 per 1000",
                                                        ifelse(births_r_std <= 20e-3, "<=20 per 1000",
                                                        ifelse(births_r_std <= 25e-3, "<=25 per 1000",
                                                        ifelse(births_r_std <= 30e-3, "<=30 per 1000", ">30 per 1000"))))))))

tmp <- colombia %>% left_join(y = stand_fert[, c("mun", "births_r_std_disc")], by = "mun") %>% rename(value = births_r_std_disc) %>% dplyr::select(mun, value, geometry)

f1 <- ggplot() +
  geom_sf(data = tmp, aes(geometry = geometry, fill = value), color = "black") +
  scale_fill_carto_d(name = "Standardized fertility rate in 2018", palette = "Burg", direction = 1) +
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  guides(fill = guide_legend(order = 1))

ggsave(f1, file = file.path(out.dir, paste("Colombia_fertility_rate_2018_", stamp, ".png", sep = "")), width = 10, height = 10)
