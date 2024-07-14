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
fert <- fert %>% mutate(population = ifelse(is.na(population), 0, population))
fert <- fert %>% mutate(loc = factor(loc))

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

# Reference population (computed based on `fert`)
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

# Add `DEP`
stand_fert <- stand_fert %>% rename(mun = loc) %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun")
# Add `GDP` (missing 1998-2004 and 2020-2021)
stand_fert <- stand_fert %>% left_join(y = gdp, by = c("year", "dep")) 
# Add `MPI`
stand_fert <- stand_fert %>% left_join(y = dmpi[, c("mun", "mpi")], by = "mun")
# Add `VA`
stand_fert <- stand_fert %>% left_join(y = dgbpm[, c("mun", "va")], by = "mun")
stand_fert <- stand_fert %>% mutate(va_per_capita = va / pop_n)
# Add `capital`
stand_fert <- stand_fert %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun")

stand_fert <- stand_fert %>% mutate(births_r_std_disc = ifelse(births_r_std == 0, " 0",
                                                        ifelse(births_r_std <= 5e-3,  "<= 5 per 1000",
                                                        ifelse(births_r_std <= 10e-3, "<=10 per 1000",
                                                        ifelse(births_r_std <= 15e-3, "<=15 per 1000",
                                                        ifelse(births_r_std <= 20e-3, "<=20 per 1000",
                                                        ifelse(births_r_std <= 25e-3, "<=25 per 1000",
                                                        ifelse(births_r_std <= 30e-3, "<=30 per 1000", ">30 per 1000"))))))))

#########################
#########################
########## NEW ##########
#########################
#########################

###################
### FIRST  PART ###
###################

dref <- fert %>% filter(year == 2018) %>% group_by(gender, age) %>% summarise(natl_pop_n = sum(population)) %>% ungroup()
dref <- dref %>% mutate(natl_pop_t = sum(natl_pop_n))
dref <- dref %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t)

da <- fert %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(births = mean(births)) %>% ungroup()
tmp <- fert %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
da <- da %>% left_join(y = tmp, by = c("loc", "gender", "age"))
da <- da %>% mutate(mun_birth_r_2018 = births / population)
da <- da %>% mutate(mun_birth_r_2018 = ifelse(population == 0, 0, mun_birth_r_2018))
da <- da %>% filter(age != "0-9")
da <- da %>% mutate(mun_birth_r_2018_log = log(mun_birth_r_2018))

da <- da %>% left_join(y = dref, by = c("gender", "age"))

# Calculate standardized death rates
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
da <- da %>% rename(mun = loc) %>% group_by(mun, year) %>% summarise(pop_n = sum(population),
                                                                     births = sum(births),
                                                                     birth_r = (sum(births) / sum(population)),
                                                                     birth_r_std = (sum(mun_birth_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()


# Add `geo_info`
da <- da %>% left_join(y = geo_info, by = "mun")

# Add `value added` info
tmp <- subset(dgbpm, select = c(mun, va))
setorder(tmp, va)
tmp <- tmp %>% as.data.table()
tmp[, mun_rnk := 1:nrow(tmp)]
tmp <- tmp %>% as_tibble() %>% mutate(mun = factor(mun))
da <- da %>% left_join(y = tmp, by = "mun")

# Reset Bogota as capital of Cundinamarca
da <- da %>% as.data.table()
set(da, da[, which(dep == 11)], "dep", 25L)
set(da, da[, which(dep == 25)], "dep_name", 'Cundinamarca\nincl Bogotá')
set(da, da[, which(mun == 25001)], "capital", FALSE)
da <- da %>% as_tibble()

# Add `density` info
colombia <- colombia %>% mutate(area_2 = as.double(units::set_units(st_area(geometry), km^2)))
da <- da %>% left_join(y = colombia[, c("mun", "area_2")])
da <- da %>% mutate(pop_r = pop_n / area_2)

da <- da %>% mutate(pop_r_disc = ifelse(pop_r < 5,   "(1) 0-4",
                                 ifelse(pop_r < 25,  "(2) 5-24",
                                 ifelse(pop_r < 50,  "(3) 25-49",
                                 ifelse(pop_r < 100, "(4) 50-99",
                                 ifelse(pop_r < 250, "(5) 100-249", "(6) 250+"))))))



tmp_dgbp <- dgbp %>% rename(dep_gdp_per_capita = gdp_per_capita)
da <- da %>% as.data.table; tmp_dgbp <- tmp_dgbp %>% as.data.table()
da <- merge(da, tmp_dgbp, by = c("dep", "year"), all.x = TRUE)
da[, va_per_capita := va / pop_n]
da <- da %>% as_tibble(); tmp_dgbp <- tmp_dgbp %>% as_tibble()

tmp <- da %>% group_by(dep_name, dep_gdp_per_capita) %>% summarise(sum_mum_va_per_capita = sum(va_per_capita)) %>% ungroup() %>% as.data.table()
setorder(tmp, -dep_gdp_per_capita)
tmp <- tmp %>% as_tibble()


p <- ggplot(da, aes(x = va, y = birth_r_std, col = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_point(data = subset(da, capital == 0)) +
  geom_point(data = subset(da, capital == 1)) +
  labs(col = "") + 
  scale_x_log10() +
  scale_colour_carto_d(palette = "Vivid", direction = -1) +
  theme_bw() +
  labs(x = "Valore agregado", y = "Mean standardized birth rates 2017-2019") +
  facet_wrap(~ dep_name, ncol = 6) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p <- ggplot(da, aes(x = va, y = birth_r_std, col = pop_r_disc)) +
  geom_point() +
  labs(col = "") + 
  scale_x_log10() +
  scale_colour_carto_d(palette = "Burg") +
  theme_bw() +
  labs(x = "Valore agregado", y = "Mean standardized birth rates 2017-2019", colour = "Population per km2") +
  facet_wrap(~ dep_name, ncol = 6) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p <- ggplot(da, aes(x = va, y = birth_r_std)) +
  geom_point(aes(col = pop_r_disc, 
                 pch = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")), 
                 size = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  labs(col = "") + 
  scale_x_log10() +
  scale_size_manual(values = c(4,1)) +
  scale_colour_carto_d(palette = "Burg") +
  theme_bw() +
  labs(x = "Valore agregado", y = "Mean standardized birth rates 2017-2019", colour = "Population per km2", size = "", pch = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  geom_smooth(colour = "black") +
  geom_smooth(data = subset(da, capital == 1), colour = "black", lty = "dotted", method = "lm") 

p <- ggplot(da, aes(x = pop_r, y = birth_r_std, col = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_point(data = subset(da, capital == 0)) +
  geom_point(data = subset(da, capital == 1)) +
  labs(col = "") + 
  scale_x_log10() +
  scale_colour_carto_d(palette = "Vivid", direction = -1) +
  theme_bw() +
  labs(x = "Population density", y = "Mean standardized birth rates 2017-2019") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + geom_smooth(colour = "black")

p <- ggplot(da, aes(x = va, y = (pop_n / area_2), col = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  geom_point(data = subset(da, capital == 0)) +
  geom_point(data = subset(da, capital == 1)) +
  labs(col = "") + 
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_carto_d(palette = "Vivid", direction = -1) +
  theme_bw() +
  labs(x = "Valore agregado", y = "Population density 2018 per km2") +
  facet_wrap(~ dep_name, ncol = 6) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### GAM approach ###

# Determine best fit distribution to standardized death rates
das <- da %>% filter(va > 1e6)
fln <- fitdist(das$birth_r_std, "lnorm")
fg  <- fitdist(das$birth_r_std, "gamma")
fw  <- fitdist(das$birth_r_std, "weibull")
cdfcomp(list(fln, fg, fw), legendtext = c("lognormal", "gamma", "weibull"))
gofstat(list(fln, fg, fw), fitnames = c("lognormal", "gamma", "weibull"))
# both AIC and BIC select "weibull"

# Read quantiles of best fit
dva6 <- unname(unlist(quantile(fw, prob = c(0.025, 0.5, 0.975))$quantiles))

# Determine average trend in standardized death rates by value added with GAM
tmp <- da %>% dplyr::select(mun, va, birth_r_std)
tmp <- tmp %>% as.data.table()
set(tmp, NULL, "va", tmp[, log10(va)])
tmp <- tmp %>% as_tibble()
fgam <- gam(birth_r_std ~ s(va, bs = "cs"), data = tmp, method = "REML")
dvaa <- data.table(va = seq(log10(min(da$va, na.rm = T)), log10(max(da$va, na.rm = T)), by = 0.01))
dvaa[, death_r_std_fitted := unname(unlist(mgcv:::predict.gam(fgam, dvaa, type = 'response', se.fit = FALSE)))]
set(dvaa, NULL, "va", dvaa[, 10^va])
dvaa <- dvaa %>% as_tibble()


da <- da %>% st_drop_geometry()
p <- ggplot(da, aes(x = va, y = birth_r_std)) +
  geom_ribbon(aes(ymin = dva6[1], ymax = dva6[3]), fill = "chocolate4", alpha = 0.2) +
  geom_hline(yintercept = dva6[2], lty = "dashed", col = "black", lwd = 0.8 ) +
  geom_point(aes(col = pop_r_disc,  
                 pch = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")), 
                 size = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  labs(col = "") + 
  scale_x_log10() +
  scale_size_manual(values = c(4, 1)) +
  scale_colour_carto_d(palette = "Burg") +
  theme_bw() +
  labs(x = "Valore agregado", y = "Mean standardized birth rates 2017-2019", colour = "Population per km2", size = "", pch = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  geom_line(data = dvaa, aes(x = va, y = death_r_std_fitted), colour = "black", lwd = 0.8) 


# Work out adjustment multiplier


dvaa <- da %>% dplyr::select(mun, va, birth_r_std) %>% as.data.table()
set(dvaa, NULL, "va", dvaa[, log10(va)])
fgam <- gam(birth_r_std ~ s(va, bs = "cs"), data = dvaa, method = "REML")
dvaa[, birth_r_std_fitted := unname(unlist(mgcv:::predict.gam(fgam, dvaa, type = "response", se.fit = FALSE)))]
set(dvaa, NULL, "va", dvaa[, 10^va])

tmp <- dvaa[, list(it = 1:1e3, birth_r_std_adjust = rgamma(1e3, fg$estimate[1], fg$estimate[2])), by = c("mun", "va")]
dvaa <- merge(dvaa, tmp, by = c("mun", "va"))

tmp <- unique(subset(dvaa, select = -c(it, birth_r_std_adjust)))
tmp[, birth_r_std_adjust := unname(unlist(quantile(fg, prob = c(0.5))$quantiles))]
tmp[, it := 0L]

dvaa <- rbind(tmp, dvaa)
dvaa[, birth_r_std_adjust_multiplier := pmax(1.0, birth_r_std_adjust / birth_r_std_fitted)]

# Apply adjustments and plot
da <- da %>% as.data.table()
da <- merge(da, subset(dvaa, it == 0, select = c(mun, birth_r_std_adjust_multiplier)), by = c("mun"))
da[, birth_r_std_adj := birth_r_std * birth_r_std_adjust_multiplier]
da[, birth_r_std_adj_disc := cut(birth_r_std_adj, breaks = c(-1, 1e-7, 10e-3, 20e-3, 30e-3, 40e-3, 1), 
                                 labels = c("(1) 0", "(2) <=1 per 1000", "(3) <=10 per 1000", " (4) <=20 per 1000", " (5) <=30 per 1000", "(6) >40 per 1000"), right = TRUE)]

tmp <- data.table(mun = colombia$mun)
tmp[, id := 1:nrow(tmp)]
tmp <- merge(tmp, subset(da, select = c(mun, birth_r_std_adj_disc)))
setkey(tmp, id)
tmp2 <- copy(colombia)
tmp2[["value"]] <- tmp$birth_r_std_adj_disc
# DO NOT PLOT IT, as it may take forever
p <- ggplot() +
  geom_sf(data = tmp2, aes(geometry = geometry, fill = value), color = "black") +
  scale_fill_carto_d(name = "Standardized fertility rate in 2018\nadjusted for under-reporting using the GAM model", palette = "Burg", direction = 1) +
  theme_bw() +
  theme(legend.position = "right",
        text = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + guides(fill = guide_legend(order = 1))

# Based on GAM still
da <- da %>% as_tibble()
p <- ggplot(da, aes(x = va, y = birth_r_std_adj)) +
  geom_ribbon(aes(ymin = dva6[1], ymax = dva6[3]), fill = "chocolate4", alpha = 0.2) +
  geom_point(aes(col = pop_r_disc,  
                 pch = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")), 
                 size = factor(capital, levels = c(1, 0), labels = c("State capital", "Other")))) +
  labs(col = "") + 
  scale_x_log10() +
  scale_size_manual(values = c(4, 1)) +
  scale_colour_carto_d(palette = "Burg") +
  theme_bw() +
  labs(x = "Valore agregado", y = "Adjusted standardized birth rates", colour = "Population per km2", size = "", pch = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  geom_smooth(colour = "black")

###################
### SECOND PART ###
###################

dref <- fert %>% filter(age != "0-9", year > 2011, year < 2022) %>% group_by(gender, age, year) %>% summarise(natl_pop_n = sum(population), natl_birth_n = sum(births)) %>% ungroup()
dref <- dref %>% mutate(natl_birth_r = natl_birth_n / natl_pop_n)
dref <- dref %>% mutate(natl_birth_r_log = log(natl_birth_n / natl_pop_n))

# Determine age midpoints
tmp <- dref %>% dplyr::select(gender, age) %>% distinct()
tmp <- tmp %>% mutate(age2 = age)
tmp <- tmp %>% mutate(age2 = ifelse(age == "50+" & gender == "Female", "50-54", age2)) # Unnecessary
tmp <- tmp %>% mutate(age2 = ifelse(age == "55+" & gender ==   "Male", "55-59", age2)) # Unnecessary
tmp <- tmp %>% mutate(age_from = as.integer(gsub("([0-9]+)-([0-9]+)", "\\1", age2)))
tmp <- tmp %>% mutate(age_to   = as.integer(gsub("([0-9]+)-([0-9]+)", "\\2", age2)))
tmp <- tmp %>% mutate(age_mid  = (age_from + age_to + 1L) / 2)
dref <- dref %>% left_join(y = tmp[, c("age", "gender", "age_from", "age_to", "age_mid")], by = c("age", "gender"))

# Fit spline to age trends in log death rates over continuous age mid points
dref <- dref %>% as.data.table()
tmp <- dref[, {
  d <- list(natl_birth_r_log = natl_birth_r_log, age_mid = age_mid)
  f <- gam(natl_birth_r_log ~ s(age_mid, bs = "cs", k = 5), data = d, method = "REML")
  z <- mgcv:::predict.gam(f, d, type = 'response', se.fit = TRUE)
  list(natl_birth_r_log_fitted = as.numeric(z$fit), natl_birth_r_log_fitted_se = as.numeric(z$se.fit), age_mid = age_mid)
}, by = c("gender", "year")] %>% as_tibble()
dref <- dref %>% as_tibble()
dref <- dref %>% left_join(y = tmp, by = c("year", "gender", "age_mid"))

# Fit spline to age trends in death rates over continuous age mid points
dref <- dref %>% as.data.table()
tmp <- dref[, {
  d <- list(natl_birth_r = natl_birth_r, age_mid = age_mid)
  f <- gam(natl_birth_r ~ s(age_mid, bs = "cs", k = 7), data = d, method = "REML")
  z <- mgcv:::predict.gam(f, d, type = 'response', se.fit = TRUE)
  list(natl_birth_r_fitted = as.numeric(z$fit), natl_birth_r_fitted_se = as.numeric(z$se.fit), age_mid = age_mid)
}, by = c("gender", "year")] %>% as_tibble()
dref <- dref %>% as_tibble()
dref <- dref %>% left_join(y = tmp, by = c("year", "gender", "age_mid"))

# Plot on log scale
p <- ggplot(dref, aes(x = age_mid, y = natl_birth_r_log, colour = gender)) +
  geom_point() +
  scale_colour_carto_d(palette = "Vivid") +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~ year, ncol = 3) + labs(y = "National log birth rate")

p <- ggplot(dref, aes(x = age_mid, colour = factor(year), group = interaction(year, gender))) +
  geom_point(aes(y = natl_birth_r_log)) +
  ggsci::scale_color_npg() +
  geom_line(aes(y = natl_birth_r_log_fitted)) +
  theme_bw() +
  facet_grid(~ gender) +  labs(y = "National log birth rate")

# Plot on natural scale
p <- ggplot(dref, aes(x = age_mid, colour = factor(year), group = interaction(year, gender))) +
  geom_point(aes(y = natl_birth_r)) +
  ggsci::scale_color_npg() +
  geom_line(aes(y = natl_birth_r_fitted)) +
  theme_bw() +
  facet_grid(~ gender) + labs(y = "National birth rate")

###################
### THIRD  PART ###
###################

# Plot age curves by municipality against national age curve
da <- fert %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(births = mean(births)) %>% ungroup()
tmp <- fert %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
da <- da %>% left_join(y = tmp, by = c("loc", "gender", "age"))
da <- da %>% mutate(mun_birth_r_2018 = births / population)
da <- da %>% mutate(mun_birth_r_2018 = ifelse(population == 0, 0, mun_birth_r_2018))
da <- da %>% filter(age != "0-9")
da <- da %>% mutate(mun_birth_r_2018_log = log(mun_birth_r_2018))

# Merge national reference population weights
dref <- dref %>% as.data.table()
tmp <- subset(dref, 
              year == 2018, 
              select = c(gender, age, age_mid, natl_birth_r_log_fitted, 
                         natl_birth_r_log_fitted_se, natl_birth_r_fitted, 
                         natl_birth_r_fitted_se)) %>% as_tibble()
dref <- dref %>% as_tibble()
da <- da %>% left_join(y = tmp, by = c("gender", "age"))


# Add `geo_info`
da <- da %>% rename(mun = loc) %>% left_join(y = geo_info, by = "mun")

# Add `value added` info
tmp <- subset(dgbpm, select = c(mun, va))
setorder(tmp, va)
tmp <- tmp %>% as.data.table()
tmp[, mun_rnk := 1:nrow(tmp)]
tmp <- tmp %>% as_tibble() %>% mutate(mun = factor(mun))
da <- da %>% left_join(y = tmp, by = "mun")

# Reset Bogota as capital of Cundinamarca
da <- da %>% as.data.table()
set(da, da[, which(dep == 11)], "dep", 25L)
set(da, da[, which(dep == 25)], "dep_name", 'Cundinamarca\nincl Bogotá')
set(da, da[, which(mun == 25001)], "capital", FALSE)
da <- da %>% as_tibble()

# High economic output
tmp <- subset(da, floor((max(mun_rnk, na.rm = T) - mun_rnk + 1L) / 30 ) == 0)
p <- ggplot(tmp, aes(x = age_mid, colour = va, group = interaction(mun, gender))) +
  geom_line(aes(y = mun_birth_r_2018)) +
  geom_line(aes(age_mid, y = natl_birth_r_fitted), colour = "black", lwd = 1.25) +
  scale_colour_viridis_c(begin = 0.4, end = 0.8) +
  theme_bw() +
  facet_grid(~ gender) + labs(y = "Log birth rates\n in 30 municipalities with highest VA")

# Low economic output
tmp <- subset(da, floor(mun_rnk / 30) == 0)
p <- ggplot(tmp, aes(x = age_mid, colour = va, group = interaction(mun, gender))) +
  geom_line(aes(y = mun_birth_r_2018)) +
  geom_line(aes(age_mid, y = natl_birth_r_fitted), colour = "black", lwd = 1.25) +
  scale_colour_viridis_c(option = "magma", begin = 0.4, end = 0.8) +
  theme_bw() +
  facet_grid(~ gender) + labs(y = "Log birth rates\n in 30 municipalities with lowest VA")


da <- da %>% mutate(va_disc = ifelse(va < 1e5, "(1) <100,000",
                              ifelse(va < 5e5, "(2) 100,000-500,000",
                              ifelse(va < 1e6, "(3) 500,000-1,000,000", "(4) >1,000,000"))))

# Plot for the municipalities according to economic index
da_tmp <- da %>% filter(!is.na(va_disc))
p <- ggplot(da_tmp, aes(x = age_mid)) +
  geom_boxplot(aes(x = age_mid, y = mun_birth_r_2018, group = age, colour = va_disc)) +
  geom_line(aes(age_mid, y = natl_birth_r_fitted), colour = "black", lwd = 1.25) +
  scale_y_sqrt() +
  scale_colour_carto_d(palette = "Burg") +
  theme_bw() +
  facet_grid(va_disc ~ gender) +
  labs(y = "Municipal log birth rate", colour = "Valure agregado\n in million pesos")

# Department capitals
tmp <- da %>% filter(capital == 1)
p <- ggplot(tmp, aes(x = age_mid, colour = mun_name, group = interaction(mun, gender))) +
  geom_line(aes(y = mun_birth_r_2018_log)) +
  geom_line(aes(age_mid, y = natl_birth_r_log_fitted), colour = "black", lwd = 1.25) +
  theme_bw() +
  facet_grid(~ gender) + labs(y = "Municipal log birth rate")

