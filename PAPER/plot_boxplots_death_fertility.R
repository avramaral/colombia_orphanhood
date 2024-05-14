source("header.R")
source("PROCESS_DATA/aux_process.R")
source("PROCESS_DATA/aux_orphanhood.R")

##################################################
# Set common parameters and read data
##################################################

per1K <- TRUE
type.input <- "Municipality" # Only "Municipality"

rates <- readRDS(file = "PROCESS_DATA/EQUALIZED_RATES.RDS")

mortality_rates <- rates$mortality_rates
fertility_rates <- rates$fertility_rates

# Rate
mortality_rates$population <- ceiling(mortality_rates$population)
fertility_rates$population <- ceiling(fertility_rates$population)
mortality_rates$death_rate <- compute_rate(count = mortality_rates$deaths, pop = mortality_rates$population)
fertility_rates$fertility_rate <- compute_rate(count = fertility_rates$births, pop = fertility_rates$population)

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
# Redefine MPM
##################################################

if (TRUE) { # (type.input == "Municipality") {
  
  CAT_MPM <- geo_info %>% select(MPM_cat, MPM_name) %>% distinct() %>% arrange(MPM_cat) %>% select(MPM_name) %>% c() %>% unlist() %>% unname() 
  
  exclude_low_density <- TRUE
  n_excluded_muns <- 29
  
  original_pop <- read_csv("DATA/Population from Census/backup_cp_pop_years_list_municipality.csv")
  original_pop <- original_pop %>% filter(Year == 2018)
  original_pop <- original_pop %>% group_by(Mun) %>% summarise(total_pop = sum(Population)) %>% ungroup() %>% rename(mun = Mun) %>% mutate(mun = factor(mun))
  
  col_tmp <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry) %>% rename(mun = Code) %>% mutate(mun = factor(mun))
  col_tmp <- col_tmp %>% mutate(area = st_area(geometry))
  col_tmp <- col_tmp %>% left_join(y = original_pop, by = "mun")
  col_tmp <- col_tmp %>% mutate(pop_density = (total_pop / area)) %>% arrange(desc(pop_density)) %>% select(-geometry)
  col_tmp <- col_tmp %>% left_join(y = geo_info[, c("mun", "mun_name", "dep_name")], by = "mun") %>% select(mun, mun_name, dep_name, total_pop, area, pop_density) %>% rename(population = total_pop)
  
  less_dense_mun <- col_tmp %>% tail(n_excluded_muns) %>% select(mun) %>% c() %>% unlist() %>% unname() %>% as.character() %>% as.numeric()
  
  # Order the municipalities based on `something`
  # POVERTY (`MPM`)
  
  MPM <- geo_info[, c("mun", "MPM")]
  MPM <- MPM  %>% arrange(MPM)
  if (exclude_low_density) { MPM <- MPM %>% filter(!(as.numeric(as.character(mun)) %in% less_dense_mun)) }
  
  n_mun <- MPM %>% nrow()
  n_per <- floor(n_mun / 5)
  
  MPM$quint_MPM <- c(rep(1:5, each = n_per), rep(5, n_mun - (n_per * 5)))
  MPM <- MPM %>% mutate(quint_MPM = factor(x = quint_MPM, labels = CAT_MPM, levels = 1:5))
  MPM <- MPM %>% select(mun, quint_MPM) %>% rename(MPM = quint_MPM)
}

##################################################
# Exclude municipalities with small population
##################################################

valid_muns      <- geo_info[(!geo_info$mun %in% less_dense_mun), "mun"] %>% c() %>% unlist() %>% unname()
geo_info        <- geo_info %>% filter(mun %in% valid_muns)
population      <- population %>% filter(loc %in% valid_muns) 
mortality_rates <- mortality_rates %>% filter(loc %in% valid_muns) 
fertility_rates <- fertility_rates %>% filter(loc %in% valid_muns) 

# ##################################################
# # Convert it to the desired resolution
# ##################################################
# 
# tmp_data <- convert_resolution(mortality_rates = mortality_rates, fertility_rates = fertility_rates, population = population, death_count = death_count, type.input = type.input)
# 
# mortality_rates <- tmp_data$mortality_rates
# fertility_rates <- tmp_data$fertility_rates
# population      <- tmp_data$population

##################################################
# Summarize quantities
##################################################

yys_all <- 1998:2021 # 1998:2021
zero_one <- function (x, ...) { ((x - min(x)) / diff(range(x))) }

for (yys in yys_all) {
  
  # yys <- 1998:2021
  
  n_mun <- geo_info %>% nrow()
  n_per <- floor(n_mun / 5)
  
  ordering <- c(rep(1:5, each = n_per), rep(5, n_mun - (n_per * 5)))
  mortality_rates_summ <- mortality_rates %>% filter(year %in% yys) %>% select(gender, loc, year, age, death_rate    ) %>% group_by(gender, age, loc) %>% summarize(death_rate     = mean(death_rate))     %>% arrange(death_rate, .by_group = TRUE) %>% ungroup()
  fertility_rates_summ <- fertility_rates %>% filter(year %in% yys) %>% select(gender, loc, year, age, fertility_rate) %>% group_by(gender, age, loc) %>% summarize(fertility_rate = mean(fertility_rate)) %>% arrange(fertility_rate, .by_group = TRUE) %>% ungroup()
  
  mortality_rates_summ$quint_mort <- rep(ordering, 19)
  fertility_rates_summ$quint_fert <- rep(ordering, 18)
  if (FALSE) {
    mortality_rates_summ <- mortality_rates_summ %>% mutate(death_rate     = zero_one(death_rate))
    fertility_rates_summ <- fertility_rates_summ %>% mutate(fertility_rate = zero_one(fertility_rate))
  }
  
  geo_info <- read_csv(file = "DATA/geo_info.csv", col_types = "fcfcfcfcfcfcf")
  geo_info <- geo_info %>% filter(mun %in% valid_muns)
  geo_info <- geo_info %>% select(mun, PDET, MPM_cat) %>% mutate(loc = mun)
  mortality_rates_summ <- mortality_rates_summ %>% left_join(y = geo_info, by = "loc")
  fertility_rates_summ <- fertility_rates_summ %>% left_join(y = geo_info, by = "loc")
  
  plot_box_mun <- function (data, group.type = "PDET", ...) {
    
    # PDET
    COL_PDET <- c("#43A278", "#D36167")
    CAT_PDET <- c("No PDET", "PDET")
    names(COL_PDET) <- CAT_PDET
    # Poverty
    COL_PVRT <- rev(pal_frontiers()(5)) 
    CAT_PVRT <- c("[00%,  20%)", "[20%,  40%)", "[40%,  60%)", "[60%,  80%)", "[80%, 100%]")
    names(COL_PVRT) <- CAT_PVRT
    
    data_fem <- data %>% filter(gender == "Female") %>% arrange(mun)
    data_mal <- data %>% filter(gender == "Male"  ) %>% arrange(mun)
    
    data_fem <- data_fem %>% mutate(age = factor(x = age, levels = sort(unique(data_fem$age))))
    data_mal <- data_mal %>% mutate(age = factor(x = age, levels = sort(unique(data_mal$age))))
    
    data <- bind_rows(data_fem, data_mal)
    data <- data %>% mutate(gender = factor(x = gender, levels = c("Female", "Male")))
    data <- data %>% mutate(PDET = factor(x = PDET, labels = CAT_PDET, levels = 0:1))
    data <- data %>% mutate(MPM_cat = factor(x = MPM_cat, labels = CAT_PVRT, levels = 1:5))
    
    if (sum("quint_mort" %in% colnames(data))) {
      var_name <- "quint_mort"
      data <- data %>% mutate(quint_mort = factor(x = quint_mort, labels = CAT_PVRT, levels = 1:5))
    } else {
      var_name <- "quint_fert"
      data <- data %>% mutate(quint_mort = factor(x = quint_fert, labels = CAT_PVRT, levels = 1:5))
    }
    
    if (group.type == "PDET") {
      CAT <- CAT_PDET
      COL <- COL_PDET
      VAR <- "PDET"
    } else {
      CAT <- CAT_PVRT
      COL <- COL_PVRT
      VAR <- "MPM_cat"
    }
    
    if (sum(colnames(data) == "death_rate") == 1) {
      RES <- "death_rate"
      y_lab <- "Death rate"
    } else {
      RES <- "fertility_rate"
      y_lab <- "Fertility rate"
    }
    
    # Compute percentiles for custom boxplot
    data_errorbar <- data %>% group_by(gender, age, .data[[VAR]]) %>% summarise(as_tibble_row(quantile(.data[[RES]], probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
    tmp_mm        <- data %>% group_by(gender, age, .data[[VAR]]) %>% summarise(mm = mean(.data[[RES]], na.rm = TRUE))
    data_errorbar$mm <- tmp_mm$mm
    
    # pp <- ggplot(data) +
    #   aes(x = age, y = .data[[RES]], fill = .data[[VAR]]) + 
    #   geom_boxplot(width = 0.5) +
    #   # geom_boxplot(outlier.shape = NA, width = 0.5) +
    #   # geom_point(data = function(x) subset(x, outlier_lwr | outlier_upr), position = position_jitter(w = 0.175, h = 0), size = 0.5) + # Outliers
    #   scale_fill_manual(values = COL, name = "") +
    #   facet_wrap(~ gender, ncol = 1, strip.position = "right") +
    #   labs(x = "", y = y_lab, title = ifelse(length(yys) == 1, yys, paste(head(yys, 1), "-", tail(yys, 1), sep = ""))) +
    #   theme_bw() + 
    #   theme(text = element_text(size = 12, family = "LM Roman 10"), 
    #         plot.title = element_text(size = 10),
    #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    #         strip.background = element_rect(fill = NA, color = NA))
    
    pp <- ggplot(data_errorbar) +
      aes(x = age, fill = .data[[VAR]]) +
      geom_boxplot(aes(ymin = `2.5%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `97.5%`), stat = "identity", width = 0.75) +
      # geom_point(aes(y = mm), color = "red", shape = 19, size = 2) +
      scale_fill_manual(values = COL, name = "") +
      facet_wrap(~ gender, ncol = 1, strip.position = "right") +
      labs(x = "", y = y_lab, title = ifelse(length(yys) == 1, yys, paste(head(yys, 1), "-", tail(yys, 1), sep = ""))) +
      theme_bw() +
      theme(text = element_text(size = 12, family = "LM Roman 10"),
            plot.title = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.background = element_rect(fill = NA, color = NA))
    
    pp
  }
  
  # mortality_rates_summ_mod <- mortality_rates_summ %>% filter(age != "15-19", loc != 91460)
  
  tb_PDET <- table(mortality_rates_summ$PDET   ); tb_PDET <- tb_PDET / sum(tb_PDET) 
  tb_PVRT <- table(mortality_rates_summ$MPM_cat); tb_PVRT <- tb_PVRT / sum(tb_PVRT) 
  
  #####
  # Redefine the MPM based on the ordered municipalities
  #####
  
  mortality_rates_summ <- mortality_rates_summ %>% left_join(y = MPM, by = "mun")
  mortality_rates_summ$MPM <- as.integer(mortality_rates_summ$MPM)
  mortality_rates_summ <- mortality_rates_summ %>% select(-MPM_cat) %>% rename(MPM_cat = MPM)
  
  fertility_rates_summ <- fertility_rates_summ %>% left_join(y = MPM, by = "mun")
  fertility_rates_summ$MPM <- as.integer(fertility_rates_summ$MPM)
  fertility_rates_summ <- fertility_rates_summ %>% select(-MPM_cat) %>% rename(MPM_cat = MPM)
  
  #####
  #####
  #####
  
  p_mort_PDET <- plot_box_mun(data = mortality_rates_summ, group.type = "PDET")
  p_mort_PVRT <- plot_box_mun(data = mortality_rates_summ, group.type = "PVRT")
  p_fert_PDET <- plot_box_mun(data = fertility_rates_summ, group.type = "PDET")
  p_fert_PVRT <- plot_box_mun(data = fertility_rates_summ, group.type = "PVRT")
  
  yy_name <- ifelse(length(yys) == 1, yys, paste(head(yys, 1), "-", tail(yys, 1), sep = ""))
  
  ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/RATES/", yy_name, "_p_mort_PDET.jpeg", sep = ""), plot = p_mort_PDET, width = 3000, height = 2000, units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/RATES/", yy_name, "_p_mort_PVRT.jpeg", sep = ""), plot = p_mort_PVRT, width = 3000, height = 2000, units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/RATES/", yy_name, "_p_fert_PDET.jpeg", sep = ""), plot = p_fert_PDET, width = 3000, height = 2000, units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/RATES/", yy_name, "_p_fert_PVRT.jpeg", sep = ""), plot = p_fert_PVRT, width = 3000, height = 2000, units = c("px"), dpi = 300, bg = "white")
  
}

