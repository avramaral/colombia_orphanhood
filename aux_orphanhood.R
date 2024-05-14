##############################
# Convert rates to the desired geographical resolution (e.g., Municipality, Department, Region, or National)
##############################

convert_resolution <- function (geo_info, mortality_rates, fertility_rates, population, death_count, type.input, ...) {

  # Number of individuals in `15-19` per age
  pop_2018_children <- create_pop_15_19()
  pop_2018_children$mun <- factor(pop_2018_children$mun)
  
  valid_muns <- unname(unlist(c(geo_info$mun)))
  pop_2018_children <- pop_2018_children %>% filter(mun %in% valid_muns)
  
  # # Proportion of `15-19` that were `15-17`
  # prop_15_17 <- read_csv(file = "DATA/Population from Census/2018_prop_15_17_mun.csv", col_types = cols()); colnames(prop_15_17) <- c("mun", "prop")
  # prop_15_17$mun <- factor(prop_15_17$mun)
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  if (type.input != "Municipality") {
    
    mortality_rates <- mortality_rates %>% rename(mun = loc)
    fertility_rates <- fertility_rates %>% rename(mun = loc)
    
    mortality_rates <- left_join(x = mortality_rates, y = geo_info, by = c("mun")) %>% select(gender, all_of(tmp_loc), year, age, deaths, population, death_rate)
    fertility_rates <- left_join(x = fertility_rates, y = geo_info, by = c("mun")) %>% select(gender, all_of(tmp_loc), year, age, births, population, fertility_rate)
    
    mortality_rates <- mortality_rates %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(deaths = sum(deaths), population = sum(population), death_rate = sum(death_rate)) %>% ungroup()
    fertility_rates <- fertility_rates %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(births = sum(births), population = sum(population), fertility_rate = sum(fertility_rate)) %>% ungroup()
    
    mortality_rates <- mortality_rates %>% mutate(death_rate     = compute_rate(count = mortality_rates$deaths, pop = mortality_rates$population) * ifelse(per1K, 1000, 1))
    fertility_rates <- fertility_rates %>% mutate(fertility_rate = compute_rate(count = fertility_rates$births, pop = fertility_rates$population) * ifelse(per1K, 1000, 1))
    
    mortality_rates <- mortality_rates %>% rename(loc = all_of(tmp_loc))
    fertility_rates <- fertility_rates %>% rename(loc = all_of(tmp_loc))
    
    population <- population %>% rename(mun = loc)
    population <- left_join(x = population, y = geo_info, by = c("mun")) %>% select(gender, all_of(tmp_loc), year, age, population)
    population <- population %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(population = sum(population)) %>% ungroup()
    population <- population %>% rename(loc = all_of(tmp_loc))
    
    death_count <- left_join(x = death_count, y = geo_info, by = c("mun")) %>% select(gender, all_of(tmp_loc), year, deaths, age)
    death_count <- death_count %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(deaths = sum(deaths)) %>% ungroup() %>% select(gender, all_of(tmp_loc), year, deaths, age)
    death_count <- death_count %>% rename(loc = all_of(tmp_loc))
    
    ##############################
    
    pop_2018_children <- left_join(x = pop_2018_children, y = geo_info, by = c("mun")) %>% select(all_of(tmp_loc), age_group, age, population) %>% group_by(.data[[tmp_loc]], age_group, age) %>% summarise(population = sum(population)) %>% ungroup()
  }
  
  ##############################
  
  pop_2018_children <- pop_2018_children %>% group_by(.data[[tmp_loc]]) %>% mutate(total = sum(population))
  pop_2018_children <- pop_2018_children %>% filter(age <= 17)
  pop_2018_15_17 <- pop_2018_children %>% group_by(.data[[tmp_loc]], total) %>% summarize(population = sum(population)) %>% ungroup()
  pop_2018_15_17 <- pop_2018_15_17 %>% mutate(prop = (population / total)) %>% select(-c(total, population))
  colnames(pop_2018_15_17) <- c("loc", "prop")
  
  ##############################
  
  list(mortality_rates = mortality_rates, fertility_rates = fertility_rates, population = population, death_count = death_count, prop_15_17 = pop_2018_15_17)
}

##############################
# Create file, at the municipality level, with the number of children in `15-19` per age
##############################

create_pop_15_19 <- function (file_name = NA, ...) {
  if (is.na(file_name)) {
    file_name <- "DATA/Population from Census/pop_15_19_mun.csv"
  }
  if (!file.exists(file_name)) {
    pop_2018 <- as_tibble(read_xlsx(path = "DATA/Population from Census/2018 Census/PERSONAS_DEMOGRAFICO_Cuadros_CNPV_2018.xlsx", sheet = "3PM"))
    pop_2018 <- pop_2018[(156:nrow(pop_2018)), ]
    pop_2018 <- pop_2018[, -c(6:ncol(pop_2018))]
    colnames(pop_2018) <- c("dep", "mun", "age_group", "age", "population")
    pop_2018 <- pop_2018 %>% mutate(dep = na.locf(dep), mun = na.locf(mun))
    
    rows_15 <- which(pop_2018$age_group == "15 a 19")
    pop_2018_children <- pop_2018[as.vector(sapply(rows_15, function(i) i:(i + 4))), ]
    pop_2018_children$population <- as.numeric(pop_2018_children$population)
    pop_2018_children <- pop_2018_children %>% select(-dep) %>% mutate(mun = as.factor(as.numeric(sub("^(.*?)_.*", "\\1", mun))), age = as.numeric(age))
    
    write_csv(x = pop_2018_children, file = file_name)
  } else {
    pop_2018_children <- read_csv(file = file_name, col_types = cols())
  }
  
  pop_2018_children
}  

##################################################
# FUNCTIONS FROM ORIGINAL PROJECT
##################################################

########################################
# CHILDREN
########################################

##############################
# Process the number of children (general function)
##############################

process_number_children_year <- function (yy, type.input, fertility_rates, per1K = TRUE, ...) {
  
  ################
  ##### Male #####
  ################
  
  data_m <- fertility_rates %>% filter(gender == "Male")
  data_m$fertility_rate <- data_m$fertility_rate / ifelse(per1K, 1000, 1)
  
  locs <- unique(data_m$loc)
  
  print("Processing children of male individuals...")
  count <- 1
  if (type.input != "National") { pb <- txtProgressBar(min = 1, max = length(locs), initial = 1) }
  for (l in locs) {
    tmp <- data_m %>% filter(loc == l)
    group <- paste("col_", gsub(" ", "-", yy), "_", gsub(" ", "-", l), sep = "")
    
    process_children_father_55_plus_year(type.input = type.input, yy = yy, group = group, data_m = tmp)
    
    count <- count + 1
    if (type.input != "National") { setTxtProgressBar(pb, count) }
  }
  if (type.input != "National") { close(pb) }
  
  ##################
  ##### Female #####
  ##################
  
  data_f <- fertility_rates %>% filter(gender == "Female")
  data_f$fertility_rate <- data_f$fertility_rate / ifelse(per1K, 1000, 1)
  
  locs <- unique(data_f$loc)
  
  print("Processing children of female individuals...")
  count <- 1
  if (type.input != "National") { pb <- txtProgressBar(min = 1, max = length(locs), initial = 1) }
  for (l in locs) {
    tmp <- data_f %>% filter(loc == l)
    group <- paste("col_", gsub(" ", "-", yy), "_", gsub(" ", "-", l), sep = "")
    
    process_children_all_year(type.input = type.input, yy = yy, group = group, data_f = tmp)
    # process_fertility_plots_year(in.dir = in.dir, prj.dir = prj.dir, cur.yr = yy, type.input = type.input, group = group, d = m)
    
    count <- count + 1
    if (type.input != "National") { setTxtProgressBar(pb, count) }
  }
  if (type.input != "National") { close(pb) }
}

##############################
# Process the number of children for male individuals
##############################

process_children_father_55_plus_year <- function (type.input, yy, group, data_m, ...) {
  
  children = matrix(rep(0, 100 * 18), nrow = 100)
  names(children) = paste0(seq(0:17), "years")
  
  children[100, 18:1] <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[99, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[98, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[97, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[96, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[95, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[94, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[93, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[92, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[91, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[90, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[89, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[88, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[87, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[86, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[85, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[84, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[83, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[82, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[81, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[80, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[79, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[78, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[77, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[76, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[75, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[74, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[73, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  children[72, 18:1]  <-  c(data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy - 17,yy))])
  
  children[71, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year == yy - 17)],
                          data_m$fertility_rate[which(data_m$age == "55+"   & data_m$year %in% seq(yy - 16, yy - 12))] ,
                          data_m$fertility_rate[which(data_m$age == "55+"   & data_m$year %in% seq(yy - 11,yy - 7))],
                          data_m$fertility_rate[which(data_m$age == "55+"   & data_m$year %in% seq(yy - 6, yy - 2))], 
                          data_m$fertility_rate[which(data_m$age == "55+"   & data_m$year %in% seq(yy - 1, yy))])   
  
  children[70, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year %in% seq(yy - 17, yy - 16))] ,
                          data_m$fertility_rate[which(data_m$age == "55+"   & data_m$year %in% seq(yy - 15, yy - 11))] ,
                          data_m$fertility_rate[which(data_m$age == "55+"   & data_m$year %in% seq(yy - 10, yy - 6))],
                          data_m$fertility_rate[which(data_m$age == "55+"   & data_m$year %in% seq(yy - 5,  yy - 1))], 
                          data_m$fertility_rate[which(data_m$age == "55+"   & data_m$year == yy)]) 
  
  children[69, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-17,yy-15))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-14, yy-10))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-9,yy-5))],
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-4, yy))]) 
  
  children[68, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-17,yy-14))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-13, yy-9))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-8,yy-4))],
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-3, yy))])
  
  children[67, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-17,yy-13))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-12, yy-8))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-7,yy-3))],
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-2, yy))]) 
  
  children[66, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-17)]  ,
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-16,yy-12))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-11, yy-7))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-6,yy-2))],
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-1, yy))])
  
  children[65, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year%in% seq(yy-17,yy-16))]  ,
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-15,yy-11))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-10, yy-6))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-5,yy-1))],
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year == yy)])
  
  children[64, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year%in% seq(yy-17,yy-15))] ,
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-14,yy-10))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-9, yy-5))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-4,yy))])
  
  
  children[63, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year%in% seq(yy-17,yy-14))] ,
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-13,yy-9))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-8, yy-4))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-3,yy))])
  
  children[62, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-17, yy-13))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-12,yy-8))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-7, yy-3))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-2,yy))])
  
  children[61, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-17)], 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-16, yy-12))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-11,yy-7))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-6, yy-2))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year %in% seq(yy-1,yy))])
  
  children[60, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-17, yy-16))]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-15, yy-11))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-10,yy-6))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-5, yy-1))] ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year == yy)])
  
  children[59, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-17, yy-15))]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-14, yy-10))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-9,yy-5))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-4, yy))])
  
  children[58, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-17, yy-14))], 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-13, yy-9))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-8,yy-4))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-3, yy))])
  
  children[57, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-17, yy-13))] , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-12, yy-8))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-7,yy-3))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-2, yy))])
  
  
  children[56, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-16, yy-12))] , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-11, yy-7))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-6,yy-2))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year%in% seq(yy-1, yy))])
  
  children[55, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year %in% seq(yy-17, yy-16))] , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-15, yy-11))] , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-10, yy-6))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-5,yy-1))]  ,
                          data_m$fertility_rate[which(data_m$age == "55+" & data_m$year == yy)])
  
  children[54, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-4,yy))])
  
  children[53, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)], 
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-3,yy))])
  
  children[52, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-2,yy))])
  
  children[51, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-1,yy))])
  
  children[50, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year == yy)])
  
  children[49, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[48, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[47, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[46, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[45, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[44, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[43, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[42, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[41, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[40, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[39, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[38, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[37, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[36, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[35, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[34, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[33, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[32, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[31, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[30, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[29, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] ,
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[28, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[27, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[26, 17:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[25, 16:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[24, 15:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[23, 14:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[22, 13:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[21, 12:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[20, 11:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[19, 10:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[18, 9:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] ,
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[17, 8:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] ,
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[16, 7:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] ,
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[15, 6:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] ,
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[14, 5:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )
  
  children[13, 4:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )
  
  children[12, 3:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )
  
  children[11, 2:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )
  
  children[10, 1]   <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )  
  
  children <- as.data.frame(children)
  names(children) = paste(seq(0:17) - 1, " years", sep = "")
  
  write_csv(x = children, file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_child_raw_m.csv", sep = ""))
              
  plot_c <- as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age <- rep(1:100, 18)
  plot_c$child_age  <- sort(rep(seq(18) - 1, 100))
  setnames(plot_c, 1, "prob")
  plot_c$gender = "male"
  
  write_csv(x = plot_c, file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_m.csv", sep = ""))
              
  ddf <- as.data.frame(apply(children, 1, sum))
  names(ddf) <- "children"
  ddf$gender <- "male"
  ddf$age <- 1:100
  
  write_csv(x = ddf, file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_children_m.csv", sep = ""))
}

##############################
# Process the number of children for female individuals
##############################

process_children_all_year <- function (type.input, yy, group, data_f, ...) {
  
  children <- matrix(rep(0, 100 * 18), nrow = 100)
  names(children) <- paste(seq(0:17), "years", sep = "")
  
  children[66, 18]    <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-17)]) 
  children[65, 18:17] <- data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-17,yy-16))]
  children[64, 18:16] <- data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-17, yy-15))]
  children[63, 18:15] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-17, yy-14))])
  children[62, 18:14] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-17, yy-13))])
  children[61, 18:13] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-17)], data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-16, yy-12))])
  
  #####
  
  children[60, 18:12] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-17, yy-16))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-15, yy-11))])
  #####
  
  children[59, 18:11] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-17, yy-15))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-14, yy-10))])
  
  
  children[58, 18:10] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-17, yy-14))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-13, yy-9))])
  
  
  children[57, 18:9] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-17, yy-13))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-12, yy-8))])
  
  
  children[56, 18:8] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-17)], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-16, yy-12))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-11, yy-7))])
  
  
  children[55, 18:7] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-17, yy-16))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-15, yy-11))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-10, yy-6))])
  
  children[54, 18:6] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-17, yy-15))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-14, yy-10))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-9, yy-5))])
  
  children[53, 18:5] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-17, yy-14))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-13, yy-9))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-8, yy-4))])
  
  children[52, 18:4] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-17, yy-13))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-12, yy-8))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-7, yy-3))])
  
  children[51, 18:3] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-17)], 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-16, yy-12))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-11, yy-7))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-6, yy-2))])
  
  children[50, 18:2] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year %in% seq(yy-17,yy-16))],
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-15, yy-11))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-10, yy-6))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-5, yy-1))])
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-17)], 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)]) 
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-17)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-17)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-12)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)],
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[42,18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[41,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[40,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[39,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[38,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[37,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[36,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[35,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[34,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[33,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[32,18:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[31,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[30,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[29,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[28,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[27,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[26,17:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[25,16:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[24,15:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[23,14:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[22,13:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[21,12:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[20,11:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[19,10:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[18,9:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[17,8:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[16,7:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[15,6:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[14,5:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children[13,4:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children[12,3:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children[11,2:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children[10,1] <- c(  data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children <- as.data.frame(children)
  names(children) <- paste(seq(0:17) - 1, " years", sep = "")
  write_csv(x = children, file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_child_raw_f.csv", sep = ""))
  
  plot_c <- as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$mother_age <- rep(1:100, 18)
  plot_c$child_age <- sort(rep(seq(18) - 1, 100))
  setnames(plot_c, 1, "prob")
  plot_c$gender <- "female"
  write_csv(x = plot_c, file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_f.csv", sep = ""))
  
  setnames(plot_c, "mother_age", "parents_age")
  plott <- read.csv(paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_m.csv", sep = ""))
  setnames(plott, "father_age", "parents_age")
  plot_all <- rbind(plot_c, plott)
  write_csv(x = plot_all, file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_both.csv", sep = ""))

  ddf <- as.data.frame(apply(children, 1, sum))
  names(ddf) <- "children"
  ddf$gender <- "female"
  ddf$age <- 1:100
  write_csv(x = ddf, file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_children_f.csv", sep = ""))
  
  ddf   <- read.csv(paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_children_f.csv", sep = ""))
  ddf_2 <- read.csv(paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_children_m.csv", sep = ""))
  # Truncate fertility for men for analysis
  ddf_2$children[ddf_2$age > 77] <- 0
  ddf <- rbind(ddf, ddf_2)
  write_csv(x = ddf, file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_children.csv", sep = ""))
}

########################################
# ORPHANS
########################################

##############################
# Process the number of orphans (general function)
##############################

process_nb_orphans_table_dep_national_year <- function (yy, type.input, death_count, ...) {
  
  d_deaths <- death_count %>% filter(year == yy)
  d_deaths <- na.omit(d_deaths)
  d_deaths <- as.data.table(d_deaths)
  
  locs <- unique(d_deaths$loc)
  i <- 0
  dor <- vector('list', length(unique(locs)))
  dor.age <- vector('list', length(unique(locs)))
  
  for (l in locs) {
    
    # Process the orphans by age of adults
    i <- i + 1
    tmp <- d_deaths[loc == l]
    # If due to suppression issue, the subset table contains no data, then we skip that
    if (nrow(tmp) > 0) {
      group <- paste0("col", "_", gsub(' ', '-', yy), "_", gsub(' ', '-', l))
      
      # print(paste("Processing orphans by age of parents in file: ", group, "...", sep = ""))
      out <- process_orphans_dep_national(d_merge = tmp, group = group)
      dor[[i]] <- out$d_age
      
      # print(paste("Processing orphans by age of children ...", sep = ""))
      out.age <- process_orphans_with_age(d_merge = tmp, group = group, l = l)
      dor.age[[i]] <- out.age$d_age
      
    }
  }
  
  tmp <- data.table::rbindlist(dor, use.names = TRUE, fill = TRUE)
  tmp.age <- data.table::rbindlist(dor.age, use.names = TRUE, fill = TRUE)
  
  write_csv(x = tmp,     file = paste("PROCESS_DATA/DATA/RESULTS/", toupper(type.input), "/", type.input, "_parents_deaths_orphans_summary_",          yy, ".csv", sep = ""))
  write_csv(x = tmp.age, file = paste("PROCESS_DATA/DATA/RESULTS/", toupper(type.input), "/", type.input, "_parents_deaths_orphans_with_age_summary_", yy, ".csv", sep = ""))
}

##############################
# Process the number of orphans without age
##############################

process_orphans_dep_national <- function (d_merge, group, ...) {
  
  d_children <- as.data.table(read_csv(file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_children.csv", sep = ""), col_types = cols()))
  # Age range of parents based on the deaths data
  d_children[, age := age %/% 5]
  d_children[, age := paste0(age * 5, '-' , (age + 1) * 5 - 1)]
  d_children[, age := ifelse(age %in% c('0-4', '5-9'), '0-9', ifelse(age %in% c('85-89', '90-94', '95-99', '100-104'), '85+', age))]
  
  # Truncate male fertility to 59 (so no men over 76 have children under 18)
  d_children$ageid <- rep(seq(1, 100, 1), 2)
  d_children[gender == 'male' & ageid > 76, children := 0]
  d_children[, ageid := NULL]
  
  # Truncate female fertility to 49 (so no women over 66 have children under 18)
  d_children$ageid <- rep(seq(1, 100, 1), 2)
  d_children[gender == 'female' & ageid > 66, children := 0]
  d_children[, ageid := NULL]
  
  d_children <- d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>% select(-children) %>% ungroup() %>% distinct()
  if ('sex' %in% colnames(d_merge)) {
    d_merge$gender <- as.character(d_merge$sex)
  } else{
    d_merge$gender <- as.character(d_merge$gender)
    
  }
  d_children$gender <- ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children <- as.data.table(d_children)
  d_m1 <- merge(d_merge, d_children, by = c('age', 'gender'), all.x = T)
  d_m1 <- as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age <- factor(d_m1$age, levels = c("0-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39" ,"40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
  # d_m1[!is.na(d_m1$age), ]
  write_csv(x = d_m1, file = paste("PROCESS_DATA/DATA/RESULTS/", toupper(type.input), "/parents_deaths_orphans_", group, ".csv", sep = ""))
  
  d_summary <- d_m1 %>% select(age, gender, loc, deaths, orphans)
  d_summary$age <- as.character(d_summary$age)
  # Merge to age groups for each state
  d_summary$age <- ifelse(d_summary$age %in% c("15-19", "20-24", "25-29"), '15-29', ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), '30-64', ifelse(d_summary$age %in% c('0-14'), '0-14', '65+')))
  
  d_summary <- as.data.table(d_summary)
  d_summary <- d_summary[, list(deaths = round(sum(deaths, na.rm = TRUE)), nb_orphans = round(sum(orphans, na.rm = TRUE))), by = c('loc', 'age', 'gender')]
  
  list(d_summary = d_summary, d_age = d_m1)
}

##############################
# Process the number of orphans with age
##############################

process_orphans_with_age <- function(d_merge, group, ...) {
  
  # Additional analysis: age of orphans estimation
  # All contains the huge matrix which are really useful to explore the age of children
  d_children <- as.data.table(read_csv(file = paste("PROCESS_DATA/DATA/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_both.csv", sep = ""), col_types = cols()))
  
  # Age range of parents based on the deaths data
  d_children[, age := parents_age %/% 5]
  d_children[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  d_children[, age := ifelse(age %in% c('0-4', '5-9'), '0-9', ifelse(age %in% c('85-89', '90-94', '95-99', '100-104'), '85+', age))]
  
  # Truncate male fertility to 59 (so no men over 76 have children under 18)
  d_children[gender == 'male' & parents_age > 76, prob := 0]
  # Truncate female fertility to 49 (so no women over 66 have children under 18)
  d_children[gender == 'female' & parents_age > 66, prob := 0]
  
  d_children <- d_children[, list(nb_c = mean(prob)), by = c('age', 'gender', 'child_age')]
  if ('sex' %in% colnames(d_merge)) {
    d_merge$gender <- as.character(d_merge$sex)
  } else {
    d_merge$gender <- as.character(d_merge$gender)
    
  }
  d_children$gender <- ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children <- as.data.table(d_children)
  d_m1 <- merge(d_merge, d_children, by = c('age', 'gender'), all.x = T, allow.cartesian = T)
  d_m1 <- as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age <- factor(d_m1$age, levels = c("0-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39" ,"40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
  
  d_summary <- d_m1 %>% select(age, gender, child_age, loc, deaths, orphans)
  d_summary$age <- as.character(d_summary$age)
  # Merge to age groups for each state
  d_summary$age <- ifelse(d_summary$age %in% c("15-19", "20-24", "25-29"), '15-29', ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), '30-64', ifelse(d_summary$age %in% c('0-14'), '0-14', '65+')))
  setnames(d_summary, 'child_age', 'age.children')
  d_summary <- as.data.table(d_summary)
  d_summary <- d_summary[, list(deaths = round(sum(deaths, na.rm = T)), nb_orphans = round(sum(orphans, na.rm = T))), by = c('loc', 'age', 'gender', 'age.children')]
  
  list(d_summary = d_summary, d_age = d_m1)
}

########################################
# OUTPUT
########################################

##############################
# Create table with number of orphans
##############################

orphan_table <- function (type.input, population, prop_15_17, geo_info, per_n_children = 100000, ...) {
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  infiles <- (list.files(path = paste("PROCESS_DATA/DATA/RESULTS/", toupper(type.input), sep = ""), pattern = paste(type.input, "_parents_deaths_orphans_summary", sep = ""), full.names = TRUE, recursive = FALSE))
  orphans_all_years <- data.table()
  for (i in seq_len(length(infiles))) {
    infile <- infiles[i]
    orphans_all_years <- rbind(orphans_all_years, data.table(read.csv(infile)))
  }
  
  orphans_locs_years <- orphans_all_years[, list(orphans = sum(orphans)), by = c("loc", "year")]
  orphans_locs_years <- as_tibble(orphans_locs_years) %>% mutate({{tmp_loc}} := loc) %>% select(all_of(tmp_loc), year, orphans)
  orphans_locs_years[, tmp_loc] <- factor(unname(unlist(c(orphans_locs_years[, tmp_loc]))))
  cp_orphans_locs_years <- orphans_locs_years; colnames(cp_orphans_locs_years) <- c("loc", "year", "orphans") # For later use
  tmp_geo_info <- geo_info[, c(tmp_loc, paste(tmp_loc, "_name", sep = ""))] %>% unique
  if (type.input != "Municipality") {
    orphans_locs_years <- left_join(x = orphans_locs_years, y = tmp_geo_info, by = tmp_loc) %>% select(all_of(paste(tmp_loc, "_name", sep = "")), year, orphans)
  } else {
    orphans_locs_years <- left_join(x = orphans_locs_years, y = tmp_geo_info, by = tmp_loc) %>% mutate(mun_name = paste(mun, " ", mun_name, sep = "")) %>% select(all_of(paste(tmp_loc, "_name", sep = "")), year, orphans)
  }
  
  orphans_table <- orphans_locs_years %>%  pivot_wider(names_from = year, values_from = orphans)
  setnames(orphans_table, paste(tmp_loc, "_name", sep = ""), type.input)
  write_csv(x = orphans_table, file = paste("PROCESS_DATA/DATA/RESULTS/orphans_by_year_", tmp_loc, ".csv", sep = ""))
  
  ##############################
  ##############################
  
  tmp_population <- population %>% filter(age %in% c("0-9", "10-14", "15-19"))
  tmp_population <- left_join(x = tmp_population, y = prop_15_17, by = c("loc"))
  tmp_population <- tmp_population %>% mutate(population = ifelse(age == "15-19", population * prop, population)) %>% select(-prop)
  tmp_population <- tmp_population %>% group_by(loc, year) %>% summarize(children = sum(population)) %>% ungroup()
  
  orphans_and_rate <- left_join(x = cp_orphans_locs_years, y = tmp_population, by = c("loc", "year"))
  orphans_and_rate <- orphans_and_rate %>% mutate(orphan_rate = ceiling(orphans * per_n_children / children))
  write_csv(x = orphans_and_rate, file = paste("PROCESS_DATA/DATA/RESULTS/orphans_and_rate_by_year_", tmp_loc, "_list_per_", as.integer(per_n_children), ".csv", sep = ""))
  
  colnames(orphans_and_rate)[1] <- tmp_loc
  if (type.input != "Municipality") {
    orphans_per_child <- left_join(x = orphans_and_rate, y = tmp_geo_info, by = tmp_loc) %>% select(all_of(paste(tmp_loc, "_name", sep = "")), year, orphan_rate)
  } else {
    orphans_per_child <- left_join(x = orphans_and_rate, y = tmp_geo_info, by = tmp_loc) %>% mutate(mun_name = paste(mun, " ", mun_name, sep = "")) %>% select(all_of(paste(tmp_loc, "_name", sep = "")), year, orphan_rate)
  }
  orphans_rate_table <- orphans_per_child %>% pivot_wider(names_from = year, values_from = orphan_rate) 
  write_csv(x = orphans_rate_table, file = paste("PROCESS_DATA/DATA/RESULTS/orphans_rate_by_year_", tmp_loc, "_per_", as.integer(per_n_children), ".csv", sep = ""))
}
