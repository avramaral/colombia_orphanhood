
# Make lower case, replace space by "_", and remove accents and special characters
transform_string <- function (x, ...) {
  x <- gsub(" ", "_", x)
  x <- gsub("[^A-Za-z0-9_]", "", x)
  x <- tolower(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x
}

##############################
##############################

convert_resolution_pop <- function (type.input, population, geo_info, valid_muns = NA, ...) {
  
  # Number of individuals in `15-19` per age
  pop_2018_children <- create_pop_15_19()
  pop_2018_children$mun <- factor(pop_2018_children$mun)
  
  if (!is.na(valid_muns[1])) {
    pop_2018_children <- pop_2018_children %>% filter(mun %in% valid_muns)
  }
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  if (type.input != "Municipality") {
    
    population <- population %>% rename(mun = loc)
    population <- left_join(x = population, y = geo_info, by = c("mun")) %>% select(gender, all_of(tmp_loc), year, age, population)
    population <- population %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(population = sum(population)) %>% ungroup()
    population <- population %>% rename(loc = all_of(tmp_loc))
    
    pop_2018_children <- left_join(x = pop_2018_children, y = geo_info, by = c("mun")) %>% select(all_of(tmp_loc), age_group, age, population) %>% group_by(.data[[tmp_loc]], age_group, age) %>% summarise(population = sum(population)) %>% ungroup()
    
  }
  
  ##############################
  
  pop_2018_children <- pop_2018_children %>% group_by(.data[[tmp_loc]]) %>% mutate(total = sum(population))
  pop_2018_children <- pop_2018_children %>% filter(age <= 17)
  pop_2018_15_17 <- pop_2018_children %>% group_by(.data[[tmp_loc]], total) %>% summarize(population = sum(population)) %>% ungroup()
  pop_2018_15_17 <- pop_2018_15_17 %>% mutate(prop = (population / total)) %>% select(-c(total, population))
  colnames(pop_2018_15_17) <- c("loc", "prop")
  
  ##############################
  
  list(population = population, prop_15_17 = pop_2018_15_17)
}

##############################
##############################

generate_incidence_table <- function (type.input, per_n_children, geo_info, should_round = TRUE, ...) {
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  infiles <- (list.files(path = paste("PROCESS_DATA/DATA/RESULTS/", toupper(type.input), sep = ""), pattern = paste(type.input, "_parents_deaths_orphans_summary", sep = ""), full.names = TRUE, recursive = FALSE))
  orphans_all_years <- data.table()
  
  for (i in seq_len(length(infiles))) {
    infile <- infiles[i]
    orphans_all_years <- rbind(orphans_all_years, data.table(read.csv(infile)))
  }
  
  ##############################
  
  orphans_locs_years <- orphans_all_years[, list(orphans = sum(orphans)), by = c("loc", "year", "gender")]
  orphans_locs_years <- as_tibble(orphans_locs_years) %>% mutate({{tmp_loc}} := loc) %>% select(all_of(tmp_loc), gender, year, orphans) %>% arrange({{tmp_loc}}, gender, year)
  orphans_locs_years[, tmp_loc] <- factor(unname(unlist(c(orphans_locs_years[, tmp_loc]))))
  if (type.input == "Municipality") {
    valid_muns <- unname(unlist(c(geo_info$mun)))
    orphans_locs_years <- orphans_locs_years %>% filter(mun %in% valid_muns)
  }
  cp_orphans_locs_years <- orphans_locs_years; colnames(cp_orphans_locs_years) <- c("loc", "gender", "year", "orphans") # For later use
  tmp_geo_info <- geo_info[, c(tmp_loc, paste(tmp_loc, "_name", sep = ""))] %>% unique
  orphans_locs_years <- left_join(x = orphans_locs_years, y = tmp_geo_info, by = tmp_loc) %>% select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = "")), gender, year, orphans) 
  orphans_locs_years <- orphans_locs_years %>% filter((year >= 2015),  (year <= 2021))
  
  ##############################
  
  if (type.input == "Municipality") {
    population <- population %>% filter(loc %in% valid_muns)
    prop_15_17 <- prop_15_17 %>% filter(loc %in% valid_muns)
  }
  
  tmp_population <- population %>% filter(age %in% c("0-9", "10-14", "15-19"))
  tmp_population <- left_join(x = tmp_population, y = prop_15_17, by = c("loc"))
  tmp_population <- tmp_population %>% mutate(population = ifelse(age == "15-19", population * prop, population)) %>% select(-prop)
  tmp_population <- tmp_population %>% group_by(loc, year) %>% summarize(children = sum(population)) %>% ungroup()
  
  orphans_and_rate <- left_join(x = cp_orphans_locs_years, y = tmp_population, by = c("loc", "year"))
  if (should_round) {
    orphans_and_rate <- orphans_and_rate %>% mutate(orphan_rate = ceiling(orphans * per_n_children / children))
  } else {
    orphans_and_rate <- orphans_and_rate %>% mutate(orphan_rate = orphans * per_n_children / children)
  }
  orphans_and_rate <- orphans_and_rate %>% filter((year >= 2015),  (year <= 2021))
  
  colnames(orphans_and_rate)[1] <- tmp_loc
  orphans_per_child <- left_join(x = orphans_and_rate, y = tmp_geo_info, by = tmp_loc) %>% select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = "")), gender, year, orphan_rate)
  
  ##############################
  
  # Convert columns and types (not mandatory)
  
  colnames(orphans_locs_years)[ncol(orphans_locs_years)] <- "n_orp"
  colnames(orphans_per_child)[ncol(orphans_per_child)] <- "n_orp"
  
  orphans_locs_years$n_orp <- as.numeric(orphans_locs_years$n_orp)
  orphans_per_child$n_orp <- as.numeric(orphans_per_child$n_orp)
  
  orphans_locs_years$year <- as.integer(orphans_locs_years$year)
  orphans_per_child$year <- as.integer(orphans_per_child$year)
  
  ##############################
  
  # Check for missing locations
  
  if (type.input == "Municipality") {
    i <- 1
    for (yy in 2015:2021) {
      tmp_orphans_locs_years <- orphans_locs_years %>% filter(year == yy)
      tmp_orphans_per_child  <- orphans_per_child  %>% filter(year == yy)
      for (g in c("Female", "Male")) {
        tmp_tmp_orphans_locs_years <- tmp_orphans_locs_years %>% filter(gender == g)
        tmp_tmp_orphans_locs_years <- right_join(x = tmp_tmp_orphans_locs_years, y = tmp_geo_info, by = c(tmp_loc, paste(tmp_loc, "_name", sep = "")))
        tmp_tmp_orphans_locs_years <- tmp_tmp_orphans_locs_years %>% mutate(gender = g, year = yy, n_orp = ifelse(is.na(n_orp), 0, n_orp))
        
        tmp_tmp_orphans_per_child  <- tmp_orphans_per_child  %>% filter(gender == g)
        tmp_tmp_orphans_per_child  <- right_join(x = tmp_tmp_orphans_per_child , y = tmp_geo_info, by = c(tmp_loc, paste(tmp_loc, "_name", sep = "")))
        tmp_tmp_orphans_per_child  <- tmp_tmp_orphans_per_child  %>% mutate(gender = g, year = yy, n_orp = ifelse(is.na(n_orp), 0, n_orp))
        
        if (i == 1) {
          final_orphans_locs_years <- tmp_tmp_orphans_locs_years
          final_orphans_per_child  <- tmp_tmp_orphans_per_child
        } else {
          final_orphans_locs_years <- bind_rows(final_orphans_locs_years, tmp_tmp_orphans_locs_years)
          final_orphans_per_child  <- bind_rows(final_orphans_per_child , tmp_tmp_orphans_per_child ) 
        }
        i <- i + 1
      }
    }
    
    orphans_locs_years <- final_orphans_locs_years
    orphans_per_child  <- final_orphans_per_child 
  }
  
  list(orphans = orphans_locs_years, orphans_per_child = orphans_per_child, tmp_population = tmp_population, per_n_children = per_n_children)
}

##############################
##############################

generate_prevalence_table <- function (type.input, per_n_children, geo_info, should_round = TRUE, ...) {
  
  if (should_round) { decimal_places <- 0 } else { decimal_places <- 2 }
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  prop_15_17 <- prop_15_17 %>% arrange(loc)
  
  pop_all_years <- left_join(x = population, y = prop_15_17, by = "loc")
  pop_all_years <- pop_all_years %>% mutate(population = if_else(age == "15-19", population * prop, population)) %>% select(gender, loc, year, age, population)
  
  if (length(unique(pop_all_years$loc)) < 1000) { stop("Error. `pop_all_years` is assumed to be at the municipality level.") }
  if (type.input == "Department") {
    pop_all_years <- pop_all_years %>% rename(mun = loc)
    pop_all_years <- pop_all_years %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun") %>% select(gender, dep, year, age, population) %>% group_by(gender, dep, year, age) %>% summarise(population = sum(population)) %>% ungroup() %>% rename(loc = dep)
  } else if (type.input == "Region") {
    pop_all_years <- pop_all_years %>% rename(reg = loc)
    pop_all_years <- pop_all_years %>% left_join(y = geo_info[, c("mun", "reg")], by = "mun") %>% select(gender, reg, year, age, population) %>% group_by(gender, reg, year, age) %>% summarise(population = sum(population)) %>% ungroup() %>% rename(loc = reg)
  } else if (type.input == "National") {
    pop_all_years <- pop_all_years %>% rename(nat = loc)
    pop_all_years <- pop_all_years %>% left_join(y = geo_info[, c("mun", "nat")], by = "mun") %>% select(gender, nat, year, age, population) %>% group_by(gender, nat, year, age) %>% summarise(population = sum(population)) %>% ungroup() %>% rename(loc = nat)
  } else { error("Choose a valid `type.input`.") }
  
  
  nb_orphs_fem       <- nb_orphs_mal     <- list()
  nb_orphs_per_n_fem <- nb_orphs_per_n_mal <- list()
  nb_children_pp_fem <- nb_children_pp_mal <- list()
  total_pop <- 0
  j <- 0
  for (y in 2004:2021) {
    diff_y <- 2021 - y
    age_ch <- 17 - diff_y
    
    j <- j + 1
    
    orphans_by_age <- read_csv(paste("PROCESS_DATA/DATA/RESULTS/", toupper(type.input), "/", type.input, "_parents_deaths_orphans_with_age_summary_", y, ".csv", sep = ""), col_types = cols())
    # Due to rounding effects, the numbers in `department_parents_deaths_orphans_with_age_summary` are slightly different than the ones in `department_parents_deaths_orphans_summary`.
    orphans_by_age <- orphans_by_age %>% filter(child_age <= age_ch)
    locs <- orphans_by_age %>% select(loc) %>% unique() %>% c() %>% unlist() %>% unname()
    nb_orphs_fem[[j]]       <- nb_orphs_mal[[j]]       <- rep(0, length(locs))
    nb_orphs_per_n_fem[[j]] <- nb_orphs_per_n_mal[[j]] <- rep(0, length(locs))
    nb_children_pp_fem[[j]] <- nb_children_pp_mal[[j]] <- rep(0, length(locs))
    i <- 0
    for (l in locs) {
      i <- i + 1
      orphans_by_age_loc <- orphans_by_age %>% filter(loc == l)
      
      for (g in c("Female", "Male")) {
        orphans_by_age_loc_gender <- orphans_by_age_loc %>% filter(gender == g)
        
        total_nb_orphs <- orphans_by_age_loc_gender %>% select(orphans) %>% sum()
        # Already corrected. "15-19" actually refers to "15-17".
        y <- 2021
        tmp_population <- pop_all_years %>% filter(year == y, loc == l, age %in% c("0-9", "10-14", "15-19")) %>% select(population) %>% sum()
        if (g == "Female") {
          nb_orphs_fem[[j]][i]       <- round(total_nb_orphs, 2)
          nb_orphs_per_n_fem[[j]][i] <- round(total_nb_orphs / tmp_population * per_n_children, 2)
          nb_children_pp_fem[[j]][i] <- round(tmp_population, 2)
        } else {
          nb_orphs_mal[[j]][i]       <- round(total_nb_orphs, 2)
          nb_orphs_per_n_mal[[j]][i] <- round(total_nb_orphs / tmp_population * per_n_children, 2)
          nb_children_pp_mal[[j]][i] <- round(tmp_population, 2)
        }
      }
    }
  }
  
  aux_nb_orphs <- function (data, locs, tmp_loc, g, ...) {
    data <- as.data.frame(do.call(cbind, data))
    colnames(data) <- 2004:2021
    data <- data.frame(loc = locs, n_orp = rowSums(data))
    colnames(data) <- c(tmp_loc, "n_orp")
    data[, tmp_loc] <- factor(data[, tmp_loc])
    tmp_geo_info <- geo_info %>% select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = ""))) %>% distinct()
    data <- left_join(x = data, y = tmp_geo_info, by = tmp_loc)
    data <- data %>% select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = "")), "n_orp") %>% arrange({{tmp_loc}})
    data$gender <- g
    data %>% select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = "")), gender, n_orp)
  }
  
  nb_orphs_fem <- aux_nb_orphs(data = nb_orphs_fem, locs = locs, tmp_loc = tmp_loc, g = "Female") 
  nb_orphs_mal <- aux_nb_orphs(data = nb_orphs_mal, locs = locs, tmp_loc = tmp_loc, g = "Male")

  nb_orphs <- bind_rows(nb_orphs_fem, nb_orphs_mal)
  
  ##########
  
  nb_orphs_per_n_fem <- aux_nb_orphs(data = nb_orphs_per_n_fem, locs = locs, tmp_loc = tmp_loc, g = "Female")
  nb_orphs_per_n_mal <- aux_nb_orphs(data = nb_orphs_per_n_mal, locs = locs, tmp_loc = tmp_loc, g = "Male")

  nb_orphs_per_n <- bind_rows(nb_orphs_per_n_fem, nb_orphs_per_n_mal)
  
  ##########
  
  tmp_nb_children_pp_fem <- nb_orphs_fem %>% rename(children = n_orp)
  tmp_nb_children_pp_fem$children <- nb_children_pp_fem[[1]]
  nb_children_pp_fem <- tmp_nb_children_pp_fem
  
  tmp_nb_children_pp_mal <- nb_orphs_mal %>% rename(children = n_orp)
  tmp_nb_children_pp_mal$children <- nb_children_pp_mal[[1]]
  nb_children_pp_mal <- tmp_nb_children_pp_mal
  
  nb_children_pp <- as_tibble(bind_rows(nb_children_pp_fem, nb_children_pp_mal))
  
  ##########
  
  list(orphans = as_tibble(nb_orphs), orphans_per_child = as_tibble(nb_orphs_per_n), nb_children = nb_children_pp, per_n_children = per_n_children)
}
