##################################################
# Basic auxiliary functions
##################################################

compute_rate <- function (count, pop, ...) { 
  r <- (count / pop) 
  r[is.nan(r)] <- 0 # 0/0 
  # r[is.infinite(r)] <- 0 # x/0, x > 0
  r
}

# Compute the number of changes based on two objects, so we can use it to produce a report
nb_changes <- function (new, old, col, possible_loc, initial_year = 1998, final_year = 2021, ...) { 
  
  new <- new %>% filter(year >= initial_year, year <= final_year)
  old <- old %>% filter(year >= initial_year, year <= final_year)
  
  new <- new %>% arrange(gender, loc, year, age)
  old <- old %>% arrange(gender, loc, year, age)
  
  new <- new %>% filter(loc %in% possible_loc)
  old <- old %>% filter(loc %in% possible_loc)
  
  new <- new[, col] %>% c() %>% unlist() %>% unname()
  old <- old[, col] %>% c() %>% unlist() %>% unname()
  
  nb <- sum(new != old)
  
  list(nb, length(new), paste(round(nb / length(new) * 100, 2), "%", sep = "")) 
}

# https://stackoverflow.com/questions/70088426/is-there-a-efficient-way-to-mutate-only-on-rows-that-meet-a-condition-think-mut
mutate_when <- function (.data, when, ...) {
  dots <- enquos(...)
  names <- names(dots)
  
  mutate(.data, {
    test <- {{ when }}
    
    changed <- data.frame(!!!dots, stringsAsFactors = FALSE)
    out <- across(all_of(names))
    # assuming `changed` and `out` have the same data frame type
    
    out[test, ] <- changed[test, ]
    out
  })
}

set_to_NA <- function (data, tbexcluded, rate, mod, ...) {
  # data: original data set
  # tbexcluded: data set with rows to be excluded
  # rate (str): column name (besides `population`) that must be set to `NA`
  # mod: step number to check when the values were set to `NA`
  
  tbexcluded <- tbexcluded %>% select(gender, loc, year, age)
  tbexcluded$loc <- factor(tbexcluded$loc)
  tbexcluded$exclude <- TRUE
  
  data$loc <- factor(data$loc)
  data <- left_join(x = data, y = tbexcluded, by = c("gender", "loc", "year", "age"))
  tmp_idx <- which(!is.na(data$exclude))
  data[tmp_idx, c("population", rate)] <- NA
  data[tmp_idx, "mod"] <- mod
  data <- data %>% select(-exclude)
  
  data
}

equalize_pops <- function (mortality_rates, fertility_rates, per1K, ...) { # Double-check this
  
  new_mortality_rates <- mortality_rates
  new_fertility_rates <- fertility_rates
  
  new_mortality_rates_female <- new_mortality_rates %>% filter(gender == "Female") %>% arrange(gender, loc, year, age)
  new_mortality_rates_male   <- new_mortality_rates %>% filter(gender == "Male")   %>% arrange(gender, loc, year, age)
  new_fertility_rates_female <- new_fertility_rates %>% filter(gender == "Female") %>% arrange(gender, loc, year, age)
  new_fertility_rates_male   <- new_fertility_rates %>% filter(gender == "Male")   %>% arrange(gender, loc, year, age)
  
  # Female
  new_mortality_rates_female_45_49 <- new_mortality_rates_female %>% filter(age != "50+") %>% arrange(gender, loc, year, age)
  pop_female_45_49 <- (new_mortality_rates_female_45_49$population + new_fertility_rates_female$population) / 2
  new_mortality_rates_female[(new_mortality_rates_female$age != "50+"), "population"] <- pop_female_45_49
  new_fertility_rates_female$population <- pop_female_45_49
  
  # Male
  pop_male <- (new_mortality_rates_male$population + new_fertility_rates_male$population) / 2
  new_mortality_rates_male$population <- pop_male
  new_fertility_rates_male$population <- pop_male
  
  # Bind
  new_mortality_rates <- bind_rows(new_mortality_rates_female, new_mortality_rates_male) %>% arrange(gender, loc, year, age)
  new_fertility_rates <- bind_rows(new_fertility_rates_female, new_fertility_rates_male) %>% arrange(gender, loc, year, age)
  
  # Re-compute rates
  new_mortality_rates$death_rate     <- compute_rate(count = new_mortality_rates$deaths, pop = new_mortality_rates$population) * ifelse(per1K, 1000, 1)
  new_fertility_rates$fertility_rate <- compute_rate(count = new_fertility_rates$births, pop = new_fertility_rates$population) * ifelse(per1K, 1000, 1)
  
  list(new = list(new_mortality_rates = new_mortality_rates, new_fertility_rates = new_fertility_rates), old = list(mortality_rates = mortality_rates, fertility_rates = fertility_rates))
}

update_pop <- function (mortality_rates, fertility_rates, population_locs, per1K, ...) {
  tmp_mortality <- mortality_rates 
  tmp_fertility <- fertility_rates
  
  eq_population <- equalize_pops(mortality_rates, fertility_rates, per1K)
  tmp_mortality <- eq_population$new$new_mortality_rates
  eq_population <- tmp_mortality %>% select(gender, loc, year, age, population) %>% arrange(gender, loc, year, age)
  eq_population <- full_join(x = population_locs, y = eq_population, by = c("gender", "loc", "year", "age"))
  eq_population <- eq_population %>% mutate(population = ifelse(!is.na(population.y), population.y, population.x)) %>% select(-c(population.x, population.y))
  
  eq_population
}

##################################################
# Functions to IDENTIFY problematic entries
##################################################

##############################
# Impossible mortality rates
##############################

identify_impossible_mortality_rates <- function (mortality_rates, per1K, ...) {
  
  tmp_mortality_rates <- mortality_rates %>% filter(death_rate > ifelse(per1K, 1000, 1))
  
  tmp_mortality_rates %>% select(gender, loc, year, age)
}

##############################
# Impossible fertility rates
##############################

identify_impossible_fertility_rates <- function (fertility_rates, ...) {
  
  tmp_fertility <- fertility_rates %>% filter(population == 0, births != 0)
  
  tmp_fertility %>% select(gender, loc, year, age)
}

##############################
# Unlikely mortality rates
##############################

identify_unlikely_mortality_rates <- function (mortality_rates, initial_year = 2005, final_year = 2018, n_sd = 3, ...) {
  
  mortality_ref <- mortality_rates %>% filter(year >= initial_year, year <= final_year)
  mortality_ref <- mortality_ref %>% 
                     select(-c(deaths, population)) %>% 
                     group_by(gender, loc, age) %>% 
                     summarize(mean_rate = mean(death_rate, na.rm = TRUE), sd_rate = sqrt(var(death_rate, na.rm = TRUE))) %>% 
                     ungroup()
  mortality_ref <- mortality_ref %>% mutate(min_threshold = ifelse((mean_rate - (n_sd * sd_rate)) <= 0, 0, (mean_rate - (n_sd * sd_rate))))
  mortality_ref <- mortality_ref %>% mutate(max_threshold = (mean_rate + (n_sd * sd_rate)))
  
  n_cases <- nrow(mortality_ref) 
  
  print(paste("Identifying unlikely mortality rates (", n_cases, ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
  for (i in 1:n_cases) {
    
    tmp_case <- mortality_ref[i, ]
    
    tmp_gender        <- tmp_case$gender
    tmp_loc           <- tmp_case$loc
    tmp_age           <- tmp_case$age
    tmp_min_threshold <- tmp_case$min_threshold
    tmp_max_threshold <- tmp_case$max_threshold 
    
    tmp_mortality <- mortality_rates %>% filter(gender == tmp_gender, loc == tmp_loc, age == tmp_age)
    tmp_mortality <- tmp_mortality %>% mutate(outlier = ifelse((death_rate < tmp_min_threshold) | (death_rate > tmp_max_threshold), TRUE, FALSE))
    
    tmp_mortality <- tmp_mortality %>% filter(outlier)
    
    if (i == 1) {
      unl_mor <- tmp_mortality
    } else {
      unl_mor <- bind_rows(unl_mor, tmp_mortality)
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  unl_mor <- unl_mor %>% select(gender, loc, year, age)

  list(unl_mor = unl_mor, mortality_ref = mortality_ref)
}

##############################
# Unlikely fertility rates
##############################

identify_unlikely_fertility_rates <- function (fertility_rates, initial_year = 2005, final_year = 2018, n_sd = 3, ...) {
  
  fertility_ref <- fertility_rates %>% filter(year >= initial_year, year <= final_year)
  fertility_ref <- fertility_ref %>% 
                     select(-c(births, population)) %>% 
                     group_by(gender, loc, age) %>% 
                     summarize(mean_rate = mean(fertility_rate, na.rm = TRUE), sd_rate = sqrt(var(fertility_rate, na.rm = TRUE))) %>% 
                     ungroup()
  fertility_ref <- fertility_ref %>% mutate(min_threshold = ifelse((mean_rate - (n_sd * sd_rate)) <= 0, 0, (mean_rate - (n_sd * sd_rate))))
  fertility_ref <- fertility_ref %>% mutate(max_threshold = (mean_rate + (n_sd * sd_rate)))
  
  n_cases <- nrow(fertility_ref) 
  
  print(paste("Identifying unlikely fertility rates (", n_cases, ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
  for (i in 1:n_cases) {
    
    tmp_case <- fertility_ref[i, ]
    
    tmp_gender        <- tmp_case$gender
    tmp_loc           <- tmp_case$loc
    tmp_age           <- tmp_case$age
    tmp_min_threshold <- tmp_case$min_threshold
    tmp_max_threshold <- tmp_case$max_threshold 
    
    tmp_fertility <- fertility_rates %>% filter(gender == tmp_gender, loc == tmp_loc, age == tmp_age)
    tmp_fertility <- tmp_fertility %>% mutate(outlier = ifelse((fertility_rate < tmp_min_threshold) | (fertility_rate > tmp_max_threshold), TRUE, FALSE))
    
    tmp_fertility <- tmp_fertility %>% filter(outlier)
    
    if (i == 1) {
      unl_fer <- tmp_fertility
    } else {
      unl_fer <- bind_rows(unl_fer, tmp_fertility)
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  unl_fer <- unl_fer %>% select(gender, loc, year, age)
  
  list(unl_fer = unl_fer, fertility_ref = fertility_ref)
}

##############################
# Spatial outliers
##############################

detect_spatial_outliers_independent <- function (colombia, spatial_obj, data, variable, gg, aa, mx_lag, n_sd = 3, local = TRUE, ...) {

  nb <- spatial_obj$nb
  loc_available <- spatial_obj$loc_available
  loc_ignore <- spatial_obj$loc_ignore

  # Filter available locations
  data <- data %>% filter(loc %in% loc_available, !(loc %in% loc_ignore))

  # Filter stratum
  # alternative_aa <- ifelse(aa %in% c("50+", "50-54"), "50+/50-54", "NONE")
  data <- data %>% filter(gender == gg, age == aa)

  # Compute the neighbors of order `mx_lag`
  nbs <- neighbors_n_order(spatial_obj, mx_lag = mx_lag)

  detected_out <- as_tibble(data.frame(loc = as.character(), year = as.double()))
  for (ll in names(nbs)) {

    # Collect information about the neighbors
    tmp_nbs  <- nbs[[ll]]
    tmp_data <- data %>% filter(loc %in% tmp_nbs)

    count_var <- ifelse(variable == "death_rate", "deaths", "births")

    tmp_data <- tmp_data %>%
                  select(-c(any_of(count_var), population, mod)) %>%
                  group_by(year) %>%
                  summarize(mean_rate = mean(get(variable), na.rm = TRUE), sd_rate = sqrt(var(get(variable), na.rm = TRUE))) %>%
                  ungroup() %>%
                  arrange(year)
    data_ref <- tmp_data %>% mutate(min_threshold = mean_rate - (n_sd * sd_rate))
    data_ref <- data_ref %>% mutate(max_threshold = mean_rate + (n_sd * sd_rate))

    # Analyze the current location
    filtered_data <- data %>% filter(loc == ll)
    current <- filtered_data %>% select(all_of(variable)) %>% c() %>% unlist() %>% unname()

    is_outlier <- (current < data_ref$min_threshold) | (current > data_ref$max_threshold)
    if (sum(is_outlier, na.rm = TRUE) > 0) {
      tmp_years <- filtered_data$year[is_outlier]
      tmp_years <- tmp_years[!is.na(tmp_years)]
      detected_out <- bind_rows(detected_out, as_tibble(data.frame(loc = ll, year = tmp_years)))
    }
  }

  detected_out
}

detect_spatial_outliers_summary <- function (colombia, spatial_obj, data, variable, gg, aa, mx_lag = mx_lag, n_sd = 3, local = TRUE, ...) {

  nb <- spatial_obj$nb
  loc_available <- spatial_obj$loc_available
  loc_ignore <- spatial_obj$loc_ignore

  # Filter available locations
  data <- data %>% filter(loc %in% loc_available, !(loc %in% loc_ignore))

  # Filter stratum
  alternative_aa <- ifelse(aa %in% c("50+", "50-54"), "50+/50-54", "NONE")
  data <- data %>% filter(gender == gg, age %in% c(aa, alternative_aa))

  if (!local) { # Based on the global neighboring
    val <- data[, variable] %>% c() %>% unlist() %>% unname()
    
    mn_tmp <- mean(val, na.rm = T)
    sd_tmp <- sqrt(var(val, na.rm = T))
    
    min_threshold <- mn_tmp - (n_sd * sd_tmp)# ; min_threshold <- 0
    max_threshold <- mn_tmp + (n_sd * sd_tmp)
    
    pos_outliers <- which((val < min_threshold) | (val > max_threshold))
    
    code_outliers <- data[pos_outliers, ] %>% select(loc) %>% c() %>% unlist() %>% unname() %>% as.character()
    code_outliers <- as_tibble(code_outliers)
    code_outliers$outlier <- 1
    colnames(code_outliers) <- c("loc", "outlier")
    code_outliers$loc <- as.double(code_outliers$loc)
    code_outliers
  }
  
  if (local) { # Based on the local neighboring
    # Compute the neighbors of order `mx_lag`
    nbs <- neighbors_n_order(spatial_obj, mx_lag = mx_lag)
    
    detected_out <- c()
    for (ll in names(nbs)) {
      
      # Collect information about the neighbors
      tmp_nbs  <- nbs[[ll]]
      tmp_data <- data %>% filter(loc %in% tmp_nbs)
      
      val <- tmp_data[, variable] %>% c() %>% unlist() %>% unname()
      
      mn_tmp <- mean(val, na.rm = TRUE)
      sd_tmp <- sqrt(var(val, na.rm = TRUE))
      
      min_threshold <- mn_tmp - (n_sd * sd_tmp)
      max_threshold <- mn_tmp + (n_sd * sd_tmp)
      
      # Analyze the current location
      current <- data %>% filter(loc == ll) %>% select(all_of(variable)) %>% c() %>% unlist() %>% unname()
      
      is_outlier <- (current < min_threshold) | (current > max_threshold)
      if (is_outlier) {
        detected_out <- c(detected_out, ll)
      }
    }
    
    code_outliers <- as_tibble(data_frame(loc = as.double(detected_out), outlier = 1))
  }

  code_outliers
}

neighbors_n_order <- function (spatial_obj, mx_lag = 2, ...) {
  
  nb <- spatial_obj$nb
  loc_available <- spatial_obj$loc_available
  loc_ignore <- spatial_obj$loc_ignore
  
  n_loc <- length(nb)
  
  if (mx_lag >= 2) {
    nb_lags <- nblag(nb, maxlag = mx_lag)
    
    nb_list <- list()
    for (i in 1:mx_lag) {
      nb_lags_list <- nb2listw(nb_lags[[i]])
      for (j in 1:n_loc) {
        if (i == 1) {
          nb_list[[j]] <- nb_lags_list$neighbours[[j]]
        } else {
          nb_list[[j]] <- c(nb_list[[j]], nb_lags_list$neighbours[[j]])
        }
      }
    }
  } else if (mx_lag == 1) {
    nb_list <- list()
    
    nb_lags_list <- nb2listw(nb)
    for (j in 1:n_loc) {
      nb_list[[j]] <- nb_lags_list$neighbours[[j]]
    }
  } else {
    stop("`mx_lag` should be >= 1.")
  }
  
  if (length(loc_available) == length(nb_list)) {
    names(nb_list) <- loc_available
    for (i in 1:length(nb_list)) {
      idx_tmp <- nb_list[[i]] 
      nb_list[[i]] <- loc_available[idx_tmp]
    }
  } else { stop("Error.") }
  
  nb_list
}

identify_spatial_outliers_independent  <- function (data, variable, per1K = TRUE, n_sd = 3, mx_lag = 1, local = TRUE, ...) {

  combinations <- data %>% unite(combined, gender, age, sep = " ") %>% select(combined) %>% distinct()
  combinations <- combinations %>% separate(combined, into = c("gender", "age"), sep = " ")
  combinations <- as.data.frame(combinations)

  pb <- txtProgressBar(min = 1, max = nrow(combinations), initial = 1)
  for (z in 1:nrow(combinations)) {
    setTxtProgressBar(pb, z)

    gg <- combinations$gender[z]
    aa <- combinations$age[z]

    # Detection
    detected_out <- detect_spatial_outliers_independent(colombia, spatial_obj, data = data, variable = variable, gg = gg, aa = aa, mx_lag = mx_lag, n_sd = n_sd, local = local)
    detected_out$outlier <- TRUE

    count_var    <- ifelse(variable == "death_rate", "deaths", "births")
    tmp_outliers <- data %>% filter(gender == gg, age == aa) %>% select(-c(any_of(count_var), population, any_of(variable), mod))
    tmp_outliers <- right_join(x = tmp_outliers, detected_out, by = c("loc", "year")) %>% select(-outlier)

    if (z == 1) {
      spa_out <- tmp_outliers
    } else {
      spa_out <- bind_rows(spa_out, tmp_outliers)
    }
  }
  close(pb)

  list(spa_out = spa_out, data_ref = NA)
}

identify_spatial_outliers_summary <- function (data, sub_data, variable, sub_variable, sub_count, per1K = TRUE, n_sd = 3, mx_lag = 1, local = FALSE, ...) {

  combinations <- sub_data %>% unite(combined, gender, age, sep = " ") %>% select(combined) %>% distinct()
  combinations <- combinations %>% separate(combined, into = c("gender", "age"), sep = " ")
  combinations <- as.data.frame(combinations)

  pb <- txtProgressBar(min = 1, max = nrow(combinations), initial = 1)
  for (z in 1:nrow(combinations)) {
    gg <- combinations$gender[z]
    aa <- combinations$age[z]

    # Detection
    detected_out <- detect_spatial_outliers_summary(colombia, spatial_obj, data = data, variable = variable, gg = gg, aa = aa, mx_lag = mx_lag, n_sd = n_sd, local = local)
    new_colombia <- colombia
    new_colombia$loc <- as.double(as.character(new_colombia$loc))
    new_colombia <- right_join(x = detected_out, y = new_colombia, by = "loc") %>% mutate_if(is.numeric, coalesce, 0) %>% arrange(loc)

    code_out <- new_colombia %>% filter(outlier == 1) %>% select(loc) %>% c() %>% unlist() %>% unname() %>% as.character()
    data_out <- sub_data %>% filter(gender == gg, age == aa, loc %in% code_out)

    if (z == 1) {
      spa_out <- data_out
    } else {
      spa_out <- bind_rows(spa_out, data_out)
    }

    setTxtProgressBar(pb, z)
  }
  close(pb)

  spa_out <- spa_out %>% select(gender, loc, year, age)

  list(spa_out = spa_out, data_ref = NA)
}

##################################################
# Functions for IMPUTATION to problematic entries
##################################################

##############################
# Negative population
##############################

imputation_negative_population <- function (data, per1K = TRUE, ...) {
  
  mx_year <- max(data$year)
  tmp_idx <- which(data$mod == 1)
  
  new_values <- c()
  
  pb <- txtProgressBar(min = 1, max = length(tmp_idx), initial = 1) 
  for (i in 1:length(tmp_idx)) {
    setTxtProgressBar(pb, i)
    
    tmp_value <- data[tmp_idx[i], ]
    ll <- tmp_value$loc
    gg <- tmp_value$gender
    aa <- tmp_value$age
    yy <- tmp_value$year
    
    if (yy == mx_year) {
      new_values <- c(new_values, 0)
    } else {
      tmp_replacement <- NA
      count <- 1
      while (is.na(tmp_replacement)) {
        if ((yy + count) == mx_year) { tmp_replacement <- 0 } else {
          tmp_replacement <- data %>% filter(gender == gg, loc == ll, year == (yy + count), age == aa) %>% select(population) %>% c() %>% unlist() %>% unname()
        }
        count <- count + 1
      }
      new_values <- c(new_values, tmp_replacement)
    }
  }
  close(pb)
  
  data[tmp_idx, "population"] <- new_values
  
  col_names <- colnames(data)
  if ("deaths" %in% col_names) {
    count_name <- "deaths"; rate_name <- "death_rate"
  } else {
    count_name <- "births"; rate_name <- "fertility_rate"
  }
  
  data[tmp_idx, rate_name] <- compute_rate(count = unname(unlist(c(data[tmp_idx, count_name]))), pop = unname(unlist(c(data[tmp_idx, "population"])))) * ifelse(per1K, 1000, 1)
  data
}

##############################
# Impossible mortality rates
##############################

imputation_impossible_mortality_rates <- function (mortality_rates, population_locs, per1K, mods = 2, ...) {
  
  mn_year <- min(mortality_rates$year)
  mx_year <- max(mortality_rates$year)
  
  tmp_mortality <- mortality_rates %>% filter(mod %in% mods)
  n_cases <- nrow(tmp_mortality) 
  track_alternative_mortality <- c()
  
  print(paste("Fixing impossible mortality rates (", n_cases, ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
  for (i in 1:n_cases) {
    setTxtProgressBar(pb, i)
    
    tmp_case <- tmp_mortality[i, ]
    
    tmp_gender     <- tmp_case$gender
    tmp_loc        <- tmp_case$loc
    tmp_year       <- tmp_case$year
    tmp_age        <- tmp_case$age
    tmp_deaths     <- tmp_case$deaths
    tmp_population <- tmp_case$population
    
    if (tmp_year == mn_year) {
      ys <- tmp_year + 1
    } else if (tmp_year == mx_year) {
      ys <- tmp_year - 1
    } else {
      ys <- c((tmp_year - 1), (tmp_year + 1))
    }
    
    new_pop <- population_locs %>% filter(gender == tmp_gender, loc == tmp_loc, year %in% ys, age == tmp_age) %>% select(population) %>% c() %>% unlist() %>% unname() %>% median() %>% ceiling()
    
    if (new_pop < tmp_deaths) { # The problem persists.
      new_pop <- tmp_deaths
      track_alternative_mortality <- c(track_alternative_mortality, i)
    }
    
    tmp_death_rate <- compute_rate(count = tmp_deaths, pop = new_pop) * ifelse(per1K, 1000, 1)
    
    mortality_rates <- mortality_rates %>% mutate(population = replace(population, (gender == tmp_gender & loc == tmp_loc & year == tmp_year & age == tmp_age), new_pop))
    mortality_rates <- mortality_rates %>% mutate(death_rate = replace(death_rate, (gender == tmp_gender & loc == tmp_loc & year == tmp_year & age == tmp_age), tmp_death_rate))
  
  }
  close(pb)
 
  mortality_rates 
}

##############################
# Impossible fertility rates
##############################

imputation_impossible_fertility_rates <- function (fertility_rates, population_locs, per1K, mods = c(2), max_children = 999, ...) {
  
  mn_year <- min(fertility_rates$year)
  mx_year <- max(fertility_rates$year)
  
  tmp_fertility <- fertility_rates %>% filter(mod %in% mods)
  n_cases <- nrow(tmp_fertility) 
  track_alternative_fertility <- c()
  
  print(paste("Fixing impossible fertility rates (", n_cases, ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
  for (i in 1:n_cases) {
    
    tmp_case <- tmp_fertility[i, ]
    
    tmp_gender     <- tmp_case$gender
    tmp_loc        <- tmp_case$loc
    tmp_year       <- tmp_case$year
    tmp_age        <- tmp_case$age
    tmp_births     <- tmp_case$births
    tmp_population <- tmp_case$population
    
    if (tmp_year == mn_year) {
      ys <- tmp_year + 1
    } else if (tmp_year == mx_year) {
      ys <- tmp_year - 1
    } else {
      ys <- c((tmp_year - 1), (tmp_year + 1))
    }
    
    new_pop <- population_locs %>% filter(gender == tmp_gender, loc == tmp_loc, year %in% ys, age == tmp_age) %>% select(population) %>% c() %>% unlist() %>% unname() %>% median() %>% ceiling()
    
    if (new_pop == 0) { # The problem persists.
      new_pop <- ceiling(tmp_births / max_children)
      track_alternative_fertility <- c(track_alternative_fertility, i)
    }
    
    tmp_fertility_rate <- compute_rate(count = tmp_births, pop = new_pop) * ifelse(per1K, 1000, 1)
    
    fertility_rates <- fertility_rates %>% mutate(population     = replace(population,     (gender == tmp_gender & loc == tmp_loc & year == tmp_year & age == tmp_age), new_pop))
    fertility_rates <- fertility_rates %>% mutate(fertility_rate = replace(fertility_rate, (gender == tmp_gender & loc == tmp_loc & year == tmp_year & age == tmp_age), tmp_fertility_rate))
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  fertility_rates
}

##############################
# Unlikely mortality rates
##############################

# imputation_unlikely_mortality_rates <- function (mortality_rates, mortality_ref, per1K, mm = 3, ...) {
#   
#   filtered_mortality_rates <- mortality_rates %>% filter(mod == mm)
#   
#   n_cases <- nrow(mortality_ref) 
#   
#   print(paste("Fixing unlikely mortality rates (", n_cases, ").", sep = ""))
#   pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
#   for (i in 1:n_cases) {
#     
#     tmp_case <- mortality_ref[i, ]
#     
#     tmp_gender        <- tmp_case$gender
#     tmp_loc           <- tmp_case$loc
#     tmp_age           <- tmp_case$age
#     tmp_min_threshold <- tmp_case$min_threshold
#     tmp_max_threshold <- tmp_case$max_threshold 
#     
#     tmp_mortality <- filtered_mortality_rates %>% filter(gender == tmp_gender, loc == tmp_loc, age == tmp_age)
#     
#     ##############################
#     # Correction
#     ##############################
#     
#     if (nrow(tmp_mortality) != 0) {
#       tmp_rate <- tmp_case$mean_rate
#       tmp_mortality <- tmp_mortality %>% mutate(death_rate = ifelse(is.na(death_rate), tmp_rate, death_rate))
#       tmp_mortality <- tmp_mortality %>% mutate(population = ifelse(is.na(population), ceiling(deaths * ifelse(per1K, 1000, 1) /  death_rate), population))
#       tmp_mortality$population[is.na(tmp_mortality$population)] <- 0 # 0/0
#       # Exception: non-zero incidence, but zero `rate`
#       tmp_mortality <- tmp_mortality %>% mutate(population = ifelse(is.infinite(population), deaths, population))
#       # To fix the rounding effect from `ceiling`
#       tmp_mortality$death_rate <- compute_rate(count = tmp_mortality$deaths, pop = tmp_mortality$population) * ifelse(per1K, 1000, 1)
#       # Exception: non-zero incidence, but zero `population` 
#       tmp_mortality <- tmp_mortality %>% mutate(population = ifelse(is.infinite(death_rate), deaths, population))
#       # To fix the above exception
#       tmp_mortality$death_rate <- compute_rate(count = tmp_mortality$deaths, pop = tmp_mortality$population) * ifelse(per1K, 1000, 1)
#     }
#     
#     # Bind temporary values
#     if (i == 1) {
#       new_mortality_rates <- tmp_mortality
#     } else {
#       new_mortality_rates <- bind_rows(new_mortality_rates, tmp_mortality)
#     }
#     
#     setTxtProgressBar(pb, i)
#   }
#   close(pb)
#   
#   filtered_mortality_rates <- mortality_rates %>% filter(mod != mm)
#   
#   new_mortality_rates <- bind_rows(new_mortality_rates, filtered_mortality_rates) %>% arrange(gender, loc, year, age)
#   new_mortality_rates
# }  

imputation_unlikely_mortality_rates <- function (mortality_rates, mortality_ref, population_locs, per1K, mm = 3, use_limits = TRUE, ...) {
  
  filtered_mortality_rates <- mortality_rates %>% filter(mod == mm)
  
  n_cases <- nrow(mortality_ref) 
  
  cc <- 0
  print(paste("Fixing unlikely mortality rates (", n_cases, ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
  for (i in 1:n_cases) {
    
    tmp_case <- mortality_ref[i, ]
    
    tmp_gender        <- tmp_case$gender
    tmp_loc           <- tmp_case$loc
    tmp_age           <- tmp_case$age
    tmp_min_threshold <- tmp_case$min_threshold
    tmp_max_threshold <- tmp_case$max_threshold 
    
    tmp_mortality <- filtered_mortality_rates %>% filter(gender == tmp_gender, loc == tmp_loc, age == tmp_age)
    backup_pop_00 <- population_locs %>% filter(gender == tmp_gender, loc == tmp_loc, age == tmp_age)
    
    ##############################
    # Correction
    ##############################
    
    if (use_limits) { # Set the population, such that the new rates are on the threshold limits
      if (nrow(tmp_mortality) != 0) {
        n_outliers <- nrow(tmp_mortality)
        
        for (j in 1:n_outliers) {
          
          tmp_outlier_case <- tmp_mortality[j, ]
          
          tmp_year <- tmp_outlier_case$year
          backup_pop_01 <- backup_pop_00 %>% filter(year == tmp_year)
          
          backupPopulation <- backup_pop_01 %>% select(population) %>% c() %>% unlist() %>% unname()
          backupDeath_rate <- compute_rate(count = tmp_outlier_case$deaths, pop = backupPopulation) * ifelse(per1K, 1000, 1)
          
          if (backupDeath_rate <= tmp_min_threshold) {
            tmp_outlier_case$death_rate <- tmp_min_threshold
          } else if (backupDeath_rate > tmp_max_threshold) {
            tmp_outlier_case$death_rate <- tmp_max_threshold
          } else {
            tmp_outlier_case$death_rate <- backupDeath_rate; cc <- cc + 1 # Not an outlier
          }
          tmp_outlier_case$population <- ceiling(tmp_outlier_case$deaths * ifelse(per1K, 1000, 1) /  tmp_outlier_case$death_rate)
          # Fix `ceiling` effect
          tmp_outlier_case$death_rate <- compute_rate(count = tmp_outlier_case$deaths, pop = tmp_outlier_case$population) * ifelse(per1K, 1000, 1)
          
          # Treat odd scenarios
          if (is.na(tmp_outlier_case$population)) { # 0/0
            tmp_outlier_case$population <- 0
          } else if (is.infinite(tmp_outlier_case$population)) { # 0/x, x > 0
            tmp_outlier_case$population <- backupPopulation
          }
          
          if (j == 1) {
            partial_mortality <- tmp_outlier_case
          } else {
            partial_mortality <- bind_rows(partial_mortality, tmp_outlier_case)
          }
        }
        tmp_mortality <- partial_mortality
      }
    }
    
    if (!use_limits) { # Set the population, such that the rates are the same as the average
      if (nrow(tmp_mortality) != 0) {
        tmp_rate <- tmp_case$mean_rate
        tmp_mortality <- tmp_mortality %>% mutate(death_rate = ifelse(is.na(death_rate), tmp_rate, death_rate))
        tmp_mortality <- tmp_mortality %>% mutate(population = ifelse(is.na(population), ceiling(deaths * ifelse(per1K, 1000, 1) /  death_rate), population))
        tmp_mortality$population[is.na(tmp_mortality$population)] <- 0 # 0/0
        # Exception: non-zero incidence, but zero `rate`
        tmp_mortality <- tmp_mortality %>% mutate(population = ifelse(is.infinite(population), deaths, population))
        # To fix the rounding effect from `ceiling`
        tmp_mortality$death_rate <- compute_rate(count = tmp_mortality$deaths, pop = tmp_mortality$population) * ifelse(per1K, 1000, 1)
        # Exception: non-zero incidence, but zero `population` 
        tmp_mortality <- tmp_mortality %>% mutate(population = ifelse(is.infinite(death_rate), deaths, population))
        # To fix the above exception
        tmp_mortality$death_rate <- compute_rate(count = tmp_mortality$deaths, pop = tmp_mortality$population) * ifelse(per1K, 1000, 1)
      }
    }
    
    # Bind temporary values
    if (i == 1) {
      new_mortality_rates <- tmp_mortality
    } else {
      new_mortality_rates <- bind_rows(new_mortality_rates, tmp_mortality)
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  print(paste("Number of non-outliers: ", cc, sep = ""))
  
  filtered_mortality_rates <- mortality_rates %>% filter(mod != mm)
  
  new_mortality_rates <- bind_rows(new_mortality_rates, filtered_mortality_rates) %>% arrange(gender, loc, year, age)
  new_mortality_rates
} 

##############################
# Unlikely fertility rates
##############################

imputation_unlikely_fertility_rates <- function (fertility_rates, fertility_ref, population_locs, per1K, mm = 3, use_limits = TRUE, ...) {

  filtered_fertility_rates <- fertility_rates %>% filter(mod == mm)
  
  n_cases <- nrow(fertility_ref) 
  
  cc <- 0
  print(paste("Fixing unlikely fertility rates (", n_cases, ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
  for (i in 1:n_cases) {
    
    tmp_case <- fertility_ref[i, ]
    
    tmp_gender        <- tmp_case$gender
    tmp_loc           <- tmp_case$loc
    tmp_age           <- tmp_case$age
    tmp_min_threshold <- tmp_case$min_threshold
    tmp_max_threshold <- tmp_case$max_threshold
    
    tmp_fertility <- filtered_fertility_rates %>% filter(gender == tmp_gender, loc == tmp_loc, age == tmp_age)
    backup_pop_00 <- population_locs %>% filter(gender == tmp_gender, loc == tmp_loc, age == tmp_age)
    
    ##############################
    # Correction
    ##############################
    
    if (use_limits) { # Set the population, such that the new rates are on the threshold limits
      if (nrow(tmp_fertility) != 0) {
        n_outliers <- nrow(tmp_fertility)
        
        for (j in 1:n_outliers) {
          
          tmp_outlier_case <- tmp_fertility[j, ]
          
          tmp_year <- tmp_outlier_case$year
          backup_pop_01 <- backup_pop_00 %>% filter(year == tmp_year)
          
          backupPopulation <- backup_pop_01 %>% select(population) %>% c() %>% unlist() %>% unname()
          backupFerti_rate <- compute_rate(count = tmp_outlier_case$births, pop = backupPopulation) * ifelse(per1K, 1000, 1)
          
          if (backupFerti_rate <= tmp_min_threshold) {
            tmp_outlier_case$fertility_rate <- tmp_min_threshold
          } else if (backupFerti_rate > tmp_max_threshold) {
            tmp_outlier_case$fertility_rate <- tmp_max_threshold
          } else {
            tmp_outlier_case$fertility_rate <- backupFerti_rate; cc <- cc + 1 # Not an outlier
          }
          tmp_outlier_case$population <- ceiling(tmp_outlier_case$births * ifelse(per1K, 1000, 1) /  tmp_outlier_case$fertility_rate)
          # Fix `ceiling` effect
          tmp_outlier_case$fertility_rate <- compute_rate(count = tmp_outlier_case$births, pop = tmp_outlier_case$population) * ifelse(per1K, 1000, 1)
          
          # Treat odd scenarios
          if (is.na(tmp_outlier_case$population)) { # 0/0
            tmp_outlier_case$population <- 0
          } else if (is.infinite(tmp_outlier_case$population)) { # 0/x, x > 0
            tmp_outlier_case$population <- backupPopulation
          }
          
          if (j == 1) {
            partial_fertility <- tmp_outlier_case
          } else {
            partial_fertility <- bind_rows(partial_fertility, tmp_outlier_case)
          }
        }
        tmp_fertility <- partial_fertility
      }
    }
    
    if (!use_limits) { # Set the population, such that the rates are the same as the average
      if (nrow(tmp_fertility) != 0) {
        tmp_rate <- tmp_case$mean_rate
        tmp_fertility <- tmp_fertility %>% mutate(fertility_rate = ifelse(is.na(fertility_rate), tmp_rate, fertility_rate))
        tmp_fertility <- tmp_fertility %>% mutate(population = ifelse(is.na(population), ceiling(births * ifelse(per1K, 1000, 1) /  fertility_rate), population))
        tmp_fertility$population[is.na(tmp_fertility$population)] <- 0 # 0/0
        # Exception: non-zero incidence, but zero `rate`
        tmp_fertility <- tmp_fertility %>% mutate(population = ifelse(is.infinite(population), births, population))
        # To fix the rounding effect from `ceiling`
        tmp_fertility$fertility_rate <- compute_rate(count = tmp_fertility$births, pop = tmp_fertility$population) * ifelse(per1K, 1000, 1)
        # Exception: non-zero incidence, but zero `population` 
        tmp_fertility <- tmp_fertility %>% mutate(population = ifelse(is.infinite(fertility_rate), births, population))
        # To fix the above exception
        tmp_fertility$fertility_rate <- compute_rate(count = tmp_fertility$births, pop = tmp_fertility$population) * ifelse(per1K, 1000, 1)
      }
    }
   
    # Bind temporary values
    if (i == 1) {
      new_fertility_rates <- tmp_fertility
    } else {
      new_fertility_rates <- bind_rows(new_fertility_rates, tmp_fertility)
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  print(paste("Number of non-outliers: ", cc, sep = ""))
  
  filtered_fertility_rates <- fertility_rates %>% filter(mod != mm)
  
  new_fertility_rates <- bind_rows(new_fertility_rates, filtered_fertility_rates) %>% arrange(gender, loc, year, age)
  new_fertility_rates
}

##############################
# Spatial outliers
##############################

imputation_spatial_outliers <- function (data, variable, per1K = TRUE, mx_lag = 1, mm = 4, ...) {
  
  # `spatial_obj` is assumed
  nb <- spatial_obj$nb
  loc_available <- spatial_obj$loc_available
  loc_ignore <- spatial_obj$loc_ignore
  
  nbs <- neighbors_n_order(spatial_obj, mx_lag = mx_lag)
  
  tmp_data <- data %>% filter(mod == mm)
  
  print(paste("Fixing spatial outliers for `", variable, "` (", nrow(tmp_data), ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = nrow(tmp_data), initial = 1) 
  for (i in 1:nrow(tmp_data)) {
    
    tmp_case <- tmp_data[i, ]
    
    tmp_gender <- tmp_case$gender
    tmp_loc    <- tmp_case$loc
    tmp_year   <- tmp_case$year
    tmp_age    <- tmp_case$age
    
    tmp_nbs <- nbs[[as.character(tmp_loc)]]
    
    filtered_data <- data %>% filter(gender == tmp_gender, loc %in% tmp_nbs, age == tmp_age, year == tmp_year) # year == tmp_year)
    # out_idx <- which(rstatix::is_outlier(unlist(c(filtered_data[, variable]))))
    # tmp_rate <- mean(unlist(c(filtered_data[-out_idx, variable])), na.rm = TRUE, inf.rm = TRUE)
    tmp_rate <- mean(unlist(c(filtered_data[, variable])), na.rm = TRUE, inf.rm = TRUE)
    

    tmp_data[i, variable] <- tmp_rate
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  
  if (variable == "death_rate") { variable_count <- "deaths" } else { variable_count <- "births" }
  tmp_data <- tmp_data %>% mutate(population = ceiling(get(variable_count) * ifelse(per1K, 1000, 1) / get(variable)))
  tmp_data$population[is.na(tmp_data$population)] <- 0 # 0/0
  # Exception: non-zero incidence, but zero `rate`; i.e., infinite population
  if (sum(is.infinite(tmp_data$population)) > 0) {
    idx_infinite <- which(is.infinite(tmp_data$population))
    for (i in idx_infinite) {
      tmp_case <- tmp_data[i, ]
      
      tmp_gender <- tmp_case$gender
      tmp_loc    <- tmp_case$loc
      tmp_year   <- tmp_case$year
      tmp_age    <- tmp_case$age
      
      tmp_nbs <- nbs[[as.character(tmp_loc)]]
      
      filtered_data <- data %>% filter(gender == tmp_gender, loc %in% tmp_nbs, age == tmp_age, year == tmp_year) # year == tmp_year)
      tmp_population <- mean(unlist(c(filtered_data[, "population"])), na.rm = TRUE, inf.rm = TRUE)
      tmp_data[i, "population"] <- ceiling(tmp_population)
    }
  } # DELETE: tmp_data <- tmp_data %>% mutate(population = ifelse(is.infinite(population), get(variable_count), population))
  # To fix the rounding effect from `ceiling`
  tmp_data[, variable] <- compute_rate(count = unlist(c(tmp_data[, variable_count])), pop = unlist(c(tmp_data$population))) * ifelse(per1K, 1000, 1)
  # Exception: non-zero incidence, but zero `population` 
  tmp_data <- tmp_data %>% mutate(population = ifelse(is.infinite(get(variable)), get(variable_count), population))
  # To fix the above exception
  tmp_data[, variable] <- compute_rate(count = unlist(c(tmp_data[, variable_count])), pop = unlist(c(tmp_data$population))) * ifelse(per1K, 1000, 1)
  
  # Combine the new and old data
  data <- data %>% filter(mod != mm)
  data <- bind_rows(data, tmp_data) %>% arrange(gender, loc, year, age)
  
  data
}

##############################
# Common Population
##############################

imputation_common_population <- function (mortality_rates, fertility_rates, population_locs, per1K = TRUE, max_children = 999, ...) {
  
  # Preliminaries
  
  prt_mortality_rates <- mortality_rates %>% filter(mod == 5)
  prt_fertility_rates <- fertility_rates %>% filter(mod == 5)
  
  mn_year <- min(prt_mortality_rates$year)
  mx_year <- max(prt_mortality_rates$year)
  
  # Mortality
  
  n_cases <- nrow(prt_mortality_rates)
  print(paste("Common population - mortality (", n_cases, ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
  for (i in 1:n_cases) {
    setTxtProgressBar(pb, i)
    
    tmp_case <- prt_mortality_rates[i, ]
    
    tmp_gender     <- tmp_case$gender
    tmp_loc        <- tmp_case$loc
    tmp_year       <- tmp_case$year
    tmp_age        <- tmp_case$age
    tmp_deaths     <- tmp_case$deaths
    
    if (tmp_year == mn_year) {
      ys <- tmp_year + 1
    } else if (tmp_year == mx_year) {
      ys <- tmp_year - 1
    } else {
      ys <- c((tmp_year - 1), (tmp_year + 1))
    }
    
    new_pop <- population_locs %>% filter(gender == tmp_gender, loc == tmp_loc, year %in% ys, age == tmp_age) %>% select(population) %>% c() %>% unlist() %>% unname() %>% median() %>% ceiling()
    
    if (new_pop < tmp_deaths) { # Impossible scenario
      new_pop <- tmp_deaths
    }
    
    tmp_death_rate <- compute_rate(count = tmp_deaths, pop = new_pop) * ifelse(per1K, 1000, 1)
    
    tmp_case <- tmp_case %>% mutate(population = new_pop, death_rate = tmp_death_rate)
    
    if (i == 1) {
      new_mortality_rates <- tmp_case
    } else {
      new_mortality_rates <- bind_rows(new_mortality_rates, tmp_case)
    }
  }
  close(pb)
  
  # Fertility
  
  n_cases <- nrow(prt_fertility_rates)
  print(paste("Common population - fertility (", n_cases, ").", sep = ""))
  pb <- txtProgressBar(min = 1, max = n_cases, initial = 1) 
  for (i in 1:n_cases) {
    setTxtProgressBar(pb, i)
    
    tmp_case <- prt_fertility_rates[i, ]
    
    tmp_gender     <- tmp_case$gender
    tmp_loc        <- tmp_case$loc
    tmp_year       <- tmp_case$year
    tmp_age        <- tmp_case$age
    tmp_births     <- tmp_case$births
    
    if (tmp_year == mn_year) {
      ys <- tmp_year + 1
    } else if (tmp_year == mx_year) {
      ys <- tmp_year - 1
    } else {
      ys <- c((tmp_year - 1), (tmp_year + 1))
    }
    
    new_pop <- population_locs %>% filter(gender == tmp_gender, loc == tmp_loc, year %in% ys, age == tmp_age) %>% select(population) %>% c() %>% unlist() %>% unname() %>% median() %>% ceiling()
    
    if (new_pop == 0) { # Impossible scenario
      new_pop <- ceiling(tmp_births / max_children)
    }
    
    tmp_fertility_rate <- compute_rate(count = tmp_births, pop = new_pop) * ifelse(per1K, 1000, 1)
    
    tmp_case <- tmp_case %>% mutate(population = new_pop, fertility_rate = tmp_fertility_rate)
    
    if (i == 1) {
      new_fertility_rates <- tmp_case
    } else {
      new_fertility_rates <- bind_rows(new_fertility_rates, tmp_case)
    }
  }
  close(pb)
  
  # Final steps
  
  tmp_mortality_rates <- mortality_rates %>% filter(mod != 5)
  tmp_fertility_rates <- fertility_rates %>% filter(mod != 5)
  
  fnl_mortality_rates <- bind_rows(tmp_mortality_rates, new_mortality_rates) %>% arrange(gender, loc, year, age)
  fnl_fertility_rates <- bind_rows(tmp_fertility_rates, new_fertility_rates) %>% arrange(gender, loc, year, age)
  
  population_locs <- update_pop(fnl_mortality_rates, fnl_fertility_rates, population_locs, per1K) # Correct population
  
  fnl_mortality_rates <- left_join(x = fnl_mortality_rates, y = population_locs, by = c("gender", "loc", "year", "age")) %>% mutate(population = population.y) %>% select(-c(population.x, population.y)) %>% select(gender, loc, year, age, deaths, population, death_rate, mod)
  fnl_fertility_rates <- left_join(x = fnl_fertility_rates, y = population_locs, by = c("gender", "loc", "year", "age")) %>% mutate(population = population.y) %>% select(-c(population.x, population.y)) %>% select(gender, loc, year, age, births, population, fertility_rate, mod)
  
  fnl_mortality_rates <- fnl_mortality_rates %>% mutate(death_rate     = compute_rate(count = deaths, pop = population) * ifelse(per1K, 1000, 1))
  fnl_fertility_rates <- fnl_fertility_rates %>% mutate(fertility_rate = compute_rate(count = births, pop = population) * ifelse(per1K, 1000, 1))
  
  
  list(mortality_rates = fnl_mortality_rates, fertility_rates = fnl_fertility_rates, population_locs = population_locs)
}
