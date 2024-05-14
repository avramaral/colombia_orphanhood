source("header.R")
source("PROCESS_DATA/aux_orphanhood.R")
source("PROCESS_DATA/PAPER/aux_paper.R")

##################################################
# READ DATA
##################################################

valid_muns <- readRDS(file = "PROCESS_DATA/valid_muns.RDS")
geo_info <- read_csv(file = "DATA/geo_info.csv", col_types = "fcfcfcfc"); deps <- unique(geo_info$dep)
geo_info <- geo_info %>% filter(mun %in% valid_muns)

# NATIONAL

nat_prev_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/national_pre_orphans_abs.csv", col_types = cols())
nat_prev_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/national_pre_orphans_per_100000.csv", col_types = cols())
nat_inci_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/national_inc_orphans_abs.csv", col_types = cols())
nat_inci_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/national_inc_orphans_per_100000.csv", col_types = cols())

# REGION

reg_prev_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/region_pre_orphans_abs.csv", col_types = cols())
reg_prev_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/region_pre_orphans_per_100000.csv", col_types = cols())
reg_inci_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/region_inc_orphans_abs.csv", col_types = cols())
reg_inci_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/region_inc_orphans_per_100000.csv", col_types = cols())

# DEPARTMENT

dep_prev_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/department_pre_orphans_abs.csv", col_types = cols())
dep_prev_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/department_pre_orphans_per_100000.csv", col_types = cols())
dep_inci_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/department_inc_orphans_abs.csv", col_types = cols())
dep_inci_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/department_inc_orphans_per_100000.csv", col_types = cols())

# DEPARTMENT

dep_prev_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/department_pre_orphans_abs.csv", col_types = cols())
dep_prev_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/department_pre_orphans_per_100000.csv", col_types = cols())
dep_inci_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/department_inc_orphans_abs.csv", col_types = cols())
dep_inci_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/department_inc_orphans_per_100000.csv", col_types = cols())

# MUNICIPALITY

mun_inci_abs <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/municipality_inc_orphans_abs.csv", col_types = cols())
mun_inci_rel <- read_csv(file = "PROCESS_DATA/PAPER/TABLES/municipality_inc_orphans_per_100000.csv", col_types = cols())

##################################################
# CREATE EMPTY TABLES
##################################################

main_table <- bind_cols(tibble(count = 1:40), as_tibble(data.frame(level = NA, location = NA, prevalence_count = NA, prevalence_rate = NA, incidence_count = NA, incidence_rate = NA, stringsAsFactors = FALSE)))
main_table <- main_table %>% select(-count) %>% mutate_all(list(as.numeric)) %>% mutate(level = as.character(level), location = as.character(location))

secondary_table <- bind_cols(tibble(count = 1:40), as_tibble(data.frame(level = NA, location = NA, prevalence_count_female = NA, prevalence_count_male = NA, prevalence_count_both = NA, prevalence_rate_female = NA, prevalence_rate_male = NA, prevalence_rate_both = NA, incidence_count_female = NA, incidence_count_male = NA, incidence_count_both = NA, incidence_rate_female = NA, incidence_rate_male = NA, incidence_rate_both = NA, stringsAsFactors = FALSE)))
secondary_table <- secondary_table %>% select(-count) %>% mutate_all(list(as.numeric)) %>% mutate(level = as.character(level), location = as.character(location))

tables_mun_per_dep <- list()
deps_int <- sort(as.integer(as.character(deps)))
i <- 0
for (d in deps_int) {
  i <- i + 1
  tmp_geo_info <- geo_info %>% filter(dep == d)
  tables_mun_per_dep[[i]] <- bind_cols(tibble(count = 1:nrow(tmp_geo_info)), as_tibble(data.frame(level = NA, location = NA, prevalence_count_female = NA, prevalence_count_male = NA, prevalence_count_both = NA, prevalence_rate_female = NA, prevalence_rate_male = NA, prevalence_rate_both = NA, incidence_count_female = NA, incidence_count_male = NA, incidence_count_both = NA, incidence_rate_female = NA, incidence_rate_male = NA, incidence_rate_both = NA, stringsAsFactors = FALSE)))
  tables_mun_per_dep[[i]] <- tables_mun_per_dep[[i]] %>% select(-count) %>% mutate_all(list(as.numeric)) %>% mutate(level = as.character(level), location = as.character(location))
  tables_mun_per_dep[[i]][, "level"] <- "Municipality"
}
all_dep_names <- geo_info %>% select(c("dep", "dep_name")) %>% distinct() %>% mutate(dep = as.integer(as.character(dep))) %>% arrange(dep) %>% select(dep_name) %>% c() %>% unlist() %>% unname()
names(tables_mun_per_dep) <- all_dep_names

##################################################
# INPUT DATA
##################################################

##############################
# MAIN TABLE
##############################

# NATIONAL

main_table[1, "level"] <- "National"
main_table[1, "location"] <- "Colombia"
main_table[1, "prevalence_count"] <- sum(nat_prev_abs$n_orp)
main_table[1, "prevalence_rate"] <- sum(nat_prev_rel$n_orp)
main_table[1, "incidence_count"] <- nat_inci_abs %>% filter(year == 2021) %>% select(n_orp) %>% sum()
main_table[1, "incidence_rate"] <- nat_inci_rel %>% filter(year == 2021) %>% select(n_orp) %>% sum()

# REGION

main_table[2:7, "level"] <- "Region"
main_table[2:7, "location"] <- reg_prev_abs$reg_name %>% unique() %>% sort()
main_table[2:7, "prevalence_count"] <- reg_prev_abs %>% group_by(reg_name) %>% summarise(total_orp = sum(n_orp)) %>% select(total_orp) %>% c() %>% unlist() %>% unname()
main_table[2:7, "prevalence_rate"] <- reg_prev_rel %>% group_by(reg_name) %>% summarise(total_orp = sum(n_orp)) %>% select(total_orp) %>% c() %>% unlist() %>% unname()
main_table[2:7, "incidence_count"] <- reg_inci_abs %>% filter(year == 2021) %>% group_by(reg_name) %>% summarise(total_orp = sum(n_orp)) %>% select(total_orp) %>% c() %>% unlist() %>% unname()
main_table[2:7, "incidence_rate"] <- reg_inci_rel %>% filter(year == 2021) %>% group_by(reg_name) %>% summarise(total_orp = sum(n_orp)) %>% select(total_orp) %>% c() %>% unlist() %>% unname()

# DEPARTMENT

main_table[8:40, "level"] <- "Department"
main_table[8:40, "location"] <- dep_prev_abs$dep_name %>% unique() %>% sort()
main_table[8:40, "prevalence_count"] <- dep_prev_abs %>% group_by(dep_name) %>% summarise(total_orp = sum(n_orp)) %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(total_orp) %>% c() %>% unlist() %>% unname()
main_table[8:40, "prevalence_rate"] <-  dep_prev_rel %>% group_by(dep_name) %>% summarise(total_orp = sum(n_orp)) %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(total_orp) %>% c() %>% unlist() %>% unname()
main_table[8:40, "incidence_count"] <-  dep_inci_abs %>% filter(year == 2021) %>% group_by(dep_name) %>% summarise(total_orp = sum(n_orp)) %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(total_orp) %>% c() %>% unlist() %>% unname()
main_table[8:40, "incidence_rate"] <-   dep_inci_rel %>% filter(year == 2021) %>% group_by(dep_name) %>% summarise(total_orp = sum(n_orp)) %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(total_orp) %>% c() %>% unlist() %>% unname()

# main_table <- main_table %>% mutate(across(prevalence_count:incidence_rate, scales::label_comma())) 
colnames(main_table) <- c("Level", "Location", "Prevalence count in 2021", "Prevalence rate (per 100,000 children) in 2021", "Incidence count in 2021", "Inicidence rate (per 100,000 children) in 2021")

##############################
# SECONDARY TABLE
##############################

secondary_table[, c(1, 2, 5, 8, 11, 14)] <- main_table

# NATIONAL

secondary_table[1, "prevalence_count_female"] <- nat_prev_abs %>% filter(gender == "Female") %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[1, "prevalence_count_male"  ] <- nat_prev_abs %>% filter(gender == "Male"  ) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[1, "prevalence_rate_female"] <- nat_prev_rel %>% filter(gender == "Female") %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[1, "prevalence_rate_male"  ] <- nat_prev_rel %>% filter(gender == "Male"  ) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[1, "incidence_count_female"] <- nat_inci_abs %>% filter(year == 2021, gender == "Female") %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[1, "incidence_count_male"  ] <- nat_inci_abs %>% filter(year == 2021, gender == "Male"  ) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[1, "incidence_rate_female"] <- nat_inci_rel %>% filter(year == 2021, gender == "Female") %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[1, "incidence_rate_male"  ] <- nat_inci_rel %>% filter(year == 2021, gender == "Male"  ) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

# REGION

secondary_table[2:7, "prevalence_count_female"] <- reg_prev_abs %>% filter(gender == "Female") %>% arrange(reg_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[2:7, "prevalence_count_male"  ] <- reg_prev_abs %>% filter(gender == "Male"  ) %>% arrange(reg_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[2:7, "prevalence_rate_female"] <- reg_prev_rel %>% filter(gender == "Female") %>% arrange(reg_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[2:7, "prevalence_rate_male"  ] <- reg_prev_rel %>% filter(gender == "Male"  ) %>% arrange(reg_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[2:7, "incidence_count_female"] <- reg_inci_abs %>% filter(year == 2021, gender == "Female") %>% arrange(reg_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[2:7, "incidence_count_male"  ] <- reg_inci_abs %>% filter(year == 2021, gender == "Male"  ) %>% arrange(reg_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[2:7, "incidence_rate_female"] <- reg_inci_rel %>% filter(year == 2021, gender == "Female") %>% arrange(reg_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[2:7, "incidence_rate_male"  ] <- reg_inci_rel %>% filter(year == 2021, gender == "Male"  ) %>% arrange(reg_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

# DEPARTMENT

secondary_table[8:40, "prevalence_count_female"] <- dep_prev_abs %>% filter(gender == "Female") %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[8:40, "prevalence_count_male"  ] <- dep_prev_abs %>% filter(gender == "Male"  ) %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[8:40, "prevalence_rate_female"] <- dep_prev_rel %>% filter(gender == "Female") %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[8:40, "prevalence_rate_male"  ] <- dep_prev_rel %>% filter(gender == "Male"  ) %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[8:40, "incidence_count_female"] <- dep_inci_abs %>% filter(year == 2021, gender == "Female") %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[8:40, "incidence_count_male"  ] <- dep_inci_abs %>% filter(year == 2021, gender == "Male"  ) %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

secondary_table[8:40, "incidence_rate_female"] <- dep_inci_rel %>% filter(year == 2021, gender == "Female") %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
secondary_table[8:40, "incidence_rate_male"  ] <- dep_inci_rel %>% filter(year == 2021, gender == "Male"  ) %>% mutate(dep_name = as.factor(dep_name)) %>% arrange(dep_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()

# secondary_table <- secondary_table %>% mutate(across(c("prevalence_count_female", "prevalence_count_male", "prevalence_rate_female", "prevalence_rate_male", "incidence_count_female", "incidence_count_male", "incidence_rate_female", "incidence_rate_male"), scales::label_comma())) 
full_col_names <- c("Level", "Location", "Prevalence count for Mothers in 2021", "Prevalence count for Fathers in 2021", "Prevalence count for Both in 2021", "Prevalence rate (per 100,000 children) for Mothers in 2021", "Prevalence rate (per 100,000 children) for Fathers in 2021", "Prevalence rate (per 100,000 children) for Both in 2021", "Incidence count for Mothers in 2021", "Incidence count for Fathers in 2021", "Incidence count for Both in 2021", "Inicidence rate (per 100,000 children) for Mothers in 2021", "Inicidence rate (per 100,000 children) for Fathers in 2021", "Inicidence rate (per 100,000 children) for Both in 2021")
colnames(secondary_table) <- full_col_names
  
##############################
# MUNICIPALITY TABLES
##############################

cp_full_col_names <- full_col_names
cp_full_col_names[c(6:8, 12:14)] <- c("Prevalence rate (per 1,000 children) for Mothers in 2021", "Prevalence rate (per 1,000 children) for Fathers in 2021", "Prevalence rate (per 1,000 children) for Both in 2021", "Inicidence rate (per 1,000 children) for Mothers in 2021", "Inicidence rate (per 1,000 children) for Fathers in 2021", "Inicidence rate (per 1,000 children) for Both in 2021")

i <- 0
for (d in deps_int) {
  i <- i + 1
  tmp_table <- tables_mun_per_dep[[i]]
  tmp_geo_info <- geo_info %>% filter(dep == d)
  muns <- tmp_geo_info$mun
  
  ###############
  # PROCESSING
  ###############
  
  tmp_table[, "location"] <- tmp_geo_info$mun_name %>% sort()
  ### PREVALENCE HAS NOT BEEN COMPUTED AND THEREFORE IS NOT INCLUDED
  tmp_table <- tmp_table[, -(3:8)]
  
  tmp_table[, "incidence_count_female"] <- mun_inci_abs %>% filter(year == 2021, mun %in% muns, gender == "Female") %>% arrange(mun_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
  tmp_table[, "incidence_count_male"  ] <- mun_inci_abs %>% filter(year == 2021, mun %in% muns, gender == "Male"  ) %>% arrange(mun_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
  tmp_table[, "incidence_count_both"] <- tmp_table[, "incidence_count_female"] + tmp_table[, "incidence_count_male"]
  
  tmp_table[, "incidence_rate_female"]   <- mun_inci_rel %>% filter(year == 2021, mun %in% muns, gender == "Female") %>% arrange(mun_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
  tmp_table[, "incidence_rate_male"  ]   <- mun_inci_rel %>% filter(year == 2021, mun %in% muns, gender == "Male"  ) %>% arrange(mun_name) %>% select(n_orp) %>% c() %>% unlist() %>% unname()
  tmp_table[, "incidence_rate_both"] <- tmp_table[, "incidence_rate_female"] + tmp_table[, "incidence_rate_male"]
  
  # tmp_table <- tmp_table %>% mutate(across(incidence_count_female:incidence_rate_both, scales::label_comma())) 
  colnames(tmp_table) <- cp_full_col_names[-(3:8)]
  
  ###############
  
  tables_mun_per_dep[[i]] <- tmp_table
}

##############################
# LAST FORMATTING
##############################

###############
# Main table
###############

main_table[, 4] <- paste(round(unname(unlist(c(main_table[, 4]))) / 100000 * 100, 3), "%", sep = "")
main_table[, 6] <- paste(round(unname(unlist(c(main_table[, 6]))) / 100000 * 100, 3), "%", sep = "")
colnames(main_table)[c(4, 6)] <- c("Prevalence rate (% of children aged 0-17 years) in 2021", "Inicidence rate (% of children aged 0-17 years) in 2021")
main_table <- main_table %>% mutate(across(c(3, 5), scales::label_comma())) 

nn <- "Colombia"
rr <- unname(unlist(c(unique(main_table[main_table$Level == "Region",     "Location"]))))
dd <- unname(unlist(c(unique(main_table[main_table$Level == "Department", "Location"]))))

geo_info_table <- geo_info %>% select(nat_name, reg_name, dep_name) %>% distinct()
n_dep_per_reg  <- geo_info_table$reg_name %>% table %>% c() %>% unname()

main_table_values <- main_table[, -c(1:2)]
main_table_locals <- bind_cols(tibble(count = 1:40), as_tibble(data.frame("Country" = "", "Region" = "", "Department" = "", stringsAsFactors = FALSE)))
main_table_locals <- main_table_locals %>% select(-count)
# National
main_table_locals[1, 1] <- nn
# Regional
main_table_locals[2, 2] <- rr[1]
main_table_locals[2 + sum(n_dep_per_reg[1:1]) + 1, 2] <- rr[2]
main_table_locals[2 + sum(n_dep_per_reg[1:2]) + 2, 2] <- rr[3]
main_table_locals[2 + sum(n_dep_per_reg[1:3]) + 3, 2] <- rr[4]
main_table_locals[2 + sum(n_dep_per_reg[1:4]) + 4, 2] <- rr[5]
main_table_locals[2 + sum(n_dep_per_reg[1:5]) + 5, 2] <- rr[6]
main_table_locals[2 + sum(n_dep_per_reg[1:5]) + 5, 2]
# Department
main_table_locals[3:(3 + n_dep_per_reg[1] - 1), 3] <- geo_info_table[geo_info_table$reg_name == rr[1], "dep_name"]
main_table_locals[(3 + sum(n_dep_per_reg[1:1]) + 1):(3 + sum(n_dep_per_reg[1:2])), 3] <- geo_info_table[geo_info_table$reg_name == rr[2], "dep_name"]
main_table_locals[(3 + sum(n_dep_per_reg[1:2]) + 2):(4 + sum(n_dep_per_reg[1:3])), 3] <- geo_info_table[geo_info_table$reg_name == rr[3], "dep_name"]
main_table_locals[(3 + sum(n_dep_per_reg[1:3]) + 3):(5 + sum(n_dep_per_reg[1:4])), 3] <- geo_info_table[geo_info_table$reg_name == rr[4], "dep_name"]
main_table_locals[(3 + sum(n_dep_per_reg[1:4]) + 4):(6 + sum(n_dep_per_reg[1:5])), 3] <- geo_info_table[geo_info_table$reg_name == rr[5], "dep_name"]
main_table_locals[(3 + sum(n_dep_per_reg[1:5]) + 5):(7 + sum(n_dep_per_reg[1:6])), 3] <- geo_info_table[geo_info_table$reg_name == rr[6], "dep_name"]

main_table_tmp <- bind_cols(main_table_locals, main_table_values)
main_table_tmp[ , 4:ncol(main_table_tmp)] <- NA
main_table_tmp[1, 4:ncol(main_table_tmp)] <- main_table[1, 3:ncol(main_table)]
for (i in 1:length(rr)) {
  n_row_ori <- which(main_table$Location == rr[i])
  n_row_new <- which(main_table_locals$Region == rr[i])
  main_table_tmp[n_row_new, 4:ncol(main_table_tmp)] <- main_table[n_row_ori, 3:ncol(main_table)]
}
for (j in 1:length(dd)) {
  n_row_ori <- which(main_table$Location == dd[j])
  n_row_new <- which(main_table_locals$Department == dd[j])
  main_table_tmp[n_row_new, 4:ncol(main_table_tmp)] <- main_table[n_row_ori, 3:ncol(main_table)]
}

main_table <- main_table_tmp

###############
# Secondary table
###############

secondary_table[,  6] <- paste(round(unname(unlist(c(secondary_table[,  6]))) / 100000 * 100, 3), "%", sep = "")
secondary_table[,  7] <- paste(round(unname(unlist(c(secondary_table[,  7]))) / 100000 * 100, 3), "%", sep = "")
secondary_table[,  8] <- paste(round(unname(unlist(c(secondary_table[,  8]))) / 100000 * 100, 3), "%", sep = "")
secondary_table[, 12] <- paste(round(unname(unlist(c(secondary_table[, 12]))) / 100000 * 100, 3), "%", sep = "")
secondary_table[, 13] <- paste(round(unname(unlist(c(secondary_table[, 13]))) / 100000 * 100, 3), "%", sep = "")
secondary_table[, 14] <- paste(round(unname(unlist(c(secondary_table[, 14]))) / 100000 * 100, 3), "%", sep = "")
colnames(secondary_table)[c(6:8, 12:14)] <- c("Prevalence rate (% of children aged 0-17 years) for Mothers in 2021", 
                                              "Prevalence rate (% of children aged 0-17 years) for Fathers in 2021",
                                              "Prevalence rate (% of children aged 0-17 years) for Both in 2021",
                                              "Inicidence rate (% of children aged 0-17 years) for Mothers in 2021",
                                              "Inicidence rate (% of children aged 0-17 years) for Fathers in 2021",
                                              "Inicidence rate (% of children aged 0-17 years) for Both in 2021")
secondary_table <- secondary_table %>% mutate(across(c(3:5, 9:11), scales::label_comma())) 

secondary_table_values <- secondary_table[, -c(1:2)]
secondary_table_locals <- bind_cols(tibble(count = 1:40), as_tibble(data.frame("Country" = "", "Region" = "", "Department" = "", stringsAsFactors = FALSE)))
secondary_table_locals <- secondary_table_locals %>% select(-count)
# National
secondary_table_locals[1, 1] <- nn
# Regional
secondary_table_locals[2, 2] <- rr[1]
secondary_table_locals[2 + sum(n_dep_per_reg[1:1]) + 1, 2] <- rr[2]
secondary_table_locals[2 + sum(n_dep_per_reg[1:2]) + 2, 2] <- rr[3]
secondary_table_locals[2 + sum(n_dep_per_reg[1:3]) + 3, 2] <- rr[4]
secondary_table_locals[2 + sum(n_dep_per_reg[1:4]) + 4, 2] <- rr[5]
secondary_table_locals[2 + sum(n_dep_per_reg[1:5]) + 5, 2] <- rr[6]
secondary_table_locals[2 + sum(n_dep_per_reg[1:5]) + 5, 2]
# Department
secondary_table_locals[3:(3 + n_dep_per_reg[1] - 1), 3] <- geo_info_table[geo_info_table$reg_name == rr[1], "dep_name"]
secondary_table_locals[(3 + sum(n_dep_per_reg[1:1]) + 1):(3 + sum(n_dep_per_reg[1:2])), 3] <- geo_info_table[geo_info_table$reg_name == rr[2], "dep_name"]
secondary_table_locals[(3 + sum(n_dep_per_reg[1:2]) + 2):(4 + sum(n_dep_per_reg[1:3])), 3] <- geo_info_table[geo_info_table$reg_name == rr[3], "dep_name"]
secondary_table_locals[(3 + sum(n_dep_per_reg[1:3]) + 3):(5 + sum(n_dep_per_reg[1:4])), 3] <- geo_info_table[geo_info_table$reg_name == rr[4], "dep_name"]
secondary_table_locals[(3 + sum(n_dep_per_reg[1:4]) + 4):(6 + sum(n_dep_per_reg[1:5])), 3] <- geo_info_table[geo_info_table$reg_name == rr[5], "dep_name"]
secondary_table_locals[(3 + sum(n_dep_per_reg[1:5]) + 5):(7 + sum(n_dep_per_reg[1:6])), 3] <- geo_info_table[geo_info_table$reg_name == rr[6], "dep_name"]

secondary_table_tmp <- bind_cols(secondary_table_locals, secondary_table_values)
secondary_table_tmp[ , 4:ncol(secondary_table_tmp)] <- NA
secondary_table_tmp[1, 4:ncol(secondary_table_tmp)] <- secondary_table[1, 3:ncol(secondary_table)]
for (i in 1:length(rr)) {
  n_row_ori <- which(secondary_table$Location == rr[i])
  n_row_new <- which(secondary_table_locals$Region == rr[i])
  secondary_table_tmp[n_row_new, 4:ncol(secondary_table_tmp)] <- secondary_table[n_row_ori, 3:ncol(secondary_table)]
}
for (j in 1:length(dd)) {
  n_row_ori <- which(secondary_table$Location == dd[j])
  n_row_new <- which(secondary_table_locals$Department == dd[j])
  secondary_table_tmp[n_row_new, 4:ncol(secondary_table_tmp)] <- secondary_table[n_row_ori, 3:ncol(secondary_table)]
}

secondary_table_tmp <- secondary_table_tmp[, c(1, 2, 3, 6, 9, 12, 15, 4, 7, 10, 13, 5, 8, 11, 14)]
secondary_table     <- secondary_table_tmp

###############
# Supplementary tables
###############

i <- 0
for (d in deps_int) {
  i <- i + 1
  tmp <- tables_mun_per_dep[[i]]
  
  tmp[,  6] <- paste(round(unname(unlist(c(tmp[,  6]))) / 100000 * 100, 3), "%", sep = "")
  tmp[,  7] <- paste(round(unname(unlist(c(tmp[,  7]))) / 100000 * 100, 3), "%", sep = "")
  tmp[,  8] <- paste(round(unname(unlist(c(tmp[,  8]))) / 100000 * 100, 3), "%", sep = "")
  colnames(tmp)[6:8] <- c("Inicidence rate (% of children aged 0-17 years) for Mothers in 2021",
                          "Inicidence rate (% of children aged 0-17 years) for Fathers in 2021",
                          "Inicidence rate (% of children aged 0-17 years) for Both in 2021")
  tmp <- tmp %>% mutate(across(3:5, scales::label_comma())) 
  tmp <- tmp[, c(1, 2, 5, 8, 3, 6, 4, 7)]
  
  tmp <- tmp[, -1]
  colnames(tmp)[1] <- "Municipality"
  
  tables_mun_per_dep[[i]] <- tmp
}

##############################
# SAVE TABLES
##############################

# >   excluded_rows$dep_name %>% unique
# [1] "Meta"     "Arauca"   "Vichada"  "Caquetá"  "Amazonas" "Guainía"  "Guaviare" "Vaupés"  
# >   print(excluded_rows$reg_name %>% unique())
# [1] "Llanos"   "Amazonía"

flagged_dep <- c("Meta", "Arauca", "Vichada", "Caquetá", "Amazonas", "Guainía", "Guaviare", "Vaupés")
flagged_reg <- c("Llanos", "Amazonía")
flagged_nat <- c("Colombia")

for (i in 1:nrow(main_table)) {
  tmp_dep <- main_table$Department[i]
  tmp_reg <- main_table$Region[i]
  tmp_nat <- main_table$Country[i]
  
  if (tmp_dep %in% flagged_dep) { tmp_dep <- paste(tmp_dep, "*", sep = "") }
  if (tmp_reg %in% flagged_reg) { tmp_reg <- paste(tmp_reg, "*", sep = "") }
  if (tmp_nat %in% flagged_nat) { tmp_nat <- paste(tmp_nat, "*", sep = "") }
  
  main_table$Department[i] <- tmp_dep
  main_table$Region[i]     <- tmp_reg
  main_table$Country[i]    <- tmp_nat
}

for (i in 1:nrow(secondary_table)) {
  tmp_dep <- secondary_table$Department[i]
  tmp_reg <- secondary_table$Region[i]
  tmp_nat <- secondary_table$Country[i]
  
  if (tmp_dep %in% flagged_dep) { tmp_dep <- paste(tmp_dep, "*", sep = "") }
  if (tmp_reg %in% flagged_reg) { tmp_reg <- paste(tmp_reg, "*", sep = "") }
  if (tmp_nat %in% flagged_nat) { tmp_nat <- paste(tmp_nat, "*", sep = "") }
  
  secondary_table$Department[i] <- tmp_dep
  secondary_table$Region[i]     <- tmp_reg
  secondary_table$Country[i]    <- tmp_nat
}

write_csv(x = main_table, file = "PROCESS_DATA/PAPER/TABLES/FORMATTED_TABLES/main_table.csv")
write_csv(x = secondary_table, file = "PROCESS_DATA/PAPER/TABLES/FORMATTED_TABLES/table_appendix.csv")

i <- 0
for (d in deps_int) {
  i <- i + 1
  tmp_dep_name <- all_dep_names[i]
  tmp_dep_name <- transform_string(tmp_dep_name)
  write_csv(x = tables_mun_per_dep[[i]], file = paste("PROCESS_DATA/PAPER/TABLES/FORMATTED_TABLES/MUN_PER_DEP/mun_in_", tmp_dep_name, ".csv", sep = ""))
}

##############################
##############################

