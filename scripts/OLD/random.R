
stand_mort_age_gender <- mort %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender, age) %>% summarise(deaths = mean(deaths)) %>% ungroup()
tmp <- mort %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population)
stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = tmp, by = c("loc", "gender", "age"))
stand_mort_age_gender <- stand_mort_age_gender %>% mutate(mun_death_r_2018 = deaths / population)
stand_mort_age_gender <- stand_mort_age_gender %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
stand_mort_age_gender <- stand_mort_age_gender %>% filter(age != "0-9")
stand_mort_age_gender <- stand_mort_age_gender %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))

stand_mort_age_gender <- stand_mort_age_gender %>% left_join(y = pop_ref, by = c("gender", "age"))

# Calculate standardized death rates (AGAIN) based on the average of years 2017-2019, but now stratified by age group and gender
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_mort_age_gender <- stand_mort_age_gender %>% rename(mun = loc) %>% group_by(gender, mun, year, age) %>% summarise(pop_n = sum(population),
                                                                                                                        deaths = sum(deaths),
                                                                                                                        death_r = (sum(deaths) / sum(population)),
                                                                                                                        death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()



stand_mort_gender <- mort %>% filter(year > 2016, year < 2020) %>% group_by(loc, gender) %>% summarise(deaths = mean(deaths)) %>% ungroup()
tmp <- mort %>% filter(year == 2018) %>% dplyr::select(loc, gender, age, year, population) %>% group_by(loc, gender, year) %>% summarise(population = sum(population)) %>% ungroup()
stand_mort_gender <- stand_mort_gender %>% left_join(y = tmp, by = c("loc", "gender"))
stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018 = deaths / population)
stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018 = ifelse(population == 0, 0, mun_death_r_2018))
stand_mort_gender <- stand_mort_gender %>% mutate(mun_death_r_2018_log = log(mun_death_r_2018))

stand_mort_gender <- stand_mort_gender %>% left_join(y = (pop_ref %>% group_by(gender, natl_pop_t) %>% summarise(natl_pop_n = sum(natl_pop_n)) %>% ungroup() %>% mutate(natl_pop_p = natl_pop_n / natl_pop_t) %>% dplyr::select(gender, natl_pop_n, natl_pop_t, natl_pop_p)), by = c("gender"))

# Calculate standardized death rates (AGAIN) based on the average of years 2017-2019, but now stratified by gender only
national_pop_total <- total_pop %>% filter(year == 2018) %>% dplyr::select(population) %>% sum() %>% c() %>% unlist() %>% unname()
stand_mort_gender <- stand_mort_gender %>% rename(mun = loc) %>% group_by(gender, mun, year) %>% summarise(pop_n = sum(population),
                                                                                                           deaths = sum(deaths),
                                                                                                           death_r = (sum(deaths) / sum(population)),
                                                                                                           death_r_std = (sum(mun_death_r_2018 * natl_pop_n) / national_pop_total)) %>% ungroup()

# Un