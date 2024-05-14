source("header.R")
source("PROCESS_DATA/aux_process.R")
source("PROCESS_DATA/aux_orphanhood.R")

type.input <- "Municipality"

geo_info <- read_csv(file = "DATA/geo_info.csv", col_types = cols())
locs     <- geo_info$mun %>% as.character() %>% as.numeric() %>% sort()
yys      <- 2015:2021

count <- 0
for (yy in yys) {
  count <- count + 1
  tmp_data <- read_csv(file = paste("PROCESS_DATA/DATA/RESULTS/", toupper(type.input), "/", type.input, "_parents_deaths_orphans_with_age_summary_", yy, ".csv", sep = ""), col_types = cols())
  if (count == 1) {
    data <- tmp_data
  } else {
    data <- bind_rows(data, tmp_data)
  }
}

summarised_data <- data %>% group_by(year, child_age) %>% summarise(n_orphans = sum(orphans)) %>% ungroup()

# Compute rates instead

population <- read_csv(file = "DATA/Population from Census/pop_years_list_municipality.csv", col_types = cols())
population <- population %>% filter(Year <= 2021, Year >= 2015) %>% mutate(gender = Sex, loc = factor(Mun), year = Year, age = Age, population = Population) %>% select(gender, loc, year, age, population) %>% arrange(gender, loc, year, age)
population <- population %>% filter(loc %in% locs)

pop_2018_children <- create_pop_15_19()
pop_2018_children <- pop_2018_children %>% group_by(mun) %>% mutate(total = sum(population))
pop_2018_children <- pop_2018_children %>% filter(age <= 17)
pop_2018_15_17 <- pop_2018_children %>% group_by(mun, total) %>% summarize(population = sum(population)) %>% ungroup()
pop_2018_15_17 <- pop_2018_15_17 %>% mutate(prop = (population / total)) %>% select(-c(total, population))
colnames(pop_2018_15_17) <- c("loc", "prop")

population <- population %>% left_join(y = mutate(pop_2018_15_17, loc = factor(loc)), by = "loc")
population <- population %>% mutate(population = ifelse(age == "15-19", population * prop, population)) %>% select(-prop)

population_children <- population %>% filter(age %in% c("0-9", "10-14", "15-19")) %>% group_by(year, age) %>% summarise(total_pop = sum(population)) %>% ungroup()
population_children <- population_children %>% group_by(year, age) %>% summarise(total_pop = sum(total_pop)) %>% ungroup()

summarised_data <- summarised_data %>% mutate(age = ifelse(child_age %in% 0:9, "0-9", ifelse(child_age %in% 10:14, "10-14", "15-19")))
summarised_data <- summarised_data %>% select(year, age, n_orphans)
summarised_data <- summarised_data %>% group_by(year, age) %>% summarise(total_orphans = sum(n_orphans)) %>% ungroup()
summarised_data <- summarised_data %>% left_join(population_children, by = c("age", "year"))

summarised_data <- summarised_data %>% mutate(total_orphans_per_100k = ceiling(total_orphans / total_pop * 100000))
summarised_data <- summarised_data %>% mutate(age = ifelse(age == "15-19", "15-17", age))

pp_age <- ggplot(summarised_data) +
  geom_line(aes(x = year, y = total_orphans_per_100k, color = age)) +
  scale_color_manual(values = c("red", "blue", "green"), name = paste("Orphan age", sep = "")) + 
  scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021, expand = ) +
  scale_y_continuous(limits = c(0, max(summarised_data$total_orphans_per_100k)), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Number of orphans (per 100,000 children)", pattern = "") + 
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(filename = paste("PROCESS_DATA/PAPER/IMAGES/orphans_age.jpeg", sep = ""), plot = pp_age, width = 1700, height = 1200, units = c("px"), dpi = 300, bg = "white")


