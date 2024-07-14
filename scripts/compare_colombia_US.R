suppressMessages(library(tidyverse))

home.dir <- "/Users/or105/Library/CloudStorage/OneDrive-ImperialCollegeLondon/OR_Work/OCAY_Program"
home.dir <- "/Users/avramaral/Library/CloudStorage/OneDrive-SharedLibraries-ImperialCollegeLondon/Ratmann, Oliver - OCAY_Program"
data.dir <- file.path(home.dir, "Colombia_primary_data")
out.dir <- file.path(home.dir, "Colombia_underreporting")

incidence_CL <- 0.61
incidence_US <- 0.50

prevalence_CL <- 4.55
prevalence_US <- 3.50

data <- as_tibble(data.frame(loc = c("USA", "Colombia"), inc = c(incidence_US, incidence_CL), pre = c(prevalence_US, prevalence_CL)))
data <- data %>% pivot_longer(cols = c(inc, pre), names_to = "metric", values_to = "value") %>% mutate(metric = recode(metric, inc = "Incidence", pre = "Prevalence"), loc = factor(loc, levels = c("Colombia", "USA")))

# Create the horizontal bar plot
pp <- ggplot(data, aes(x = metric, y = value, fill = loc)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(x = c(1.25, 2.25, 0.75, 1.75), y = value + 0.4, label = scales::percent(value / 100, accuracy = 0.01)),
             fill = "white", alpha = 1, hjust = 0.5,
             label.r = unit(0.15, "lines"),
             size = 12 / .pt,
             family = "LM Roman 10",
             label.padding = unit(0.2, "lines")) +
  scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0, 5.15), labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("Colombia" = "red", "USA" = "blue")) + 
  coord_flip() +
  labs(title = "Incidence and Prevalence (% of children aged 0-17 years)\nof all-cause orphanhood in Colombia and USA in 2021", x = "", y = "", fill = "Country") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10", size = 16), 
        plot.title = element_text(size = 16),
        axis.text.y = element_text(angle = 90, hjust = 0.5, margin = margin(r = 10)))
ggsave(pp, file = paste(out.dir, "/comparison_US_Colombia.png", sep = ""), width = 7, height = 6)
