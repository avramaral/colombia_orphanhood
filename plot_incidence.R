library("tidyverse")
library("patchwork")
library("viridis")
library("sf")
source("PROCESS_DATA/aux_process.R")

##################################################
# Read data
##################################################

type.input <- "Municipality" # c("Municipality", "Department")

if (type.input == "Municipality") {
  colombia <- readRDS(file = "DATA/colombia_map_municipality.RDS") %>% arrange(Code) %>% select(Code, geometry)
  
  orphans_count <- read_csv("RESULTS/orphans_by_year_mun.csv") %>% arrange(Code)
  orphans_rates <- read_csv("RESULTS/orphans_rate_by_year_mun.csv") %>% arrange(Code)
  
  orphans_count <- left_join(x = orphans_count, y = colombia, by = "Code")
  orphans_rates <- left_join(x = orphans_rates, y = colombia, by = "Code")
  
  orphans_count <- pivot_longer(orphans_count, cols = as.character(2015:2021), names_to = "Year", values_to = "Orphans") %>% select(Code, Municipality, Year, Orphans, geometry)
  orphans_rates <- pivot_longer(orphans_rates, cols = as.character(2015:2021), names_to = "Year", values_to = "Orphans") %>% select(Code, Municipality, Year, Orphans, geometry)
  orphans_count$Year <- as.double(orphans_count$Year)
  orphans_rates$Year <- as.double(orphans_rates$Year)
  colnames(orphans_count)[2] <- colnames(orphans_rates)[2] <- "Loc"
  
} else if (type.input == "Department") {
  stop("`Department` not implemented yet.")
} else { stop("Invalid `type.input`.") }


# Plotting orphan rates (per 1,000 )
for (yy in 2015:2021) {
  tmp_data <- orphans_rates %>% filter(Year == yy)
  tmp_data <- tmp_data %>% mutate_if(is.numeric, coalesce, 0) # Replace NA by 0
  
  mn <- min(tmp_data$Orphans, na.rm = T)
  mx <- max(tmp_data$Orphans, na.rm = T)
  
  breaks <- seq(0, 30, 2)
  labels <- paste0(seq(0, 28, 2), "-", c(seq(1, 27, 2), 30))
  
  tmp_data$category <- cut(tmp_data$Orphans, breaks = breaks, labels = labels, right = F)
  
  pp <- ggplot(data = tmp_data, aes(geometry = geometry)) + 
    geom_sf(aes(fill = category), color = "black") + 
    # facet_wrap(~ Year) +
    # scale_fill_viridis(breaks = labs, labels = labs, limits = c(labs[1], tail(labs, 1))) +
    scale_fill_viridis_d(guide = guide_legend(title = "Number of orphans")) +
    labs(x = "Longitude", y = "Latitude", title = paste("Number of new orphans (per 1,000 children) in ", yy, sep = ""), fill = "Number of orphans") + 
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 16)) #, hjust = 0.5))
  
  ggsave(filename = paste("PROCESS_DATA/IMAGES/INCIDENCE/orphans_rate_", yy,".jpeg", sep = ""), plot = pp, width = 3000, height = 3000, units = c("px"), dpi = 300, bg = "white")
  
}

