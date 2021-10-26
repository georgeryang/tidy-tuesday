# Load packages
library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(stringi)
library(magick)

# Load data
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# Question: What are the ultra running races with the shortest average
# distances between aid stations in the United States?

# Explore data
tabyl(race$event)
# Check summary statistics and number of missing values
summary(race$distance)
sum(is.na(race$distance))
summary(race$aid_stations)
sum(is.na(race$aid_stations))

# Clean data
race_data <- race %>%
  # Filter out races with distance = 0 and aid_stations = 0
  filter(distance != 0 & aid_stations != 0 & country == "United States") %>%
  # Make sure event names are consistent
  mutate(event = stri_trans_general(str_to_title(event), id = "Latin-ASCII"),
         dist_per_station = distance / aid_stations)
  
tabyl(race_data$event)
summary(race_data$dist_per_station)

# Visualize data

race_plot <- race_data %>%
  group_by(event) %>%
  summarize(mean_dist_per_station = mean(dist_per_station)) %>%
  slice_min(mean_dist_per_station, n = 10) %>%
  ggplot(aes(x = mean_dist_per_station,
             y = reorder(event, -mean_dist_per_station))) +
  geom_bar(stat="identity", fill = "#f6bc66") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 28, face = "bold", color = "white"),
    plot.subtitle = element_text(size = 20, color = "white"),
    axis.title = element_text(size = 14, color = "white"),
    axis.text = element_text(size = 14, color = "white"),
    plot.caption = element_text(color = "white")) +
  labs(title = "What are the ultra running races with the shortest average \ndistances between aid stations in the United States?",
       subtitle = "Beginner ultra runners might want to consider one of these races first",
       caption = "Visualization: George R. Yang (@georgeryang) | Data: International Trail Running Association (ITRA)",
       x = "Average distance between aid stations in km (shorter distance is better)",
       y = "") +
  theme(plot.background = element_rect(fill = "grey20", color = NA),
        panel.background = element_rect(fill = "grey30", color = NA))

race_plot

# ggsave(filename = "tt_ultra_trail_running.jpg", plot = race_plot, dpi = 300, width = 16, height = 9)

 # Add image
runner_original <- image_read("runner.png")
runner_resized <- image_resize(runner_original, "500x", filter = "Triangle")

figure <- image_graph(width = 4800, height = 2700, res = 300)
race_plot
dev.off()

figure_final <- figure %>% image_composite(runner_resized, offset = "+4260+430")
figure_final

image_write(figure_final, path = "tt_ultra_trail_running.jpg", format = "jpg")

