# Load packages
library(tidytuesdayR)
library(tidyverse)
library(tidytext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2021-10-19')
pumpkins <- tuesdata$pumpkins

tabyl(pumpkins$state_prov)

# Question: which regions have the heaviest pumpkins by type

# Clean data
pumpkins_data <- pumpkins %>%
  separate(id, into = c("year", "type")) %>%
  mutate(type = fct_recode(type,
                           "Field Pumpkin" = "F",
                           "Giant Pumpkin" = "P",
                           "Giant Squash" = "S",
                           "Giant Watermelon" = "W",
                           "Long Gourd" = "L",
                           "Tomato" = "T"),
         weight_lbs = parse_number(weight_lbs)
         ) %>%
  unite("region", state_prov:country, sep = ", ") %>%
  filter(!str_detect(region, "Entries") & !str_detect(region, "Other"))

tabyl(pumpkins_data$region)

# Visualize data

pumpkins_plot <- pumpkins_data %>%
  group_by(type, region) %>%
  summarize(avg_weight = mean(weight_lbs, na.rm = T)) %>%
  slice_max(avg_weight, n = 5) %>%
  mutate(region = reorder_within(region, avg_weight, type)) %>%
  ggplot(aes(avg_weight, region, fill = type)) +
  geom_col() +
  facet_wrap( ~ type, scales = "free") +
  scale_y_reordered() +
  theme_bw() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 28, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Oranges") +
  labs(x = "Average pumpkin weight in pounds",
       y = "",
       title =
         "Top Five Regions with the Highest Average Pumpkin Weights by Type")

pumpkins_plot

ggsave("tt_big_pumpkins.png", width = 16, height = 9) 
