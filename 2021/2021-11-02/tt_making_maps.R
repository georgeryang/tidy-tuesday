# Load packages
library(tidyverse)
library(geojsonio)
library(extrafont)

miami_neighborhoods <- geojson_sf("Miami_Neighborhoods_Shapefile.geojson")
miami_parks <- geojson_sf("City_Parks.geojson")

# Create map
miami_map <- 
  ggplot() +
  geom_sf(
    data = miami_neighborhoods,
    aes(geometry = geometry),
    fill = "gray20",
    color = NA
  ) +
  geom_sf(
    data = miami_parks,
    aes(geometry = geometry),
    fill = "#A3EBB1",
    color = NA
  ) +
  theme_void() +
  theme(plot.background = element_rect(color="gray90", fill="gray90"),
        panel.background = element_rect(color="gray90", fill="gray90"),
        plot.title = element_text(size = 28, face = "bold", family = "Segoe UI", color = "#443A84"),
        plot.subtitle = element_text(size = 14, family = "Segoe UI", color = "#02B7DD"),
        plot.caption = element_text(family = "Segoe UI", color = "#D93CAF")
  ) +
  labs(title = "City Parks in Miami",
       subtitle = "There seems to be a shortage of parks in the city of Miami",
       caption = "Visualization: George R. Yang (@georgeryang) | Data: Miami-Dade County's Open Data Hub") 

ggsave("tt_making_maps.png", width = 9, height = 9) 