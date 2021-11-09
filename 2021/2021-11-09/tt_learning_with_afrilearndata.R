# TidyTuesday, 2021-11-09, Learning with afrilearndata
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-11-09/readme.md

# Code help
# https://github.com/afrimapr/afrilearndata
# https://github.com/pyykkojuha/tidytuesday/blob/main/R/2021_46/tidy_2021_46.R

# Load packages
library(tidyverse)
library(afrilearndata)
library(tmap)
library(magick)
library(viridisLite)


# Create 2000 and 2020 population maps

pop_2000 <- tm_shape(afripop2000) +
  tm_raster(palette = rev(rocket(5)),
            breaks=c(0,2,20,200,2000,25000),
            title = "Number of people per square kilometer") +
  tm_shape(africountries) +
  tm_borders("white", lwd = .5) +
  tm_shape(afrihighway) +
  tm_lines(col = "red") + 
  tm_shape(africapitals) +
  tm_symbols(col = "blue", alpha=0.4, scale = .6) +
  tm_layout(frame = FALSE,
            legend.outside = TRUE, 
            legend.title.color = "gray85",
            legend.title.size = 1.25, 
            legend.text.color = "gray85",
            legend.text.size = 1.25, 
            main.title = "Africa Population Density (2000)",
            main.title.size = 2,
            main.title.color = "gray85", 
            title = "Visualization: George R. Yang (@georgeryang)\nData: afrilearndata",
            title.size = 1.25,
            title.color = "gray85",
            bg.color="gray15")

pop_2020 <- tm_shape(afripop2020) +
  tm_raster(palette = rev(rocket(5)),
            breaks=c(0,2,20,200,2000,25000),
            title = "Number of people per square kilometer") +
  tm_shape(africountries) +
  tm_borders("white", lwd = .5) +
  tm_shape(afrihighway) +
  tm_lines(col = "red") + 
  tm_shape(africapitals) +
  tm_symbols(col = "blue", alpha=0.4, scale = .6) +
  tm_layout(frame = FALSE,
            legend.outside = TRUE, 
            legend.title.color = "gray85",
            legend.title.size = 1.25, 
            legend.text.color = "gray85",
            legend.text.size = 1.25, 
            main.title = "Africa Population Density (2020)",
            main.title.size = 2,
            main.title.color = "gray85", 
            title = "Visualization: George R. Yang (@georgeryang)\nData: afrilearndata",
            title.size = 1.25,
            title.color = "gray85",
            bg.color="gray15")

# Save maps

tmap_save(pop_2000, filename="pop_2000.png", width = 16, height = 9, units = "in")
tmap_save(pop_2020, filename="pop_2020.png", width = 16, height = 9, units = "in")

# Create GIF

list.files(path='C:/Users/gryang/Documents/GitHub/tidy-tuesday/2021/2021-11-09',
  pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>%
  image_join() %>%
  image_animate(fps=1) %>%
  image_write("tt_learning_with_afrilearndata.gif")
