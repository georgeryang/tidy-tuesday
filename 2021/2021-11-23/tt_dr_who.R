# TidyTuesday, 2021-11-23, Dr. Who
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-11-23/readme.md

# Load packages

library(tidytuesdayR)
library(tidyverse)
library(gridExtra)
library(grid)

# Load data

directors <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')


# Merge and clean data

episodes_1 <- episodes %>%
  mutate(season_episode = paste(season_number, episode_number, sep = "."))

imdb_1 <- imdb %>%
  mutate(season_episode = paste(season, ep_num, sep = "."))

merged <- episodes_1 %>%
  full_join(imdb_1, by = "season_episode", suffix = c ("_e", "_i")) %>%
  full_join(directors, by = "story_number") %>%
  full_join(writers, by = "story_number")


# Q: Most number of episodes and highest ratings by directors and writers

# Directors

director_n_plot <- merged %>%
  filter(!is.na(director)) %>%
  group_by(director) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  top_n(n = 5) %>%
  ggplot(aes(x = n, y = reorder(director, n))) +
  geom_bar(stat="identity", fill = "#00203c") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 14, by = 1)) +
  labs(title = "Directors with the Most Number of Episodes",
       x = "Number of episodes",
       y = "") +
  geom_label(aes(
    x = n,
    y = reorder(director, n),
    label = n
  ))

director_rating_plot <- merged %>%
  filter(!is.na(director)) %>%
  group_by(director) %>%
  summarize(mean_rating = mean(rating_i, na.rm = TRUE)) %>%
  arrange(-mean_rating) %>%
  top_n(n = 5) %>%
  ggplot(aes(x = mean_rating, y = reorder(director, mean_rating))) +
  geom_bar(stat="identity", fill = "#003b6f") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 9.5, by = 0.5)) +
  labs(title = "Directors with the Highest IMDB Ratings",
       x = "IMDB rating",
       y = "") +
  geom_label(aes(
    x = mean_rating,
    y = reorder(director, mean_rating),
    label = format(mean_rating, digits = 2)
  ))

# Writers

writer_n_plot <- merged %>%
  filter(!is.na(writer)) %>%
  group_by(writer) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  top_n(n = 5) %>%
  ggplot(aes(x = n, y = reorder(writer, n))) +
  geom_bar(stat = "identity", fill = "#6f8ea9") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 48, by = 5)) +
  labs(title = "Writers with the Most Number of Episodes",
       x = "Number of episodes",
       y = "") +
  geom_label(aes(
    x = n,
    y = reorder(writer, n),
    label = n
  ))

writer_rating_plot <- merged %>%
  filter(!is.na(writer)) %>%
  group_by(writer) %>%
  summarize(mean_rating = mean(rating_i, na.rm = TRUE)) %>%
  arrange(-mean_rating) %>%
  top_n(n = 5) %>%
  ggplot(aes(x = mean_rating, y = reorder(writer, mean_rating))) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 9.5, by = 0.5)) +
  geom_bar(stat="identity", fill = "#a6b8c7") +
  labs(title = "Writers with the Highest IMDB Ratings",
       x = "IMDB rating",
       y = "") +
  geom_label(aes(
    x = mean_rating,
    y = reorder(writer, mean_rating),
    label = format(mean_rating, digits = 2)
  ))


# Create grid

dr_who_plots <- grid.arrange(
  director_n_plot,
  director_rating_plot,
  writer_n_plot,
  writer_rating_plot,
  nrow = 2,
  ncol = 2,
  top = textGrob(
    "Doctor Who Writers and Directors with the Most Number of Episodes and Highest Ratings",
    gp = gpar(fontface = 2, fontsize = 16),
    x = 0,
    hjust = 0
  ),
  bottom = textGrob(
    "Visualization: George R. Yang (@georgeryang) | Data: datatardis (R package by Jonathan Kitt)",
    gp = gpar(fontface = 3, fontsize = 10),
    x = 1,
    hjust = 1
  )
)

plot(dr_who_plots)

ggsave(
  filename = "tt_dr_who.jpg",
  plot = dr_who_plots,
  dpi = 300,
  width = 16,
  height = 9
)
