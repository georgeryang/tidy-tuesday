# TidyTuesday, 2021-11-30, World Cup Cricket
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-11-30/readme.md

# Code help
# https://github.com/Annapurani93/women-ultra-runners/blob/main/Code.R

# Load packages
library(tidyverse)
library(janitor)
library(waffle)
library(ggtext)

# Load data
matches <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

# Explore data
head(matches)
matches %>%
  group_by(winner) %>%
  summarize(n = n()) %>%
  arrange(-n)


# Q: Share of top 3 winners over time -----------------------------------------

# Clean data

matches_1 <- matches %>%
  # recode winner and get count
  mutate(winner_4cat = case_when(
    winner == "Australia" ~ "australia",
    winner == "South Africa" ~ "southafrica",
    winner == "Pakistan" ~ "pakistan",
    TRUE ~ "other"
  )) %>%
  separate(match_date , into = c("date", "year"), sep = ", ") %>%
  select(match_id, winner_4cat, year) %>%
  drop_na() %>%
  group_by(winner_4cat, year) %>%
  count() %>%
  # pivot to wide and calculate winner percentage by year
  pivot_wider(names_from = winner_4cat, values_from = n) %>%
  select(year, australia, southafrica, pakistan, other) %>%
  rowwise() %>%
  mutate(total = sum(australia, southafrica, pakistan, other)) %>%
  rowwise() %>%
  mutate(australia_percent = round((australia / total) * 100)) %>%
  rowwise() %>%
  mutate(southafrica_percent = round((southafrica / total) * 100)) %>%
  rowwise() %>%
  mutate(pakistan_percent = round((pakistan / total) * 100)) %>%
  rowwise() %>%
  mutate(other_percent = round((other / total) * 100)) %>%
  select(-australia, -southafrica, -pakistan, -other, -total) %>%
  # pivot to long
  pivot_longer(!year, names_to = "winner", values_to = "percentage") %>%
  mutate(winner = case_when(
    winner == "australia_percent" ~ "Australia",
    winner == "southafrica_percent" ~ "South Africa",
    winner == "pakistan_percent" ~ "Pakistan",
    winner == "other_percent" ~ "Other"
  ))

# Create waffle plot

cricket_plot <- matches_1 %>%
  mutate(winner = fct_relevel(winner,
                              "Australia", "South Africa", "Pakistan", "Other")) %>%
  ggplot(aes(fill = winner, values = percentage)) +
  expand_limits(x = c(0, 0), y = c(0, 0)) +
  geom_waffle(
    n_rows = 10,
    size = 1,
    flip = TRUE,
    color = alpha("white", 1 / 3),
    make_proportional = TRUE,
    height = 0.8,
    width = 0.8
  ) +
  scale_fill_manual(name = NULL,
                    values = c("#001B69", "#E23629", "#003F15", "white")) +
  facet_wrap(~ year, ncol = 5) +
  coord_equal() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    strip.background = element_rect(fill="gray85"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="gray95"),
    panel.background = element_rect(fill="gray85")
  ) +
  labs(title = "Top 3 International Cricket Match Winners By Year",
       subtitle = "Australia, South Africa, and Pakistan have won about 32% to 50% of all matches every year",
       caption = "Visualization: George R. Yang (@georgeryang) | Data: ESPN Cricinfo (by way of Hassanasir)") +
  theme(
    plot.title = element_markdown(face = "bold", size = 24),
    plot.subtitle = element_markdown(face = "bold", size = 14),
    plot.caption = element_markdown(face = "italic", size = 10)
  )

ggsave(
  filename = "tt_world_cup_cricket.jpg",
  plot = cricket_plot,
  dpi = 300,
  width = 16,
  height = 7.5
)

# Alt text: This waffle plot shows the percentage of wins by the top 3
# international cricket teams by year, from 1996 to 2005. Australia, South
# Africa, and Pakistan have won about 32% to 50% of all matches every year.