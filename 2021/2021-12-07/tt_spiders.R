# TidyTuesday, 2021-12-07, Spiders
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-12-07/readme.md

# Code help
# https://gist.github.com/leeolney3/a73876553927c27640021b1f1300e551


# Load packages
library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)

# Load data
spiders <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')


# Explore data
head(spiders)
summary(spiders$year)
tabyl(spiders$family)

# Q: Timeline of spider discoveries -------------------------------------------

# Top 10 families
spiders_1 <- spiders %>%
  count(family, sort=T) %>%
  slice(1:10)

# Count families by year and create timeline
spiders_2 <- spiders %>%
  count(family, year) %>%
  filter(family %in% spiders_1$family)

spiders_3 <- spiders_2 %>%
  complete(year, family) %>%
  mutate(n = replace_na(n, 0)) %>%
  group_by(family) %>%
  summarize(timeline = list(n))
  
# Join top 10 and timeline
spiders_4 <- spiders_1 %>%
  left_join(spiders_3, by = "family") 

# Create table
table_1 <- spiders_4 %>%
  gt() %>%
  gt_theme_nytimes() %>%
  gt_sparkline(timeline, width = 200, label = FALSE,
               range_colors = c("red", "blue"),
               line_color = "black") %>%
  cols_label(family = "Family", n="Species Count", timeline="Discovery Timeline") %>%
  tab_header(title = "Discovery of Spider Families Over Time",
             subtitle = "10 spider families with the most number of species discoveries, from 1757 to 2021") %>%
  tab_source_note(source_note = "Visualization: George R. Yang (@georgeryang) | Data: World Spider Database") %>%
  tab_options(source_notes.padding = px(10),
              source_notes.font.size = px(11)) 

table_1 %>% gtsave("tt_spiders.html")


# Side note: I noticed shortly after finishing this that @leeolney3 already
# published a more complex version, but this is good practice anyways
# https://github.com/leeolney3/TidyTuesday/blob/main/2021/2021_50/2021_50.R