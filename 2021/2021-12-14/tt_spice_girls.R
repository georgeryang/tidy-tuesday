# TidyTuesday, 2021-12-14, Spice Girls
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-12-14/readme.md


# Load packages
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(hwordcloud)

# Load data
lyrics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')

# Explore data
str(lyrics)
head(lyrics)
length(unique(lyrics$song_id))
# 31 songs total


# Clean lyrics data
lyrics_1 <- lyrics %>%
  group_by(song_id) %>%
  mutate(lyrics_by_song = paste0(line, collapse =  " ")) %>%
  distinct(lyrics_by_song)

# Cleaning code from Towards Data Science
# https://towardsdatascience.com/text-mining-with-r-gathering-and-cleaning-data-8f8b0d65e67c

lyrics_all <- lyrics_1$lyrics_by_song

# Remove mentions, urls, emojis, numbers, punctuations, etc.
lyrics_all <- gsub("@\\w+", "", lyrics_all)
lyrics_all <- gsub("https?://.+", "", lyrics_all)
lyrics_all <- gsub("\\d+\\w*\\d*", "", lyrics_all)
lyrics_all <- gsub("#\\w+", "", lyrics_all)
lyrics_all <- gsub("[^\x01-\x7F]", "", lyrics_all)
lyrics_all <- gsub("[[:punct:]]", " ", lyrics_all)
# Remove spaces and newlines
lyrics_all <- gsub("\n", " ", lyrics_all)
lyrics_all <- gsub("^\\s+", "", lyrics_all)
lyrics_all <- gsub("\\s+$", "", lyrics_all)
lyrics_all <- gsub("[ |\t]+", " ", lyrics_all)

lyrics_all <- tolower(lyrics_all)

# Put the data to a new column
lyrics_1["fixed_lyrics"] <- lyrics_all
head(lyrics_1$fixed_lyrics, 10)


# Cleaning code from Towards Data Science
# https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
lyrics_words <- lyrics_1 %>%
  ungroup(song_id) %>%
  select(fixed_lyrics) %>%
  unnest_tokens(word, fixed_lyrics)
words <- lyrics_words %>% count(word, sort=TRUE)

# Create word cloud

# Wordcloud
wordcloud2(words, color = "random-light", backgroundColor = "#lightgray")

# Hwordcloud: https://czxb.github.io/br/hwordcloudppt.html
words_1 <- words %>% head(100)
hwordcloud(
  text = words_1$word,
  size = words_1$n,
  theme = "grid",
  title = "Top Words in Spice Girls Lyrics",
  titleAlign = "center",
  # titleColor = "black",
  titleSize = "20px",
  subtitle = "Visualization: George R. Yang (@georgeryang) | Data: Genius (Jacquie Tran)",
  # subtitleColor = "black",
  subtitleAlign = "center",
  subtitleSize = "8px"
)