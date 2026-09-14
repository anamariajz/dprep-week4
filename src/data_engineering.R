# Load the data
library(tidyverse)
video_view <- read_csv("data/video_view.csv")
users <- read_csv("data/users.csv")
impressions <- read_csv("data/impressions.csv")
watch_events <- read_csv("data/watch_events.csv")
sessions <- read_csv("data/sessions.csv")

# Exercise 1
video_features <- video_view %>%
  mutate(
    watch_rate_rank = rank(-watch_rate), 
    reach_band = case_when(
      impressions_n < 20 ~ "Low",
      impressions_n < 60 ~ "Medium",
      TRUE ~ "High"
    ),
    high_quality = avg_watch_share >= 0.40
  ) %>% 
  distinct(video_id, .keep_all = TRUE)

write_csv(video_features, "temp/video_features.csv")

video_features %>%
  arrange(watch_rate_rank) %>%
  head(10)


# Exercise 2
creator_summary <- video_features %>%
  group_by(creator_id) %>%
  summarise(
    videos_n = n(),
    impressions_total = sum(impressions_n),
    watched_total = sum(watched_n),
    avg_watch_rate = mean(watch_rate),
    median_watch_seconds = median(total_watch_seconds)
  ) %>%
  arrange(desc(impressions_total))
engagement_by_band <- video_features %>%
  group by (reach_band) %>%
summarise(
  videos_n = n(),
  avg_watch_rate = mean(watch_rate)
)


# Exercise 3



# Exercise 4



# Exercise 5
