# Prerequisite:
#
# Download data files in the terminal.
#
#   for i in {2009..2016};
#     do curl -O https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_$i.rds;
#   done

# Load libraries
# install.packages("nflfastR")
library(nflfastR)
library(tidyverse)
library(gghighcontrast)
library(scales)
# library(caTools)

# Don't display numbers in scientific notation
options(scipen = 9999)

load_data <- function(start_year, end_year) {
  # For each year, build stats.
  data <- data.frame()
  for (year in seq(start_year, end_year, by = 1)) {
    pbp_single_year <-
      readRDS(str_interp('data/play_by_play_${year}.rds'))
    data <- bind_rows(data, pbp_single_year)
  }
  return(data)
}

# TODO: Load 2009 through 2016 or maybe all the way to 2019
pbp_final <- load_data(2009, 2016) %>%
  mutate(
    winner = if_else(
      home_score > away_score,
      home_team,
      if_else(home_score < away_score, away_team, "TIE")
    ),
    poswins = ifelse(winner == posteam, "Yes", "No")
  ) %>%
  filter(winner != "TIE")

pbp_final$qtr = as.factor(pbp_final$qtr)
pbp_final$down = as.factor(pbp_final$down)
pbp_final$poswins = as.factor(pbp_final$poswins)


pbp_reduced = pbp_final %>%
  filter(play_type != "No Play" &
           qtr != 5 &
           qtr != 6 &
           down != "NA" &
           poswins != "NA") %>%
  select(
    game_id,
    # GameID
    game_date,
    # Date
    posteam,
    home_team,
    away_team,
    winner,
    qtr,
    down,
    ydstogo,
    game_seconds_remaining,
    # TimeSecs
    yardline_100,
    # yrdline100
    score_differential,
    # ScoreDiff
    poswins
  )

# Sample 80% of rows
set.seed(123)
indexes = sample(1:nrow(pbp_reduced), round(nrow(pbp_reduced) * 0.8), replace =
                   FALSE)
train <- pbp_reduced[indexes,]

model1 = glm(
  poswins ~ qtr + down + ydstogo + game_seconds_remaining + yardline_100 + score_differential,
  train,
  family = "binomial"
)
summary(model1)

pred1 = predict(model1, train, type = "response")
train = cbind(train, pred1)
train = mutate(train, pred1h = ifelse(posteam == home_team, pred1, 1 - pred1))


plot <- ggplot(
  filter(train, game_id == "2016_01_CAR_DEN"),
  aes(x = game_seconds_remaining, y = pred1h)
) +
  geom_hline(yintercept = 0.5, color = "white") +
  geom_line(size = 2, colour = "white") +
  scale_x_reverse() +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  theme_high_contrast(base_family = "InputMono") +
  labs(
    title = "Carolina at Denver, 2016 Week 1",
    subtitle = "Win probability",
    caption = "Data from nflfastR",
    x = "Time Remaining (seconds)",
    y = "Home Win Probability"
  )
