#Install nflscrapR
library(devtools)
devtools::install_github(repo = "maksimhorowitz/nflscrapR")
devtools::install_github("dgrtwo/gganimate")
#Load libraries
library(nflscrapR)
library(tidyverse)
library(caTools)

# TODO: For each year
pbp1 = season_play_by_play(2016)
saveRDS(pbp1, "pbp_data_2016.rds")

# Combine years
pbp = bind_rows(pbp1, pbp2, pbp3, pbp4, pbp5, pbp6, pbp7, pbp8)
saveRDS(pbp, "pbp_data.rds")

# TODO: For each year
games2016 = season_games(Season = 2016)

# Combine years
games = bind_rows(
  games2016,
  games2015,
  games2014,
  games2013,
  games2012,
  games2011,
  games2010,
  games2009
)
saveRDS(games, "games_data.rds")

# Join PBP with game outcomes
pbp_final = full_join(games, pbp_raw, by = "GameID")
saveRDS(pbp_final, "pbp_final.rds")

# Create fields to show if team won or lost
pbp_final = pbp_final %>% mutate(winner = ifelse(homescore > awayscore, home, away))
pbp_final = pbp_final %>% mutate(poswins = ifelse(winner == posteam, "Yes", "No"))
pbp_final$qtr = as.factor(pbp_final$qtr)
pbp_final$down = as.factor(pbp_final$down)
pbp_final$poswins = as.factor(pbp_final$poswins)


pbp_reduced = pbp_final %>%
  filter(PlayType != "No Play" &
           qtr != 5 &
           down != "NA" &
           poswins != "NA") %>%
  select(
    GameID,
    Date,
    posteam,
    HomeTeam,
    AwayTeam,
    winner,
    qtr,
    down,
    ydstogo,
    TimeSecs,
    yrdline100,
    ScoreDiff,
    poswins
  )

set.seed(123)
split = sample.split(pbp_reduced$poswins, SplitRatio = 0.8)
train = pbp_reduced %>% filter(split == TRUE)
test = pbp_reduced %>% filter(split == FALSE)

model1 = glm(poswins ~ qtr + down + ydstogo + TimeSecs + yrdline100 + ScoreDiff,
             train,
             family = "binomial")
summary(model1)

pred1 = predict(model1, train, type = "response")
train = cbind(train, pred1)
train = mutate(train, pred1h = ifelse(posteam == HomeTeam, pred1, 1 - pred1))


ggplot(filter(train, GameID == "2016090800"),
       aes(x = TimeSecs, y = pred1h)) +
  geom_line(size = 2, colour = "orange") +
  scale_x_reverse() +
  ylim(c(0, 1)) +
  theme_minimal() +
  xlab("Time Remaining (seconds)") +
  ylab("Home Win Probability")
