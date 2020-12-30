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

# Don't display numbers in scientific notation
options(scipen = 9999)

# Colors
dayglo_orange = "#ff6700"
light_blue = "#0098ff"
red = "#ff0000"
grey = "#808080"
kiwi_green = "#8ee53f"
dark_olive_green = "#556b2f"
dark_raspberry = "#872657"
rich_black = "#010203"

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

# Load 2009 through 2016 or maybe all the way to 2019
pbp_final <- load_data(2009, 2020) %>%
  mutate(
    winner = if_else(
      home_score > away_score,
      home_team,
      if_else(home_score < away_score, away_team, "TIE")
    ),
    poswins = ifelse(winner == posteam, 1, 0),
    scoring_play = ifelse(
      play_type == "extra_point" |
        play_type == "field_goal" | !is.na(td_team),
      1,
      0
    )
  ) %>%
  filter(winner != "TIE")

# pbp_final$qtr = as.factor(pbp_final$qtr)
pbp_final$down = as.factor(pbp_final$down)
pbp_final$poswins = as.factor(pbp_final$poswins)


pbp_reduced = pbp_final %>%
  filter(
    !is.na(score_differential),
    !is.na(play_type),
    !is.na(poswins),
    !is.na(down),
    !is.na(yardline_100),
    !is.na(defteam_timeouts_remaining),
    !is.na(posteam_timeouts_remaining)
  ) %>%
  filter(qtr <= 4) %>%
  select(
    game_id,
    game_date,
    posteam,
    home_team,
    away_team,
    winner,
    qtr,
    down,
    ydstogo,
    half_seconds_remaining,
    game_seconds_remaining,
    yardline_100,
    score_differential,
    defteam_timeouts_remaining,
    posteam_timeouts_remaining,
    scoring_play,
    poswins,
    wp
  )

# Sample 80% of rows
set.seed(123)
indexes = sample(1:nrow(pbp_reduced),
                 round(nrow(pbp_reduced) * 0.8),
                 replace = FALSE)
train <- pbp_reduced[indexes,]

model1 = glm(
  poswins ~
    qtr +
    down +
    ydstogo +
    half_seconds_remaining +
    game_seconds_remaining +
    yardline_100 +
    score_differential +
    defteam_timeouts_remaining +
    posteam_timeouts_remaining,
  train,
  family = "binomial"
)

pbp_reduced <- pbp_reduced %>%
  mutate(
    pred1h =
      predict(model1, pbp_reduced[row_number(),], type = "response"),
    # Recalculate all win probabilities relative to home team
    pred1h = ifelse(posteam == home_team, pred1h, 1 - pred1h),
    # Recalculate all nflfastR wp relative to home team
    wp = ifelse(posteam == home_team, wp, 1 - wp)
  )

# Plot
plot_for_game_id <- function(single_game_id) {
  foreground_color = rich_black
  background_color = "white"
  pbp_single_game <- filter(pbp_reduced, game_id == single_game_id)

  # TODO: Get home_team and away_team and annotate on chart
  home_team_name <- pbp_single_game[1,]$home_team
  away_team_name <- pbp_single_game[1,]$away_team

  plot <- ggplot(pbp_single_game,
                 aes(x = game_seconds_remaining, y = pred1h)) +
    # 50% reference
    geom_hline(yintercept = 0.5, color = foreground_color) +
    # Quarters
    geom_vline(xintercept = 900, color = grey) +
    geom_vline(xintercept = 1800, color = grey) +
    geom_vline(xintercept = 2700, color = grey) +

    # Win Probability
    geom_line(aes(y = wp), color = dayglo_orange) +
    geom_line(size = 2, colour = foreground_color) +

    # Scoring events
    geom_rug(
      data = filter(pbp_single_game, scoring_play == 1),
      color = foreground_color,
      sides = "b"
    ) +

    # Annotate with team names
    annotate("text", x=3600, y=0.95, color=foreground_color, family="InputMono", label=home_team_name) +
    annotate("text", x=3600, y=0.05, color=foreground_color, family="InputMono", label=away_team_name) +

    # Formatting
    scale_x_reverse() +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    theme_high_contrast(
      base_family = "InputMono",
      background_color = background_color,
      foreground_color = foreground_color
    ) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(
      title = str_interp("${single_game_id}"),
      subtitle = "Custom win probability model compared to nflfastR WP (orange)",
      caption = "Data from nflfastR",
      x = "Quarters",
      y = "Home Win Probability"
    )
}

# Plot a few games
game_ids <- c("2019_10_SEA_SF",
              "2019_17_SF_SEA",
              "2016_01_CAR_DEN",
              "2020_16_MIA_LV",
              "2020_16_TB_DET")
for (game_id in game_ids) {
  plot <- plot_for_game_id(game_id)

  ggsave(
    str_interp("wp-${game_id}.png"),
    plot = plot,
    width = 9,
    height = 6
  )
}
