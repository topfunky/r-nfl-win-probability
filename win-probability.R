# Load libraries
# install.packages("nflfastR", "ggimage", "devtools")
# devtools::install_github("topfunky/gghighcontrast")
library(nflfastR)
library(tidyverse)
library(gghighcontrast)
library(scales)
library(ggimage)

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

if (!dir.exists("data")) {
  dir.create("data")
}

load_data <- function(start_year, end_year, logos) {
  # For each year, load data
  data <- data.frame()
  for (year in seq(start_year, end_year, by = 1)) {
    # Download file to cache if not present
    url = str_interp(
      "https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_${year}.rds"
    )
    local_filename = str_interp("data/play_by_play_${year}.rds")
    if (!file.exists(local_filename)) {
      download.file(url, local_filename)
    }

    pbp_single_year <-
      readRDS(str_interp('data/play_by_play_${year}.rds'))
    data <- bind_rows(data, pbp_single_year)
  }

  data <-
    data %>% inner_join(logos, by = c("posteam" = "team_abbr"))
  return(data)
}

# Be nice to the ESPN CDN by storing team logo images locally.
cache_logo_image <- function(logo) {
  local_filename = str_interp("data/${logo[['team_abbr']]}.png")
  if (!file.exists(local_filename)) {
    download.file(logo[['team_logo_espn']], local_filename)
  }
  return(local_filename)
}

load_logos <- function() {
  logos <- teams_colors_logos
  logos <- logos %>%
    select(team_abbr, team_logo_espn) %>%
    mutate(team_logo_local = apply(logos, 1, cache_logo_image))

  return(logos)
}

is_scoring_team <-
  function(posteam,
           play_type,
           td_team,
           safety,
           this_team,
           that_team) {
    (td_team == this_team) |
      (posteam == this_team &
         (play_type == "extra_point" |
            play_type == "field_goal")) | (posteam == that_team & safety == 1)
  }

load_data_and_train_model <- function(start_year, end_year, logos) {
  pbp <- load_data(start_year, end_year, logos) %>%
    filter(
      !is.na(score_differential),!is.na(play_type),!is.na(down),!is.na(yardline_100),!is.na(defteam_timeouts_remaining),!is.na(posteam_timeouts_remaining),
      qtr <= 4
    ) %>%
    mutate(
      winner = if_else(
        home_score > away_score,
        home_team,
        if_else(home_score < away_score, away_team, "TIE")
      ),
      # Record winner on each row
      poswins = ifelse(winner == posteam, 1, 0),
      is_home_team = ifelse(posteam == home_team, 1, 0),
      # Build a field with all scoring plays for display on chart
      home_scoring_play = is_scoring_team(posteam, play_type, td_team, safety, home_team, away_team),
      away_scoring_play = is_scoring_team(posteam, play_type, td_team, safety, away_team, home_team),
    ) %>%
    filter(winner != "TIE", !is.na(poswins)) %>%
    select(
      game_id,
      game_date,
      posteam,
      poswins,
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
      home_scoring_play,
      away_scoring_play,
      home_wp,
      is_home_team,
      team_logo_local
    )

  win_prediction_model <- build_win_prediction_model(pbp)

  pbp <- pbp %>%
    mutate(
      home_wp_custom =
        predict(win_prediction_model, pbp[row_number(), ], type = "response"),
      # Recalculate win probabilities relative to home team
      home_wp_custom = ifelse(posteam == home_team, home_wp_custom, 1 - home_wp_custom),
    )
  return(pbp)
}

build_win_prediction_model <- function(data) {
  # Sample 80% of rows
  set.seed(123)
  indexes = sample(1:nrow(data),
                   round(nrow(data) * 0.8),
                   replace = FALSE)
  train <- data[indexes, ]

  win_prediction_model = glm(
    poswins ~
      qtr +
      down +
      ydstogo +
      half_seconds_remaining +
      game_seconds_remaining +
      yardline_100 +
      score_differential +
      defteam_timeouts_remaining +
      posteam_timeouts_remaining +
      is_home_team,
    train,
    family = "binomial"
  )
  return(win_prediction_model)
}

# Plot
plot_for_data <- function(data, logos) {
  foreground_color = rich_black
  background_color = "white"

  single_game_id <- data[1, ]$game_id

  game_title_pieces <- strsplit(single_game_id, "_")[[1]]
  game_year <- game_title_pieces[1]
  game_week <- game_title_pieces[2]

  # Get home_team and away_team and annotate on chart
  home_team_abbr <- data[1, ]$home_team
  away_team_abbr <- data[1, ]$away_team

  # Build a data frame with coordinates of team logo to place on chart
  logo_placement_data <- data.frame(
    x = c(3600, 3600),
    y = c(0.875, 0.125),
    team_abbr = c(home_team_abbr, away_team_abbr),
    stringsAsFactors = FALSE
  ) %>% inner_join(logos, by = "team_abbr")

  plot <- ggplot(data,
                 aes(x = game_seconds_remaining, y = home_wp_custom)) +
    # 50% reference line
    geom_hline(yintercept = 0.5,
               color = grey,
               size = 1) +

    # Reference line for each quarter (and halftime)
    geom_vline(xintercept = 15 * 60, color = grey) +
    geom_vline(xintercept = 30 * 60, color = grey) +
    geom_vline(xintercept = 45 * 60, color = grey) +

    annotate(
      "text",
      x = 58.75 * 60,
      y = 0.98,
      label = "Q1",
      family = "InputMono",
      color = grey
    ) +
    annotate(
      "text",
      x = 43.75 * 60,
      y = 0.98,
      label = "Q2",
      family = "InputMono",
      color = grey
    ) +
    annotate(
      "text",
      x = 28.75 * 60,
      y = 0.98,
      label = "Q3",
      family = "InputMono",
      color = grey
    ) +
    annotate(
      "text",
      x = 13.75 * 60,
      y = 0.98,
      label = "Q4",
      family = "InputMono",
      color = grey
    ) +

    # Win Probability
    geom_line(aes(y = home_wp), color = light_blue) +
    geom_line(size = 1, color = foreground_color) +

    # Scoring events
    geom_rug(
      data = filter(data, away_scoring_play == 1),
      color = foreground_color,
      sides = "b",
      size = 1.5
    ) +
    geom_rug(
      data = filter(data, home_scoring_play == 1),
      color = foreground_color,
      sides = "t",
      size = 1.5
    ) +

    # Draw home and away team logo
    geom_image(
      data = logo_placement_data,
      aes(x = x, y = y, image = team_logo_local),
      size = 0.08,
      asp = 16 / 9
    ) +

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
      title = str_interp("${game_year} Week ${game_week}: ${away_team_abbr} at ${home_team_abbr}"),
      subtitle = "Custom win probability model compared to nflfastR WP (blue)",
      caption = "Data from nflfastR",
      x = "Quarters",
      y = "Home Win Probability"
    )
}

# Load 2009 through 2016 or maybe all the way to 2019
logos <- load_logos()
pbp_data <- load_data_and_train_model(2009, 2020, logos)

# Plot a few games
game_ids <- c("2019_10_SEA_SF",
  "2019_17_SF_SEA",
  "2016_01_CAR_DEN",
  "2020_16_MIA_LV",
  "2020_16_LA_SEA",
  "2020_16_TB_DET")
for (single_game_id in game_ids) {
  plot <-
    plot_for_data(filter(pbp_data, game_id == single_game_id), logos)

  ggsave(
    str_interp("wp-${single_game_id}.png"),
    plot = plot,
    width = 9,
    height = 6
  )
}
