{
  library(tidyverse) 
  library(devtools)
  library(dplyr)
  library(gganimate)
  library(ggforce)
  library(ggplot2)
  library(ggthemes)
  library(ggrepel)
  library(readr)
  library(roll)
  library(nflverse)
  library(gt)
  library(gtExtras)
  library(patchwork)
  setwd("/Users/maxwhalen/Documents/GitHub/Big-Data-Bowl")
  
  # * load helper functions ----
  source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
  source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")
}

################################## LOAD DATA ###################################

{
  plays <- read_csv("plays.csv") %>% mutate(newId = as.numeric(paste0(gameId, playId)))
  tackles <- read_csv("tackles.csv")  %>% mutate(newId = as.numeric(paste0(gameId, playId)))
  players <- read_csv("players.csv")
  games <- read_csv("games.csv") 
  pbp_22 <- load_pbp(seasons = 2022) %>% mutate(newId = as.numeric(paste0(old_game_id, play_id)))
  ftn_22 <- load_ftn_charting(seasons = 2022)
  rosters_22 <- load_rosters(seasons = 2022)
  alignment_mapping <- read_csv("alignment_mapping.csv")
  pff <- read_csv("run_defense_summary.csv")
  participation <- load_participation(seasons = 2022) %>% mutate(newId = as.numeric(paste0(old_game_id, play_id)))
  
  # Filter to get the plays you want
  pbp_22 %>% 
    filter(rush_attempt == 1) %>% 
    mutate(newId = paste0(old_game_id, play_id)) %>% 
    pull(newId) %>% 
    unique() -> run_plays
  
  plays <- plays %>% left_join(pbp_22 %>% select(success, newId, week), by = c("newId"))
  
  tackles %>% 
    group_by(playId, gameId) %>% 
    summarise(
      tackles = sum(tackle),
      assists = sum(assist)
    ) %>% 
    filter(tackles == 0 & assists > 0) %>% 
    arrange(-(assists)) %>% 
    mutate(no_tackle = TRUE) %>% 
    select(-c(tackles, assists)) -> no_tackles_data
  
  tackles %>% 
    left_join(no_tackles_data) %>% 
    mutate(tackle = if_else(no_tackle & assist == 1, 1, tackle)) %>% 
    filter(tackle > 0 | pff_missedTackle > 0) %>% 
    select(-no_tackle) -> new_tackles
}

{
  tracking <- tibble()
  for(i in 1:9){
    temp <- data.table::fread(paste0("tracking_week_", i,".csv"))
    tracking <- bind_rows(tracking, temp) %>% 
      mutate(newId = as.numeric(paste0(gameId, playId)))
  }
  
  tracking <- tracking %>% rotate_to_ltr()

  tracking %>% 
    filter(newId %in% run_plays) -> run_tracking
}

{
  new_plays <- plays %>% 
    filter(newId %in% run_plays) %>% 
    select(newId, ballCarrierId, defensiveTeam, passResult, down, yardsToGo, expectedPointsAdded,
           playNullifiedByPenalty, defendersInTheBox, offenseFormation, passProbability, success)
  
  run_tracking %>% 
    inner_join(new_plays, 
               by = c("newId"), 
               relationship = "many-to-many") -> run_tracking
}

{
  tracking <- 1
  temp <- 1
  pbp <- 1
  ftn_22 <- 1
}

{ 
  run_tracking %>% 
    filter(grepl("tackle", event)) %>% 
    select(newId, tackleFrame = frameId) %>% 
    distinct() -> tackle_frame_data
  
  run_tracking %>% 
    filter(grepl("handoff", event)) %>% 
    select(newId, handoffFrame = frameId) %>% 
    group_by(newId) %>% 
    # Account for reverses / double handoffs
    slice_min(handoffFrame) %>% 
    distinct() -> handoff_frame_data
  
  play_time_data <- left_join(tackle_frame_data, handoff_frame_data)
  
  run_tracking %>% 
    filter(defensiveTeam != club & club != "football" & ballCarrierId != nflId) %>% 
    select(newId, frameId, off_x = x, off_y = y, off_o = o, off_dir = dir) -> off_locations
  
  run_tracking %>% 
    filter(nflId == ballCarrierId) %>% 
    left_join(players %>% select(nflId, ball_weight = weight)) %>% 
    select(newId, frameId, ball_x = x, ball_y = y, ball_dist_los = dist_from_los, 
           ball_s = s, ball_a = a, ball_sx = s_x, ball_sy = s_y, ball_weight) %>% 
    mutate(ball_momentum = ball_s * ball_weight)-> ball_location
  
  left_join(off_locations, ball_location) -> off_ball_locations
  
  run_tracking %>% 
    filter(defensiveTeam == club) %>% 
    inner_join(play_time_data, by = c("newId")) -> run_tracking
  
  {
    play_time_data <- 1
    tackle_frame_data <- 1
    handoff_frame_data <- 1
    off_locations <- 1
    ball_locations <- 1
  }
  
  paste0(run_tracking$newId, run_tracking$frameId) -> relevant_o_frames
  
  off_ball_locations %>% 
    filter(paste0(newId, frameId) %in% relevant_o_frames) %>% 
    select(-off_dir) -> off_ball_locations
  
  run_tracking  %>% 
    filter(frameId >= 5 & frameId <= tackleFrame) %>% 
    inner_join(off_ball_locations, by = c("newId", "frameId")) -> run_tracking
  
  run_tracking %>% 
    mutate(dist_to_o = sqrt((off_x - x)^2 + (off_y - y)^2)) -> run_tracking
  
  run_tracking %>% 
    group_by(frameId, newId, nflId) %>% 
    slice_min(dist_to_o, n = 3) -> run_tracking
  
   run_tracking %>%
    ungroup() %>% 
    mutate(dist_to_ball = sqrt((x - ball_x)^2 + (y - ball_y)^2)) -> run_tracking
  # group_by(nflId, newId) %>% 
  # mutate(
  #   last_dist = lag(dist_to_ball),
  #        ground_gained = roll::roll_sum(dist_to_ball, 8)/4
  #        ) %>% 
  
  
  run_tracking %>% 
    mutate(o_dist_to_ball = sqrt((off_x - ball_x)^2 + (off_y - ball_y)^2)) -> run_tracking
  
  run_tracking %>% 
    compute_o_diff(prefix = "off") %>% 
    rename( def_x = x, def_y = y, x = off_x, y = off_y) %>% 
    compute_o_diff(prefix = "def") %>% 
    rename(off_y = y, off_x = x, x = def_x, y = def_y) -> run_tracking
  
  run_tracking %>% 
    mutate(engaged = (o_dist_to_ball < dist_to_ball) & 
             ((o_to_off < 45) | (o_to_def < 40)) & (dist_to_o < 2)) %>% 
    group_by(frameId, newId, nflId) %>% 
           # Make engaged options evident
    mutate(num_engaged = if_else(engaged, row_number(), 0)) %>% 
    slice_min(dist_to_o) -> run_tracking
  
  run_tracking %>% 
    left_join(new_tackles %>% select(newId, nflId, tackle, pff_missedTackle),
              by = c("newId", "nflId")) %>% 
    mutate(tackle = if_else(defensiveTeam == club & !is.na(tackle), tackle, 0),
           tackle = if_else(club != defensiveTeam, NA, tackle)) -> run_tracking
  
  run_tracking %>% 
    left_join(alignment_mapping %>% 
                 mutate(newId = as.numeric(newId),
                        tech = paste0(tech, "-")) %>% # So when we save as csv it doesn't go to numeric
                 select(-displayName), 
              by = join_by("nflId", "newId"),
              relationship = "many-to-many") %>% 
    ungroup() -> run_tracking
  
  run_tracking %>% 
    mutate(
      tackle = if_else(club == defensiveTeam & is.na(tackle), 1, tackle),
      # Adjust success for the defense
      success = if_else(success == 1, 0, 1),
      success_tackle = if_else(tackle + success == 2, 1, 0)
    ) %>% 
    drop_na(success_tackle) %>% 
    mutate(
      position = case_when(
        tech %in% c("0-", "1-", "2-", "3-") ~ "IDL",
        tech %in% c("4-", "5-", "6-") ~ "EDGE",
        tech %in% c("50-", "60-") ~ "OLB",
        tech %in% c("00-", "10-", "20-", "30-", "40-") ~ "ILB",
        .default = "Secondary"
      )) -> run_tracking
  
  run_tracking %>% 
    left_join(participation %>% 
                select(newId, defense_personnel, offense_personnel)) %>% 
    left_join(games %>% select(gameId, week)) -> run_tracking
  
}

{
  run_tracking %>% 
    select(nflId, newId, pff_missedTackle, tackle) %>% 
    distinct() %>% 
    mutate(pff_missedTackle = if_else(is.na(pff_missedTackle), 0, pff_missedTackle))%>%
    group_by(nflId) %>%
    mutate(missed_tackles = roll::roll_sum(pff_missedTackle, width = 500, min_obs = 1),
           tackles = roll::roll_sum(tackle, width = 500, min_obs = 1),
           missed_tackle_rate = missed_tackles / (missed_tackles + tackles),
           missed_tackle_rate = if_else(is.na(missed_tackle_rate), 0, missed_tackle_rate)) -> missed_tackle_data
  
  run_tracking %>% 
    left_join(players %>% select(roster_position = position, nflId)) %>% 
    left_join(missed_tackle_data %>% select(nflId, newId, missed_tackle_rate)) -> run_tracking
  
  run_tracking %>% 
    mutate(
      # Accounting for lack of precision in original code
      position = if_else(tech == "4" & roster_position %in% c("NT", "DT"),  "IDL", position),
      down = as.factor(down),
      success_yards = case_when(
        down == "1" ~ yardsToGo * 0.4,
        down == "2" ~ yardsToGo * 0.6,
        down %in% c("3", "4") ~ yardsToGo,
        .default = NA
      ),
      gained_success = ball_dist_los > success_yards,
      high_run_prob = passProbability < 1/4
    ) -> run_tracking
  
    # Filter data not part of pursuing a ballCarrier
    ## For my purposes penetration skill is seperate from tackle skill
    ## Cases like trap plays and zone reads allow penetration that != inc 
    ## likelihood of tackle at mesh point
  run_tracking %>% 
    mutate(
      d_dist_from_success = success_yards - dist_from_los,
      ball_dist_from_success = success_yards - ball_dist_los
      ) %>% 
    left_join(plays %>% select(newId, possessionTeam)) -> run_tracking 
  
  run_tracking %>% 
    mutate(
      position = if_else(position == "Secondary",
                         case_when(
                           roster_position %in% c("CB", "DB") ~ "CB",
                           roster_position %in% c("FS", "SS") ~ "S",
                           .default = "Secondary"
                         ), position)
    ) -> run_tracking
  
  run_tracking %>% 
    mutate(ball_dist_sideline = case_when(
      (53 - ball_y) > (ball_y - 0) ~ ball_y - 0,
      (53 - ball_y) < (ball_y - 0) ~ 53 - ball_y,
      .default = 53/2
    )) -> run_tracking
  
  run_tracking %>% 
    compute_o_diff() -> run_tracking
  
  run_tracking %>%
    group_by(nflId, newId) %>% 
    mutate(dist_to_ball_lag = lag(dist_to_ball),
           ground_gained = dist_to_ball_lag - dist_to_ball,
           sum_ground_gained = roll::roll_sum(ground_gained, width = 100, min_obs = 1),
           distance_closed = sum_ground_gained / (sum_ground_gained + dist_to_ball),
           distance_closed = if_else(is.na(distance_closed), 0, distance_closed),
           position = if_else(grepl("LB", position), "Off-Ball LB", position),
           #position = if_else(is.na(tech), position, as.character(tech)),
           #position = if_else(tech == 0 & grepl("LB", roster_position), "00", position),
           #position = if_else(position == "EDGE", "5", position), 
           # TODO Fix this to be case_when, didn't expect this to be one thing after the next
           num_engaged = as.character(num_engaged)) %>% 
    ungroup() %>% 
    inner_join(players %>% select(nflId, weight)) %>% 
    mutate(momentum = s*weight) -> run_train
  
  run_train %>% 
    left_join(pbp_22 %>% select(newId, yards_gained)) %>% 
    mutate(
      success = if_else(yards_gained < success_yards, 1, 0),
      success_tackle = if_else(tackle + success == 2, 1, 0)
    ) -> run_train
  
}

############################# MODEL TRAINING ###################################

{
  set.seed(13)
  
  write_csv(run_train, "train_data.csv")
  run_train <- data.table::fread("train_data.csv") %>% mutate(newId = as.numeric(newId))
}

{
  # Install and load the keras package
  library(keras)
  library(dummy)
  
  categorical_cols <- c("position", "num_engaged")
  
  run_train %>% 
    drop_na(o_to_ball, position, num_engaged, dist_to_ball, ball_dist_from_success, distance_closed,
           missed_tackle_rate, s, a, weight, dist_from_los, ball_a, ball_sx, ball_sy, 
           ball_dist_sideline) -> run_train
  
  run_train %>% 
    select(o_to_ball, position, num_engaged, dist_to_ball, ball_dist_from_success,
            missed_tackle_rate, s, a, weight, dist_from_los, ball_a, ball_sx,
            ball_dist_sideline) -> run_train_limited 
  
  run_train_limited %>% 
    mutate(
      #engaged = if_else(engaged, 1, 0),
      ball_dist_from_success = if_else(ball_dist_from_success > 0, 1, 0),
      #d_dist_from_success = if_else(d_dist_from_success < 0, 1, 0),
    ) %>% 
    select(-all_of(categorical_cols)) %>% 
    mutate_all(~scale(.)) %>% 
    bind_cols(dummy(run_train %>% select(all_of(categorical_cols)))) %>% 
    mutate_all(~as.numeric(as.character(.))) %>% 
    drop_na()-> nn_train
  
  # Create a neural network model
  model <- keras_model_sequential() 
  
  # Add layers to the model
  model %>% 
    layer_dense(units = ncol(nn_train), activation = 'relu', input_shape = ncol(nn_train)) %>% 
    layer_dense(units = ceiling(ncol(nn_train)/2), activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'sigmoid')
  
  
  # Compile the model
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = 'adam',
    metrics = c('accuracy')
  )
  
  y_vals <- as.matrix(run_train$success_tackle)
  
  # Train the model
  history <- model %>% fit(
    x = nn_train %>% as.matrix(),
    y = y_vals,
    epochs = 1,
    batch_size = 32,
    validation_split = 0.3
  )
  
  # Plot training history
  plot(history)
  
  # Evaluate the model
  model %>% evaluate(
    x = nn_train %>% as.matrix(),
    y = y_vals
  )
  
  # Make predictions
  predictions <- model %>% predict(
    x = nn_train %>% as.matrix()
  )
  
  run_train$pred <- predictions
}

{ 
  run_train %>% 
    group_by(newId, nflId) %>% 
    slice_min(frameId) %>% 
    select(newId, nflId, frameId, start_pred = pred) -> start_pred_data
  
  run_train %>% 
    left_join(start_pred_data, relationship = "many-to-many") %>%
    left_join(games %>% select(gameId, week)) %>% 
    # Identifying likelihood of tackle gained after snap
    mutate(pred_lag = lag(pred),
           pred_gained = pred - pred_lag,
           #pred = if_else(is.na(pred_gained), start_pred, pred_gained)
           ) -> run_data
  
  run_data %>% 
    filter(frameId >= handoffFrame) %>% 
    group_by(displayName, nflId, newId) %>% 
    summarise(
      pred = sum(pred, na.rm = T),
      success_tackle_pct = mean(success_tackle),
      tackle_pct = mean(tackle),
      count = n()
    ) %>% 
    mutate(pred = pred/count) %>% # Normalizing for plays of various lengths
    group_by(displayName, nflId) %>% 
    summarise(
      total_gain = sum(pred),
      gain_per_play = mean(pred),
      success_tackle_pct = mean(success_tackle_pct),
      tackle_pct = mean(tackle_pct),
      plays = n()
    ) %>% 
    left_join(run_train %>% 
                group_by(nflId, displayName) %>% 
                summarise(position = DescTools::Mode(position))) %>% 
    arrange(-gain_per_play) %>% 
    ungroup() %>% 
    filter(plays > 100) %>% 
    mutate(rank = row_number()) -> comparison_data
  
  run_data %>% 
    group_by(displayName, nflId, newId, week) %>% 
    summarise(
      pred = sum(pred, na.rm = T),
      success_tackle_pct = mean(success_tackle),
      tackle_pct = mean(tackle),
      count = n()
    ) %>% 
    mutate(pred = pred/count) %>% 
    mutate(group = if_else(week > 5, "Group 2", "Group 1")) %>% 
    group_by(displayName, nflId, group) %>% 
    summarise(
      total_gain = sum(pred),
      gain_per_play = mean(pred),
      success_tackle_pct = mean(success_tackle_pct),
      tackle_pct = mean(tackle_pct),
      plays = n()
    ) %>% 
    filter(plays >= if_else(group == "Group 1", 50, 40)) %>% 
    arrange(-gain_per_play) -> week_comparison_data
  
  comparison_data %>% 
    #left_join(run_train %>% select(nflId, roster_position), by = c("nflId")) %>% 
    distinct() %>% 
    group_by(position) %>% 
    slice_max(gain_per_play, n = 5) %>% 
    #left_join(pff %>% select(displayName = player, run_grade = grades_run_defense)) %>% 
    print(n=Inf)
  
}

{ 
  week_comparison_data %>% 
    filter(group == "Group 1") %>% 
    left_join(week_comparison_data %>% 
                filter(group == "Group 2"),
              by = c("displayName", "nflId")) -> week_plot_data
}

{
  week_plot_data %>% 
    left_join(comparison_data %>% select(nflId, Position = position), by = c("nflId")) %>% 
    drop_na() %>% 
    ggplot(aes(x = gain_per_play.x, y = gain_per_play.y, color = Position)) +
    geom_point(size = 4, alpha = 0.6) +
    theme_clean() +
    geom_smooth(method = "lm", se = F, size = 0.75) +
    ggpmisc::stat_poly_eq(size = 6) +
    scale_color_wsj() +
    labs(title = "Fig. A: Evaluating Stability of SRSS",
         subtitle = "Minimum 50 Snaps in Each Group",
         x = "SRSS Score Weeks 1-5",
         y = "SRSS Score Weeks 6-9")-> p1
  
  week_plot_data %>% 
    left_join(comparison_data %>% select(nflId, position), by = c("nflId")) %>% 
    drop_na() %>% 
    ggplot(aes(x = success_tackle_pct.x, y = success_tackle_pct.y, color = position)) +
    geom_point(size = 4, alpha = 0.6, size = 0.75) +
    theme_clean() +
    geom_smooth(method = "lm", se = F) +
    ggpmisc::stat_poly_eq(size = 6) +
    scale_color_wsj() +
    labs(title = "Fig. B: Evaluating Successful Tackles per Snap",
         subtitle = "Minimum 50 Snaps in Each Group",
         x = "ST/Snap Weeks 1-5",
         y = "ST/Snap Score Weeks 6-9")-> p2
  
  week_plot_data %>% 
    left_join(comparison_data %>% select(nflId, Position = position))  %>% 
    drop_na() %>% 
    # mutate(gain_per_play.x = if_else(gain_per_play.x < 0.01, 0.01, gain_per_play.x),
    #        success_tackle_pct.y = if_else(success_tackle_pct.y < 0.001, 0.001, success_tackle_pct.y)) %>% 
    ggplot(aes(x = gain_per_play.x, y = success_tackle_pct.y, color = Position)) +
    geom_point(size = 4, alpha = 0.6) +
    #ggrepel::geom_text_repel(aes(label = displayName), point.padding = 0.5, box.padding = 0.5) +
    theme_clean() +
    geom_smooth(method = "lm", se = F, size = 0.75) +
    ggpmisc::stat_poly_eq(size = 6) +
    scale_color_wsj() +
    labs(title = "Fig C. How well does SRSS Predict Successful Tackles per Snap?",
         subtitle = "Minimum 50 Snaps in Each Group",
         x = "SRSS Score Weeks 1-5",
         y = "ST/Snap Score Weeks 6-9") -> p3
  
  
  combined <- ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
  
  # Save the combined plot
  ggsave("combined_plot.pdf", combined)
}

{
  # Cumulative Results
  run_train %>% 
    group_by(newId) %>% 
    summarise(value = mean(pred), success = mean(success)) %>% 
    mutate(value = round(value, digits = 3)) %>% 
    group_by(value) %>% 
    summarise(mean_success = mean(success),
              count = n()) %>% 
    arrange(value) %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = value, y = mean_success)) +
    geom_point(alpha = 0.5, size = 4, fill = "black") +
    #geom_smooth() +
    theme_clean() +
    labs( x = "Mean SRSS Score (Cumulative)",
          y = "Success Rate",
          title = "Fig. D: The Predictive Power of Cumulative SRSS",
          subtitle = "SRSS scores were rounded to their third digit and grouped, where n > 10")
  
  ggsave("Graphics/CumulativeSRSS.png")
}

################################### RANKINGS TABLE #############################

{
  # Final Table
  comparison_data %>% 
    mutate(srss_index = scales::rescale_max(gain_per_play),
           total_srss_index = scales::rescale_max(total_gain)) %>%
    arrange(-srss_index) %>% 
    left_join(rosters_22 %>% select(displayName = full_name, team)) %>%
    left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), by = c("team" = "team_abbr")) %>% 
    head(20) %>% 
    select(Rank = rank, ` ` = team_logo_espn, Player = displayName, Position = position, total_srss_index, srss_index, success_tackle_pct) %>% 
    make_table() %>% 
    gtExtras::gt_img_rows(columns = ` `) %>% 
    data_color( # Update cell colors...
      columns = c(total_srss_index, srss_index), # ...for mean_len column
      colors = scales::col_numeric(
        palette = c("#b8ebff", "#e6f6ff", "#ffdddd", "#ffcccb", "#febdbb"),
        domain = c(1, 0) # Column scale endpoints
      )
    ) %>% 
    data_color( # Update cell colors...
      columns = success_tackle_pct, # ...for mean_len column
      colors = scales::col_numeric(
        palette = c("#b8ebff", "#e6f6ff", "#ffdddd", "#ffcccb", "#febdbb"),
        domain = c(0, 0.17) # Column scale endpoints
      )
    ) %>%
    fmt_number(c(total_srss_index, srss_index), decimals = 3) %>% 
    fmt_percent(success_tackle_pct) %>% 
    cols_label(total_srss_index = "Total SRSS",
               srss_index = "SRSS",
               success_tackle_pct = "Run Stop %") %>% gtsave("Graphics/FinalTable.png")
  
    
}

{
  run_train %>% 
    group_by(newId) %>% 
    summarise(value = mean(pred), epa = mean(expectedPointsAdded)) %>% 
    ggplot(aes(x = value, y = epa)) + 
    ggthemes::theme_clean() + 
    geom_point(alpha = 0.4) + 
    geom_smooth(method = "lm") + 
    ggpmisc::stat_poly_eq() +
    xlim(0, 0.1) +
    ylim(-2.5, 2.5)
}

################################ ANIMATE PLAY ##################################

{
  # Find places to choose from
  run_train %>% 
    filter(success == 1 & yards_gained <= 4) %>% 
    group_by(newId) %>% 
    summarise(value = mean(pred), 
              min = min(pred),
              max = max(pred),
              success= mean(success), 
              play_length = n_distinct(frameId),
              epa = mean(expectedPointsAdded),
              yards = mean(yards_gained)) %>% 
    mutate(diff = max - min) %>% 
    arrange(value) %>% 
    mutate(newId = as.character(newId))
  
  # Eagles newId: 20220925081422
  
  run_train %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(newId == 20220918002034) %>% 
    select(row, newId, frameId, nflId, highlight_col = pred,
           handoffFrame, tackleFrame, ball_s) -> pred_data
  
  run_tracking %>% 
    filter(newId == 20220918002034) %>% 
    left_join(pred_data %>% select(-handoffFrame)) %>% 
    left_join(plays %>% select(newId, ballCarrierId)) %>% 
    mutate(highlight_col = if_else(is.na(highlight_col), 0, highlight_col),
           handoffFrame = min(pred_data$handoffFrame),
           tackleFrame = max(pred_data$tackleFrame)) -> track
  
  colors <- track$highlight_col
  
  full_colors <- c()
  
  for(i in colors){
    temp <- interpolate_color(i, max_val = max(run_train$pred))
    full_colors <- c(full_colors, temp)
  }
  
  track$highlight_color <- full_colors
  
  track %>% 
    filter(ball_s > 1) %>% 
    slice_max(frameId) %>% 
    select(nflId, last_color_frame = frameId, last_color = highlight_color) -> last_color_data
  
  track %>% 
    left_join(last_color_data) %>% 
    mutate(highlight_color = if_else(frameId >= last_color_frame, last_color, highlight_color)) -> track
  
  print(gga <- gganimate_play(track %>% filter(frameId <= 44 & frameId >= 4), pbp_22, highlight = T, zoom = T, 
                        animated_output = "gif", animated_res = 200))
  anim_save("Graphics/BadSuccess.gif", gga)
  
  pred_data %>% filter(nflId == 47872) %>% pull(row) -> rows_to_explain
}

################################## LIME ########################################

# Bust, which is very unfortunate don't really get why b/c the scores look great

{
  library(lime)
  
  # Setup lime::model_type()
  model_type.keras.engine.sequential.Sequential <- function(x, ...) {
    "classification"}
  
  # Setup lime::predict_model()
  predict_model.keras.engine.sequential.Sequential <- function (x, newdata, type, ...) {
    pred <- predict(object = x, x = as.matrix(newdata))
    data.frame (Positive = pred, Negative = 1 - pred) }
  
  # Teste do modelo
  predict_model (x       = model, 
                 newdata = (nn_train), 
                 type    = 'raw')
  
  
  # Usando lime() no train set
  explainer <- lime(x = as.data.frame(nn_train), model= model)
  
  # Rodando o Explainer
  
  system.time (
    explanation <- lime::explain (
      x = as.data.frame(nn_train[rows_to_explain[c(1,15, 24)], ]),
      explainer    = explainer, 
      n_features = 10,
      n_labels = 1))
  
  plot_features (explanation) +
    labs (title = "LIME: Feature Importance Visualization")
}

################################## PLOT PLAY ###################################

{
  tracking %>% 
    # sample(run_tracking$newId, 1)
    filter(newId == sample(run_tracking$newId, 1) & frameId == 1) %>% 
    ungroup() %>% 
    mutate(row = rnorm(1)) %>% 
    left_join(alignment_mapping, by = c("nflId", "newId")) %>% 
    group_by(nflId) %>% 
    slice_min(row) %>%
    ungroup() %>% 
    left_join(players %>% select(nflId, roster_pos = position)) %>% 
    mutate(position = case_when(
      is.na(tech) ~ roster_pos,
      .default = tech
    )) -> track
  
  plot_play(track, pbp_22, orientation = F, zoom = T, frame = 1, flip = F, numbers = F, dot_size = 12)
  
  
  ggsave("Graphics/def_alignment_sample.png")
}

