{
  library(tidyverse) 
  library(dplyr)
  library(gganimate)
  library(ggforce)
  library(ggplot2)
  library(ggrepel)
  library(plotly)
  library(transformr)
  library(ggshadow)
  library(igraph)
  library(readr)
  library(nflverse)
  library(gt)
  library(gtExtras)
  library(patchwork)
  setwd("/Users/maxwhalen/Documents/GitHub/Big-Data-Bowl")
  
}

#' Plot a play
#'
#' @description Plot or animate a play.
#' @param df_track A df of tracking data from one play.
#' @param orientation Show lines representing where player is facing (default = T).
#' @param dot_size Size of player dots (default = 6).
#' @param segment_length Length of orientation segment lines (default = 2.5).
#' @param segment_size Width of orientation segment lines (default = 1.5).
#' @param numbers Show player jersey numbers (default = T).
#' @param animated Whether play is animated, rather than a still frame (default = T).
#' @param animated_h If animated, height of animated image (default = 4).
#' @param animated_w If animated, width of animated image (default = 8).
#' @param animated_res If animated, resolution of animated image (default = 200).
#' @param frame frameId to plot (default = NULL, ie plot all provided frames).
#' @param zoom zoom to play area (default = FALSE, ie plot shows full field).
#'  @param highlight highlight a column attribute (default = FALSE, colmust be named 'highlight_col').
#' @export
plot_play <- function(
    df_track_og,
    pbp,
    orientation = TRUE,
    flip = FALSE,
    dot_size = 7,
    segment_length = 2.5,
    segment_size = 1.5,
    numbers = TRUE,
    animated = FALSE,
    animated_h = 4,
    animated_w = 8,
    animated_res = 200,
    frame = NULL,
    zoom = TRUE,
    highlight = FALSE,
    voronoi = FALSE,
    shadow_size = 15,
    animated_output = "mp4"
) {
  
  df_track_og %>% 
    left_join(
      pbp_22 %>% select(newId, nflfastr_game_id = game_id, down, qtr, ydstogo, 
                        desc, defensiveTeam = defteam), 
      by = c("newId")
    ) %>% 
    left_join(teams_colors_logos %>% select(club = team_abbr, team_color, team_color2)) %>%
    mutate(defense = if_else(club == defensiveTeam, 1, 0)) -> df_track
  
  caption <- glue::glue("{df_track$nflfastr_game_id[1]} {df_track$down[1]}&{df_track$ydstogo[1]}: Q{df_track$qtr[1]} {df_track$desc[1]}")
  
  if (!is.null(frame)) {
    
    df_track_filtered <- df_track %>% filter(frameId == frame) %>% distinct()
    
  }
  
  df_track_filtered %>% 
    filter(defensiveTeam == club) %>% 
    pull(team_color) %>% 
    unique() -> def_team_color
  
  df_track_filtered %>% 
    filter(defensiveTeam != club & club != "football") %>% 
    pull(team_color) %>% 
    unique() -> off_team_color
  
  
  fig <- ggfootball(left_endzone_color = def_team_color, right_endzone_color = off_team_color) 
  
  
  if (highlight) { #TODO MAKE WORK
    fig <- fig +
      geom_point(data = df_track_filtered, 
                 aes(x, y),
                 color = ifelse(
                   df_track_filtered$nflId == df_track_filtered$ballCarrierId,
                   "blue", "red"
                 ),
                 shape = ifelse(
                   df_track_filtered$club == "football",
                   18, 19
                 ),
                 size = ifelse(
                   df_track_filtered$club == "football",
                   0, (dot_size * (shadow_size*0.66)) *  df_track_filtered$highlight_col
                 ),
                 alpha = 0.5 + df_track_filtered$highlight_col*0.2
      ) + 
      geom_point(data = df_track_filtered, 
                 aes(x, y),
                 color = ifelse(
                   df_track_filtered$nflId == df_track_filtered$ballCarrierId,
                   "#088F8F", "orangered"
                 ),
                 shape = ifelse(
                   df_track_filtered$club == "football",
                   18, 19
                 ),
                 size = ifelse(
                   df_track_filtered$club == "football",
                   0, (dot_size * shadow_size) *  df_track_filtered$highlight_col
                 ),
                 alpha = 0.25 + df_track_filtered$highlight_col*0.1
      ) 
  } 
  
  fig <- fig +
    geom_point(data = df_track_filtered, 
               aes(x, y),
               color = if_else(df_track_filtered$club == "football", "#825736", df_track_filtered$team_color),
               shape = ifelse(
                 df_track_filtered$club == "football",
                 18, 19
               ),
               size = ifelse(
                 df_track_filtered$club == "football",
                 dot_size*0.5, dot_size
               )
    ) +
    geom_point(data = df_track_filtered, 
               aes(x, y),
               color = if_else(df_track_filtered$club == "football", "white", df_track_filtered$team_color2),
               shape = ifelse(
                 df_track_filtered$club == "football",
                 5, 1
               ),
               size = ifelse(
                 df_track_filtered$club == "football",
                 dot_size*0.45 , dot_size * 0.95
               ),
               stroke = dot_size*0.2
    ) +
    labs(
      caption = caption
    ) +
    theme(
      plot.title = element_blank(),
      plot.margin = margin(.1, 0, .5, 0, "cm"),
      plot.caption = element_blank()
    )
  
  if (orientation == TRUE & "o" %in% names(df_track_filtered)) {
    
    fig <- fig +
      # orientation lines
      geom_segment(
        data = df_track_filtered,
        aes(x, y, xend = x + segment_length * o_x, yend = y + segment_length * o_y),
        color = df_track_filtered$team_color, 
        linewidth = segment_size
      )
    
  }
  
  if (numbers) {
    
    fig <- fig +
      geom_text(
        data = df_track_filtered,
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = "white",
        size = 0.5*dot_size
      )
    
  } else{
    fig <- fig +
      geom_text(
        data = df_track_filtered,
        mapping = aes(x = x, y = y, label = position),
        colour = "white",
        size = 0.5*dot_size
      )
  }
  
  if(zoom) {
    fig <- fig + 
      coord_cartesian(xlim = c(min(df_track_filtered$x) - 5, 
                               max(df_track_filtered$x) + 5),
                      ylim = c(min(df_track_filtered$y) - 2, 
                                max(df_track_filtered$y) + 2),)
  }
  
  if(flip){
    fig <- fig + coord_flip()
  }
  
  if(voronoi) {
    fig <- fig +
      geom_voronoi_tile(
        data = df_track_filtered,
        aes(x = x, y = y, alpha = 0.75),
        
      )
  }
  
  return(fig)
  
}


#' Plot a play
#'
#' @description Plot or animate a play.
#' @param df_track A df of tracking data from one play.
#' @param orientation Show lines representing where player is facing (default = T).
#' @param dot_size Size of player dots (default = 6).
#' @param segment_length Length of orientation segment lines (default = 2.5).
#' @param segment_size Width of orientation segment lines (default = 1.5).
#' @param numbers Show player jersey numbers (default = T).
#' @param animated Whether play is animated, rather than a still frame (default = T).
#' @param animated_h If animated, height of animated image (default = 4).
#' @param animated_w If animated, width of animated image (default = 8).
#' @param animated_res If animated, resolution of animated image (default = 200).
#' @param frame frameId to plot (default = NULL, ie plot all provided frames).
#' @param zoom zoom to play area (default = FALSE, ie plot shows full field).
#'  @param highlight highlight a column attribute (default = FALSE, colmust be named 'highlight_col').
#' @export
gganimate_play <- function(
    df_track_og,
    pbp_22 = pbp_22,
    orientation = FALSE,
    dot_size = 5,
    segment_length = 2.5,
    segment_size = 1.5,
    numbers = TRUE,
    animated = TRUE,
    animated_h = 4,
    animated_w = 8,
    animated_res = 600,
    zoom = FALSE,
    highlight = FALSE,
    coord_flip = FALSE,
    shadow_size = 15,
    animated_output = "mp4"
) {
  
  df_track_og %>% 
    left_join(
      pbp_22 %>% select(newId, nflfastr_game_id = game_id, qtr, ydstogo, desc, 
                        defensiveTeam = defteam, down),
      by = c("newId")
    ) %>%
    mutate(defense = if_else(club == defensiveTeam, 1, 0)) %>% 
    left_join(teams_colors_logos, by = c("club" = "team_abbr")) -> df_track_filtered
  
  subtitle <- glue::glue("{df_track_filtered$nflfastr_game_id[1]}: {df_track_filtered$down[1]}&{df_track_filtered$ydstogo[1]}: Q{df_track_filtered$qtr[1]}") 
  caption <- glue::glue("{df_track_filtered$desc[1]}")
  # Pass in color data
  df_track_filtered %>% 
    filter(defensiveTeam == club) %>% 
    pull(team_color) %>% 
    unique() -> def_team_color
  
  df_track_filtered %>% 
    filter(defensiveTeam != club & club != "football") %>% 
    pull(team_color) %>% 
    unique() -> off_team_color
  
  
  fig <- ggfootball(left_endzone_color = def_team_color, right_endzone_color = off_team_color) 

  
  if (highlight) { #TODO MAKE WORK
    fig <- fig +
      geom_point(data = df_track_filtered, 
                 aes(x, y),
                 color = case_when(
                   (df_track_filtered$nflId == df_track_filtered$ballCarrierId) & (df_track_filtered$handoffFrame <= df_track_filtered$frameId) ~ "#825736",
                   (df_track_filtered$club != df_track_filtered$defensiveTeam) ~ "white",
                   (df_track_filtered$club == df_track_filtered$defensiveTeam) & (df_track_filtered$frameId < df_track_filtered$handoffFrame) ~ "yellow",
                   df_track_filtered$club == df_track_filtered$defensiveTeam ~ df_track_filtered$highlight_color,
                   df_track_filtered$club == "football" ~ "#825736"
                 ),
                 fill = "white",
                 shape = ifelse(
                   df_track_filtered$club == "football",
                   18, 19
                 ),
                 size = ifelse(
                   df_track_filtered$club == "football",
                   dot_size * 0.5, dot_size
                 ),
                 alpha = 0.8,
                 stroke = 1
      ) 
      
  } else{
    fig <- fig +
      geom_point(data = df_track_filtered, 
                 aes(x, y),
                 color = if_else(df_track_filtered$club == "football", "#825736", df_track_filtered$team_color),
                 shape = ifelse(
                   df_track_filtered$club == "football",
                   18, 19
                 ),
                 size = ifelse(
                   df_track_filtered$club == "football",
                   dot_size*0.5, dot_size
                 )
      ) +
      geom_point(data = df_track_filtered, 
                 aes(x, y),
                 color = if_else(df_track_filtered$club == "football", "white", df_track_filtered$team_color2),
                 shape = ifelse(
                   df_track_filtered$club == "football",
                   5, 1
                 ),
                 size = ifelse(
                   df_track_filtered$club == "football",
                   dot_size*0.45 , dot_size * 0.95
                 ),
                 stroke = dot_size*0.2
      )
  }
  
  if (orientation == TRUE & "o" %in% names(df_track_filtered)) {
    
    fig <- fig +
      # orientation lines
      geom_segment(
        data = df_track_filtered,
        aes(x, y, xend = x + segment_length * o_x, yend = y + segment_length * o_y),
        color = df_track_filtered$team_color, 
        linewidth = segment_size
      )
    
  }
  
  if (numbers) {
    
    fig <- fig +
      geom_text(
        data = df_track_filtered,
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = if_else(df_track_filtered$defensiveTeam == df_track_filtered$club, "white", "black"),
        size = 0.5*dot_size
      )
    
  }
  
  if(zoom) {
    fig <- fig + 
      coord_cartesian(xlim = c(min(df_track_filtered$x) - 1, max(df_track_filtered$x) + 1))
    
    animated_w <- animated_w * 0.7
  }
  
  if(coord_flip) {
    fig <- fig +
      coord_flip()
  }
  
  fig <- fig  +
    labs(
      caption = caption,
      subtitle = gsub("_", " ", subtitle),
    ) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5, size = 10),
      plot.margin = margin(0.5, 0, 0.25, 0, "cm")
    )
  
  if (animated) {
    
    if (animated_output == "mp4") {
      renderer <- gganimate::gifski_renderer()
    } else {
      renderer <- gganimate::av_renderer()
    }
    
    animated_fig <- fig +
      gganimate::transition_states(frameId, transition_length = 1, state_length = 1) +
      ease_aes('linear') +
      enter_appear() +
      exit_disappear()
    
    fig_gif <- gganimate::animate(
      animated_fig,
      renderer = renderer,
      height = animated_h, width = animated_w, units = "in",
      res = animated_res,
      #nframes = n_distinct(df_track_og$frameId),
      #start_pause = min(df_track_og$frameId),
      #end_pause = max(df_track_og$frameId)
    )
  }
  
  return(fig_gif)
  
}

# helper function to not make every table have so many lines of code
make_table <- function(df) {
  df %>%
    gt::gt() %>%
    gt::tab_style(
      style = gt::cell_text(color = "black", weight = "bold"),
      locations = list(
        gt::cells_column_labels(dplyr::everything())
      )
    ) %>%
    gt::tab_options(
      row_group.border.top.width = gt::px(3),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.top.color = "black",
      table.border.top.width = gt::px(1),
      table.border.bottom.color = "white",
      table.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = gt::px(2),
      row.striping.background_color = '#FFFFFF',
      row.striping.include_table_body = TRUE,
      table.background.color = '#F2F2F2',
      data_row.padding = gt::px(2),
      table.font.size = gt::px(16L)
    ) %>%
    return()
}

############################ HELPER FUNCTIONS ##################################
interpolate_color <- function(value, 
                              max_val = 1,
                             low_color="yellow", 
                             mid_color="red", 
                             high_color="darkred") {
  
  # Create color palette
  colors <- c(low_color, mid_color, high_color)
  
  return_color <- npreg::number2color(value, c(low_color, mid_color, high_color), 
                                      xmin = 0, xmax = max_val)
  
  return(return_color)
}


#' Plot American Football Field
#'
#' This function produces a plot of an American football field using ggplot2
#' objects. Originally, this function was created for the NFL Big Data Bowl 2021
#' on Kaggle (\url{https://www.kaggle.com/c/nfl-big-data-bowl-2021}). To keep
#' with the conventions of the datasets provided, the plotted field spans from
#' 0 - 120 in the x-direction and 0 - 53.3 in the y-direction. It is expected
#' that users will add other ggplot2 objects (e.g., points representing players,
#' text annotations) to produce complete visualizations.
#'
#' @param left_endzone_color Color of left end zone, specified in quotes
#' @param right_endzone_color Color of right end zone, specified in quotes
#' @param field_color Color of field (not including end zones), specified
#'     in quotes
#' @param field_alpha Opacity of field color (not including end zones),
#'     specified as numeric between 0.0 and 1.0
#' @param top_buffer Empty space provided on top of plot, specified as numeric
#' @param bottom_buffer Empty space provided at bottom of plot, specified as
#'     numeric
#' @param left_buffer Empty space provided to left of plot, specified as numeric
#' @param right_buffer Empty space provided to right of plot, specified as
#'     numeric
#' @param five_yd_lines Boolean value indicating whether to include white
#'     vertical lines at each five yard increment
#' @param ydline_labels Boolean value indicating whether to include yard line
#'     labels every ten yards
#' @param ydline_label_size Size of text used for yard line labels, specified
#'     as numeric
#' @param outer_hash Boolean value indicating whether to include hash marks
#'     outside of yard line labels (near sidelines)
#' @param inner_hash Boolean value indicating whether to include hash marks
#'     near middle of field
#'
#' @return The output will be a plot of an American football field
#'
#' @examples
#' ggfootball()
#' ggfootball(left_endzone = "red", right_endzone = "blue",
#'     field_alpha = 0.7)
#' ggfootball() + geom_point(data =
#'     data.frame(x = c(10, 20), y = c(20, 30)),
#'     aes(x = x, y = y))
#'
#' @export

ggfootball <- function(left_endzone_color = "gray90",
                       right_endzone_color = "gray90",
                       field_color = "green4",
                       field_alpha = 0.85,
                       top_buffer = 0,
                       bottom_buffer = 0,
                       left_buffer = 0,
                       right_buffer = 0,
                       five_yd_lines = TRUE,
                       ydline_labels = TRUE,
                       ydline_label_size = 6,
                       outer_hash = TRUE,
                       inner_hash = FALSE) {
  
  # Make middle of field green
  gplot <- ggplot2::ggplot() + ggplot2::geom_rect(data = NULL,
                                                  ggplot2::aes(xmin = 10, xmax = 110, ymin = 0, ymax = 53.3),
                                                  fill = field_color, color = "black", alpha = field_alpha) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 10, xmax = 15, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 20, xmax = 25, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 30, xmax = 35, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 40, xmax = 45, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 50, xmax = 55, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 60, xmax = 65, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 70, xmax = 75, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 80, xmax = 85, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 90, xmax = 95, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    geom_rect(data = NULL,
              ggplot2::aes(xmin = 100, xmax = 105, ymin = 0, ymax = 53.3),
              fill = field_color, color = "black", alpha = field_alpha*1.12) +
    
    # Add endzones
    ggplot2::geom_rect(data = NULL,
                       ggplot2::aes(xmin = 0, xmax = 10, ymin = 0, ymax = 53.3),
                       fill = left_endzone_color, color = "black") +
    ggplot2::geom_rect(data = NULL,
                       ggplot2::aes(xmin = 110, xmax = 120, ymin = 0, ymax = 53.3),
                       fill = right_endzone_color, color = "black") +
    
    # Format gridlines, tick marks, tick labels, and border of plot window
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 16),
                   #, legend.position = "none" # Optional hiding of legend
    ) +
    
    # Add x and y axis limits
    ggplot2::lims(x = c(0 - left_buffer, 120 + right_buffer),
                  y = c(0 - bottom_buffer, 53.3 + top_buffer))
  
  # Add vertical lines at each 5-yard increment
  if(five_yd_lines) {
    # Create data frame with necessary x and y coordinates
    five_yard_df <- data.frame(x = seq(from = 15, to = 105, by = 5))
    # Add to existing plot
    gplot <- gplot +
      ggplot2::geom_segment(data = five_yard_df,
                            mapping = ggplot2::aes(x = x, xend = x,
                                                   y = -Inf, yend = 53.3),
                            color = "white")
  }
  
  # Add yardline labels
  if(ydline_labels) {
    # Create data frame with labels and coordinates
    yard_labels_df <- data.frame(x = seq(from = 20, to = 100, by = 10),
                                 y = rep(x = 4, n = 9),
                                 digits = c(seq(from = 10, to = 50, by = 10),
                                            seq(from = 40, to = 10, by = -10)))
    # Add to existing plot
    gplot <- gplot +
      ggplot2::geom_text(data = yard_labels_df,
                         mapping = ggplot2::aes(x = x, y = y, label = digits),
                         color = "white", size = ydline_label_size, family = "Clarendon")
    gplot <- gplot +
      ggplot2::geom_text(data = yard_labels_df,
                         mapping = ggplot2::aes(x = x, y = 53.3 - y,
                                                label = digits),
                         color = "white", angle = 180, size = ydline_label_size,
                         family = "Clarendon")
  }
  
  # Add outer hash marks to field
  if(outer_hash) {
    # Create data frame with hash mark x-coordinates
    hash_df <- data.frame(x = 11:109)
    # Add to existing plot
    gplot <- gplot +
      ggplot2::geom_segment(data = hash_df,
                            mapping = ggplot2::aes(x = x, xend = x,
                                                   y = 0.5, yend = 1.5),
                            color = "white") +
      ggplot2::geom_segment(data = hash_df,
                            mapping = ggplot2::aes(x = x, xend = x,
                                                   y = 51.8, yend = 52.8),
                            color = "white")
  }
  
  # Add inner hash marks to field
  if(inner_hash) {
    # Create data frame with hash mark x-coordinates
    hash_df <- data.frame(x = 11:109)
    # Add to existing plot
    gplot <- gplot +
      ggplot2::geom_segment(data = hash_df,
                            mapping = ggplot2::aes(x = x, xend = x,
                                                   y = 17.8, yend = 18.8),
                            color = "white") +
      ggplot2::geom_segment(data = hash_df,
                            mapping = ggplot2::aes(x = x, xend = x,
                                                   y = 34.6, yend = 35.6),
                            color = "white")
  }
  
  # Create final solid black outlines for the field
  gplot <- gplot +
    ggplot2::geom_rect(data = NULL,
                       ggplot2::aes(xmin = 10, xmax = 110,
                                    ymin = 0, ymax = 53.3),
                       fill = NA, color = "black") +
    ggplot2::geom_rect(data = NULL,
                       ggplot2::aes(xmin = 0, xmax = 120,
                                    ymin = 0, ymax = 53.3),
                       fill = NA, color = "black") +
    theme(
        plot.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 10),
        plot.margin = margin(0, 0, 0, 0, "cm")
      )
  
  # Return plot
  gplot
}

