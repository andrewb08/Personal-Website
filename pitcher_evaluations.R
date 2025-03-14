library(tidyverse)
library(stringr)
library(gridExtra)
library(knitr)
library(kableExtra)

get_mode <- function(column) {
  freq_table <- table(column)
  mode_value <- names(freq_table)[which.max(freq_table)]
  mode_value <- sub(":\\d{2}$", "", mode_value)
  return(mode_value)
}

pitcher_plots <- function(Trackman, Season, Pitcher, game_date, Total = TRUE) {
  #Name <- str_split(Pitcher, ", ")[[1]][1]
  Name <- Pitcher
  
  Game <- Trackman %>%
    filter(Pitcher == Name,
           PitchCall != 'Undefined',
           !TaggedPitchType %in% c('Other', 'Auto'))
  Game$PitchCall <- ifelse(Game$PitchCall == 'BallinDirt', 'BallCalled', Game$PitchCall)
  
  
  # Game$result <- rep(NA, nrow(Game))
  
  Overall <- Season %>% 
    filter(Pitcher == Name,
           PitchCall != 'Undefined',
           TaggedPitchType != 'Other')
  
  # assigning pitches colors for plots
  pitch_colors <- c('Fastball' = '#d22d49', 'Sinker' = '#fe9d00', 'Cutter' = '#933f2c', 'Curveball' = '#00d1ed',
                    'Slider' = '#c3bd0d', 'ChangeUp' = '#23be41', 'Splitter' = 'blue', 'Other' = '#Acafaf', 
                    'Sweeper' = 'purple')
  
  # correcting pitch order
  Game$TaggedPitchType <- factor(Game$TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball',
                                                                  'Slider', 'Sweeper','ChangeUp', 'Splitter', 'Other'))
  
  # strike zone
  k_zone <- c(-0.78, 0.78, 1.5, 3.35)
  
  # pitch functions
  is_zone <- function(height, side, zone = k_zone) {
    zones <- 0
    for (i in seq_along(height)) {
      if (is.na(height[i]) | is.na(side[i])) {
        zones <- zones
      } else if (height[i] > k_zone[3] & height[i] < k_zone[4] & side[i] > k_zone[1] & side[i] < k_zone[2]) {
        zones <- zones + 1
      }
    }
    return(zones)
  }
  
  is_swing <- function(PitchCall) {
    swings <- 0
    for (i in seq_along(PitchCall)) {
      if (PitchCall[i] %in% c('StrikeSwinging', 'FoulBall', 'FoulBallFieldable', 'FoulBallNotFieldable','InPlay')) {
        swings <- swings + 1
      }
    }
    return(swings)
  }
  
  is_chase <- function(height, side, pitch_call, zone = k_zone) {
    chases <- 0
    for (i in seq_along(height)) {
      if ((!is_zone(height[i], side[i])) & is_swing(pitch_call[i])) {
        chases <- chases + 1
      }
    }
    return(chases)
  }
  
  # movement plot
  ## creating means
  means <- Game %>% 
    group_by(TaggedPitchType) %>% 
    summarize(InducedVertBreak = mean(InducedVertBreak, na.rm = T),
              HorzBreak = mean(HorzBreak, na.rm = T)) %>% 
    select(TaggedPitchType, InducedVertBreak, HorzBreak)
  
  movement <- ggplot(Game, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
    geom_point(size = 4, alpha = .65) +
    geom_point(data = means, shape = 21, size = 4, fill = 'white', color = 'black', stroke = 1.5) +
    scale_color_manual(values = pitch_colors) +
    geom_vline(xintercept = 0, alpha = .8) +
    geom_hline(yintercept = 0, alpha = .8) +
    labs(title = str_c(Pitcher, ' Movement Profile'), subtitle = "Pitcher POV") +
    ylab('Induced Vertical Break') +
    xlab('Horizontal Break') +
    ylim(c(-28, 28)) +
    xlim(c(-28, 28)) +
    guides(color = guide_legend(title = 'Pitch')) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          aspect.ratio = 1.5)
  
  # location plot
  location <- ggplot(Game, aes(x = PlateLocSide, y = PlateLocHeight,
                               color = TaggedPitchType, shape = PitchCall)) +
    geom_point(size = 3) +
    scale_color_manual(values = pitch_colors) +
    geom_segment(x = -.78, y = 1.5, xend = .78, yend = 1.5, color = 'black') +
    geom_segment(x = -.78, y = 3.35, xend = .78, yend = 3.35, color = 'black') +
    geom_segment(x = -.78, y = 1.5, xend = -.78, yend = 3.35, color = 'black') +
    geom_segment(x = .78, y = 1.5, xend = .78, yend = 3.35, color = 'black') +
    ggtitle(str_c(Pitcher, ' Location'), subtitle = "Pitcher POV") +
    ylab('PlateZ') +
    xlab('PlateX') +
    ylim(c(0.5, 4)) +
    xlim(c(-1.5, 1.5)) +
    guides(color = guide_legend(title = 'Pitch'),
           shape = guide_legend(title = 'Pitch Result')) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          aspect.ratio = 1.5)
  
  # pitch metrics and stats for game
  metrics_game <- Game %>% 
    mutate(pitch_total = n()) %>% 
    group_by(TaggedPitchType) %>% 
    summarize(count = n(),
              usage = round(count/mean(pitch_total), 3) * 100,
              min_velo = round(min(RelSpeed, na.rm = T), 1),
              avg_velo = round(mean(RelSpeed, na.rm = T), 1),
              max_velo = round(max(RelSpeed, na.rm = T), 1),
              spin = round(mean(SpinRate, na.rm = T), -1),
              relZ = round(mean(RelHeight, na.rm = T), 2),
              relX = round(mean(RelSide, na.rm = T), 2),
              extension = round(mean(Extension, na.rm = T), 2),
              ivb = round(mean(InducedVertBreak, na.rm = T), 1),
              hb = round(mean(HorzBreak, na.rm = T), 1),
              vaa = round(mean(VertApprAngle, na.rm = T), 2),
              in_zones = is_zone(PlateLocHeight, PlateLocSide),
              out_zones = count - in_zones,
              zone_rate = round(in_zones/count, 3) * 100,
              chases = is_chase(PlateLocHeight, PlateLocSide, PitchCall),
              chase_rate = round(chases/out_zones, 3) * 100,
              called_strikes = sum(PitchCall == 'StrikeCalled'),
              swings = sum(PitchCall %in% c('StrikeSwinging', 'FoulBall', 'FoulBallFieldable', 'FoulBallNotFieldable', 
                                            'InPlay')),
              whiffs = sum(PitchCall == 'StrikeSwinging'),
              swing_pct = round(swings/count, 3) * 100,
              whiff_rate = round(whiffs/swings, 3) * 100,
              sw_str_pct = round(whiffs/count, 3) * 100,
              CSW = round((called_strikes + whiffs)/count, 3) * 100,
              tilt = get_mode(Tilt)) %>%
    arrange(TaggedPitchType) %>% 
    select('pitch' = TaggedPitchType, usage, min_velo, avg_velo, max_velo, spin, relZ, relX, extension,
           ivb, hb, vaa, tilt,'zone%' = zone_rate, 'swing%' = swing_pct, 'chase%' = chase_rate, 'whiff%' = whiff_rate,
           'swstr%' = sw_str_pct, 'csw%' = CSW) %>% 
    add_column('date' = game_date, .before = 'usage')
  
  # pitch metrics and stats for season
  if (Total) {
    metrics_tot <- Overall %>% 
      mutate(pitch_total = n()) %>% 
      group_by(TaggedPitchType) %>% 
      summarize(count = n(),
                usage = round(count/mean(pitch_total), 3) * 100,
                min_velo = round(min(RelSpeed, na.rm = T), 1),
                avg_velo = round(mean(RelSpeed, na.rm = T), 1),
                max_velo = round(max(RelSpeed, na.rm = T), 1),
                spin = round(mean(SpinRate, na.rm = T), -1),
                relZ = round(mean(RelHeight, na.rm = T), 2),
                relX = round(mean(RelSide, na.rm = T), 2),
                extension = round(mean(Extension, na.rm = T), 2),
                ivb = round(mean(InducedVertBreak, na.rm = T), 1),
                hb = round(mean(HorzBreak, na.rm = T), 1),
                vaa = round(mean(VertApprAngle, na.rm = T), 2),
                in_zones = is_zone(PlateLocHeight, PlateLocSide),
                out_zones = count - in_zones,
                zone_rate = round(in_zones/count, 3) * 100,
                chases = is_chase(PlateLocHeight, PlateLocSide, PitchCall),
                chase_rate = round(chases/out_zones, 3) * 100,
                called_strikes = sum(PitchCall == 'StrikeCalled'),
                swings = sum(PitchCall %in% c('StrikeSwinging', 'FoulBall', 'FoulBallFieldable', 'FoulBallNotFieldable',
                                              'InPlay')),
                whiffs = sum(PitchCall == 'StrikeSwinging'),
                swing_pct = round(swings/count, 3) * 100,
                whiff_rate = round(whiffs/swings, 3) * 100,
                sw_str_pct = round(whiffs/count, 3) * 100,
                CSW = round((called_strikes + whiffs)/count, 3) * 100, 
                tilt = get_mode(Tilt)) %>% 
      arrange(TaggedPitchType) %>% 
      select('pitch' = TaggedPitchType, usage, min_velo, avg_velo, max_velo, spin, relZ, relX, extension,
             ivb, hb, vaa, tilt, 'zone%' = zone_rate, 'swing%' = swing_pct, 'chase%' = chase_rate, 'whiff%' = whiff_rate,
             'swstr%' = sw_str_pct, 'csw%' = CSW) %>% 
      add_column('date' = 'Season', .before = 'usage')
  
    
    # pitch metrics and stats total
    metrics <- rbind(metrics_game, metrics_tot) %>% 
      arrange(pitch) %>% 
      kable(format = 'latex',
            align = 'c') %>%
      kable_styling(position = 'center',
                    latex_options = c('striped', 'repeat_header', 'scale_down')) %>% 
      row_spec(seq(from = 2, to = (nrow(metrics_game)+nrow(metrics_tot)), by = 2),
               background = '#AECEFF')
  } else {
    metrics <- metrics_game %>% 
      arrange(pitch) %>% 
      kable(format = 'latex',
            align = 'c') %>%
      kable_styling(position = 'center',
                    latex_options = c('striped', 'repeat_header', 'scale_down'))
  }
  
  cat("\n\n\\pagebreak\n")
  cat("**", Pitcher, " -- ", game_date, "**\n\n", sep = "", "\n\n\n\n")
  grid.arrange(movement, location, ncol = 2)
  cat("\n\n")
  print(metrics)
  cat("\n\n\\pagebreak\n")
  
}





