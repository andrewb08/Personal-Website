library(tidyverse)
library(stringr)

Hitter_Plot <- function(Trackman, Hitter, Date) {
  name_list <- str_split(Hitter, ", ")
  Name <- str_glue(name_list[[1]][2], name_list[[1]][1], .sep = " ")
  
  Game <- Trackman %>%
    filter(Batter == Hitter,
           PitchCall != 'Undefined',
           !is.na(PlateLocHeight),
           !is.na(PlateLocSide)) %>% 
    arrange(Date, Inning, PAofInning, PitchofPA)
  
  Game$result <- rep(NA, nrow(Game))
  for (i in 1:nrow(Game)) {
    if (Game$PitchCall[i] == 'BallCalled' | Game$PitchCall[i] == 'StrikeCalled') {
      Game$result[i] <- 'Take'
    } else if (Game$PitchCall[i] == 'StrikeSwinging') {
      Game$result[i] <- 'Whiff'
    } else if (Game$PitchCall[i] == 'InPlay' & !is.na(Game$ExitSpeed[i]) & Game$ExitSpeed[i] >= 90) {
      Game$result[i] <- 'Hard Contact'
    } else {
      Game$result[i] <- 'Contact'
    }
  }
  
  Game$PitchType <- rep(NA, nrow(Game))
  for (i in 1:nrow(Game)) {
    if (Game$TaggedPitchType[i] %in% c('Fastball', 'Sinker')) {
      Game$PitchType[i] <- 'Fastball'
    } else if (Game$TaggedPitchType[i] %in% c('Slider', 'Curveball', 'Cutter')) {
      Game$PitchType[i] <- 'Breaking Ball'
    } else if (Game$TaggedPitchType[i] %in% c('Changeup', 'ChangeUp', 'Splitter')) {
      Game$PitchType[i] <- 'Off Speed'
    } else {
      Game$PitchType[i] <- 'Other'
    }
  }
  
  cols <- c("Take" = "black", "Whiff" = "#384CDF", "Hard Contact" = "red", "Contact" = "#135F0F")
  #options(repr.plot.width = 10, repr.plot.height = 18)
  
  cat("\n\n\\pagebreak\n")
  cat("**", Hitter, " -- ", Date, "**\n\n", sep = "")
  hitter_plot <- ggplot(Game, aes(x = -1*PlateLocSide, y = PlateLocHeight, shape = factor(PitchType), color = factor(result))) +
    geom_point(size = 3) +
    geom_segment(aes(x = -.82, y = 1.5, xend = .82, yend = 1.5), color = 'black') +
    geom_segment(aes(x = -.82, y = 3.5, xend = .82, yend = 3.5), color = 'black') +
    geom_segment(aes(x = -.82, y = 1.5, xend = -.82, yend = 3.5), color = 'black') +
    geom_segment(aes(x = .82, y = 3.5, xend = .82, yend = 1.5), color = 'black') +
    geom_segment(aes(x = .82, y = 1.5+2/3, xend = -.82, yend = 1.5+2/3), color = 'black', size = .35) +
    geom_segment(aes(x = .82, y = 1.5+4/3, xend = -.82, yend = 1.5+4/3), color = 'black', size = .35) +
    geom_segment(aes(x = .27, y = 1.5, xend = .27, yend = 3.5), color = 'black', size = .35) +
    geom_segment(aes(x = -.27, y = 1.5, xend = -.27, yend = 3.5), color = 'black', size = .35) +
    xlim(-2.7, 2.7) +
    ylim(-2, 6) +
    scale_colour_manual(values = cols) +
    ggtitle(Hitter, subtitle = "Umpire View") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    labs(shape = 'Pitch Type',
         color = 'Pitch Result',
         x = 'Horizontal Pitch Location (ft)',
         y = 'Vertical Pitch Location (ft)') +
    theme(aspect.ratio = 1.5)
  print(hitter_plot)
  cat("\n\n\\pagebreak\n")
  
}












