library(here)
library(tidyverse)
library(sf)
library(ggplot2)
library("car")
library("lubridate")
library(gganimate)
library(stringr)
library(ggmap)
library(future)
library(av)

# Define custom column names
column_names <- c("Date", "lat", "lon", "WMO_number", "Cycle_number", "Deep")

# Read the CSV file with custom column names
Argo_data <- read.csv(here("Data", "traj_positions_animation_13Nov2023.csv"), header = FALSE, col.names = column_names)

Argo_data$R_Time <- ymd_hms(Argo_data$Date)

Argo_data_top <- Argo_data %>% 
  #filter(Deep == 1)
  filter(year(R_Time) >= 2023 & year(R_Time) <= 2023) # Filter first 5 years
  #slice_sample(n=400000) # whole timeseries sample



tracks <- ggplot() +
  geom_path(data = Argo_data_top, aes(x = lon, y = lat,
                                      #color=daily_filtered_long_drift_long_SI,
                                      group = WMO_number), color = 'steelblue4', alpha = 0.1) +
  geom_point(data = Argo_data_top,
             aes(x=lon,y=lat,group=WMO_number),
             fill='steelblue', color = 'white', alpha = 0.7, shape=21, size = 2)+
  xlab("Longitude") +
  #scale_color_continuous(palette = RColorBrewer::brewer.pal(8, "PuOr"))+
  ylab("Latitude") +
  ggtitle("Trajectory Line Track")+
  coord_quickmap()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank())

animate(tracks + transition_reveal(Argo_data_top$R_Time) + ease_aes('linear') + exit_fade() + 
          #shadow_wake(wake_length = 0.1, alpha = FALSE) +
          ggtitle("Date: {frame_along}"),
        duration=30,fps = 24, width = 1920, height = 1080, 
        bg = 'transparent')
anim_save(here("Figures",paste("ARGO_Tracks_ALL_2023.gif", sep="")),
          bg = "transparent")

#### SPARKLE Argo data scene ----

Argo_data_sparkle <- Argo_data %>%
  filter(year(R_Time) >= 2023 & year(R_Time) <= 2023) %>% # Filter first 5 years
  distinct(WMO_number, .keep_all=TRUE) %>% 
  mutate(sample_group = sample(1:10, size = n(), replace = TRUE))

dots <- Argo_data_sparkle %>% 
  #filter(sample_group == 1) %>% 
  ggplot() +
  #geom_path(data = Argo_data_sparkle, aes(x = lon, y = lat, group = sample_group), color = 'steelblue4', alpha = 0.1) +
  geom_point(data = Argo_data_sparkle,
             aes(x=lon,y=lat, group = WMO_number),
             fill='steelblue', color = 'white', alpha = 0.7, shape=21, size = 2)+
  xlab("Longitude") +
  #scale_color_continuous(palette = RColorBrewer::brewer.pal(8, "PuOr"))+
  ylab("Latitude") +
  ggtitle("Trajectory Line Track")+
  coord_quickmap()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank())

dots


animate(dots + transition_states(Argo_data_sparkle$sample_group) + 
          #enter_fade() + exit_fade() +
          #enter_grow()+ exit_shrink()+
          shadow_wake(wake_length = 0.1, alpha = FALSE) +
          ggtitle("Date: {frame_along}"),
        duration=10,fps = 24, width = 1920*2, height = 1080*2, 
        bg = 'transparent')
anim_save(here("Figures",paste("ARGO_Sparkle-Dots.gif", sep="")),
          bg = "transparent")


