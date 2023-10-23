library(here)
library(tidyverse)
library(sf)
library(ggplot2)
library("car")
library("lubridate")
library(gganimate)
library(stringr)
library(ggmap)

# Define custom column names
column_names <- c("Date", "lat", "lon", "WMO_number", "Cycle_number")

# Read the CSV file with custom column names
Argo_data <- read.csv(here("Data", "traj_positions_animation_12Sep2023.csv"), header = FALSE, col.names = column_names)

Argo_data$R_Time <- ymd_hms(Argo_data$Date)

Argo_data_top <- Argo_data %>% 
  slice_sample(n=500000)

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

animate(tracks + transition_reveal(Argo_data_top$R_Time) + ease_aes('linear') + 
          shadow_wake(wake_length = 0.1, alpha = FALSE) +
          ggtitle("Date: {frame_along}"),
        duration=30,fps = 24, width = 1920, height = 1080, bg = 'transparent')
anim_save(here("Figures",paste("ARGO_Tracks1.gif", sep="")),
          bg = "transparent")