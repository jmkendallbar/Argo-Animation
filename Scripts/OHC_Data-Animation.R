library(here)
library(tidyverse)
library(sf)

library(ggplot2)
library("car")
library("lubridate")
library(gganimate)
library(stringr)
library(ggmap)
library(RColorBrewer)

# Read the CSV file with custom column names
OHC_data <- read_csv(here("Data", "OHC_Data.csv"))

# Identify columns that start with a number
numeric_columns <- grep("^[0-9]", names(OHC_data), value = TRUE)

# Pivot the selected columns to longer format
OHC_data_long <- OHC_data %>%
  mutate(Year = Year - 0.5) %>% 
  pivot_longer(cols = all_of(numeric_columns),  # Select numeric columns
               names_to = "depth_integration",  # Name of the label column
               values_to = "value") %>%         
  mutate(depth_integration = as.numeric(depth_integration), #factor(depth_integration, levels = unique(depth_integration)),
         Date = make_date(year = as.numeric(Year), month = 1, day = 1)) %>% 
  filter(depth_integration != 1950)


OHC_plot <- ggplot() +
  geom_path(data = OHC_data_long, aes(x = Date, y = value,
                                      #color=daily_filtered_long_drift_long_SI,
                                      group = depth_integration, color = depth_integration), alpha = 0.5, size=2) +
  geom_point(data = OHC_data_long, aes(x = Date, y = value,
                                       #color=daily_filtered_long_drift_long_SI,
                                       fill = depth_integration,
                                       group = Year, color = depth_integration), 
             color = 'white', alpha = 0.7, shape=21, size = 8)+
  xlab("Time") +
  #scale_color_continuous(palette = RColorBrewer::brewer.pal(8, "PuOr"))+
  ylab("Heat energy") +
  ggtitle("Heat over time and depth")+
  #scale_fill_brewer(palette = "Spectral")+
  #scale_color_brewer(palette = "Spectral")+
  scale_fill_gradientn(colors = brewer.pal(11, "Spectral")) +
  scale_color_gradientn(colors = brewer.pal(11, "Spectral")) +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank())
OHC_plot

animate(OHC_plot + transition_reveal(OHC_data_long$depth_integration) + 
          ease_aes('linear') + 
          shadow_wake(wake_length = 0.1, alpha = FALSE) +
          ggtitle("Year: {frame_along}"),
        start_pause = 15, end_pause = 15,
        duration=30,fps = 24, width = 1920, height = 1080, res= 72, bg = 'transparent')
anim_save(here("Figures",paste("OHC_data_by_depth_transreveal.gif", sep="")),
          bg = "transparent")

library(transformr)
animate(OHC_plot + transition_states(OHC_data_long$depth_integration, transition_length = 10) +
          ease_aes('linear') +
          shadow_wake(wake_length = 0.1, alpha = FALSE) +
          ggtitle("Year: {frame_along}"),
        start_pause = 15, end_pause = 15,
        duration = 30, fps = 24, width = 1920, height = 1080, bg = 'transparent')
anim_save(here("Figures", paste("OHC_data_by_depth_transstates.gif", sep = "")), bg = "transparent")

OHC_byyear_plot <- ggplot() +
  geom_path(data = OHC_data_long, aes(x = Date, y = value,
                                      #color=daily_filtered_long_drift_long_SI,
                                      group = depth_integration, color = depth_integration), alpha = 0.5,
            size= 4) +
  geom_point(data = OHC_data_long, aes(x = Date, y = value,
                                       #color=daily_filtered_long_drift_long_SI,
                                       fill = depth_integration,
                                       group = depth_integration, color = depth_integration), 
             color = 'white', alpha = 0.7, shape=21, size = 8)+
  xlab("Time") +
  #scale_color_continuous(palette = RColorBrewer::brewer.pal(8, "PuOr"))+
  ylab("Heat energy") +
  ggtitle("Heat over time and depth")+
  #scale_fill_brewer(palette = "Spectral")+
  #scale_color_brewer(palette = "Spectral")+
  scale_fill_gradientn(colors = brewer.pal(11, "Spectral")) +
  scale_color_gradientn(colors = brewer.pal(11, "Spectral")) +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank())
OHC_byyear_plot

animate(OHC_byyear_plot + transition_reveal(OHC_data_long$Date) + ease_aes('linear') + 
          shadow_wake(wake_length = 0.1, alpha = FALSE) +
          ggtitle("Year: {frame_along}"),
        duration=10,fps = 24, width = 1920, height = 1080, bg = 'transparent')
anim_save(here("Figures",paste("OHC_data_byyear.gif", sep="")),
          bg = "transparent")



animate(tracks + transition_reveal(Argo_data_top$R_Time) + ease_aes('linear') + 
          shadow_wake(wake_length = 0.1, alpha = FALSE) +
          ggtitle("Date: {frame_along}"),
        duration=30,fps = 24, width = 1920, height = 1080, bg = 'transparent')
anim_save(here("Figures",paste("Daily_ALL_Tracks3.gif", sep="")),
          bg = "transparent")
