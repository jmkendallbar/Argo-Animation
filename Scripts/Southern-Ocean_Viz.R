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
library(marmap)

greenblues <- c("#01665e", "#c7eae5","#999999","#FCFCFC")
bluegreys <- c("steelblue4", "#C7E0FF","#999999","#FCFCFC")

world_shp <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

# Get southern ocean bathymetry
bathys_10 <- getNOAA.bathy(180, -180, -90, 90, resolution = 10,
                          antimeridian = TRUE)

Fig3F_Sleep_through_trip_map <- autoplot.bathy(bathys_10, geom = c("raster"), coast = FALSE, show.legend = TRUE) +
  scale_fill_gradientn(colours = bluegreys, values = scales::rescale(c(min(bathys_10), 0, 1, max(bathys_10)))) + #values = scales::rescale(c(min(bathys_10), 0, 0.001, max(bathys_10)))
  theme(legend.position = "top")+
  #geom_sf(aes(), data = world_shp, colour = "dark grey", fill = "dark grey") +
  #coord_sf(xlim = c(-6500, 6500), ylim = c(-6500, 6500), crs = prj, expand = T) +
  
  #geom_point(data = Daily_ALL, aes(x = Lon360, y = Lat, color = Percent_of_Trip), alpha = 1, size = 0.05, show.legend=FALSE) +
  scale_color_gradientn(colors = brewer.pal(7, "Spectral"))+
  theme_bw() +
  labs(x = "Longitude", y = "Latitude")

# Define custom column names
column_names <- c("Date", "lat", "lon", "WMO_number", "Cycle_number", "Deep")

# Read the CSV file with custom column names
Argo_data <- read.csv(here("Data", "traj_positions_animation_13Nov2023.csv"), header = FALSE, col.names = column_names)

Argo_data$R_Time <- ymd_hms(Argo_data$Date)

Argo_data_east <- Argo_data %>% 
  #filter(Deep == 1)
  filter(lon >= 0 & lon <= 90) %>% 
  filter(year(R_Time) >= 2004) %>% # & year(R_Time) <= 2023) # Filter first 5 years
  slice_sample(n=100000) # whole timeseries sample

east_tracks <- ggplot() +
  geom_path(data = Argo_data_east, aes(x = lon, y = lat,
                                      #color=daily_filtered_long_drift_long_SI,
                                      group = WMO_number), color = 'steelblue4', alpha = 0.05) +
  geom_point(data = Argo_data_east,
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
east_tracks

east_anim <- animate(east_tracks + transition_reveal(Argo_data_east$R_Time) + ease_aes('linear') + exit_fade() + 
          #shadow_wake(wake_length = 0.1, alpha = 0.1) +
          ggtitle("Date: {frame_along}"),
        duration=30,fps = 24, width = 1920, height = 1080, 
        bg = 'transparent')
anim_save(here("Figures",paste("ARGO_Tracks_ALL_E_0-90_2004-2023.gif", sep="")),
          bg = "transparent")

#### ----

# Define custom column names
column_names <- c("Date", "lat", "lon", "WMO_number", "Cycle_number", "Deep")

# Read the CSV file with custom column names
Argo_data <- read.csv(here("Data", "traj_positions_animation_13Nov2023.csv"), header = FALSE, col.names = column_names)

# Transform date-time column to R date-time format with lubridate
Argo_data$R_Time <- ymd_hms(Argo_data$Date)

# Create a directory to save the figures
dir.create(here("Figures"), showWarnings = FALSE)

# Define the longitude ranges for each iteration with a 45-degree overlap
overlap_degrees <- 45
longitude_ranges <- seq(0, 360, by = 90 - overlap_degrees)

for (i in 1:(length(longitude_ranges) - 1)) {
  # Filter data based on longitude range
  lon_min <- longitude_ranges[i]
  lon_max <- longitude_ranges[i + 1]
  
  Argo_data_filtered <- Argo_data %>%
    filter(lon >= lon_min & lon < lon_max) %>%
    filter(year(R_Time) >= 2004) %>%
    slice_sample(n = 100000)
  
  # Create ggplot
  tracks_plot <- ggplot() +
    geom_path(data = Argo_data_filtered, aes(x = lon, y = lat,
                                             group = WMO_number), color = 'steelblue4', alpha = 0.05) +
    geom_point(data = Argo_data_filtered,
               aes(x = lon, y = lat, group = WMO_number),
               fill = 'steelblue', color = 'white', alpha = 0.7, shape = 21, size = 2) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Trajectory Line Track") +
    coord_quickmap(xlim = c(0, 360), ylim = c(-90, 90)) +  # Constrain x and y axes
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank())
  
  # Create animation
  anim <- animate(tracks_plot + transition_reveal(Argo_data_filtered$R_Time) +
                    ease_aes('linear') + exit_fade() +
                    ggtitle(paste("Longitude Range: ", lon_min, "-", lon_max, ", Date: {frame_along}")),
                  duration = 30, fps = 24, width = 1920, height = 1080, bg = 'transparent')
  
  # Save animation with a unique name
  anim_save(here("Figures", paste("ARGO_Tracks_", lon_min, "-", lon_max, "_2004-2023.gif", sep = "")),
            animation = anim, bg = "transparent")
}


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


#### Borrowed code ----
# from: https://gist.github.com/jamesgrecian/55db6153d16445e88c5ff39222b7e20f

prj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"


# download bathymetry data from marmap
bathy <- marmap::getNOAA.bathy(lon1 = -180,
                               lon2 = 180,
                               lat1 = -90,
                               lat2 = -30,
                               resolution = 10)
# convert bathmetry data to raster
bat <- marmap::as.raster(bathy)
top <- marmap::as.raster(bathy)
bat[bat > 0] <- NA # not interested in values on land...
top[top < 0] <- NA # not interested in values on land...

# project bathymetry data
bat_prj <- raster::projectRaster(bat, res = 50, method = "bilinear", crs = prj)
top_prj <- raster::projectRaster(top, res = 50, method = "bilinear", crs = prj)

# convert bathymetry data to xyz tibble for plotting in ggplot2
bat_df <- bat_prj %>% raster::rasterToPoints() %>% as_tibble()
top_df <- top_prj %>% raster::rasterToPoints() %>% as_tibble()
bat_df # check format looks right
names(bat_df)[3] <- "depth" # change variable name
names(top_df)[3] <- "depth" # change variable name

# load shapefile from rnatural earth package
world_shp <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

# define cropping polygon to clip shapefile to southern ocean
CP <- sf::st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 0),
                  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% sf::st_as_sfc()

# crop shapefile to southern hemisphere and project
sf::sf_use_s2(FALSE) # switch off strange new sf warnings
world_shp <- sf::st_crop(sf::st_buffer(world_shp, 0), CP)
world_shp <- world_shp %>% sf::st_transform(prj)

# quick example plot
p1 <- ggplot() + 
  theme_bw() +
  geom_raster(aes(x = x, y = y, fill = depth), data = bat_df) +
  scale_fill_gradientn(colours = bluegreys, values = scales::rescale(c(min(bat_df), 0, 1, max(top_df)))) + #
  #geom_sf(aes(), data = world_shp, colour = "dark grey", fill = "dark grey") +
  geom_raster(aes(x = x, y = y, fill = depth), data = top_df) +
  coord_sf(xlim = c(-6500, 6500), ylim = c(-6500, 6500), crs = prj, expand = T) +
  xlab("") + ylab("")

# make a 'pretty' circular plot
# this requires making a masking shape that sits over the plot
# the hole in the middle of the shape lets you see through
# it's a bodge... 

# create a shape that is the same size as the visible hole you want
# in this case I want to see up to 7000 km from the centre of the map
# the projection has the south pole xy coordinate as 0,0 
little <- st_point(c(0,0)) %>% st_sfc(crs = prj) %>% st_buffer(dist = 7000)

# create enclosing rectangle to mask the map with
xlim <- sf::st_bbox(little)[c("xmin", "xmax")]*1.5
ylim <- sf::st_bbox(little)[c("ymin", "ymax")]*1.5
# turn into enclosing rectangle
encl_rect <- list(cbind(c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
                        c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1]))) %>%
  sf::st_polygon() %>%
  sf::st_sfc(crs = prj)

# now cookie cut out the circle from the enclosing rectangle and the earth outline
cookie <- sf::st_difference(encl_rect, little)

p2 <- ggplot() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = depth), data = bat_df) +
  geom_sf(aes(), data = world_shp, colour = "dark grey", fill = "dark grey") +
  geom_sf(aes(), data = cookie, fill = "white") +
  coord_sf(xlim = c(-6500, 6500), ylim = c(-6500, 6500), crs = prj, expand = T) +
  xlab("") + ylab("")

p1 + p2