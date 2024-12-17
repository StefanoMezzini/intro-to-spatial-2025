library('sf')           # for simple features (points, lines, polygons)
library('canadianmaps') # for shapefile of Canada
library('dplyr')        # for data wrangling
library('ggplot2')      # for fancy plots
source('r-scripts/default-ggplot-theme.R')

# import data and plot it with base R ----
# point data
ubco <- st_as_sf(data.frame(long = -119.39466, lat = 49.93949),
                 coords = c('long', 'lat'))
plot(ubco)

# line data
kelowna_roads <- read_sf('data/kelowna-roads/Road.shp')
plot(kelowna_roads)
plot(st_geometry(kelowna_roads))

# polygons
# polygon of Kelowna from https://opendata.kelowna.ca/datasets/751a41f3ad0840ab80ba8d30090ae4eb_9/explore
kelowna_polygon <- read_sf('data/kelowna-shapefile/City_Boundary.shp')
plot(kelowna_polygon)
plot(st_geometry(kelowna_polygon))

# plot all three together 
plot(st_geometry(kelowna_polygon), col = 'grey80')
plot(st_geometry(kelowna_roads), add = TRUE, col = 'black')
plot(ubco, add = TRUE, col = c('red'), pch = 19)

# check each object's projection
st_crs(kelowna_polygon)
st_crs(kelowna_roads)
st_crs(ubco)

# specify the CRS for the UBCO point
st_crs(ubco) <- 'EPSG:4326' # long lat
st_crs(ubco)

# reproject the point
ubco <- st_transform(ubco, crs = st_crs(kelowna_roads))
plot(ubco, add = TRUE, col = c('black'), pch = 19, cex = 2)
plot(ubco, add = TRUE, col = c('red'), pch = 19)

# polygon of Canada
bc <- filter(PROV, PT == 'BC') %>%
  st_geometry() %>%
  st_transform('EPSG:3005')
plot(bc)

plot(kelowna_polygon, add = TRUE, col = 'red3') # has a different CRS
plot(st_transform(kelowna_polygon, crs = 'EPSG:3005'),
     add = TRUE, col = 'red3')

# summarizing data ----
# summarizing each province's health regions back into the total provincial boundary
plot(st_geometry(HR))

# make unions of each health region within each province
HR %>%
  group_by(PRNAME) %>%
  summarise(g = st_geometry(geometry) %>%
              st_make_valid() %>%
              st_union()) %>%
  plot()

# buffer the HR polygons before union to ensure overlap
HR %>%
  group_by(PRNAME) %>%
  summarise(g = st_geometry(geometry) %>%
              st_make_valid() %>%
              st_buffer(1e3) %>% # buffer by 1 km to avoid lines
              st_union()) %>%
  st_buffer(-1e3) %>% # reverse the buffering for an approximate fix
  plot()

# what happens if we buffer too much and don't fix it
HR %>%
  group_by(PRNAME) %>%
  summarise(g = st_geometry(geometry) %>%
              st_make_valid() %>%
              st_buffer(20e3) %>% # buffer by 20 km
              st_union()) %>%
  plot()

# plotting simple features in ggplot ----
ggplot() +
  geom_sf(data = kelowna_polygon) +
  geom_sf(data = kelowna_roads, color = 'black', alpha = 0.8) +
  geom_sf(data = ubco, color = 'red', size = 2)
