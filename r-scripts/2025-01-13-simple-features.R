# questions not addressed in the scripts:
# - tips on efficient data/code management
# - plotting using R vs GIS; benefits of using R

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
plot(ubco, axes = TRUE)

# line data
kelowna_roads <- read_sf('data/kelowna-roads/Road.shp')
plot(kelowna_roads)
plot(st_geometry(kelowna_roads))

# polygons
# polygon of Kelowna from https://opendata.kelowna.ca/datasets/751a41f3ad0840ab80ba8d30090ae4eb_9/explore
kelowna_polygon <- read_sf('data/kelowna-shapefile/City_Boundary.shp')
plot(kelowna_polygon)
kelowna_polygon <- st_geometry(kelowna_polygon)
plot(kelowna_polygon)

# plot all three together 
plot(kelowna_polygon, col = 'grey90')
plot(st_geometry(kelowna_roads), add = TRUE, col = 'black')
plot(ubco, add = TRUE, col = 'red', pch = 19) # does not plot?

# check each object's projection
st_crs(kelowna_polygon)
st_crs(kelowna_roads)
st_crs(ubco)

# specify the CRS for the UBCO point
st_crs(ubco) <- 'EPSG:4326' # long lat
st_crs(ubco)

# reproject the point
ubco <- st_transform(ubco, crs = st_crs(kelowna_roads))
plot(ubco, add = TRUE, col = 'red2', pch = 19, cex = 1)

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
  plot(key.pos = 3) # legend on top

# buffer the HR polygons before union to ensure overlap
# removes broken lines inside each province/territory
# not the cleanest way of fixing the issue, but it works if the buffering
# scale is negligible, relatie to your scale of interest
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
# MOTI = ministry of transport and and infrastructure
ggplot() +
  geom_sf(data = kelowna_polygon, fill = 'grey95', color = 'black') +
  geom_sf(aes(color = road_class, linewidth = road_class), kelowna_roads) +
  geom_sf(data = ubco, color = 'black', size = 2, alpha = 0.75) +
  scale_color_manual(values = c(Arterial = '#EE6677',
                                Local = 'black',
                                MajorCollector = '#228833',
                                MinorCollector = '#CCBB44',
                                MOTI = '#66CCEE',
                                Strata = '#AA3377'),
                     na.value = 'darkorange', name = 'Road type') +
  scale_linewidth_manual(values = c(Arterial = 0.75,
                                    Local = 0.25,
                                    MajorCollector = 0.75,
                                    MinorCollector = 0.5,
                                    MOTI = 1,
                                    Strata = 0.25),
                         na.value = 0.25, name = 'Road type')

# can work with sf objects just like any tibble/dataframe
major_ways <- filter(kelowna_roads,
                     road_class != 'Local',
                     road_class != 'Strata')

ggplot() +
  geom_sf(data = kelowna_polygon, fill = 'grey95', color = 'black') +
  geom_sf(aes(color = road_class, linewidth = road_class), major_ways) +
  geom_sf(data = ubco, color = 'black', size = 2, alpha = 0.75) +
  scale_color_manual(values = c(Arterial = '#EE6677',
                                MajorCollector = '#228833',
                                MinorCollector = '#CCBB44',
                                MOTI = '#66CCEE',
                                Strata = '#AA3377'), name = 'Road type') +
  scale_linewidth_manual(values = c(Arterial = 0.75,
                                    MajorCollector = 0.75,
                                    MinorCollector = 0.5,
                                    MOTI = 1,
                                    Strata = 0.25), name = 'Road type')
