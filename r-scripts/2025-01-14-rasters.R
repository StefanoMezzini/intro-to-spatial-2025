library('sf')           # for simple features (points, lines, polygons)
library('canadianmaps') # for shapefile of Canada
library('dplyr')        # for data wrangling
library('lubridate')    # for working with dates
library('purrr')        # for functional programming; map()
library('ggplot2')      # for fancy plots
library('terra')        # for rasters
source('r-scripts/default-ggplot-theme.R')

ndvi_pal <- colorRampPalette(c('darkblue', 'dodgerblue', '#744700',
                               '#d9bb94', 'darkgreen'))(100)

# import necessary shapefiles ----
kelowna_polygon <- read_sf('data/kelowna-shapefile/City_Boundary.shp')
ubco <- st_as_sf(data.frame(long = -119.39466, lat = 49.93949),
                 coords = c('long', 'lat')) %>%
  st_set_crs('EPSG:4326') %>%
  st_transform(st_crs(kelowna_polygon))
kelowna_roads <- read_sf('data/kelowna-roads/Road.shp') %>%
  filter(! road_class %in% c('Local', 'Strata'))
okanagan_lake <- read_sf('data/okanagan-lake/okanagan-lake.shp') %>%
  st_transform(crs(kelowna_polygon))

# rasters ----
# import raster stack of NDVI (satellite-derived measure of "greenness")
ndvi <- rast('data/okanagan-ndvi.tif')
plot(ndvi, col = ndvi_pal, range = c(-1, 1))
plot(ndvi[[1]], col = ndvi_pal, range = c(-1, 1))

# projections ----
st_crs(ndvi) # sf function
crs(ndvi) # terra function

# w kelowna shapefile CRS
layout(1:2)
plot(ndvi[[1]], col = ndvi_pal, range = c(-1, 1), main = 'Long-Lat projection')
project(ndvi[[1]], crs(kelowna_polygon)) %>%
  plot(col = ndvi_pal, range = c(-1, 1), main = 'UTM zone 11N projection')
layout(1)

# cropping rasters ----
kelowna_ndvi <- crop(ndvi, kelowna_polygon) # need to reproject the raster

kelowna_ndvi <- ndvi %>%
  project(crs(kelowna_polygon)) %>%
  crop(kelowna_polygon)

plot(kelowna_ndvi, col = ndvi_pal, range = c(-1, 1))

# masking rasters ----
kelowna_ndvi <- kelowna_ndvi %>%
  mask(kelowna_polygon) %>% # drop values outside of Kelowna
  mask(okanagan_lake, inverse = TRUE) # drop values in Okanagan Lake
plot(kelowna_ndvi, col = ndvi_pal, range = c(-1, 1))

# extracting values for UBCO from each raster ----
names(kelowna_ndvi)
ubco_ndvi <-
  tibble(date = gsub(pattern = 'MOD13A2_NDVI_', replacement = '',
                     x = names(kelowna_ndvi)) %>%
           as_date(format = '%Y_%j'),
         doy = yday(date),
         year = year(date),
         ndvi = extract(kelowna_ndvi, ubco, ID = FALSE) %>%
           unlist())
ubco_ndvi

# plotting the values for UBCO ---
# by date
ggplot(ubco_ndvi, aes(date, ndvi, color = ndvi)) +
  geom_point() +
  scale_color_gradientn('NDVI', limits = c(-1, 1), colours = ndvi_pal) +
  labs(x = 'Date', y = 'NDVI')

# by day of year
ggplot(ubco_ndvi, aes(doy, ndvi, color = ndvi)) +
  geom_point() +
  geom_smooth(color = 'black', method = 'gam', formula = y ~ s(x, bs = 'cc'),
              method.args = list(knots = list(doy = c(0.5, 366.5))),
              fullrange = TRUE, n = 400) +
  scale_x_continuous('Day of year', c(0, 365),
                     breaks = c(1, 100, 200, 300, 365)) +
  ylab('NDVI') +
  scale_color_gradientn('NDVI', limits = c(-1, 1), colours = ndvi_pal)

# by year
ggplot(ubco_ndvi, aes(factor(year), ndvi)) +
  geom_violin(fill = 'grey') +
  geom_jitter(height = 0, width = 0.25, alpha = 0.5) +
  labs(x = 'Year', y = 'NDVI')

# summarizing and modeling raster data ----
# summarizing data (e.g., monthly to yearly)
# not a good way of modeling: should use a spatial statistical model
yearly_means <-
  tibble(year = 2012:2014,
         raster = map(year, function(.y) {
           mean(kelowna_ndvi[[grepl(.y, names(kelowna_ndvi))]])
         }))
plot(yearly_means$raster[[1]], col = ndvi_pal, range = c(-1, 1))
plot(yearly_means$raster[[2]], col = ndvi_pal, range = c(-1, 1))
plot(yearly_means$raster[[3]], col = ndvi_pal, range = c(-1, 1))

# see https://events.ok.ubc.ca/series/fitting-models-to-data-not-data-to-models-workshop-series/

# calculate distance from features ----
# distance from ubco in kelowna
dist_ubco <- kelowna_ndvi[[1]] %>% # need to start from a raster
  distance(ubco) %>%
  mask(kelowna_polygon) %>%
  mask(okanagan_lake, inverse = TRUE)
plot(dist_ubco)

# roads in kelowna
dist_roads <- rast(kelowna_ndvi[[1]], nlyr = 1) %>%
  distance(kelowna_roads) %>%
  mask(kelowna_polygon) %>%
  mask(okanagan_lake, inverse = TRUE)
names(dist_roads) <- NULL # remove NDVI name
plot(dist_roads)
range(values(dist_roads), na.rm = TRUE)
hist(dist_roads)

# spatial clustering ----
# k-means cannot deal with NAs
dist_roads[is.na(dist_roads)] <- -9999
plot(dist_roads) # should not have any NAs
dist_roads_df <- as.data.frame(dist_roads, cell = TRUE)
head(dist_roads_df)

k <- 3
clusters <- kmeans(dist_roads_df$V1, centers = k + 1) # find clusters
dist_roads_df$cluster[dist_roads_df$cell] <- clusters$cluster # assign cluster values
head(dist_roads_df)

na_cluster_id <- unique(dist_roads_df$cluster[dist_roads_df$V1 == -9999])
na_cluster_id

#' make clusters from 1 to `k`
dist_roads_df <- dist_roads_df %>%
  mutate(center = clusters$centers[clusters$cluster],
         center = case_when(cluster < na_cluster_id ~ center,
                             cluster == na_cluster_id ~ NA_real_,
                             cluster > na_cluster_id ~ center - 1))

head(dist_roads_df)
dist_roads_clustered <- rast(dist_roads)
dist_roads_clustered[dist_roads_df$cell] <- round(dist_roads_df$center)

# clustered values in meters
# smooth spatial modeling is better than clustering
plot(dist_roads_clustered)
