install.packages('move')

library(moveVis)
library(move)
library(raster)

data("whitestork_data", package = "moveVis")

colnames(df)
range(df$timestamp)

#>  [1] "2018-07-30 02:00:23 UTC" "2018-09-29 20:00:41 UTC" 
#>  
df$name <- sapply(df$`individual-local-identifier`, function(x)
  strsplit(x, " /")[[1]][1], USE.NAMES = F)
df$name <- gsub("-", " ", gsub("[+]", "", gsub(" ", "", df$name)))
unique(df$name)


m <- move(x = df[["location-long"]], y = df[["location-lat"]],
          time = df[["timestamp"]], animal = df[["name"]],
          proj = "+proj=longlat +datum=WGS84 +no_defs",
          removeDuplicatedTimestamps = TRUE)


lag <- unlist(timeLag(m, unit = "mins"))
median(lag)
#> [1] 5
sd(lag)
#> [1] 36.86127
#> 
#> interpolation, named align_move():
m <- align_move(m, res = 180, digit = 0, unit = "mins")
length(unique(timestamps(m)))

get_maptypes()

frames <- frames_spatial(m, trace_show = TRUE, equidistant = FALSE,
                         map_service = "osm", map_type = "terrain_bg")
frames_spatial()

frames[[1]]

animate_frames(frames, width = 800, height = 800,
               out_file = "S2_white_storks_osm.mov", end_pause = 1)


###############################################################



library(dplyr)
library(ggplot2)
library(lubridate)
library(foreach)
library(tidyverse)
library(sf)

bdat <- read.csv(file.path("data", "all_geos_merged2.csv"))

# filter data to remove NA values
bdat <- bdat %>%
  select(c(lon, lat, arr, dep, tag, species)) %>%
  filter(species == "rekn")

bdat <- bdat[complete.cases(bdat), ]

# calculate time differences
bdat_sp <- bdat %>%
  mutate(arrive = ymd_hm(arr),
         depart = ymd_hm(dep)) %>%
  mutate(year = year(arrive))

bdat_sf <- st_as_sf(bdat_sp, coords = c("lon","lat"))

ggplot()

mapview::mapview(bdat_sp, map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"), color = "grey40")

library(leaflet)

basemap <- leaflet() %>%
  # add different provider tiles
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap"
  ) %>%
  addProviderTiles(
    "Stamen.Toner",
    group = "Stamen.Toner"
  ) %>%
  addProviderTiles(
    "Stamen.Terrain",
    group = "Stamen.Terrain"
  ) %>%
  addProviderTiles(
    "Esri.WorldStreetMap",
    group = "Esri.WorldStreetMap"
  ) %>%
  addProviderTiles(
    "Wikimedia",
    group = "Wikimedia"
  ) %>%
  addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "OpenStreetMap", "Stamen.Toner",
      "Stamen.Terrain", "Esri.WorldStreetMap",
      "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
    ),
    # position it on the topleft
    position = "topleft"
  )


pal <- colorFactor(
  palette = "viridis",
  domain = unique(bdat_sp$tag))

tags <- unique(bdat_sp$tag)


bdat_sp1 <- bdat_sp  %>%
  filter(tag == tags[3])

leaflet() %>%
  # add a dark basemap
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(data = bdat_sp1, lng = bdat_sp1$lon, lat = bdat_sp1$lat, color = ~pal(tag)) %>%
  addPolylines(data = bdat_sp1, lng = bdat_sp1$lon, lat = bdat_sp1$lat, group = bdat_sp1$tag)

fillColor = ~pal(tag))

#bdat_sp 


  bdat_sp













# maptools 


library(maptools)
data(wrld_simpl)
wrld_simpl <- nowrapRecenter(wrld_simpl, avoidGEOS = TRUE)
plot(wrld_simpl)








#Projections
WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
NAEA <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
NAEA <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Transform Canada into WGS84

Canada <- spTransform(Canada,CRS = WGS84)

# Set the projection for shapefiles
crs(UnitedStates) <- WGS84
crs(Mexico) <- WGS84
crs(ROSEdist100LR) <- WGS84
crs(AK) <- WGS84
