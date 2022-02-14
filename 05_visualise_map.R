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
library(leaflet)

bdat <- read.csv(file.path("data", "all_geos_merged2.csv"))

# filter data to remove NA values
bdat <- bdat %>%
  select(c(lon, lat, arr, dep, dur, tag, species)) %>%
  filter(species == "rekn")

bdat <- bdat[complete.cases(bdat), ]

# calculate time differences
bdat_sp <- bdat %>%
  mutate(arrive = ymd_hm(arr),
         depart = ymd_hm(dep)) %>%
  mutate(year = year(arrive)) %>%
  mutate(arr_month = month(arrive),
         dep_month = month(depart))

#month_col = sort(unique(bdat_sp$arr_month))
palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)

#bdat_sf <- st_as_sf(bdat_sp, coords = c("lon","lat"))

#ggplot()

#mapview::mapview(bdat_sp, map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"), color = "grey40")

library(leaflet)

# basemap <- leaflet() %>%
#   # add different provider tiles
#   addProviderTiles(
#     "OpenStreetMap",
#     # give the layer a name
#     group = "OpenStreetMap"
#   ) %>%
#   addProviderTiles(
#     "Stamen.Toner",
#     group = "Stamen.Toner"
#   ) %>%
#   addProviderTiles(
#     "Stamen.Terrain",
#     group = "Stamen.Terrain"
#   ) %>%
#   addProviderTiles(
#     "Esri.WorldStreetMap",
#     group = "Esri.WorldStreetMap"
#   ) %>%
#   addProviderTiles(
#     "Wikimedia",
#     group = "Wikimedia"
#   ) %>%
#   addProviderTiles(
#     "CartoDB.Positron",
#     group = "CartoDB.Positron"
#   ) %>%
#   addProviderTiles(
#     "Esri.WorldImagery",
#     group = "Esri.WorldImagery"
#   ) %>%
#   # add a layers control
#   addLayersControl(
#     baseGroups = c(
#       "OpenStreetMap", "Stamen.Toner",
#       "Stamen.Terrain", "Esri.WorldStreetMap",
#       "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
#     ),
#     # position it on the topleft
#     position = "topleft"
#   )


pal <- colorFactor(
  palette = "viridis",
  domain = unique(bdat_sp$tag))

tags <- unique(bdat_sp$tag)


bdat_sp1 <- bdat_sp  %>%
  filter(tag == tags[5])


palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)

birdmap <- leaflet(bdat_sp1) %>%
  # add a dark basemap
  addProviderTiles("CartoDB.DarkMatter") %>%
  #addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addCircleMarkers(lng = bdat_sp1$lon, lat = bdat_sp1$lat, 
             weight = 2, color = ~palette1(bdat_sp1$arr_month), 
             #fill = TRUE,
             radius = ~dur/10,
             fillColor = ~palette1(bdat_sp1$arr_month),
             popup = ~tag) %>%
  addPolylines(data = bdat_sp1, lng = bdat_sp1$lon, lat = bdat_sp1$lat, 
               color = "white",  opacity = 0.1, stroke = TRUE) %>%
  addLegend("bottomleft", pal = palette1, values = ~bdat_sp1$arr_month,
            title = "Arrival Month",
            opacity = 1)

birdmap


# Mapping clusters of Bird locations

birdmapall <- leaflet(bdat_sp) %>%
  # add a dark basemap
  addProviderTiles("CartoDB.DarkMatter") %>%
  #addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addCircleMarkers(clusterOptions = markerClusterOptions(),
                   weight = 2, color = ~palette1(bdat_sp1$arr_month), 
                   fill = TRUE,
                   radius = ~dur/10,
                   fillColor = ~palette1(bdat_sp1$arr_month))


    
    data = bdat_sp, lng = bdat_sp$lon, lat = bdat_sp$lat)



  addPolylines(data = bdat_sp1, lng = bdat_sp1$lon, lat = bdat_sp1$lat, 
               color = "white",  opacity = 0.1, stroke = TRUE)

birdmap





























## Static maps: 
library(maptools)
library(viridis)
library(dplyr)
library(ggplot2)
library(lubridate)
library(foreach)
library(tidyverse)
library(sf)
library(leaflet)


bdat <- read.csv(file.path("data", "all_geos_merged2.csv"))

# filter data to remove NA values
bdat <- bdat %>%
  select(c(lon, lat, arr, dep, dur, tag, species)) %>%
  filter(species == "rekn")

bdat <- bdat[complete.cases(bdat), ]

# calculate time differences
bdat_sp <- bdat %>%
  mutate(arrive = ymd_hm(arr),
         depart = ymd_hm(dep)) %>%
  mutate(year = year(arrive)) %>%
  mutate(arr_month = month(arrive),
         dep_month = month(depart))


tags <- unique(bdat_sp$tag)

roof.loc = bdat_sp %>%
  filter(tag %in% tags[1:5])

  
data(wrld_simpl)
wrld <- wrld_simpl[!(wrld_simpl$NAME %in% c("Greenland","Antarctica")),]


band.aid <- data.frame(x=c(180.075, 179.85, 179.85, 180.075, 180.075, 179.85, 180.075, 180.075, 179.85, 179.85), 
                     y=c(68.8, 68.9, 65.15, 65.295, 68.8, 71.07, 71.07, 71.42, 71.42, 71.07))

band.aid$id<-c(1,1,1,1,1,2,2,2,2,2)


fill.map <- ggplot(wrld,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="grey85",color="black")+
  coord_map(orientation=c(90,0,148), xlim=c(200.00, 321.0), ylim=c(-50.00, 60.00))+
  #coord_map(projection="azequalarea", orientation=c(36,127,0), xlim=c(106.00, 217.00), ylim=c(16.00, 75.00))+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())#+
 # geom_polygon(data=band.aid,aes(x=x, y=y, group=id), fill="grey85", color="grey85")

fill.map

#my_title <- expression(paste(italic("arct"),"_5060 stationary sites (2+ days)"))

outmap <- fill.map + geom_path(data=roof.loc, show.legend=FALSE, linetype = "dotted", colour="dark grey", size=1, alpha=0.85, aes(x=lon, y=lat, group = tag))+
  geom_point(data=roof.loc, alpha=1, shape = 19, aes(x=lon, y=lat, group = tag, size=dur, colour=arr_month))+
  facet_wrap(~tag)+#scale_size(limits = c(2,300))+ scale_color_gradientn(colours = rainbow(4), limits=c(1, 12))+
  scale_size(limits = c(2,300))+ scale_color_viridis() + #colours = terrain.colors(4), limits=c(1, 12))+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(colour="month", size="days at site")+
  theme(legend.box="vertical", legend.position = c(0.1, 0.25))+
  #labs(title=my_title)+
  theme(text=element_text(family="Times New Roman", size=12))

