
###############################################################
# Data Viz/ for Red Knot migration geolocator data

# written by Gen Perkins
# last edit : 2022-07-10


library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(lubridate)
library(foreach)
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)
library(dplyr)

bdat <- read.csv(file.path("data", "location_estimates_final.csv"))

# filter data to remove NA values
bdat <- bdat %>%
  dplyr::select(c(id, location.long, location.lat, arrive.date, depart.date, animal.id, deploy_id))


bdat <- bdat[complete.cases(bdat), ] # note this removes the breed locations


# calculate time differences
bdat_sp <- bdat %>%
  mutate(arrive = ymd(arrive.date),
         depart = ymd(depart.date)) %>%
  mutate(year = year(arrive)) %>%
  mutate(arr_month = month(arrive),
         dep_month = month(depart)) %>%
  mutate(dur = depart -arrive) %>%
  mutate(lat = as.numeric(location.lat), 
         lng = location.long) %>%
  mutate(dur_no = as.numeric(dur),
         lng_new = lng + 360)

#month_col = sort(unique(bdat_sp$arr_month))
palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)

pal <- colorFactor(
  palette = "viridis",
  domain = unique(bdat_sp$animal.id))

tags <- unique(bdat_sp$animal.id)


bdat_sp1 <- bdat_sp  %>%
  filter(animal.id == tags[63])
bdat_sp1 

palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)


birdmap <- leaflet(bdat_sp1) %>%
  # add a dark basemap
  addProviderTiles("CartoDB.DarkMatter") %>%
  #addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addCircleMarkers(lng = bdat_sp1$lng, lat = bdat_sp1$lat, 
             weight = 2, color = ~palette1(bdat_sp1$arr_month), 
             #fill = TRUE,
             radius = ~dur_no/2,
             fillColor = ~palette1(bdat_sp1$arr_month),
             popup = ~animal.id) %>%
  addPolylines(data = bdat_sp1, lng = bdat_sp1$lng, lat = bdat_sp1$lat, 
               color = "white",  opacity = 0.1, stroke = TRUE) %>%
  addLegend("bottomleft", pal = palette1, values = ~bdat_sp1$arr_month,
            title = "Arrival Month",
            opacity = 1)

birdmap


# Mapping clusters of Bird locations

birdmapall <- leaflet(bdat_sp) %>%
  # add a dark basemap
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(lng = bdat_sp$lng, lat = bdat_sp$lat, 
                   weight = 2, color = ~palette1(bdat_sp$arr_month), 
                   #fill = TRUE,
                   radius = ~dur_no/8,
                   fillColor = ~palette1(bdat_sp$arr_month),
                   popup = ~animal.id) %>%
  addPolylines(data = bdat_sp, lng = bdat_sp$lng, lat = bdat_sp$lat, 
               color = "grey",  opacity = 0.1, stroke = TRUE) %>%
  addLegend("bottomleft", pal = palette1, values = ~bdat_sp$arr_month,
            title = "Arrival Month",
            opacity = 1)

birdmapall
 #   data = bdat_sp, lng = bdat_sp$lon, lat = bdat_sp$lat)



#  addPolylines(data = bdat_sp1, lng = bdat_sp1$lon, lat = bdat_sp1$lat, 
#               color = "white",  opacity = 0.1, stroke = TRUE)

birdmap




library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library("magrittr")

## LOAD DATA
## Also, clean up variable names, and convert dates
inurl <- "https://data.cityofchicago.org/api/views/22s8-eq8h/rows.csv?accessType=DOWNLOAD"
dat <- data.table::fread(inurl) %>% 
  setnames(., tolower(colnames(.))) %>% 
  setnames(., gsub(" ", "_", colnames(.))) %>% 
  .[!is.na(longitude)] %>% 
  .[ , date := as.IDate(date, "%m/%d/%Y")] %>% 
  .[]    
dat.df <- as.data.frame(dat)
## MAKE CONTOUR LINES
## Note, bandwidth choice is based on MASS::bandwidth.nrd()
kde <- bkde2D(dat[ , list(longitude, latitude)],
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])
































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



## ANIMATION 

#install.packages('move')

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


