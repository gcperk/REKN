#########################

# subpopulations comparisons

library(leaflet)
library(RColorBrewer)
library(lubridate)
library(sp)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(foreach)
library(tidyverse)
library(viridis)
library(dplyr)
library(mapview)
library("rnaturalearth")
library("rnaturalearthdata")
#library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps



source("01_load_data.R")

# define folder structire
data.dir <- file.path ("data", "location_estimates")
out.dir <- file.path("outputs")

list.files(out.dir)

rekn <- readRDS(file.path(out.dir, "BNP_rekn_summary.rds"))
#rose_extra <- rekn %>% filter(animal.id == "tex_4a3")%>%
#  dplyr::select(animal.id, year,  data_type) 

#rekn <- rekn %>% filter(!animal.id == "tex_4a3") 

rekngps <- readRDS(file.path(out.dir, "newstead_rekn_gps.RDS" ))




rekn <- rekn %>% dplyr::select(animal.id, Subpop,lng, lat, arrive, depart, year,arr_month,dep_month, dur_no, data_type)
rekngps <- rekngps %>% dplyr::select(animal.id, Subpop,lng, lat, arrive, depart, year,arr_month,dep_month, dur_no,data_type)

ru <- bind_rows(rekn, rekngps)

ru  <- ru  %>% 
  mutate(Subpop1 = case_when(
    Subpop == "SE Carribean North America" ~ "SE_US",
    Subpop == "NorthCoast SAM" ~ "N_SA",
    Subpop == "South East Mainland Nth AM" ~ "SE_US",
    Subpop == "West Gulf" ~ "WGWP_SA",
    Subpop == "Roselarii" ~ "Roselarii" ,
    Subpop == "TDF" ~ "TDF" ,
    Subpop == "Uncertain" ~ "Uncertain" 
  ))


# summary 
rpop <- ru %>%
  dplyr::select(animal.id, Subpop, data_type) %>%
  group_by(Subpop, data_type)%>%
  summarise(count = length(unique(animal.id)))


write.csv(ru, file.path(out.dir,"rekn_rufa_subpop.csv"))

######################################################################
##Basic plotting 

ru <- ru[complete.cases(ru), ]
rf_sf <- st_as_sf(ru, coords = c("lng","lat"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

# tm_shape(world) +
#   tm_polygons() +
#   tm_shape(urb_1970_2030) +
#   tm_symbols(col = "black", border.col = "white", size = "population_millions") +
#   tm_facets(by = "year", nrow = 2, free.coords = FALSE)
# 

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")


global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 2, colour = "blue") +
  facet_wrap(~Subpop1)+
 # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-170.15, -30), ylim = c(-60, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global 
ggsave(file.path(out.dir, "rufa_map.jpg"))
ggsave(file.path(out.dir, "rufa_map_facet.jpg"))



##########################################################################

# monthly maps all subspecies 

rekn <- read.csv(file.path(out.dir, "rekn_rufa_full_locations.csv"))

rekn <- rekn %>% 
  filter(Subpop1 %in% c( "SE_US" ,"N_SA","WGWP_SA" ,"TDF" )) %>%
  filter(!is.na(lat)) %>%
  filter(arr_month %in% c(4,5,6,7,8,9,10,11))


rf_sf <- st_as_sf(rekn, coords = c("lng","lat"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")


global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf , aes(color = Subpop1), alpha = 0.5, size = 1) +
  facet_wrap(~arr_month, nrow = 2)+
  scale_color_viridis_d() +
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-125.15, -35), ylim = c(-60, 75), expand = FALSE)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global 


ggsave(file.path(out.dir, "rufa_subpop_month_facet.jpg"))


############# Version 2: 

may <- rekn %>% filter(arr_month == 5)
june <- rekn %>% filter(arr_month == 6)
july <- rekn %>% filter(arr_month == 7)
august  <- rekn %>% filter(arr_month == 8)
september <- rekn %>% filter(arr_month == 9)


moi <- september

rf_sf <- st_as_sf(moi, coords = c("lng","lat"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")


global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, aes(color = Subpop1)) +
  facet_wrap(~Subpop1, nrow = 1)+
  #facet_grid(Subpop1~arr_month)+
  scale_color_viridis_d() +
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-115.15, -35), ylim = c(-12, 60), expand = FALSE)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
       axis.text.y=element_blank(),
         legend.position="none")+
  ggtitle("September")

global 


ggsave(file.path(out.dir, "rufa_subpop_september_facet.jpg"))









########################################################################
# leafleTT maps

# sub population Duration figures 

rekn <- read.csv(file.path(out.dir, "rekn_rufa_subpop.csv"))

rekn <- rekn %>% 
  filter(Subpop1 %in% c( "SE_US" ,"N_SA","WGWP_SA" ,"TDF" )) %>%
  filter(!is.na(lat))

sp <- unique(rekn$Subpop1)

spp <- sp[4]

spp

bdat_sp <- ru %>% 
  filter(Subpop1 == spp)


#month_col = sort(unique(bdat_sp$arr_month))
palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)

pal <- colorFactor(
  palette = "viridis",
  domain = unique(bdat_sp$animal.id))

tags <- unique(bdat_sp$animal.id)

birdmapall <- leaflet(bdat_sp) %>%
  # add a dark basemap
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = bdat_sp$lng, lat = bdat_sp$lat, 
                   weight = 3, color = ~palette1(bdat_sp$arr_month), 
                   fill = TRUE,
                   label = ~arr_month,
                   radius = ~dur_no/10,
                   fillColor = ~palette1(bdat_sp$arr_month)) %>%
  #popup = ~animal.id) %>%
  addPolylines(data = bdat_sp, lng = bdat_sp$lng, lat = bdat_sp$lat, 
               color = "grey",  opacity = 0.1, stroke = TRUE) %>%
  addLegend("bottomleft", pal = palette1, values = ~bdat_sp$arr_month,
            title = "Arrival Month",
            opacity = 1)

birdmapall
































# Unusual movement patterns 

# tex_7ml and tex+8pn 

bdat_sp <- ru %>% 
  filter(animal.id %in% c( "tex_7ml", "tex_8pn", "tex_6j3"))


#month_col = sort(unique(bdat_sp$arr_month))
palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)

pal <- colorFactor(
  palette = "viridis",
  domain = unique(bdat_sp$animal.id))

tags <- unique(bdat_sp$animal.id)

bdat_sp <- bdat_sp %>% dplyr::filter(animal.id == "tex_6j3")

birdmapall <- leaflet(bdat_sp) %>%
  # add a dark basemap
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = bdat_sp$lng, lat = bdat_sp$lat, 
                   weight = 3, color = ~palette1(bdat_sp$arr_month), 
                   fill = TRUE,
                   label = ~arrive,
                   radius = ~dur_no/6,
                   fillColor = ~palette1(bdat_sp$arr_month)) %>%
  #popup = ~animal.id) %>%
  addPolylines(data = bdat_sp, lng = bdat_sp$lng, lat = bdat_sp$lat, 
               color = "grey",  opacity = 0.2, stroke = TRUE) %>%
  addLegend("bottomleft", pal = palette1, values = ~bdat_sp$arr_month,
            title = "Arrival Month",
            opacity = 1)

birdmapall



















# 
# # using TM map 
# 
# tm_shape(Americas, bbox = bbox_new)+
#   tm_fill(col = "grey", alpha = 0.8)+
#   tm_shape(rf_sf)+
#   tm_symbols (col = "black") +
#   tm_facets(by = "Subpop1", nrow = 2, free.coords = FALSE)#+ 
#   #tm_compass(type = "8star", position = c("left", "top")) #+
#   #tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
# 
# 


###############################################################################
# Annimated Leaflet mapping 

palette1 <- colorFactor(palette = 'viridis', ru$Subpop, reverse = TRUE)
                        
birdmapall <- leaflet(ru) %>%
  # add a dark basemap
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ru$lng, lat = ru$lat, 
                   weight = 2, color = ~palette1(ru$Subpop), 
                   radius = 4,
                   #radius = ~dur_no/10,
                   fillColor = ~palette1(ru$Subpop),
                   popup = ~animal.id) %>%
 # addPolylines(data = ru, lng = ru$lng, lat = ru$lat, 
#               color = "grey",  opacity = 0.1, stroke = TRUE) %>%
  addLegend("bottomleft", pal = palette1, values = ~ru$Subpop,
            title = "Sub Population",
            opacity = 1)

birdmapall
#   data = bdat_sp, lng = bdat_sp$lon, lat = bdat_sp$lat)

mapshot(birdmapall, file.path(out.dir, "subpop_map.png"))


#month_col = sort(unique(bdat_sp$arr_month))
pop <- as.factor(unique(ru$Subpop))
palette1 <- colorFactor(palette = 'viridis', ru$Subpop, reverse = TRUE



birdmap <- leaflet(popf ) %>%
  # add a dark basemap
  addProviderTiles("CartoDB.DarkMatter") %>%
  #addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addCircleMarkers(lng = popf$lng, lat = popf$lat, 
                   weight = 2, color = ~palette1(ru$Subpop), 
                   #fill = TRUE,
                   radius = ~dur_no/2,
                   fillColor = ~palette1(ru$Subpop),
                   popup = ~animal.id) #%>%
#  addPolylines(data = popf, lng = popf$lng, lat = popf$lat, 
#               color = "white",  opacity = 0.1, stroke = TRUE)# %>%
# addLegend("bottomleft", pal = palette1, values = ~popf$Subpop,
#            title = "Arrival Month",
#            opacity = 1)

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



