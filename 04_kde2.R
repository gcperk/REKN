
library(lubridate)
library(adehabitatHR)
library(sp)
library(ggplot2)
library(sf)
library(stringr)
library(dplyr)
library(lubridate)
library(adehabitatHR)
library("leaflet")
library("data.table")
library("rgdal")
library("KernSmooth")
library("magrittr")
library(tidyr)


source("01_load_data.R")

# prepare datasets for running KDE. 

# - generate points for static locations based on the time lag. 
# - to be completed for rufa geolocator data sets 


#######################################################################

# Create a full daily data set for geolocator datasets (Rufa)

rekn <- read.csv(file.path(out.dir, "rekn_rufa_subpop.csv"))%>% 
  filter(!animal.id == "tex_4a3") 

rekn <- rekn %>% 
  #filter(Subpop1 %in% c( "SE_US" ,"N_SA","WGWP_SA" ,"TDF" )) %>%
  filter(!is.na(lat))

tags <- unique(rekn$animal.id)

tbl <- tibble()

for( i in 1:length(tags)) {
    #i = 56
    tt = tags[i]
    print(tt)
    trekn <- rekn %>% filter(animal.id == tt)%>%
      dplyr::filter(dur_no >0)
    
    
    for(ii in 1:nrow(trekn)){
       #ii = 1
      tline <- trekn[ii, ]
      print(tline$X)
      trep <- tline$dur_no
      ttt <- tline %>% dplyr::select(lng, lat)
      lng <- jitter(rep(ttt[1,"lng"], trep))
      lat <- jitter(rep(ttt[1,"lat"],trep))
      extra <- data.frame(lat, lng)
    
      datecol = ymd(tline$arrive)
      
       for(iii in 1:trep) {
         #iii = 2
         days_to_subtract = iii
         datess <- ymd(tline$arrive) + days_to_subtract
         datecol <- c(datecol, ymd(datess))
       }
      datecol <- datecol[-1]
      extras <- cbind(extra, datecol) 
      extras <- extras %>% mutate(arrive = as.character(datecol))
      
      tout <- bind_rows(tline, extras) %>%
        fill(animal.id,Subpop, data_type, Subpop1) %>%
        mutate(year = year(arrive)) %>%
        mutate(arr_month = month(arrive)) %>%
        dplyr::select(-datecol)
      
      tbl <- bind_rows(tbl, tout)
    
      }
  
}

tbl

write.csv(tbl, file.path(out.dir, "rekn_rufa_full_locations.csv"))


##############################################################################################

# Run kde 's 
tbl <- read.csv(file.path(out.dir, "rekn_rufa_full_locations.csv")) %>% 
  filter(!animal.id == "tex_4a3") %>%
  filter(!Subpop1 == "uncertain")


#version 1: 


  
jdatxy <- tbl[c("lng","lat")] %>% filter(!is.na(lat)) 

# run kde

tdata <- data.frame(jdatxy)

kde <- bkde2D(tdata,
              bandwidth=c(.5, .5), gridsize = c(1000,1000)) # need to adjust this
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





# version 2: 




#subpops = as.list(unique(tbl$Subpop1))
hrpc = c( 95)

#for (s in subpops) {
  
  #s = subpops[5]
  
  tdata <- tbl %>%
   # filter(Subpop1 == s) %>%
    droplevels() %>%
    dplyr::select( lng, lat) %>%
    distinct()
  
  # Create a SpatialPointsDataFrame by defining the coordinates
  coordinates(tdata) <- c("lng", "lat")
  proj4string(tdata) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  tdfgeo <- spTransform(tdata, CRS("+init=epsg:4087")) # Transform to UTM
  
  kde <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)

  saveRDS(kde, file = file.path(out.dir, paste0(s,"_lscv_model.rds")))
  
  for (p in hrpc){
    tryCatch({
      ver <- getverticeshr(kde, p)
      ver.sf <- st_as_sf(ver)
      st_write(ver.sf, file.path(out.dir, paste0( p, "_", s, "_lscv.gpkg")), delete_dsn = TRUE)
      st_write(ver.sf, file.path(out.dir, paste0( p, "_", s, "_lsvc.shp")))
      
    },
    error = function(e){
      print( paste0("unable to generate vertices for ", p, "% vertices for ", s))
    })
    
  } # end of hrpc loop
  
  
} # end of season loop





##############################################################
# Alternate kde measure 
##############################################################


kde <- bkde2D(tdata,
              #bandwidth=c(.8, .8), gridsize = c(10000,10000)) 
              #bandwidth=c(.5, .5), gridsize = c(10000,10000)) 
              bandwidth=c(.5, .5), gridsize = c(10000,10000)) 
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

#CL1 <- CL #bandwidth=c(.8, .8), gridsize = c(10000,10000))
CL2 <- CL #bandwidth=c(.5, .5), gridsize = c(10000,10000))


## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

sfgons <- st_as_sf(spgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])


spgons2 <- spgons




