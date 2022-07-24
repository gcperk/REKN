
library(lubridate)
library(adehabitatHR)
library(sp)
library(ggplot2)
library(sf)
library(stringr)
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
  mutate(lat = location.lat, 
         lng = location.long) %>%
  mutate(dur_no = as.numeric(dur)) %>%
  mutate(y = as.numeric(location.lat), 
         x = location.long)


# create a new value for each of the days?

fnames = unique(bdat_sp$animal.id)


tdata <- bdat_sp  %>%
 # filter(animal.id == fnames[3]) %>%
  droplevels() %>%
  dplyr::select(x,y) %>%
  distinct()

tdata  <- tdata[complete.cases(tdata ), ]

# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(tdata) <- c("x", "y")
proj4string(tdata) <- CRS("+init=epsg:3005")
tdfgeo <- tdata

##longitude [-180, 360] or lattitude [-90, 90] will be stopped.

kde  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"))
ver95 <- getverticeshr( kde, 95)

#  ver95.sf <- st_as_sf( ver95 )
saveRDS(kde, file = file.path(out_path, paste0(fname, "_kde", p, "_", s, "_href_model.rds")))




##############################################################################################

library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library("magrittr")

bdat_spxy <- bdat_sp %>% select(c(x,y))
bdat_spxy <-bdat_spxy[complete.cases(bdat_spxy), ] 


kde <- bkde2D(bdat_spxy[1:10,],
              bandwidth=c(.0045, .0068), gridsize = c(10000,10000)) # need to adjust this
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




############################################################################################


# run KDE for Johnsons dataset 

library(lubridate)
library(adehabitatHR)
library(sp)
library(ggplot2)
library(sf)
library(stringr)
library(dplyr)
library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library("magrittr")

jdat <- read.csv(file.path("data","location_estimates", "daily_positions_johnson.csv"))
                 
jdatxy <- jdat[,c("Median.long","Median.lat")]

kde <- bkde2D(jdatxy,
              bandwidth=c(.0045, .0068), gridsize = c(10000,10000)) # need to adjust this
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
                 
               
######


kde2 <- bkde2D(jdatxy,
              bandwidth=c(.045, .068), gridsize = c(1000,1000)) # need to adjust this
CL2 <- contourLines(kde2$x1 , kde2$x2 , kde2$fhat)       

## EXTRACT CONTOUR LINE LEVELS
LEVS2 <- as.factor(sapply(CL2, `[[`, "level"))
NLEV2 <- length(levels(LEVS2))

## CONVERT CONTOUR LINES TO POLYGONS
pgons2 <- lapply(1:length(CL2), function(i)
  Polygons(list(Polygon(cbind(CL2[[i]]$x, CL2[[i]]$y))), ID=i))
spgons2 = SpatialPolygons(pgons2)

## Leaflet map with polygons
leaflet(spgons2) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV2, NULL)[LEVS2])               

##############



kde3 <- bkde2D(jdatxy,
               bandwidth=c(.45, .68), gridsize = c(1000,1000)) # need to adjust this
CL3 <- contourLines(kde3$x1 , kde3$x2 , kde3$fhat)       

## EXTRACT CONTOUR LINE LEVELS
LEVS3 <- as.factor(sapply(CL3, `[[`, "level"))
NLEV3 <- length(levels(LEVS3))

## CONVERT CONTOUR LINES TO POLYGONS
pgons3 <- lapply(1:length(CL3), function(i)
  Polygons(list(Polygon(cbind(CL3[[i]]$x, CL3[[i]]$y))), ID=i))
spgons3 = SpatialPolygons(pgons3)

## Leaflet map with polygons
leaflet(spgons3) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV3, NULL)[LEVS3])               


# filter data to remove NA values
jdat <- jdat %>% dplyr::select(c(Date, Median.long, Median.lat, Birdid))



######################################


kde4 <- bkde2D(jdatxy,
               bandwidth=c(.5, .5), gridsize = c(1000,1000)) # need to adjust this
CL4 <- contourLines(kde4$x1 , kde4$x2 , kde4$fhat)       

## EXTRACT CONTOUR LINE LEVELS
LEVS4 <- as.factor(sapply(CL4, `[[`, "level"))
NLEV4 <- length(levels(LEVS4))

## CONVERT CONTOUR LINES TO POLYGONS
pgons4 <- lapply(1:length(CL4), function(i)
  Polygons(list(Polygon(cbind(CL4[[i]]$x, CL4[[i]]$y))), ID=i))
spgons4 = SpatialPolygons(pgons4)

## Leaflet map with polygons
leaflet(spgons4) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV4, NULL)[LEVS4])               



###################################################


kde5 <- bkde2D(jdatxy,
               bandwidth=c(.05, .05), gridsize = c(10000,10000)) # need to adjust this
CL5 <- contourLines(kde5$x1 , kde5$x2 , kde5$fhat)       

## EXTRACT CONTOUR LINE LEVELS
LEVS5 <- as.factor(sapply(CL5, `[[`, "level"))
NLEV5 <- length(levels(LEVS5))

## CONVERT CONTOUR LINES TO POLYGONS
pgons5 <- lapply(1:length(CL5), function(i)
  Polygons(list(Polygon(cbind(CL5[[i]]$x, CL5[[i]]$y))), ID=i))
spgons5 = SpatialPolygons(pgons5)

## Leaflet map with polygons
leaflet(spgons5) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV5, NULL)[LEVS4])               


# finer grain size = less generalise i.e focus on the points and not areas. 10000 is too fine. 



## Leaflet map with polygons
leaflet(spgons3) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV3, NULL)[LEVS3])     

leaflet() %>%
  addTiles %>%
  addPolygons(spgons3)


Refs

https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package




















