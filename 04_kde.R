
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


#######################################################################

# kernal density estimates Rufa 

rekn <- read.csv(file.path(out.dir, "rekn_rufa_subpop.csv"))

rekn <- rekn %>% 
  filter(Subpop1 %in% c( "SE_US" ,"N_SA","WGWP_SA" ,"TDF" ))

tags <- unique(rekn$animal.id)

for( i in 1:length(tags)) {
    i = 2
    tt = tags[i]
    
    trekn <- rekn %>% filter(animal.id == tt)%>%
      dplyr::filter(dur_no >0)
    
    trekn_out <- trekn
    
    for(ii in 1:nrow(trekn)){
     ii = 4
      tline <- trekn[ii, ]
      print(tline$X)
      trep <- tline$dur_no
      ttt <- tline %>% dplyr::select(lng, lat)
      lng <- jitter(rep(ttt[1,"lng"], trep))
      lat <- jitter(rep(ttt[1,"lat"], trep))
      extra <- data.frame(lat, lng)
    
      datecol = ymd(tline$arrive)
      
       for(iii in 1:length(trep)) {
         #iii = 2
         days_to_subtract = iii
         datess <- ymd(tline$arrive) + days_to_subtract
         datecol <- c(datecol, ymd(datess))
       }
      
      extras <- cbind(extra, datecol) 
      extras <- extras %>% mutate(arrive = as.character(datecol))
      
      tout <- bind_rows(tline, extras) %>%
        fill(animal.id,Subpop, data_type, Subpop1) %>%
        mutate(year = year(arrive)) %>%
        mutate(arr_month = month(arrive)) %>%
        dplyr::select(-datecol, -dep_month, -dur_no, -depart)
      
      trekn_out <- bind_rows(trekn_out, tout)
    
      }
  
}


ttt <- trekn %>% dplyr::select(lng, lat, dur_no)

lngj <- jitter(rep(ttt[1,"lng"], 10))
latj <- jitter(rep(ttt[1,"lat"], 10))


xx <- cbind(lngj,latj)
write.csv(xx, file.path(out.dir, "rekn_test.csv"))



x=jitter(x), y=jitter(y)





##############################################################################################

bdat_sp <- trekn


bdat_spxy <- bdat_sp %>% dplyr::select(c(lat,lng))
bdat_spxy <-bdat_spxy[complete.cases(bdat_spxy), ] 


kde <- bkde2D(bdat_spxy,
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

## Roselarri

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
              bandwidth=c(.5, .5), gridsize = c(1000,1000)) # need to adjust this
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)       
                


#t1: bandwidth=c(.0045, .0068), gridsize = c(10000,10000)) # too spotty with locations (needs to be better generalised)
#t2: bandwidth=c(.045, .068), gridsize = c(10000,10000)) # removed the odd spotts by is not general enough (still points )
#t3: bandwidth=c(.45, .68), gridsize = c(10000,10000)) # more generalised
#t4: bandwidth=c(.45, .68), gridsize = c(1000,1000)) # equal to previous
 
#t5: bandwidth=c(.025, .038), gridsize = c(1000,1000)) # more spotty 
#t6: bandwidth=c(.25, .38), gridsize = c(1000,1000)) # wiggly general lines - ok 
#t7:bandwidth=c(.3, .3), gridsize = c(1000,1000))# wiggly general lines similar to above
#t8; bandwidth=c(.5, .5), gridsize = c(10000,10000)) # best generalisation to date - smoothed version


#t9; bandwidth=c(.1, .1), gridsize = c(10000,10000)) # too close to spotty
#t10: bandwidth=c(.01, .01), gridsize = c(10000,10000)) # too spotty 
#bandwidth=c(.1, .1), gridsize = c(1000,1000))# too close to spotty
#  bandwidth=c(.9, .9), gridsize = c(1000,1000)) # lost the northern section 


# genral findings 
# need a larger grainsize to account for small disperate pockets of points 
# finer band width = more spotty


# short list options; 
#    bandwidth=c(.4, .8), gridsize = c(1000,1000))  # looks good. 
#      bandwidth=c(.8, .4), gridsize = c(1000,1000))  # looks good - little wobby on edges 
#bandwidth=c(.5, .5), gridsize = c(1000,1000))


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



## Leaflet map with polygons
leaflet(spgons3) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV3, NULL)[LEVS3])     

leaflet() %>%
  addTiles %>%
  addPolygons(spgons3)



### COMPARE THE OUTPUTS 



## bandwidth=c(.0045, .0068), gridsize = c(10000,10000))
## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])   
# a bit too fine 



##  bandwidth=c(.045, .068), gridsize = c(1000,1000))
## Leaflet map with polygons
leaflet(spgons2) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV2, NULL)[LEVS2])               
# filters out the 


## bandwidth=c(.45, .68), gridsize = c(1000,1000)) 
leaflet(spgons3) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV3, NULL)[LEVS3])               

## bandwidth=c(.5, .5), gridsize = c(1000,1000))
leaflet(spgons4) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV4, NULL)[LEVS4])               


#bandwidth=c(.05, .05), gridsize = c(10000,10000))
leaflet(spgons5) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV5, NULL)[LEVS4])   






kde5 <- bkde2D(jdatxy,
               bandwidth=c(.05, .05), gridsize = c(5000,5000)) # need to adjust this
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







