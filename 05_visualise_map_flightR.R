
library(dplyr)
library(ggplot2)
library(lubridate)
library(foreach)
library(tidyverse)
library(sf)
devtools::install_github("eldarrak/FLightR")
library(FlightR)



# plotting the summaried data from WE and BPN

# using a modified version of flight R function, adjusted for different input, 
# based on code here: https://github.com/eldarrak/FLightR/blob/master/R/new_plotting_functions.R


map.FLightR.ggmap<-function(Result, 
                            dates=NULL, 
                            plot.cloud=TRUE, 
                            map.options=NULL, 
                            save.options=NULL, 
                            zoom="auto", 
                            return.ggobj=FALSE, 
                            seasonal.colors=TRUE, 
                            seasonal.donut.location='topleft', 
                            seasonal.donut.proportion=0.5, save=TRUE) {
  
  # if (!ggmap::has_google_key()) stop('From August 2018 Google allows to use Google maps only for users with the API key, please get one and proceed as described here: http://ornithologyexchange.org/forums/topic/38315-mapflightrggmap-error/')
  #dates should be a data.frame with first point - starting dates and last column end dates for periods
  # 
  # Opt<-options('ggmap')
  # if(is.null(Opt$ggmap$google$account_type)) Opt$ggmap$google$account_type<-'standard'
  # if(is.null(Opt$ggmap$google$day_limit)) Opt$ggmap$google$day_limit<-2500
  # if(is.null(Opt$ggmap$google$second_limit)) Opt$ggmap$google$day_limit<-50
  # if(is.null(Opt$ggmap$google$client)) Opt$ggmap$google$client<-NA
  # if(is.null(Opt$ggmap$google$signature)) Opt$ggmap$google$signature<-NA
  # options('ggmap'= Opt$ggmap)

  bdat <- bdat %>%
    select(c(lon, lat, arr, dep, tag, species)) %>%
    filter(species == "rekn")
  
  dates <- bdat %>% select(arr, dep)

  Quantiles <- bdat
  
  
  Years <-unique(format(Quantiles$time, format="%Y"))
  eq <-c(as.POSIXct(paste(Years, "-09-22 00:00:00 GMT", sep="")), as.POSIXct(paste(Years, "-03-20 12:00:00 GMT", sep="")))
  eq <-eq[eq>min(Quantiles$time) & eq<max(Quantiles$time)]
  
  #------- vert grid
  Vert_grid<-seq(as.POSIXct("2000-01-01", tz='GMT'), as.POSIXct("2030-01-01", tz='GMT'), by="month")
  Vert_grid<-Vert_grid[Vert_grid>=min(Quantiles$time) & Vert_grid<=max(Quantiles$time)]
  
  #Longitude
  
  graphics::plot(Quantiles$lon~Quantiles$lat, las=1,col=grDevices::grey(0.1),pch=16,
                 ylab="Longitude",xlab="",lwd=2, #ylim=range(c( Quantiles$LCI.lon,
                                                              # Quantiles$UCI.lon )), 
                 type="n", axes=FALSE)
  graphics::axis(2, las=1)
  graphics::axis.POSIXct(1, x=Quantiles$lon,  format="1-%b")
  graphics::box()
  
  # add vertical lines for the first day of every month
  
  graphics::abline(v=Vert_grid, col=grDevices::grey(0.5), lty=2)
  graphics::abline(h=seq(-180, 360, by=10), col=grDevices::grey(0.5), lty=2)
  
  graphics::abline(v=eq, col=2, lwd=2, lty=1)
  
  
  graphics::polygon(x=c(Quantiles$time, rev(Quantiles$time)), 
                    y=c(Quantiles$LCI.lon, rev(Quantiles$UCI.lon)),
                    col=grDevices::grey(0.9), border=grDevices::grey(0.5))
  
  graphics::polygon(x=c(Quantiles$time, rev(Quantiles$time)),
                    y=c(Quantiles$TrdQu.lon, rev(Quantiles$FstQu.lon)),
                    col=grDevices::grey(0.7), border=grDevices::grey(0.5))
  
  graphics::lines(Quantiles$Medianlon~Quantiles$time, col=grDevices::grey(0.1),lwd=2)
  
  #Latitude
  graphics::par(mar=c(3,4,1,1))
  
  graphics::plot(Quantiles$Medianlat~Quantiles$time, las=1,col=grDevices::grey(0.1),
                 pch=16,ylab="Latitude",xlab="",lwd=2,
                 ylim=range(c( Quantiles$UCI.lat, Quantiles$LCI.lat )), type="n", axes=FALSE)
  graphics::axis(2, las=1)
  graphics::axis.POSIXct(1, x=Quantiles$time,  format="1-%b")
  graphics::box()
  graphics::abline(v=Vert_grid, col=grDevices::grey(0.5), lty=2)
  graphics::abline(h=seq(-80, 80, by=10), col=grDevices::grey(0.5), lty=2)
  
  graphics::abline(v=eq, col=2, lwd=2, lty=1)
  
  graphics::polygon(x=c(Quantiles$time, rev(Quantiles$time)), y=c(Quantiles$LCI.lat, rev(Quantiles$UCI.lat)),
                    col=grDevices::grey(0.9), border=grDevices::grey(0.5))
  
  graphics::polygon(x=c(Quantiles$time, rev(Quantiles$time)), y=c(Quantiles$TrdQu.lat, rev(Quantiles$FstQu.lat)),
                    col=grDevices::grey(0.7), border=grDevices::grey(0.5))
  
  graphics::lines(Quantiles$Medianlat~Quantiles$time, col=grDevices::grey(0.1),lwd=2)
  return(NULL)
}

  
  
  
  plot_lon_lat<-function(Result, scheme=c("vertical", "horizontal")) {
    #oldpar <- graphics::par(no.readonly = TRUE)    
    #on.exit(graphics::par(oldpar))         
   
     #Quantiles<-Result$Results$Quantiles
   
     # check whether Grid was over dateline:
    # overdateline<-ifelse(attr(Result$Spatial$Grid, 'left') >	attr(Result$Spatial$Grid, 'right'), TRUE, FALSE)
    # 
    # 
    # if (overdateline) {
    #   
    #   Quantiles$Medianlon[Quantiles$Medianlon<0]<-Quantiles$Medianlon[Quantiles$Medianlon<0]+360
    #   Quantiles$LCI.lon[Quantiles$LCI.lon<0]<-Quantiles$LCI.lon[Quantiles$LCI.lon<0]+360
    #   Quantiles$UCI.lon[Quantiles$UCI.lon<0]<-Quantiles$UCI.lon[Quantiles$UCI.lon<0]+360
    #   Quantiles$TrdQu.lon[Quantiles$TrdQu.lon<0]<-Quantiles$TrdQu.lon[Quantiles$TrdQu.lon<0]+360
    #   Quantiles$FstQu.lon[Quantiles$FstQu.lon<0]<-Quantiles$FstQu.lon[Quantiles$FstQu.lon<0]+360
    #   
    # }
    # if (scheme[1]=="horizontal") {
    #  graphics::par(mfrow=c(1,2)) 
    #} else {
    #  graphics::par(mfrow=c(2,1)) 
   # }
    
    graphics::par(mar=c(2,4,3,1),cex=1)
    suppressWarnings(Sys.setlocale("LC_ALL", "English")) 
    
    #------here I have create the equinox dates..
    
    Years <-unique(format(Quantiles$time, format="%Y"))
    eq <-c(as.POSIXct(paste(Years, "-09-22 00:00:00 GMT", sep="")), as.POSIXct(paste(Years, "-03-20 12:00:00 GMT", sep="")))
    eq <-eq[eq>min(Quantiles$time) & eq<max(Quantiles$time)]
    
    #------- vert grid
    Vert_grid<-seq(as.POSIXct("2000-01-01", tz='GMT'), as.POSIXct("2030-01-01", tz='GMT'), by="month")
    Vert_grid<-Vert_grid[Vert_grid>=min(Quantiles$time) & Vert_grid<=max(Quantiles$time)]
    
    #Longitude
    
    graphics::plot(Quantiles$Medianlon~Quantiles$time, las=1,col=grDevices::grey(0.1),pch=16,
                   ylab="Longitude",xlab="",lwd=2, ylim=range(c( Quantiles$LCI.lon,
                                                                 Quantiles$UCI.lon )), type="n", axes=FALSE)
    graphics::axis(2, las=1)
    graphics::axis.POSIXct(1, x=Quantiles$time,  format="1-%b")
    graphics::box()
    
    # add vertical lines for the first day of every month
    
    graphics::abline(v=Vert_grid, col=grDevices::grey(0.5), lty=2)
    graphics::abline(h=seq(-180, 360, by=10), col=grDevices::grey(0.5), lty=2)
    
    graphics::abline(v=eq, col=2, lwd=2, lty=1)
    
    
    graphics::polygon(x=c(Quantiles$time, rev(Quantiles$time)), 
                      y=c(Quantiles$LCI.lon, rev(Quantiles$UCI.lon)),
                      col=grDevices::grey(0.9), border=grDevices::grey(0.5))
    
    graphics::polygon(x=c(Quantiles$time, rev(Quantiles$time)),
                      y=c(Quantiles$TrdQu.lon, rev(Quantiles$FstQu.lon)),
                      col=grDevices::grey(0.7), border=grDevices::grey(0.5))
    
    graphics::lines(Quantiles$Medianlon~Quantiles$time, col=grDevices::grey(0.1),lwd=2)
    
    #Latitude
    graphics::par(mar=c(3,4,1,1))
    
    graphics::plot(Quantiles$Medianlat~Quantiles$time, las=1,col=grDevices::grey(0.1),
                   pch=16,ylab="Latitude",xlab="",lwd=2,
                   ylim=range(c( Quantiles$UCI.lat, Quantiles$LCI.lat )), type="n", axes=FALSE)
    graphics::axis(2, las=1)
    graphics::axis.POSIXct(1, x=Quantiles$time,  format="1-%b")
    graphics::box()
    graphics::abline(v=Vert_grid, col=grDevices::grey(0.5), lty=2)
    graphics::abline(h=seq(-80, 80, by=10), col=grDevices::grey(0.5), lty=2)
    
    graphics::abline(v=eq, col=2, lwd=2, lty=1)
    
    graphics::polygon(x=c(Quantiles$time, rev(Quantiles$time)), y=c(Quantiles$LCI.lat, rev(Quantiles$UCI.lat)),
                      col=grDevices::grey(0.9), border=grDevices::grey(0.5))
    
    graphics::polygon(x=c(Quantiles$time, rev(Quantiles$time)), y=c(Quantiles$TrdQu.lat, rev(Quantiles$FstQu.lat)),
                      col=grDevices::grey(0.7), border=grDevices::grey(0.5))
    
    graphics::lines(Quantiles$Medianlat~Quantiles$time, col=grDevices::grey(0.1),lwd=2)
    return(NULL)
  }
  
  
  get_buffer<-function(coords, r){
    p = sp::SpatialPoints(matrix(coords, ncol=2), proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
    aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
                    p@coords[[2]], p@coords[[1]])
    projected <- sp::spTransform(p, sp::CRS(aeqd))
    buffered <- rgeos::gBuffer(projected, width=r, byid=TRUE)
    buffer_lonlat <- sp::spTransform(buffered, CRS=p@proj4string)
    return(buffer_lonlat)
  }
  
  get_gunion_r<-function(Result) {
    Distances=  sp::spDists(Result$Spatial$Grid[1:min(c(nrow(Result$Spatial$Grid), 1000)),1:2], longlat=TRUE)
    # ok, distances go up to 51.2.. the next step is 62.. 
    # so if I round them 
    Selected_dist<-unique(sort(round(Distances/10)*10))[2]
    r<-Selected_dist*0.85*1000
    return(r)
  }
  
  get_time_spent_buffer<-function(Result, dates=NULL, percentile=0.5, r=NULL) {
    # r in meters.. 
    # dates could be either NULL - for plotting all available dates,
    # or numeric with length of one - to plot a specific twilight by number (row number in Quantiles)
    # or data.frame with two columns - one for the first date in period and one for the last date in period.
    if (is.null(dates)) {
      twilights.index<-1:length(Result$Results$Points.rle)
    } else {
      if (is.numeric(dates) & length(dates)==1) {
        twilights.index<-dates
      } else {
        twilights.index<-c()
        for (segment in 1:nrow(dates)) {
          twilights.index<-c(twilights.index, which(Result$Results$Quantiles$time>=dates[segment,1] & Result$Results$Quantiles$time<=dates[segment,2]))
        }
        if (length(twilights.index)==0) stop("dates do not overlap with the track time span!")
      }
    }
    message('function will plot ', length(twilights.index), ' twilights\n')
    
    Points_selected<-get_utilisation_points(Result, twilights.index, percentile)
    
    if (is.null(r)) {
      r=get_gunion_r(Result)
    }
    #Now I want to create a spatial buffers around points with this radius
    
    #spoints = SpatialPoints(Result$Spatial$Grid[Points_selected,1:2], proj4string= CRS("+proj=longlat +datum=WGS84"))
    
    Buffers<-apply(matrix(Result$Spatial$Grid[Points_selected,1:2], ncol=2), 1, get_buffer, r=r)
    Buff_comb<-Buffers[[1]] 
    if (length(Buffers)>1) {
      for (i in 2:length(Buffers)) {
        Buff_comb<-rgeos::gUnion(Buff_comb, Buffers[[i]])
      }
    }
    Buff_comb_simpl<-rgeos::gSimplify(Buff_comb, tol=0.01, topologyPreserve=TRUE)
    return(list(Buffer=Buff_comb_simpl, nPoints=length(Points)))
  }
  




bdat <- read.csv(file.path("data", "all_geos_merged2.csv"))

# filter data to remove NA values
bdat <- bdat %>%
  select(c(lon, lat, arr, dep, tag, species)) %>%
  filter(species == "rekn")

#bdat <- bdat[complete.cases(bdat), ]

try(map.FLightR.ggmap(Result, zoom=3, save = FALSE))



# calculate time differences
bdat_sp <- bdat %>%
  mutate(arrive = ymd_hm(arr),
         depart = ymd_hm(dep)) %>%
  mutate(year = year(arrive))

bdat_sf <- st_as_sf(bdat_sp, coords = c("lon","lat"))

ggplot()

mapview::mapview(bdat_sf, map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"), color = "grey40")

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

#fillColor = ~pal(tag))

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
