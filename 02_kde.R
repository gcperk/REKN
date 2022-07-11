
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


