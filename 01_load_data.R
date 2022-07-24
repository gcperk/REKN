# Import data and manipulate as needed 

library(leaflet)
library(RColorBrewer)
library(dplyr)
library(lubridate)
library(sp)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)

# define folder structire
data.dir <- file.path ("data", "location_estimates")
out.dir <- file.path("outputs")


# read in and format REKN data (Burger/Niles/Porter)

bdat <- read.csv(file.path(data.dir,"location_estimates_final.csv"))

ref_dat <- read_excel(file.path("ReferenceData.xlsx")) %>%
  dplyr::select(c(Subpop, `Original dataset`, "animal-id", "deployment-id", `Usable data set`)) %>%
  mutate(animal.id = `animal-id`, 
         deploy_id = `deployment-id`) %>%
  dplyr::select(c(Subpop, `Original dataset`, animal.id,`Usable data set`))%>%
  unique()

# filter data to remove NA values
bdat <- bdat %>%
  dplyr::select(c(id, location.long, location.lat, arrive.date, depart.date, animal.id)) #, deploy_id))

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
  mutate(dur_no = as.numeric(dur))


allsp <- left_join(bdat_sp, ref_dat, by = "animal.id") %>%
  mutate(data_type = "geolocator")

saveRDS(allsp, file.path(out.dir, "BNP_rekn_summary.RDS"))
write.csv(allsp, file.path(out.dir, "BNP_rekn_compiled.csv"))

# read in Rosellari tags (Johnson)

jdat <- read.csv(file.path(data.dir,"daily_positions_johnson.csv")) %>%
  rename("animal.id" = Birdid,
         "location.lat" = Median.lat,
         "location.long" = Median.long) %>%
  mutate(`Original dataset` ="Johnson_daily",
         data_type = "geolocator")

jdat  <- jdat  %>%
  mutate(arrive = ymd(Date)) %>%
  mutate(year = year(arrive)) %>%
  mutate(arr_month = month(arrive)) %>%
  group_by(animal.id)%>%
  mutate(depart.date = lead(Date))%>%
  ungroup() %>%
  mutate(depart = ymd(depart.date)) %>%
  mutate(arr_month = month(arrive),
         dep_month = month(depart)) %>%
  mutate(dur = depart -arrive) %>%
  # mutate(lat = as.numeric(location.lat), 
  #       lng = location.long) %>%
  mutate(dur_no = as.numeric(dur),
         Subpop = "rosellarri")


saveRDS(jdat, file.path(out.dir, "johnson_rose_daily.RDS"))



# read in Rosellari GPS tags (Johnson)

jgpsdat1 <- read.csv(file.path(data.dir,"REKN_GPSprocessed_2017.csv"))
jgpsdat2 <- read.csv(file.path(data.dir,"REKN_GPSprocessed_2018.csv"))


jgps <- rbind(jgpsdat1, jgpsdat2) %>%
  filter(CRC != "OK(corrected)") %>%
  filter(LocType_new != "Bad") %>%
  rename("animal.id" = FlagID,
         "location.lat" = Lat,
         "location.long" = Long) %>%
  mutate(`Original dataset` ="Johnson_GPS",
         data_type = "GPS")
# Basic summary of individuals

jgps <- jgps  %>%
  mutate(arrive = mdy(Date)) %>%
  mutate(year = year(arrive)) %>%
  mutate(arr_month = month(arrive)) %>%
  group_by(animal.id) %>%
  mutate(depart.date = lead(Date))%>%
  ungroup() %>%
  mutate(depart = mdy(depart.date)) %>%
  mutate(arr_month = month(arrive),
         dep_month = month(depart)) %>%
  mutate(dur = depart -arrive) %>%
  # mutate(lat = as.numeric(location.lat), 
  #       lng = location.long) %>%
  mutate(dur_no = as.numeric(dur),
         Subpop = "rosellarri")


saveRDS(jgps, file.path(out.dir, "johnson_rose_gps.RDS"))


##########################################################

# read in REKN GPS tags (Newstead)

ndat <- read.csv(file.path(data.dir,"CBBEP_Newstead_Red Knot Gulf to Arctic.csv"))

ndat <- ndat %>%
  filter(`lotek.crc.status.text` != "OK(corrected)")  %>%
  rename("animal.id" = individual.local.identifier,
         "location.lat" = location.lat,
         "location.long" = location.long) %>%
  mutate(`Original dataset` ="Newstead_GPS",
         data_type = "GPS") %>%
  mutate(arrive = ymd(Date)) %>%
  mutate(year = year(arrive)) %>%
  mutate(arr_month = month(arrive)) %>%
  group_by(animal.id) %>%
  mutate(depart.date = lead(Date))%>%
  ungroup() %>%
  mutate(depart = ymd(depart.date)) %>%
  mutate(arr_month = month(arrive),
         dep_month = month(depart)) %>%
  mutate(dur = depart -arrive) %>%
  # mutate(lat = as.numeric(location.lat), 
  #       lng = location.long) %>%
  mutate(dur_no = as.numeric(dur),
         Subpop = "West Gulf") %>%
  filter(year <2023)



saveRDS(ndat, file.path(out.dir, "newstead_rekn_gps.RDS"))


##########################################################################################




