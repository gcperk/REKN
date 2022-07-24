
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

bdat <- read.csv(file.path("data", "location_estimates_final.csv"))
ref_dat <- read_excel(file.path("ReferenceData.xlsx")) %>%
  dplyr::select(c(Subpop, "animal-id", "deployment-id")) %>%
  mutate(animal.id = `animal-id`, 
         deploy_id = `deployment-id`)

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
  mutate(dur_no = as.numeric(dur))



# Basic summary of individuals

tags <- bdat_sp %>%
  group_by(animal.id)%>%
  summarise(count= n(), total_length_days = sum(dur_no, na.rm = T),
            lat.min = min(lat))


allsp <- left_join(bdat_sp, ref_dat, by = 'animal.id')
write.csv(allsp, file.path("data", "location_estimates_final_edit.csv"))




hist(tags$lat.min)

tags <- tags %>%
  mutate(wgroup = case_when(
    lat.min < -25 ~ "sth SthAm",
    lat.min > - 25 & lat.min < 9 ~ "nth SthAm",
    
    
  )
         )



min_lat <- bdat_sp %>%
  group_by(animal.id) %>%

# animal id per year
yrs <- bdat_sp %>% 
  group_by(year) %>%
  summarise(count = length(unique(animal.id)))

p1 <- ggplot(yrs, aes(x = as.character(year), y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "deploy_year", y = "no.of.animals")


# total length of days recorded
tagsc <- tags %>% filter(total_length_days >1)%>%
  arrange(total_length_days)

p2 <- ggplot(tagsc, aes(y = total_length_days, x = animal.id)) +
  geom_bar(stat = "identity")


p3 <- hist(tagsc$total_length) # breaks = c(0, 30, 60, 90, 180, 360, 570, 1000))

# mark the multi yr birds 
