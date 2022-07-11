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

bdat <- read.csv(file.path("data", "location_estimates_final.csv"))


# filter data to remove NA values
bdat <- bdat %>%
  select(c(id, location.long, location.lat, arrive.date, depart.date, animal.id, deploy_id))


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
  mutate(dur_no = as.numeric(dur))


# Basic summary of individuals

tags <- bdat_sp %>%
  group_by(animal.id, deploy_id)%>%
  summarise(count= n(), total_length = sum(dur_no, na.rm = T))

# animal id per year
yrs <- bdat_sp %>% 
  group_by(year) %>%
  summarise(count = length(unique(animal.id)))

p1 <- ggplot(yrs, aes(x = as.character(year), y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "deploy_year", y = "no.of.animals")


# total length of days recorded
tagsc <- tags %>% filter(total_length>1)

p2 <- ggplot(tagsc, aes(y = total_length, x = animal.id)) +
  geom_bar(stat = "identity")



