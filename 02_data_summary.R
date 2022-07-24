# create summary of datasets

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

list.files(out.dir)

rekn <- readRDS(file.path(out.dir, "BNP_rekn_summary.rds"))
rose <- readRDS(file.path(out.dir, "johnson_rose_daily.RDS"))
rosegps <- readRDS(file.path(out.dir, "johnson_rose_gps.RDS"))
rekngps <- readRDS(file.path(out.dir, "newstead_rekn_gps.RDS" ))



# number of tags per type per year 

yrs <- rekn %>% 
  group_by(year,  data_type) %>%
  summarise(count = length(unique(animal.id)))

yrs_gps <- rekngps %>%
  group_by(year, data_type) %>%
  summarise(count = length(unique(animal.id)))

#yrs_rose <- rose %>%
#  group_by(year, data_type) %>%
#  summarise(count = length(unique(animal.id)))


#yrs_rose_gps <- rosegps %>%
#  group_by(year, data_type) %>%
#  summarise(count = length(unique(animal.id)))

yrs <- rbind(yrs, yrs_gps)

p1 <- ggplot(yrs, aes(x = as.character(year), y = count, fill = data_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Individuals")






# Length of tags

tags <- rekn %>%
  group_by(animal.id, data_type)%>%
  summarise(count= n(), total_length_days = sum(dur_no, na.rm = T))


tagsgps <- rekngps %>%
  group_by(animal.id, data_type)%>%
  summarise(count= n(), total_length_days = sum(dur_no, na.rm = T))




# total length of days recorded:

p2 <- ggplot(tags, aes(y = total_length_days, x = reorder(animal.id, total_length_days))) +
  facet_wrap(~data_type, nrow = 2)+
  geom_bar(stat = "identity")  +
  labs(x = "Individuals", y = "No. of days")
  #theme(axis.text.x = element_text(angle = 90))





# Roselaarri comparisons: 


#To do.....



