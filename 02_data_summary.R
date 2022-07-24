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

source("01_load_data.R")

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
  dplyr::select(animal.id, year,  data_type) %>%
  unique() %>%
  group_by(animal.id, data_type) %>%
  summarise(deploy_yr = min(year)) %>%
  ungroup %>%
  group_by(deploy_yr, data_type)%>%
  summarise(count = length(unique(animal.id)))

yrs_gps <- rekngps %>%
  dplyr::select(animal.id, year,  data_type) %>%
  unique() %>%
  group_by(animal.id, data_type) %>%
  summarise(deploy_yr = min(year)) %>%
  ungroup %>%
  group_by(deploy_yr, data_type)%>%
  summarise(count = length(unique(animal.id)))


yrs_rose <- rose %>%
  dplyr::select(animal.id, year,  data_type) %>%
  unique() %>%
  group_by(animal.id, data_type) %>%
  summarise(deploy_yr = min(year)) %>%
  ungroup %>%
  group_by(deploy_yr, data_type)%>%
  summarise(count = length(unique(animal.id)))


yrs_rose_gps <- rosegps %>%
  dplyr::select(animal.id, year,  data_type) %>%
  unique() %>%
  group_by(animal.id, data_type) %>%
  summarise(deploy_yr = min(year)) %>%
  ungroup %>%
  group_by(deploy_yr, data_type)%>%
  summarise(count = length(unique(animal.id)))

yrs <- rbind(yrs, yrs_gps) %>% mutate()%>% mutate( Subspecies = "rufa")
yrs_rose <- rbind(yrs_rose, yrs_rose_gps) %>% mutate( Subspecies = "roselaari")

allrekn <- rbind(yrs_rose, yrs)

x_axis_labs <- min(allrekn[,"deploy_yr"]):max(allrekn[,"deploy_yr"])



rk <- ggplot(allrekn, aes(x = as.numeric(deploy_yr), y = count, fill = data_type)) +
  facet_wrap(~Subspecies,nrow = 2, scales = "free_y")+
  geom_bar(stat = "identity") +
  labs(x = "Deployment Year", y = "No. of Individuals") +
  scale_fill_grey(start=0.7, end=0.3)+
  theme_bw() +
  scale_x_continuous(labels = x_axis_labs, breaks = x_axis_labs)
#theme_classic()
rk

ggsave( file.path(out.dir, "rekn_deploy_yrs.jpg"))


# Out put individual plots 
# 
# p1 <- ggplot(yrs, aes(x = as.numeric(deploy_yr), y = count, fill = data_type)) +
#   geom_bar(stat = "identity") +
#   labs(x = "Deployment Year", y = "No. of Individuals") +
#   scale_fill_grey(start=0.7, end=0.3)+
#   theme_bw() +
#   scale_x_continuous(labels = x_axis_labs, breaks = x_axis_labs)
# #theme_classic()
# p1
# 
# ggsave( file.path(out.dir, "rekn_deploy_yrs.jpg"))
# 
# x_axis_labs <- min(yrs_rose[,"deploy_yr"]):max(yrs_rose[,"deploy_yr"])
# 
# p1r <- ggplot(yrs_rose, aes(x = as.character(deploy_yr), y = count, fill = data_type)) +
#   geom_bar(stat = "identity") +
#   labs(x = "Year", y = "Individuals")+
#   scale_fill_grey(start=0.7, end=0.3)+
#   theme_bw() 
# 
# ggsave( file.path(out.dir, "rekn_rose_deploy_yrs.jpg"))
# 











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



