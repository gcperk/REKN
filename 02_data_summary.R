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
out.plots <- file.path("outputs", "final")

#list.files(out.dir)

rekn <- readRDS(file.path(out.dir, "BNP_rekn_summary.rds")) 
#rose_extra <- rekn %>% filter(animal.id == "tex_4a3")%>%
#  dplyr::select(animal.id, year,  data_type) 

#rekn <- rekn %>% filter(!animal.id == "tex_4a3") 

rose <- readRDS(file.path(out.dir, "johnson_rose_daily.RDS"))
#rosegps <- readRDS(file.path(out.dir, "johnson_rose_gps.RDS"))
#rekngps <- readRDS(file.path(out.dir, "newstead_rekn_gps.RDS" ))



# number of tags per type per year 

yrs <- rekn %>% 
  dplyr::select(animal.id, year,  data_type) %>%
  unique() %>%
  group_by(animal.id, data_type) %>%
  summarise(deploy_yr = min(year)) %>%
  ungroup %>%
  group_by(deploy_yr, data_type)%>%
  summarise(count = length(unique(animal.id)))
# 
# yrs_gps <- rekngps %>%
#   dplyr::select(animal.id, year,  data_type) %>%
#   unique() %>%
#   group_by(animal.id, data_type) %>%
#   summarise(deploy_yr = min(year)) %>%
#   ungroup %>%
#   group_by(deploy_yr, data_type)%>%
#   summarise(count = length(unique(animal.id)))


yrs_rose <- rose %>%
  dplyr::select(animal.id, year,  data_type) %>%
 # bind_rows(rose_extra) %>%
  unique() %>%
  group_by(animal.id, data_type) %>%
  summarise(deploy_yr = min(year)) %>%
  ungroup %>%
  group_by(deploy_yr, data_type)%>%
  summarise(count = length(unique(animal.id)))

# 
# yrs_rose_gps <- rosegps %>%
#   dplyr::select(animal.id, year,  data_type) %>%
#   unique() %>%
#   group_by(animal.id, data_type) %>%
#   summarise(deploy_yr = min(year)) %>%
#   ungroup %>%
#   group_by(deploy_yr, data_type)%>%
#   summarise(count = length(unique(animal.id)))

#yrs <- rbind(yrs, yrs_gps) %>% mutate()%>% mutate( Subspecies = "rufa")
#yrs_rose <- rbind(yrs_rose, yrs_rose_gps) %>% mutate( Subspecies = "roselaari")

yrs <- yrs %>% mutate()%>% mutate( Subspecies = "rufa")
yrs_rose <- yrs_rose %>% mutate( Subspecies = "roselaari")


allrekn <- rbind(yrs_rose, yrs)

x_axis_labs <- min(allrekn[,"deploy_yr"]):max(allrekn[,"deploy_yr"])


rk <- ggplot(allrekn, aes(x = as.numeric(deploy_yr), y = count, fill = "light grey")) +
  facet_wrap(~Subspecies,nrow = 2, scales = "free_y")+
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "Year deployed", y = "No. of Individuals") +
  #scale_fill_grey(start=0.7, end=0.3)+
  theme_bw() +
  scale_fill_grey(start=0.7, end=0.7)+
  scale_x_continuous(labels = x_axis_labs, breaks = x_axis_labs)
#theme_classic()
rk

#ggsave( file.path(out.dir, "rekn_deploy_yrs.jpg"))


jpeg(file.path(out.plots,"rekn_deploy_yrs.jpg"), width = 15, height = 10,units = "cm", res = 210)
# 2. Create the plot
rk
# 3. Close the file
dev.off()

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

# number of tags used in analysis
# 
# tags <- rekn %>%
#   group_by(animal.id, data_type)%>%
#   summarise(count= n(), total_length_days = sum(dur_no, na.rm = T)) %>%
#   ungroup()%>%
#   mutate(deploy_local = sub("\\_.*", "", animal.id))



# Figure 2: length of tags 

tags <- rekn %>%
  group_by(animal.id, data_type)%>%
  summarise(count= n(), total_length_days = sum(dur_no, na.rm = T)) %>%
  ungroup()%>%
  mutate(deploy_local = sub("\\_.*", "", animal.id))

# tagsgps <- rekngps %>%
#   group_by(animal.id, data_type)%>%
#   summarise(count= n(), total_length_days = sum(dur_no, na.rm = T))


#tags <- rbind(tags, tagsgps)

# total length of days recorded:

p2 <- ggplot(tags, aes(y = total_length_days, x = reorder(animal.id, total_length_days),fill = deploy_local)) +
  geom_bar(stat = "identity")  +
  labs(x = "Individuals", y = "No. of days")+
  #scale_fill_brewer(palette = 2)+
  scale_fill_viridis_d()+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))

p2

ggsave( file.path(out.plots, "rekn_length_geolocators1.jpg"))



jpeg(file.path(out.plots,"rekn_length_geolocators.jpg"), width = 20, height = 10,units = "cm", res = 210)
# 2. Create the plot
p2
# 3. Close the file
dev.off()







#location summary 

sum = tags %>%
  group_by(deploy_local) %>%
  summarise(#count = n(total_length_days), 
            tmin = min(total_length_days), 
            tmax = max(total_length_days),
            tmean = mean(total_length_days))

sum


# p3 <- ggplot(sum, aes(x = deploy_local, y = tmean)) +
#   geom_bar(stat = "identity")  +
#   #labs(x = "Individuals", y = "No. of days")+
#   #scale_fill_brewer(palette = 2)+
#   #scale_fill_viridis_d()+
#   theme_bw() #+
#  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
# 
# p2



