## Script to calculate time lag 
## written by Gen Perkins
## 2021-12-15

library(dplyr)
library(ggplot2)
library(lubridate)
library(foreach)
library(tidyverse)


# check the files are in the data folder
list.files(file.path("data"))

bdat <- read.csv(file.path("data", "all_geos_merged2.csv"))

# filter data to remove NA values
bdat <- bdat %>%
  select(c(lon, lat, arr, dep, tag, species)) 
 
bdat <- bdat[complete.cases(bdat), ]

# calculate time differences
bdat_sp <- bdat %>%
  mutate(arrive = ymd_hm(arr),
        depart = ymd_hm(dep)) %>%
  mutate(year = year(arrive),
        arrive_j = yday(arrive),  # calculte the julian date
        depart_j = yday(depart)) 


## calculte the gap between movement - maynot be needed

#bdat_sp <- bdat_sp %>%
#  mutate(sec_diff = seconds_to_period(arrive - lag(depart))) %>%
#  mutate(days_diff = day(sec_diff)) %>%
#  mutate(movement_days = depart_j + days_diff) %>%
#  select(-sec_diff,days_diff)

# convert to long form to plot 
bdat_sp_long <- bdat_sp %>%
  pivot_longer(cols = c("arrive_j", "depart_j"), names_to = "days")

# get list of sp 
sp <- unique(bdat$species)

# plot by species (yr or tag)
for( i in 1:length(sp)){
    
    i = 8 # testing line
    
    ii <- sp[i]
    print(ii)
   
    sp_long <- bdat_sp_long %>%
      filter(species == ii) 

    ggplot(sp_long , aes(x = value, y = lat))+
      facet_wrap(~tag)+
      geom_point(aes(colour = as.factor(year)))+ 
      geom_line(aes(colour = as.factor(year)))+ 
      labs(x = "julian date")  


    ggplot(sp_long, aes(x = value, y = lat))+
      facet_wrap(~year)+
      geom_point(aes(colour = as.factor(tag)))+ 
      geom_line(aes(colour = as.factor(tag)))+ 
      labs(x = "julian date")


}

# alternatively if you want to view a single individual: 

## get list of unique bird tags
bd_list <- unique(bdat$tag)

i = 279

   ii <- bd_list[i]#Test line
   #print(i)
   sp_dat <-  bdat_sp_long  %>%
    filter(tag == ii) 

    ggplot(sp_dat, aes(x = value, y = lat, fill = tag))+
      facet_wrap(~year)+
      geom_point()+ 
      geom_line()  + 
      labs(x = "julian date")

    
    ggplot(sp_dat, aes(x = value, y = lat))+
      facet_wrap(~tag)+
      geom_point(aes(colour = as.factor(year)))+ 
      geom_line(aes(colour = as.factor(year)))+ 
      labs(x = "julian date")
         

    
#########################################################  
# Merge all tracks together: 
library(tidyverse)
library(foreach)
library(data.table)
    
tracks <- list.files(file.path("data", "Tracks"), full.names = T)
tracks <- tracks[-1]  

trackcombo <- foreach(k = unique(tracks)) %do% {
  
    print(basename(k)) # = tracks[2]
    tfile = read_xlsx(file.path(k), 
                      col_types = c("guess", "numeric", "numeric", 
                                    "date", "date", "numeric", "guess"))

}

acc <- as.data.frame(rbindlist(trackcombo))

unique(acc$tag)
write.csv(acc, file.path("data", "constructed_tracks.csv"))
