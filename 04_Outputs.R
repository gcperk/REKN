# import data for graphics

list.files()

devtools::install_github("MTHallworth/SGAT")
devtools::install_github("MTHallworth/LLmig")
devtools::install_github("slisovski/PolarGeolocation")

#library(devtools)
#library(SGAT)
#library(LLmig)

#library(GeoLocTools)
#setupGeolocation()
#data("wrld_simpl")

# download the world data files
library(maptools)
data("wrld_simpl")
plot(wrld_simpl)


install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
       "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


#library("rnaturalearth")
#library("rnaturalearthdata")






#Check if files are identical 


f1 <- "J:\\02.Contracts\\2021_NWRC\\02_data\\REKN\\data\\BurgerPorterNiles\\rekn_246\\10B757ha_lig.txt"
f2 <- "J:\\02.Contracts\\2021_NWRC\\02_data\\REKN\\data\\BurgerPorterNiles\\rekn_246\\10B757ha_wet.txt"

inf1 <- read.delim(f1)
inf2 <- read.delim(f2)
all.equal(inf1,inf2)
identical(inf1, inf2)

