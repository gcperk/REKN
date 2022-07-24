#########################

# subpopulations


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

rekn <- readRDS(file.path(out.dir, "BNP_rekn_summary.rds"))
rekngps <- readRDS(file.path(out.dir, "newstead_rekn_gps.RDS" ))

#rose <- readRDS(file.path(out.dir, "johnson_rose_daily.RDS"))
#rosegps <- readRDS(file.path(out.dir, "johnson_rose_gps.RDS"))

rpop <- rekn %>%
  dplyr::select(animal.id, Subpop, `Original dataset`) %>%
  group_by(Subpop)%>%
  summarise(count = length(unique(animal.id)))


rpop 
