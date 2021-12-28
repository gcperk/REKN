# Output file list for REKN data
#

library(dplyr)
library(stringr)

## Point to folder where the data is stored 
# you will need to adjust this to your filepath location. 
# if you get an error check that the \ are doubles "\\" as example below


folder <- "J:\\02.Contracts\\2021_NWRC\\02_data\\BurgerPorterNiles-20211201T210714Z-001\\BurgerPorterNiles\\ALL shared files\\rekn all data"

# get list of all files and subfiles 
all.files <- list.files(folder, recursive = TRUE)
# convert to data frame to export as csv
all.files <- as.data.frame(all.files)
# write as csv
write.csv(all.files, file.path(folder, "filelist_GP.csv"))


# read in both files and find missing files: 

LW <- read.csv("filelist_LW.csv")
GP <- read.csv("filelist_GP.csv")

length(LW$all.files)
#[1] 4388
length(GP$all.files)
#[1] 4343

lw <- LW %>% pull(all.files)
gp <- GP %>% pull(all.files)

dif <- setdiff(lw,gp)
dif2 <- dif[str_detect(dif, "_.DS_Store", negate = TRUE)]

# check the names of the birds that will ran

folder <- file.path("data", "all_geos_merged2.csv" )
we <- read.csv(folder)

rekn <- we %>%
  filter(species == "rekn") 


rekn_id <- unique(rekn$tag)
# 78 unique 

