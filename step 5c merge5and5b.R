library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

################################################################################
### Merge step 5 and step 5b For VF1
################################################################################

step5 <-  readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5/VF1_step5.rds")
step5b <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5b/VF1_step5b.rds")

str(step5)
str(step5b)

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
str(all_step5)
saveRDS(all_step5,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF1_step5c.rds")

write.csv(step5, "W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/check/VF1_step5a.csv")
write.csv(step5b, "W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/check/VF1_step5b.csv")
write.csv(all_step5, "W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/check/VF1_step5c.csv")


rm(step5,step5b, all_step5)

################################################################################
### Merge step 5 and step 5b For VF2
################################################################################

step5 <-  readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5/VF2_step5.rds")
step5b <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5b/VF2_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
saveRDS(all_step5,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF2_step5c.rds")

rm(step5,step5b, all_step5)

################################################################################
### Merge step 5 and step 5b For VF3
################################################################################

step5 <-  readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5/VF3_step5.rds")
step5b <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5b/VF3_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
saveRDS(all_step5,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF3_step5c.rds")

rm(step5,step5b, all_step5)

################################################################################
### Merge step 5 and step 5b For VF4
################################################################################

step5 <-  readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5/VF4_step5.rds")
step5b <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5b/VF4_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
saveRDS(all_step5,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF4_step5c.rds")

rm(step5,step5b, all_step5)

################################################################################
### Merge step 5 and step 5b For VF4
################################################################################

step5 <-  readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5/VF5_step5.rds")
step5b <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5b/VF5_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
saveRDS(all_step5,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF5_step5c.rds")

rm(step5,step5b, all_step5)
