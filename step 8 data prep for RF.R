
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


################################################################################
##############  VF1 to VF 5   ##########################################################

VF1 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF1_step5c.rds") %>% mutate(VF_ID = "VF1")
VF2 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF2_step5c.rds") %>% mutate(VF_ID = "VF2")
VF3 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF3_step5c.rds") %>% mutate(VF_ID = "VF3")
VF4 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF4_step5c.rds") %>% mutate(VF_ID = "VF4")
VF5 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF5_step5c.rds") %>% mutate(VF_ID = "VF5")

names(VF1)

all_animals <- rbind(VF1, VF2, VF3, VF4, VF5)
                       
all_animals$local_time <- as.POSIXct(all_animals$local_time,  tz = "Australia/Adelaide")

#The other files look like this: 
# time_step	
# Time_sheep	Vs Time_animal
# ID_jaxs	
# sheep	 vs animal
# local_time	
# date	
# DOY	
# X	
# Y	
# dist_to_VF	
# VF_EX	
# #resting_percentage	#I dont have this data
# #moving_percentage	#I dont have this data
# #grazing_percentage	#I dont have this data
# round_local_time	
# Time_sheep_zone	vs Time_animal_zone
# occurance	
# what_to_retain	
# step	
# cumulativeAudioCount	
# cumulativeShockCount	
# Audio_values	
# Shock_values	


## check that I don't have duplication of time


check_for_overlap <- all_animals %>%  group_by(VF_ID) %>% 
  summarise(min_time = min(time_step, na.rm = TRUE),
            max_time = max(time_step, na.rm = TRUE))

check_for_overlap
#there seems to be a bit of overlap between VF2 to VF 3 - lets start VF3 at 08:40
#there seems to be a bit of overlap between VF4 to VF 5 - lets start VF5 at 09:40

VF3 <- VF3 %>% dplyr::filter(time_step >= ymd_hms('2019-05-23 08:40:00'))
VF5 <- VF5 %>% dplyr::filter(time_step >= ymd_hms('2019-06-03 09:40:00'))

all_animals <- rbind(VF1, VF2, VF3, VF4, VF5)
all_animals$local_time <- as.POSIXct(all_animals$local_time,  tz = "Australia/Adelaide")

################################################################################
####    remove the animals that don't have collars     ###########################
################################################################################

all_animals_collars <- all_animals

################################################################################
####    remove the time logs I don't want      ###########################
################################################################################

#Nothing selected here

###############################################################################
#### create a ID variable if the animal is in the exclusion zone or not  #####
### perhaps something like time spent in the VF ####


str(all_animals_collars)

count_VF_occurance_per_animal <- all_animals_collars %>%  group_by( animal, VF_EX) %>% 
  summarise(count_records = n())
count_VF_occurance_per_animal

#remove the NA
count_VF_occurance_per_animal <- count_VF_occurance_per_animal %>% 
  filter(!is.na(animal ))


#express as a percentage


count_VF_occurance_per_animal_wide <- count_VF_occurance_per_animal %>% 
  pivot_wider(names_from = VF_EX, 
              values_from = count_records)
count_VF_occurance_per_animal_wide

#replace NA values with zero
count_VF_occurance_per_animal_wide <- count_VF_occurance_per_animal_wide %>% dplyr::mutate(outside_VF = replace_na(outside_VF, 0))
count_VF_occurance_per_animal_wide <- count_VF_occurance_per_animal_wide %>% dplyr::mutate(inside_VF  = replace_na(inside_VF , 0))


#sum of total counts

count_VF_occurance_per_animal_wide <-count_VF_occurance_per_animal_wide %>% 
  dplyr::mutate(total_counts = inside_VF + outside_VF,
                prop_exclusion_zone = ((outside_VF /total_counts)*100)) %>% 
  arrange(animal )

count_VF_occurance_per_animal_wide

## low value the animal is spending lots of time is exclusion zone
## and high value the animal is spending lots of time in grazing zone

### turn the time spent in exclusion zone into categorical data.

count_VF_occurance_per_animal_wide <-
  count_VF_occurance_per_animal_wide %>%
  dplyr::mutate(
    compliance_score =
      case_when(
        prop_exclusion_zone <= 1.5 ~ "compliant",
        prop_exclusion_zone > 1.5 ~ "non_compliant"
      )
  )


animal_compliance_score <- count_VF_occurance_per_animal_wide %>% dplyr::select(animal, compliance_score)
animal_compliance_score

rm(all_animals, count_VF_occurance_per_animal, count_VF_occurance_per_animal_wide)


#### join animal_compliance_score to the original dataset.
str(all_animals_collars)
all_animals_collars <- left_join(all_animals_collars, animal_compliance_score)


rm(animal_compliance_score)

###################################################################################
###                 write out df ready for the next step                      ###
###################################################################################



write.csv(all_animals_collars, 
          "W:/VF/Optimising_VF/Eden Valley/data_prep/step8/step8_all_animals_collars.csv", 
          row.names=FALSE)
###################################################################################
### condense the data so it can be simply used in RF model - one row per animal  ###
###################################################################################


