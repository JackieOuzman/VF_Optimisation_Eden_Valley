
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


############################################################################################
############       bring in data           ##############################
############################################################################################

path_step1 <- "W:/VF/Optimising_VF/Chiswick/data_prep/"
raw_data <- "W:/VF/Optimising_VF/raw_data/Chiswick/"

### These are massive and I can't run them on my machine or the virtual machine I normally use I have used pearcey running the following scripts
# "\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\peracey_step1a_bring_in_data.R"
#This just meregres the raw data

VF_week1_2_3_InclusionBord <- readRDS("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/VF_week1_2_3_InclusionBord.rds")
dim(VF_week1_2_3_InclusionBord)




# "\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\peracey_step1a_bring_in_data_forVF5.R"
# "\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\peracey_step1b_bring_in_data.R"
# "\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\peracey_step1c_bring_in_data.R"
# "\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\peracey_step1d_bring_in_data.R"
# "\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\peracey_step1e_bring_in_data.R"
# "\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\peracey_step1f_bring_in_data.R"

# to produce the following files

"\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\sp_VF1_InclusionBord_animalID.csv"
"\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\sp_VF2_InclusionBord_animalID.csv"
"\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\sp_VF3_InclusionBord_animalID.csv"
"\\pearceyhome.csiro.au\HOME_INTEL\ouz001\VF_cattle\catlle_pearcey_recal_dist\Re_cal\sp_VF4_InclusionBord_animalID.csv"

#these files have been 
#1. Filtered so records with no GPS are removed
#2. Collar ID is converted to a collar ID and time clm (where the time is set to TZ = GMT)
#3. the Collar ID and animal ID has been adjusted so that the data accommodated changes in collar ID.
#4. calculates the distance from the VF
#5. Calculates is the animals is in the grazing or non grazing zone
#6. Clipped to the hard fences




### I cant find the script that merges these but it looks like this is the merged file.

VF1 <- read_csv("//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/catlle_pearcey_recal_dist/Re_cal/sp_VF1_InclusionBord_animalID.csv")
VF2 <- read_csv("//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/catlle_pearcey_recal_dist/Re_cal/sp_VF2_InclusionBord_animalID.csv")
VF3 <- read_csv("//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/catlle_pearcey_recal_dist/Re_cal/sp_VF3_InclusionBord_animalID.csv")
VF4 <- read_csv("//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/catlle_pearcey_recal_dist/Re_cal/sp_VF4_InclusionBord_animalID.csv")



VF1_VF4 <-rbind(VF1, VF2, VF3, VF4)
dim(VF1_VF4)

rm(VF1, VF2, VF3, VF4)



################################################################################
### Umm this is a problem because the workflow is slighly different.
## Lets see if I can trim based on time first


VF1_InclusionBord <- filter(VF_week1_2_3_InclusionBord, 
                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                    as_datetime('2019-05-20 14:40:00', tz="GMT")))



##########################################################################################################
#############    assign the collar ID to animal ID  ########################################################
##########################################################################################################
#bring in the sp_VF1_InclusionBord_clip if needed.




### changed the start time for could of renames

sp_VF1_InclusionBord_animalID <- mutate(VF1_InclusionBord,
                                        animal_ID = case_when(
                                          collar_ID == "ac138" ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac204" ~ "Q108",
                                          collar_ID == "ac207" ~ "Q42",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac213" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-28 06:44:00', tz="GMT")) ~ "Q47",
                                          collar_ID == "ac320" &
                                            between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",
                                          collar_ID == "ac219" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-25 11:10:00', tz="GMT"))~ "Q10",
                                          collar_ID == "ac220" &
                                            between(time, as_datetime('2019-05-25 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:18', tz="GMT"))~ "Q10",
                                          collar_ID == "ac325" ~ "Q9",
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" ~ "Q75",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3396"  &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-27 16:19:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ac209"  &
                                            between(time, as_datetime('2019-05-28 11:11:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:00:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          TRUE ~ "NA"))

#check we are assignining all the collar ID to animal names
head(sp_VF1_InclusionBord_animalID)
with(sp_VF1_InclusionBord_animalID, table(date, animal_ID))

#the location of the NA
NA_sp_VF1_InclusionBord_animalID <- filter(sp_VF1_InclusionBord_animalID,
                                           animal_ID == "NA")
with(NA_sp_VF1_InclusionBord_animalID, table(date, collar_ID))












str(GPS)


#format time and date clm from character to time
GPS <-  GPS %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))


GPS <- GPS %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

GPS <- GPS %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Sydney"))

## Add a clm for ID_jaxs
GPS <- GPS %>% 
  dplyr::mutate( ID_jaxs = row_number())


### what are the fences called in this dataset?
unique(GPS$fencesID) # we only have 3 "NULL"  "1dd82" "1f1eb" "1766d"


## reorder the clms
GPS <- GPS %>% 
  dplyr::select(ID_jaxs,deviceUIDHex:local_time)



GPS <- GPS %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Sydney"),
         DOY = yday(date))


#############################################################################################
####    Assign collar to sheep names #####
unique(GPS$deviceName)

GPS <- GPS %>% 
  mutate(Sheep_ID = case_when(
    deviceName == 9380142  ~ "1",
    deviceName == 9380674  ~ "2",
    deviceName == 9380743  ~ "3",
    deviceName == 9380451  ~ "4",
    deviceName == 9380265  ~ "5",
    deviceName == 9380470  ~ "6",
    TRUE                      ~ "other"
    
    
  ))
#only keep the collar that sue said:)
GPS <- GPS %>%
  filter(Sheep_ID != "other")

## ok lets just remove the Nulls
GPS <- GPS %>% 
  filter(fencesID!= "NULL")

## reorder the clms
# GPS <- GPS %>% 
#   dplyr::select(ID_jaxs,Sheep_ID, deviceUIDHex:local_time)




str(GPS)



############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
str(GPS)

#turn into spatial data
## remove null values in coodinates
GPS <- GPS %>% 
  filter(!is.na(gpsData.lng))

#turn into spatial data
GPS_sf <-
  st_as_sf(GPS,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(GPS_sf, crs = 28355)


rm(GPS_sf)


str(GPS_sf_trans)

############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

Chiswick_hard_fence_bound <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final.shp")  # this is the hard fences
Chiswick_hard_fence_bound <-
  st_transform(Chiswick_hard_fence_bound, crs = 28355)

Chiswick_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final_buff10.shp")  # this is the 
Chiswick_hard_fence_bound_buff <-
  st_transform(Chiswick_hard_fence_bound_buff, crs = 28355)


VF_paddock <-   st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/VF_paddock.shp")
VF_paddock <-  st_transform(VF_paddock, crs = 28355)

water_pt <-  st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/water_pt.shp")



ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = GPS_sf_trans ,alpha = 0.03) +
  theme_bw()+
  facet_wrap(.~ date)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "")


################################################################################
#### filtering out GPS data based on times start and end of the trial

GPS_sf_trans <- GPS_sf_trans %>% 
  filter(
    local_time >=  ymd_hms("2022-06-28 09:50:00", tz= "Australia/Sydney"))

GPS_sf_trans <- GPS_sf_trans %>% 
  filter(
    local_time <=  ymd_hms("2022-07-02 10:10:00", tz= "Australia/Sydney"))


### define a training period with new clm No training period aniamls were allowed to acclimatise in neighboring paddock

# GPS_sf_trans <- GPS_sf_trans %>% 
#   mutate(training_period = case_when(
#     local_time <= ymd_hms("2022-10-17 13:10:00", tz= "Australia/Sydney")~ "training",
#     TRUE                      ~ "non_training"
#     
#   ))


# Times sheep were brought in each day for the VF Chiswick trial;
# 28/6- sheep out 9:50
# 29/6 11:21- 12:21
# 30/6 10:34- 11:36
# 1/7- 10:37- 11:20
# 2/7- Brought in at 10:10
#### each day the animals were yarded so i need to remove this data

# let divide the data per day
day_28 <- GPS_sf_trans %>%  filter(date == "2022-06-28")
day_29 <- GPS_sf_trans %>%  filter(date == "2022-06-29")
day_30 <- GPS_sf_trans %>%  filter(date == "2022-06-30")
day_1 <- GPS_sf_trans %>%  filter(date == "2022-07-01")
day_2 <- GPS_sf_trans %>%  filter(date == "2022-07-02")


# keep everything after before yarding and after yarding

day_29_before_yarding <- day_29 %>%
  filter(local_time <=  ymd_hms("2022-06-29 11:21:00", tz = "Australia/Sydney"))
day_29_after_yarding <- day_29 %>%
  filter(local_time >=  ymd_hms("2022-06-29 12:21:00", tz = "Australia/Sydney"))

day_29_clean <- rbind(day_29_before_yarding, day_29_after_yarding)
rm(day_29_before_yarding, day_29_after_yarding, day_29)


day_30_before_yarding <- day_30 %>%
  filter(local_time <=  ymd_hms("2022-06-30 10:34:00", tz = "Australia/Sydney"))
day_30_after_yarding <- day_30 %>%
  filter(local_time >=  ymd_hms("2022-06-30 11:36:00", tz = "Australia/Sydney"))

day_30_clean <- rbind(day_30_before_yarding, day_30_after_yarding)
rm(day_30_before_yarding, day_30_after_yarding, day_30)

day_1_before_yarding <- day_1 %>%
  filter(local_time <=  ymd_hms("2022-07-01 10:37:00", tz = "Australia/Sydney"))
day_1_after_yarding <- day_1 %>%
  filter(local_time >=  ymd_hms("2022-07-01 11:20:00", tz = "Australia/Sydney"))

day_1_clean <- rbind(day_1_before_yarding, day_1_after_yarding)
rm(day_1_before_yarding, day_1_after_yarding, day_1)


### put it back togther 

animals_GPS_trim_time <- rbind(day_28, day_29_clean, day_30_clean, day_1_clean, day_2)

rm(day_28, day_29_clean, day_30_clean, day_1_clean, day_2)

########################################################################################



########################################################################################

### remove the water and other animals logs

unique(animals_GPS_trim_time$Sheep_ID)

animals_GPS_trim_time <- animals_GPS_trim_time %>% 
  filter(Sheep_ID !=  "other") %>% 
  filter(Sheep_ID !=  "water_pt")




## check

ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = animals_GPS_trim_time ,alpha = 0.03) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "All animal logs trimmed time",
       subtitle = "log when animals were yarded removed")



# -------------------------------------------------------------------------------------------------- ###


#I think this should be the end of step 1.


#Next steps boundaries trim location and time
## merge in other animal data
## look for weather data ? anything else??




########################################################################################################



output_path <- "W:/VF/Optimising_VF/Chiswick/data_prep"  #animals_GPS_trim_time


############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## convert the geom clm into x and y clms
str(animals_GPS_trim_time)

coordinates <-as.data.frame( st_coordinates(animals_GPS_trim_time))
GPS_trim_time_df <- as.data.frame(animals_GPS_trim_time)

GPS_trim_time_df <- GPS_trim_time_df %>% 
  dplyr::select(-"geometry")


GPS_trim_time <-   cbind(GPS_trim_time_df,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


GPS_trim_time$local_time <-   format(GPS_trim_time$local_time, usetz=TRUE)
GPS_trim_time$GMT        <-   format(GPS_trim_time$GMT, usetz=TRUE)
GPS_trim_time$start_fence <-  format(GPS_trim_time$start_fence, usetz=TRUE)
GPS_trim_time$end_fence    <- format(GPS_trim_time$end_fence, usetz=TRUE)
GPS_trim_time$start_trial    <- format(GPS_trim_time$start_trial, usetz=TRUE)

write.csv(GPS_trim_time, 
          paste0(output_path,"/Step1b_animals_GPS_trim_time.csv"), 
          row.names=FALSE)
#############################################################



