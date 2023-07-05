
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


############################################################################################
############       bring in data created in step1a           ##############################
############################################################################################

path_step1 <- "W:/VF/Optimising_VF/Chiswick/data_prep/"
raw_data <- "W:/VF/Optimising_VF/raw_data/Chiswick/"

animal_GPS_data_1 <- read_csv(paste0(raw_data, "db_trial_csiro_armidale_chiswick_mob_259_filtered.csv"))
animal_GPS_data_2 <- read_csv(paste0(raw_data, "db_trial_csiro_armidale_chiswick_neckband_serial_9380142_filtered.csv"))
GPS <- rbind(animal_GPS_data_1, animal_GPS_data_2)


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



