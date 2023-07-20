
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

Hard_fence_bound <- st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/EdenValley_site1GDA_a.shp")  # this is the hard fences
Hard_fence_bound <-
  st_transform(Hard_fence_bound, crs = 28354)

# Chiswick_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final_buff10.shp")  # this is the 
# Chiswick_hard_fence_bound_buff <-
#   st_transform(Chiswick_hard_fence_bound_buff, crs = 28354)


VF1_paddock <-   st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF1_graze.shp")
# VF_paddock <-  st_transform(VF_paddock, crs = 28354)




############################################################################################
############       bring in data created in step1a           ##############################
############################################################################################

path_step1 <- "W:/VF/Optimising_VF/Eden Valley/data_prep/"
raw_data <- "W:/VF/Optimising_VF/Eden Valley/data_prep/step1"


GPS <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF1_InclusionBord.rds")



str(GPS)


#format time and date clm from character to time
GPS <-  GPS %>%
  mutate(timeOfEvent = as.POSIXct(time, tz = "GMT", format = "%d/%m/%Y %H:%M"))


GPS <- GPS %>% 
  mutate(GMT = ymd_hms(time, tz = "GMT"))

GPS <- GPS %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))

## Add a clm for ID_jaxs
GPS <- GPS %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF1")


### what are the fences called in this dataset?
unique(GPS$fencesID) # we only have 3 "NULL"  "1dd82" "1f1eb" "1766d"


## reorder the clms



GPS <- GPS %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


#############################################################################################
####    Assign collar to animal names - I did this in step 1#####
u
#only keep records with animal ID
GPS <- GPS %>%
  filter(animal_ID != "other")

## ok lets just remove the Nulls
GPS <- GPS %>% 
  filter(fencesID!= "NULL")


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
           coords = c("lon", "lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(GPS_sf, crs = 28354)


rm(GPS_sf)


str(GPS_sf_trans)





ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF1_paddock, color = "black", fill = NA) +
  geom_sf(data = GPS_sf_trans ,alpha = 0.03) +
  theme_bw()+
  facet_wrap(.~ date)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "")


################################################################################
#### filtering out GPS data based on times start and end of the trial # I have done this already###





