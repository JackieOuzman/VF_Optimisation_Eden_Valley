### working how to cal a distance from a line.
#https://gis.stackexchange.com/questions/360675/how-to-calculate-the-distance-of-points-to-line-in-r
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



##################################################################################
###########               VF1                      ##############################
##################################################################################
GPS <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF1step3_clip.rds")


#turn into spatial data
GPS <-   st_as_sf(GPS,
                         coords = c("X", "Y"),
                         crs = 28354,
                         agr = "constant")
names(GPS)
GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              time,
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              # Audio_values,
                              # Shock_values,
                              #cumulativeAudioCount, #do I need this one?
                              #cumulativeShockCount, #do I need this one?
                              event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              #training_period
                              #ID, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
                              )
names(GPS)




############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

Hard_fence_bound <- st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/EdenValley_site1GDA_a.shp")  # this is the hard fences
Hard_fence_bound <-
  st_transform(Hard_fence_bound, crs = 28354)


VF1_paddock <-   st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF1_graze.shp")
VF1_exclusion_zone <- st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF1_NonGraz.shp")
VF_1_line <-  st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/Fence1.shp")

############################################################################################

### check by plotting

str(GPS)


ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF1_paddock, color = "blue", fill = NA) +
  geom_sf(data = VF_1_line, color = "red", fill = NA) +
  
  geom_sf(data = GPS ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")


############################################################################################


GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_1_line))
############################################################################################
### report if the point is in the exclusion zone

VF1_paddock <- VF1_paddock %>%  dplyr::select(OID_, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF1_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF1_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:OID_,geometry, VF_EX)

names(VF_points)
names(Exclusion_points)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this has been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


saveRDS(GPS_all_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step4/VF1_step4.rds")

rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_1_line, VF1_exclusion_zone, VF1_paddock)

##################################################################################
###########               VF2                      ##############################
##################################################################################


GPS <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF2step3_clip.rds")
#turn into spatial data
GPS <-   st_as_sf(GPS,
                  coords = c("X", "Y"),
                  crs = 28354,
                  agr = "constant")
names(GPS)
GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              # Audio_values,
                              # Shock_values,
                              #cumulativeAudioCount, #do I need this one?
                              #cumulativeShockCount, #do I need this one?
                              event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              #training_period
                              #ID, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
)
names(GPS)




############################################################################################
############                  bring in boundaries             ##############################
############################################################################################


VF2_paddock <-   st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF2_graze.shp")
VF2_exclusion_zone <- st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF2_NonGraz.shp")
VF_2_line <-  st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/Fence2.shp")

############################################################################################

### check by plotting

str(GPS)


# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF2_paddock, color = "blue", fill = NA) +
#   geom_sf(data = VF_2_line, color = "red", fill = NA) +
#   
#   geom_sf(data = GPS ,alpha = 0.03) +
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "check")


############################################################################################


GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_2_line))
############################################################################################
### report if the point is in the exclusion zone

VF2_paddock <- VF2_paddock %>%  dplyr::select(OID_, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF2_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF2_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:OID_,geometry, VF_EX)

names(VF_points)
names(Exclusion_points)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this has been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


saveRDS(GPS_all_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step4/VF2_step4.rds")

rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_2_line, VF2_exclusion_zone, VF2_paddock)

##################################################################################
###########               VF3                      ##############################
##################################################################################


GPS <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF3step3_clip.rds")
#turn into spatial data
GPS <-   st_as_sf(GPS,
                  coords = c("X", "Y"),
                  crs = 28354,
                  agr = "constant")
names(GPS)
GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              # Audio_values,
                              # Shock_values,
                              #cumulativeAudioCount, #do I need this one?
                              #cumulativeShockCount, #do I need this one?
                              event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              #training_period
                              #ID, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
)
names(GPS)




############################################################################################
############                  bring in boundaries             ##############################
############################################################################################


VF3_paddock <-   st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF3_graze.shp")
VF3_exclusion_zone <- st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF3_NonGraz.shp")
VF_3_line <-  st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/Fence3.shp")

############################################################################################

### check by plotting

str(GPS)


# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF2_paddock, color = "blue", fill = NA) +
#   geom_sf(data = VF_2_line, color = "red", fill = NA) +
#   
#   geom_sf(data = GPS ,alpha = 0.03) +
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "check")


############################################################################################


GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_3_line))
############################################################################################
### report if the point is in the exclusion zone

VF3_paddock <- VF3_paddock %>%  dplyr::select(OID_, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF3_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF3_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:OID_,geometry, VF_EX)

names(VF_points)
names(Exclusion_points)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this hs been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


saveRDS(GPS_all_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step4/VF3_step4.rds")



rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_3_line, VF3_exclusion_zone, VF3_paddock)





##################################################################################
###########               VF4                      ##############################
##################################################################################


GPS <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF4step3_clip.rds")
#turn into spatial data
GPS <-   st_as_sf(GPS,
                  coords = c("X", "Y"),
                  crs = 28354,
                  agr = "constant")
names(GPS)
GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              # Audio_values,
                              # Shock_values,
                              #cumulativeAudioCount, #do I need this one?
                              #cumulativeShockCount, #do I need this one?
                              event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              #training_period
                              #ID, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
)
names(GPS)




############################################################################################
############                  bring in boundaries             ##############################
############################################################################################


VF4_paddock <-   st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF4_graze.shp")
VF4_exclusion_zone <- st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF4_NonGraz.shp")
VF_4_line <-  st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/Fence4a.shp")

############################################################################################

### check by plotting

str(GPS)


# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF2_paddock, color = "blue", fill = NA) +
#   geom_sf(data = VF_2_line, color = "red", fill = NA) +
#   
#   geom_sf(data = GPS ,alpha = 0.03) +
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "check")


############################################################################################


GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_4_line))
############################################################################################
### report if the point is in the exclusion zone

VF4_paddock <- VF4_paddock %>%  dplyr::select(OID_, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF4_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF4_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:OID_,geometry, VF_EX)

names(VF_points)
names(Exclusion_points)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this hs been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


saveRDS(GPS_all_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step4/VF4_step4.rds")



rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_4_line, VF4_exclusion_zone, VF4_paddock)



##################################################################################
###########               VF5                      ##############################
##################################################################################


GPS <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF5step3_clip.rds")
#turn into spatial data
GPS <-   st_as_sf(GPS,
                  coords = c("X", "Y"),
                  crs = 28354,
                  agr = "constant")
names(GPS)
GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              # Audio_values,
                              # Shock_values,
                              #cumulativeAudioCount, #do I need this one?
                              #cumulativeShockCount, #do I need this one?
                              event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              #training_period
                              #ID, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
)
names(GPS)




############################################################################################
############                  bring in boundaries             ##############################
############################################################################################


VF5_paddock <-   st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF5_graze.shp")
VF5_exclusion_zone <- st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/VF5_NonGraz.shp")
VF_5_line <-  st_read("W:/VF/Optimising_VF/raw_data/Eden Valley/VF_Boundary/Fence5.shp")

############################################################################################

### check by plotting

str(GPS)


# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF2_paddock, color = "blue", fill = NA) +
#   geom_sf(data = VF_2_line, color = "red", fill = NA) +
#   
#   geom_sf(data = GPS ,alpha = 0.03) +
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "check")


############################################################################################


GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_5_line))
############################################################################################
### report if the point is in the exclusion zone

VF5_paddock <- VF5_paddock %>%  dplyr::select(OID_, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF5_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF5_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:OID_,geometry, VF_EX)

names(VF_points)
names(Exclusion_points)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this hs been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


saveRDS(GPS_all_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step4/VF5_step4.rds")



rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_5_line, VF5_exclusion_zone, VF5_paddock)







