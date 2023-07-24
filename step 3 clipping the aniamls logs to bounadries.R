## step 3 clipping the data - This has not been run yet

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



################################################################
### Clip to the VF1 hard fences  with    #########
################################################################


step1_2 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF1_InclusionBord.rds")
str(step1_2)

## Add a clm for ID_jaxs
step1_2 <- step1_2 %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF1")


step1_2 <- step1_2 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))





#turn into spatial data

step1_2_sf <-
  st_as_sf(step1_2,
           coords = c("lon", "lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(step1_2_sf, crs = 28354)





#To the large block boundary with buffer
step1_2_sf_clip <-
  st_intersection(GPS_sf_trans, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication




# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF1_paddock, color = "black", fill = NA) +
#    geom_sf(data = step1_2_sf_clip ,alpha = 0.03) +
#    theme_bw()+
#    facet_wrap(.~ date)+
#    theme(legend.position = "none",
#          axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#    labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip))
step1_2_sf_clip_df <- as.data.frame(step1_2_sf_clip)

step1_2_sf_clip_df <- step1_2_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_df <-   cbind(step1_2_sf_clip_df,coordinates )






path_output_files <- "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/" 
path_output_files
saveRDS(step1_2_sf_clip_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF1step3_clip.rds")

rm(step1_2, step1_2_sf, step1_2_sf_clip, step1_2_sf_clip_df)



###############################################################################
#### VF 2 


step1_2 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF2_InclusionBord.rds")

#format time and date clm from character to time

## Add a clm for ID_jaxs
step1_2 <- step1_2 %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF2")


step1_2 <- step1_2 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))





#turn into spatial data

step1_2_sf <-
  st_as_sf(step1_2,
           coords = c("lon", "lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(step1_2_sf, crs = 28354)





#To the large block boundary with buffer
step1_2_sf_clip <-
  st_intersection(GPS_sf_trans, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication




# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF1_paddock, color = "black", fill = NA) +
#   geom_sf(data = step1_2_sf_clip ,alpha = 0.03) +
#   theme_bw()+
#   facet_wrap(.~ date)+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip))
step1_2_sf_clip_df <- as.data.frame(step1_2_sf_clip)

step1_2_sf_clip_df <- step1_2_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_df <-   cbind(step1_2_sf_clip_df,coordinates )

path_output_files <- "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/" 
path_output_files
saveRDS(step1_2_sf_clip_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF2step3_clip.rds")

rm(step1_2, step1_2_sf, step1_2_sf_clip, step1_2_sf_clip_df)

###############################################################################
#### VF 3 


step1_2 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF3_InclusionBord.rds")

#format time and date clm from character to time


## Add a clm for ID_jaxs
step1_2 <- step1_2 %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF3")


step1_2 <- step1_2 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))





#turn into spatial data

step1_2_sf <-
  st_as_sf(step1_2,
           coords = c("lon", "lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(step1_2_sf, crs = 28354)





#To the large block boundary with buffer
step1_2_sf_clip <-
  st_intersection(GPS_sf_trans, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication




# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF1_paddock, color = "black", fill = NA) +
#   geom_sf(data = step1_2_sf_clip ,alpha = 0.03) +
#   theme_bw()+
#   facet_wrap(.~ date)+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip))
step1_2_sf_clip_df <- as.data.frame(step1_2_sf_clip)

step1_2_sf_clip_df <- step1_2_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_df <-   cbind(step1_2_sf_clip_df,coordinates )

path_output_files <- "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/" 
path_output_files

saveRDS(step1_2_sf_clip_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF3step3_clip.rds")
rm(step1_2, step1_2_sf, step1_2_sf_clip, step1_2_sf_clip_df)



###############################################################################
#### VF 4 


step1_2 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF4_InclusionBord.rds")

#format time and date clm from character to time

## Add a clm for ID_jaxs
step1_2 <- step1_2 %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF4")


step1_2 <- step1_2 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))





#turn into spatial data

step1_2_sf <-
  st_as_sf(step1_2,
           coords = c("lon", "lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(step1_2_sf, crs = 28354)





#To the large block boundary with buffer
step1_2_sf_clip <-
  st_intersection(GPS_sf_trans, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication




# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF1_paddock, color = "black", fill = NA) +
#   geom_sf(data = step1_2_sf_clip ,alpha = 0.03) +
#   theme_bw()+
#   facet_wrap(.~ date)+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip))
step1_2_sf_clip_df <- as.data.frame(step1_2_sf_clip)

step1_2_sf_clip_df <- step1_2_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_df <-   cbind(step1_2_sf_clip_df,coordinates )

path_output_files <- "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/" 
path_output_files

saveRDS(step1_2_sf_clip_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF4step3_clip.rds")

rm(step1_2, step1_2_sf, step1_2_sf_clip, step1_2_sf_clip_df)


###############################################################################
#### VF 5 


step1_2 <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF5_InclusionBord.rds")

#format time and date clm from character to time


## Add a clm for ID_jaxs
step1_2 <- step1_2 %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF5")


step1_2 <- step1_2 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



glimpse(step1_2)

### remove missing values

step1_2 <- step1_2 %>% 
  filter(!is.na(lon))

#turn into spatial data

step1_2_sf <-
  st_as_sf(step1_2,
           coords = c("lon", "lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(step1_2_sf, crs = 28354)





#To the large block boundary with buffer
step1_2_sf_clip <-
  st_intersection(GPS_sf_trans, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication



#GPS_sf_trans %>% filter(date == "2019-06-28") %>% 
# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF1_paddock, color = "black", fill = NA) +
#   geom_sf(data = GPS_sf_trans ,alpha = 0.03) +
#   theme_bw()+
#   facet_wrap(.~ date)+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip))
step1_2_sf_clip_df <- as.data.frame(step1_2_sf_clip)

step1_2_sf_clip_df <- step1_2_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_df <-   cbind(step1_2_sf_clip_df,coordinates )

path_output_files <- "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/" 
path_output_files
# write.csv(step1_2_sf_clip_df, 
#           paste0(path_output_files,"/VF3step3_clip.csv"), 
#           row.names=FALSE)
saveRDS(step1_2_sf_clip_df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step3/VF5step3_clip.rds")

rm(step1_2, step1_2_sf, step1_2_sf_clip, step1_2_sf_clip_df)








