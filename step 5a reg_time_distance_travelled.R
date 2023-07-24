### reg time step and distance travelled

################################################################################
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
################################################################################



################################################################################
#### --------------    Bring in data   -------------- ####
################################################################################
GPS_Dist <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step4/VF1_step4.rds")

names(GPS_Dist)

### subset the data to the clms I want.

GPS_Dist <- GPS_Dist %>% dplyr::select (ID_jaxs, animal_ID, 
                                        #DOT, #I dont have this
                                        local_time, 
                                        date,
                                        DOY, 
                                        X , 
                                        Y, 
                                        dist_to_VF, 
                                        VF_EX,
                                        #Audio_values,#bring in at the next step 5b
                                        #Shock_values, #bring in at the next step 5b
                                        # resting_percentage, #my dataset doesnt have this
                                        # moving_percentage,
                                        # grazing_percentage 
                                       )

GPS_Dist$local_time <- as.POSIXct(GPS_Dist$local_time,  tz = "Australia/Adelaide")
GPS_Dist <- GPS_Dist %>%  rename(animal = animal_ID)
str(GPS_Dist)
################################################################################
#### --------------    what is the length of the trail for VF X?   -------------- ####
################################################################################


start <- min(GPS_Dist$local_time, na.rm = TRUE)  # "2019-05-20 20:10:17 ACST"
end <-   max(GPS_Dist$local_time, na.rm = TRUE) # ""2019-05-21 00:10:00 ACST"
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.
start <- round_date(start, unit="10 mins") #2022-06-28 10:00:00 ACDT"
end <- round_date(end, unit="10 mins") # "2022-07-02 10:10:00 ACDT"

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # "14400s (~4 hours)"

################################################################################
#### --------------    make a regular time step   -------------- ####
################################################################################


regular_time_interval <-data.frame(time_step = seq(from = ymd_hms(start),
                                                   to = ymd_hms(end), 
                                                   by = '10 mins'))
################################################################################
#### ----   Write out regular time step for later reference -------------- #####
################################################################################


saveRDS(regular_time_interval,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step5/VF1_regular_time_interval.rds")




################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS_Dist)
# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_animal = paste0(round_local_time,"_", animal) ,
                Time_animal_zone = paste0(round_local_time,"_", animal, "_", VF_EX))


rm(end,start, time.duration, time.interval)


###############################################################################  
## ----- function to produce steps per animal
################################################################################

animal_list <- GPS_Dist %>% distinct(animal) %>%  arrange(animal)
dim(animal_list)
### 20 animals ID I need regular time interval for each animal
### List of sites I want to run analysis for:
animal_list
#sheep_list <- c(1:6)
animal_list <- "Q10"

### as a function
for (animal_list in animal_list){
  
################################################################################  
#regular_time_interval_per_animal_ID
################################################################################
  regular_time_interval_animal <- regular_time_interval %>% 
    dplyr::mutate(Time_animal = paste0(time_step,"_", animal_list))
  
################################################################################
#### --------------    Join regular time step to dataset  -------------- ####
################################################################################  
  
  GPS_animal <- GPS_Dist %>%  filter(animal == animal_list)
  GPS_animal <- GPS_animal %>%
    dplyr::distinct(Time_animal_zone, .keep_all = TRUE)
  
  ## the occurrence of a duplicated time_animal
  
 # It might be a better to split the data into  outside_VF and inside_VF
  
  #outside_VF <- GPS_animal %>% filter(VF_EX == "outside_VF") %>% dplyr::distinct(Time_animal, .keep_all = TRUE) #
  #inside_VF <- GPS_animal %>% filter(VF_EX == "inside_VF") %>% dplyr::distinct(Time_animal, .keep_all = TRUE) #
  
  ##try this to retain the max distance from the VF when outside the grazing zone and middle distance when inside get the ID values only
  outside_VF_ID_retain <- GPS_animal %>% filter(VF_EX == "outside_VF") %>% 
    group_by(Time_animal, ID_jaxs) %>% 
    summarise(dist = max(dist_to_VF, na.rm = TRUE))
  outside_VF_ID_retain <- as.list(outside_VF_ID_retain$ID_jaxs)
  
  outside_VF <- GPS_animal %>% filter(VF_EX == "outside_VF") %>% filter(ID_jaxs %in% outside_VF_ID_retain)
  
  
  inside_VF_ID_retain <- GPS_animal %>% filter(VF_EX == "inside_VF") %>% 
    group_by(Time_animal, ID_jaxs) %>% 
    summarise(dist = median(dist_to_VF, na.rm = TRUE))
  inside_VF_ID_retain <- as.list(inside_VF_ID_retain$ID_jaxs)
  
  inside_VF <- GPS_animal %>% filter(VF_EX == "inside_VF") %>% filter(ID_jaxs %in% inside_VF_ID_retain)
  
  
  
  
  GPS_animal <- rbind(outside_VF,inside_VF )
  
  duplication_report <- GPS_animal %>% count(Time_animal)
   
   GPS_animal <- left_join(GPS_animal,duplication_report ) %>% rename(occurance = n )
   str(GPS_animal)
  # 
   GPS_animal <- GPS_animal %>% mutate(
     what_to_retain = case_when(
       occurance == 1 & VF_EX == "outside_VF" ~ "retain",
       occurance == 2 & VF_EX == "outside_VF" ~ "retain", #
       occurance == 1 & VF_EX == "inside_VF" ~ "retain",
       TRUE                      ~ "discard"
     )
   ) 
  
  # remove the rows tp discard
  GPS_animal <- GPS_animal %>% filter(what_to_retain == "retain")
  
  GPS_animal_reg_time <- left_join(regular_time_interval_animal, GPS_animal)

  #### Trim the regular time step to match the end of animal time

  start_animal <- min(GPS_animal$local_time, na.rm = TRUE)  
  end_animal <-   max(GPS_animal$local_time, na.rm = TRUE) 
  start_animal <- round_date(start_animal, unit="10 mins")
  end_animal <- round_date(end_animal, unit="10 mins") 
  
  ## trim the joined data to the animal ID time in the trial 
  
  
  GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    dplyr::filter(between(time_step, ymd_hms(start_animal), ymd_hms(end_animal))) 
 
  
  ################################################################################
  #### Do some cals  steps or distance travelled since last logged point ---- ####
  ################################################################################ 

    GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    arrange(local_time)
  
  GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    dplyr::mutate(step = sqrt( ((lead(X) - X)^ 2) + ((lead(Y) - Y)^ 2) ) )
  
  rm(
    GPS_animal,
    regular_time_interval_animal
    )
  name <- paste0("GPS_animal_reg_time_step", animal_list)
  assign(name,GPS_animal_reg_time)
  
  }       


## --- up to here ----###
file_list <- data.frame(name_df = paste0("GPS_animal_reg_time_step",c(1:10)))






GPS_sheep_reg_time_step_all <- rbind(
  GPS_sheep_reg_time_step1,
  GPS_sheep_reg_time_step2,
  GPS_sheep_reg_time_step3,
  GPS_sheep_reg_time_step4,
  GPS_sheep_reg_time_step5,
  GPS_sheep_reg_time_step6
  # GPS_sheep_reg_time_step7,
  # GPS_sheep_reg_time_step8,
  # GPS_sheep_reg_time_step9,
  # GPS_sheep_reg_time_step10#,
  # GPS_sheep_reg_time_step11,
  # GPS_sheep_reg_time_step12,
  # GPS_sheep_reg_time_step13,
  # GPS_sheep_reg_time_step14,
  # GPS_sheep_reg_time_step15,
  # GPS_sheep_reg_time_step16,
  # GPS_sheep_reg_time_step17,
  # GPS_sheep_reg_time_step18,
  # GPS_sheep_reg_time_step19,
  # GPS_sheep_reg_time_step20,
  # GPS_sheep_reg_time_step21,
  # GPS_sheep_reg_time_step22,
  # GPS_sheep_reg_time_step23,
  # GPS_sheep_reg_time_step24,
  # GPS_sheep_reg_time_step25,
  # GPS_sheep_reg_time_step26,
  # GPS_sheep_reg_time_step27,
  # GPS_sheep_reg_time_step28,
  # GPS_sheep_reg_time_step29,
  # GPS_sheep_reg_time_step30,
  # GPS_sheep_reg_time_step31,
  # GPS_sheep_reg_time_step32,
  # GPS_sheep_reg_time_step33,
  # GPS_sheep_reg_time_step34,
  # GPS_sheep_reg_time_step35,
  # GPS_sheep_reg_time_step36
  )


##############################################################################
### create a df with the time interval for each animal This is to see what time step all animals have in common not used later#####
names(GPS_sheep_reg_time_step1)
names(regular_time_interval)

##animal 1 ###
test1  <- GPS_sheep_reg_time_step1 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep1 =ID_jaxs)

GPS_animal_reg_time <- left_join(regular_time_interval, test1)

##animal 2 ###  
test2  <- GPS_sheep_reg_time_step2 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep2 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test2)

##animal 3 ###  
test3  <- GPS_sheep_reg_time_step3 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep3 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test3)

##animal 4 ###  
test4  <- GPS_sheep_reg_time_step4 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep4 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test4)

##animal 5 ###  
test5  <- GPS_sheep_reg_time_step5 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep5 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test5)

##animal 6 ###  
test6  <- GPS_sheep_reg_time_step6 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep6 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test6)

# ##animal 7 ###  
# test7  <- GPS_sheep_reg_time_step7 %>% 
#   select(time_step, ID_jaxs) %>% 
#   filter(!is.na(ID_jaxs)) %>% 
#   rename(sheep7 =ID_jaxs)
# GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test7)
# 
# 
# ##animal 8 ###  
# test8  <- GPS_sheep_reg_time_step8 %>% 
#   select(time_step, ID_jaxs) %>% 
#   filter(!is.na(ID_jaxs)) %>% 
#   rename(sheep8 =ID_jaxs)
# GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test8)
# 
# ##animal 9 ###  
# test9  <- GPS_sheep_reg_time_step9 %>% 
#   select(time_step, ID_jaxs) %>% 
#   filter(!is.na(ID_jaxs)) %>% 
#   rename(sheep9 =ID_jaxs)
# GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test9)
# 
# ##animal 10 ###  
# test10  <- GPS_sheep_reg_time_step10 %>% 
#   select(time_step, ID_jaxs) %>% 
#   filter(!is.na(ID_jaxs)) %>% 
#   rename(sheep10 =ID_jaxs)
# GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test10)


GPS_animal_reg_time <- GPS_animal_reg_time %>% replace(is.na(.), "no_records")

GPS_animal_reg_time <- GPS_animal_reg_time %>%
  mutate(count_na = 
           str_count(sheep1, "no_records") + str_count(sheep2, "no_records") + str_count(sheep3, "no_records")+
           str_count(sheep4,"no_records") + str_count(sheep5, "no_records") + str_count(sheep6, "no_records")#+
           # str_count(sheep7, "no_records") + str_count(sheep8, "no_records") + str_count(sheep9, "no_records")+
           # str_count(sheep10, "no_records"
           ) 
         

rm(test1, test2, test3, test4, test5, test6, test7, test8, test9, test10)

GPS_animal_reg_time <- GPS_animal_reg_time %>% 
  filter(count_na == 0)

GPS_animal_reg_time <- GPS_animal_reg_time %>% 
  select(time_step)

################################################################################
#### ----   Write out regular time step for later reference -------------- #####
################################################################################

write.csv(GPS_animal_reg_time, "W:/VF/Optimising_VF/Chiswick/data_prep/GPS_animal_reg_time_common.csv", row.names = FALSE)




rm(GPS_sheep_reg_time_step1,
   GPS_sheep_reg_time_step2,
   GPS_sheep_reg_time_step3,
   GPS_sheep_reg_time_step4,
   GPS_sheep_reg_time_step5,
   GPS_sheep_reg_time_step6,
   GPS_sheep_reg_time_step7,
   GPS_sheep_reg_time_step8,
   GPS_sheep_reg_time_step9,
   GPS_sheep_reg_time_step10#,
   # GPS_sheep_reg_time_step11,
   # GPS_sheep_reg_time_step12,
   # GPS_sheep_reg_time_step13,
   # GPS_sheep_reg_time_step14,
   # GPS_sheep_reg_time_step15,
   # GPS_sheep_reg_time_step16,
   # GPS_sheep_reg_time_step17,
   # GPS_sheep_reg_time_step18,
   # GPS_sheep_reg_time_step19,
   # GPS_sheep_reg_time_step20,
   # GPS_sheep_reg_time_step21,
   # GPS_sheep_reg_time_step22,
   # GPS_sheep_reg_time_step23,
   # GPS_sheep_reg_time_step24,
   # GPS_sheep_reg_time_step25,
   # GPS_sheep_reg_time_step26,
   # GPS_sheep_reg_time_step27,
   # GPS_sheep_reg_time_step28,
   # GPS_sheep_reg_time_step29,
   # GPS_sheep_reg_time_step30,
   # GPS_sheep_reg_time_step31,
   # GPS_sheep_reg_time_step32,
   # GPS_sheep_reg_time_step33,
   # GPS_sheep_reg_time_step34,
   # GPS_sheep_reg_time_step35,
   # GPS_sheep_reg_time_step36
   )

##### Think about how to deal with yarding times - I think you may need to remove first step cal after yarding time.
## I dont think I need to do anything here - the animals we yarded over night and the data is trimmed to this already.
### If you had a yard in and out and the data wasnt trimmed this might be a problem.


# It looks to be ok because the GPS log data has it removed already and  I am using this file to join the regular time step to the GPS data





output_path <- "W:/VF/Optimising_VF/Chiswick/data_prep/"  #animals_GPS_trim_time


write.csv(GPS_sheep_reg_time_step_all, 
          paste0(output_path,"/step5_Greg_time_step_dist_travelled.csv"), 
          row.names=FALSE)







