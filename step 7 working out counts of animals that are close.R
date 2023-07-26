library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library("readxl")


#####################################################################################
### number of other animals close to animal min dist is 1.5 - this can be changed ###
#####################################################################################



Min_dist <-1.5 #the filter for the min distance between animals



list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")
#list_of_comparsions <- list_of_comparsions %>%  filter(replication == "replication1") #nope I thgink I want all the replication
list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping

list_of_comparsions <- list_of_comparsions %>% mutate(comparison = paste0("dist",comparison))




list_of_animal <- list_of_comparsions
list_of_animal <- list_of_animal %>%  distinct(animal, .keep_all = TRUE)
list_of_animal <- as.list(list_of_animal)
list_of_animal <- c(list_of_animal$animal )



### ---- VFx      ----####
matrix <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF1_step6_dist_between_animals_matrix.rds") 

reg_time_step <- matrix %>%   distinct(time_step) #creates a df with regular time step
reg_time_step <- reg_time_step %>% 
  mutate(date = date(time_step))

df <- data.frame(reg_time_step=POSIXct(),
                 numb_animal_close=integer(),
                 animal=character()) #df for the loop to write to

### loop use a list of animal as a variable to create a list and the list becomes the input for the loop
for (list_of_animal in list_of_animal){
  

animal_list_df_x <- list_of_comparsions %>%  filter(animal == list_of_animal) %>% select(comparison)
animal_list_x <- as.list(animal_list_df_x)
animal_list_comparisonx <- c(animal_list_x$comparison)
names(matrix)
animal_x <-   matrix %>%  dplyr::select(all_of(animal_list_comparisonx)) #remove the time step and only select clms that relate to the animal

animal_x[animal_x > Min_dist] <- NA #replace values >1.5 with NA

animal_x <- animal_x %>%  
  dplyr::mutate(count = ncol(animal_x) - rowSums(is.na(animal_x) | animal_x == ""))  %>% 
  dplyr::select(count )

animal_matrix <-   matrix %>%  dplyr::select(time_step)
animal_matrix <- cbind(animal_matrix, animal_x)


animal_matrix <- animal_matrix %>%
  rename(!!paste0(paste0("numb_animal_close")) := count) %>% 
  mutate(animal = list_of_animal)
 

df_temp = left_join(reg_time_step, animal_matrix)

df <- rbind(df, df_temp) 

}    


saveRDS(df,   "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF1_step7_count_close_animals.rds")
write.csv(df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF1_step7_count_close_animals.csv")

rm(list=ls())


#################################################################################
### ---- VFx      ----####
Min_dist <-1.5 #the filter for the min distance between animals

list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")
#list_of_comparsions <- list_of_comparsions %>%  filter(replication == "replication1") #nope I thgink I want all the replication
list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping

list_of_comparsions <- list_of_comparsions %>% mutate(comparison = paste0("dist",comparison))




list_of_animal <- list_of_comparsions
list_of_animal <- list_of_animal %>%  distinct(animal, .keep_all = TRUE)
list_of_animal <- as.list(list_of_animal)
list_of_animal <- c(list_of_animal$animal )

matrix <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF2_step6_dist_between_animals_matrix.rds") 

reg_time_step <- matrix %>%   distinct(time_step) #creates a df with regular time step
reg_time_step <- reg_time_step %>% 
  mutate(date = date(time_step))

df <- data.frame(reg_time_step=POSIXct(),
                 numb_animal_close=integer(),
                 animal=character()) #df for the loop to write to

### loop use a list of animal as a variable to create a list and the list becomes the input for the loop
for (list_of_animal in list_of_animal){
  
  
  animal_list_df_x <- list_of_comparsions %>%  filter(animal == list_of_animal) %>% select(comparison)
  animal_list_x <- as.list(animal_list_df_x)
  animal_list_comparisonx <- c(animal_list_x$comparison)
  names(matrix)
  animal_x <-   matrix %>%  dplyr::select(all_of(animal_list_comparisonx)) #remove the time step and only select clms that relate to the animal
  
  animal_x[animal_x > Min_dist] <- NA #replace values >1.5 with NA
  
  animal_x <- animal_x %>%  
    dplyr::mutate(count = ncol(animal_x) - rowSums(is.na(animal_x) | animal_x == ""))  %>% 
    dplyr::select(count )
  
  animal_matrix <-   matrix %>%  dplyr::select(time_step)
  animal_matrix <- cbind(animal_matrix, animal_x)
  
  
  animal_matrix <- animal_matrix %>%
    rename(!!paste0(paste0("numb_animal_close")) := count) %>% 
    mutate(animal = list_of_animal)
  
  
  df_temp = left_join(reg_time_step, animal_matrix)
  
  df <- rbind(df, df_temp) 
  
}    


saveRDS(df,   "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF2_step7_count_close_animals.rds")
write.csv(df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF2_step7_count_close_animals.csv")

rm(list=ls())

#################################################################################
### ---- VFx      ----####
Min_dist <-1.5 #the filter for the min distance between animals

list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")
#list_of_comparsions <- list_of_comparsions %>%  filter(replication == "replication1") #nope I thgink I want all the replication
list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping

list_of_comparsions <- list_of_comparsions %>% mutate(comparison = paste0("dist",comparison))




list_of_animal <- list_of_comparsions
list_of_animal <- list_of_animal %>%  distinct(animal, .keep_all = TRUE)
list_of_animal <- as.list(list_of_animal)
list_of_animal <- c(list_of_animal$animal )

matrix <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF3_step6_dist_between_animals_matrix.rds") 

reg_time_step <- matrix %>%   distinct(time_step) #creates a df with regular time step
reg_time_step <- reg_time_step %>% 
  mutate(date = date(time_step))

df <- data.frame(reg_time_step=POSIXct(),
                 numb_animal_close=integer(),
                 animal=character()) #df for the loop to write to

### loop use a list of animal as a variable to create a list and the list becomes the input for the loop
for (list_of_animal in list_of_animal){
  
  
  animal_list_df_x <- list_of_comparsions %>%  filter(animal == list_of_animal) %>% select(comparison)
  animal_list_x <- as.list(animal_list_df_x)
  animal_list_comparisonx <- c(animal_list_x$comparison)
  names(matrix)
  animal_x <-   matrix %>%  dplyr::select(all_of(animal_list_comparisonx)) #remove the time step and only select clms that relate to the animal
  
  animal_x[animal_x > Min_dist] <- NA #replace values >1.5 with NA
  
  animal_x <- animal_x %>%  
    dplyr::mutate(count = ncol(animal_x) - rowSums(is.na(animal_x) | animal_x == ""))  %>% 
    dplyr::select(count )
  
  animal_matrix <-   matrix %>%  dplyr::select(time_step)
  animal_matrix <- cbind(animal_matrix, animal_x)
  
  
  animal_matrix <- animal_matrix %>%
    rename(!!paste0(paste0("numb_animal_close")) := count) %>% 
    mutate(animal = list_of_animal)
  
  
  df_temp = left_join(reg_time_step, animal_matrix)
  
  df <- rbind(df, df_temp) 
  
}    


saveRDS(df,   "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF3_step7_count_close_animals.rds")
write.csv(df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF3_step7_count_close_animals.csv")

rm(list=ls())

#################################################################################
### ---- VFx      ----####
Min_dist <-1.5 #the filter for the min distance between animals

list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")
#list_of_comparsions <- list_of_comparsions %>%  filter(replication == "replication1") #nope I thgink I want all the replication
list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping

list_of_comparsions <- list_of_comparsions %>% mutate(comparison = paste0("dist",comparison))




list_of_animal <- list_of_comparsions
list_of_animal <- list_of_animal %>%  distinct(animal, .keep_all = TRUE)
list_of_animal <- as.list(list_of_animal)
list_of_animal <- c(list_of_animal$animal )

matrix <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF4_step6_dist_between_animals_matrix.rds") 

reg_time_step <- matrix %>%   distinct(time_step) #creates a df with regular time step
reg_time_step <- reg_time_step %>% 
  mutate(date = date(time_step))

df <- data.frame(reg_time_step=POSIXct(),
                 numb_animal_close=integer(),
                 animal=character()) #df for the loop to write to

### loop use a list of animal as a variable to create a list and the list becomes the input for the loop
for (list_of_animal in list_of_animal){
  
  
  animal_list_df_x <- list_of_comparsions %>%  filter(animal == list_of_animal) %>% select(comparison)
  animal_list_x <- as.list(animal_list_df_x)
  animal_list_comparisonx <- c(animal_list_x$comparison)
  names(matrix)
  animal_x <-   matrix %>%  dplyr::select(all_of(animal_list_comparisonx)) #remove the time step and only select clms that relate to the animal
  
  animal_x[animal_x > Min_dist] <- NA #replace values >1.5 with NA
  
  animal_x <- animal_x %>%  
    dplyr::mutate(count = ncol(animal_x) - rowSums(is.na(animal_x) | animal_x == ""))  %>% 
    dplyr::select(count )
  
  animal_matrix <-   matrix %>%  dplyr::select(time_step)
  animal_matrix <- cbind(animal_matrix, animal_x)
  
  
  animal_matrix <- animal_matrix %>%
    rename(!!paste0(paste0("numb_animal_close")) := count) %>% 
    mutate(animal = list_of_animal)
  
  
  df_temp = left_join(reg_time_step, animal_matrix)
  
  df <- rbind(df, df_temp) 
  
}    


saveRDS(df,   "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF4_step7_count_close_animals.rds")
write.csv(df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF4_step7_count_close_animals.csv")

rm(list=ls())

#################################################################################
### ---- VFx      ----####
Min_dist <-1.5 #the filter for the min distance between animals

list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")
#list_of_comparsions <- list_of_comparsions %>%  filter(replication == "replication1") #nope I thgink I want all the replication
list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping

list_of_comparsions <- list_of_comparsions %>% mutate(comparison = paste0("dist",comparison))




list_of_animal <- list_of_comparsions
list_of_animal <- list_of_animal %>%  distinct(animal, .keep_all = TRUE)
list_of_animal <- as.list(list_of_animal)
list_of_animal <- c(list_of_animal$animal )

matrix <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF5_step6_dist_between_animals_matrix.rds") 

reg_time_step <- matrix %>%   distinct(time_step) #creates a df with regular time step
reg_time_step <- reg_time_step %>% 
  mutate(date = date(time_step))

df <- data.frame(reg_time_step=POSIXct(),
                 numb_animal_close=integer(),
                 animal=character()) #df for the loop to write to

### loop use a list of animal as a variable to create a list and the list becomes the input for the loop
for (list_of_animal in list_of_animal){
  
  
  animal_list_df_x <- list_of_comparsions %>%  filter(animal == list_of_animal) %>% select(comparison)
  animal_list_x <- as.list(animal_list_df_x)
  animal_list_comparisonx <- c(animal_list_x$comparison)
  names(matrix)
  animal_x <-   matrix %>%  dplyr::select(all_of(animal_list_comparisonx)) #remove the time step and only select clms that relate to the animal
  
  animal_x[animal_x > Min_dist] <- NA #replace values >1.5 with NA
  
  animal_x <- animal_x %>%  
    dplyr::mutate(count = ncol(animal_x) - rowSums(is.na(animal_x) | animal_x == ""))  %>% 
    dplyr::select(count )
  
  animal_matrix <-   matrix %>%  dplyr::select(time_step)
  animal_matrix <- cbind(animal_matrix, animal_x)
  
  
  animal_matrix <- animal_matrix %>%
    rename(!!paste0(paste0("numb_animal_close")) := count) %>% 
    mutate(animal = list_of_animal)
  
  
  df_temp = left_join(reg_time_step, animal_matrix)
  
  df <- rbind(df, df_temp) 
  
}    


saveRDS(df,   "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF5_step7_count_close_animals.rds")
write.csv(df,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step7/VF5_step7_count_close_animals.csv")

rm(list=ls())

