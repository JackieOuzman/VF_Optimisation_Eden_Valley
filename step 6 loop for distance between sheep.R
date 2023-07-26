library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(readxl)

################  VF 1 ################ 
  
GPS_animal <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF1_step5c.rds")
GPS_animal$local_time <- as.POSIXct(GPS_animal$local_time,  tz = "Australia/Adelaide")          
GPS_animal$time_step <- as.POSIXct(GPS_animal$time_step,  tz = "Australia/Adelaide") 

reg_time_step <- GPS_animal %>% 
  distinct(time_step)
df <- reg_time_step %>%  arrange(time_step)


list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")

#list_of_comparsions <- list_of_comparsions %>%  filter(replication == "replication1") #nope I think I want all the replication
list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping
animal_list_x <- list_of_comparsions_x



animal_list_x <- as.list(animal_list_x)
animal_list_x <- c(animal_list_x$comparison)

animal_list <- animal_list_x
df <- reg_time_step %>%  arrange(time_step)


#animal_list <- "distQ10vsQ108"


for (animal_list in animal_list){
  
  comparison_a_b <- as.data.frame(str_split(animal_list, "vs"),
                               col.names = "animal_ID" )
  
  comparison_a <- comparison_a_b[1,1]
  comparison_b <- comparison_a_b[2,1]

  GPS_animal_comparsion_a <- GPS_animal %>% filter(animal == comparison_a) %>% mutate(comparison_label = "a")
  GPS_animal_comparsion_b <- GPS_animal %>% filter(animal == comparison_b) %>% mutate(comparison_label = "b")
  
  GPS_animal_comparsion <- rbind(GPS_animal_comparsion_a, GPS_animal_comparsion_b)
  
  GPS_animal_comparsion <- GPS_animal_comparsion %>% dplyr::select(time_step, X, Y,comparison_label)
  
  str(GPS_animal_comparsion)
  
  ## make it wide
  GPS_animal_comparsion_wide <- GPS_animal_comparsion %>% 
    pivot_wider(names_from = comparison_label, values_from = c(X,Y))
    GPS_animal_comparsion_wide
  
    
    GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>% 
      dplyr::mutate(
    dist = sqrt(  ((X_a - X_b)^ 2) + (Y_a - Y_b)^ 2))
    
  
    GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%  dplyr::select(time_step , dist)
    GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%
        rename(!!paste0(paste0("dist",comparison_a,"vs", comparison_b)) := dist)
    
    df = left_join(df, GPS_animal_comparsion_wide)
    df <- df %>% arrange(time_step)
    rm(
      comparison_a,
      comparison_b,
      comparison_a_b,
      GPS_animal_comparsion_a,
      GPS_animal_comparsion_b,
      GPS_animal_comparsion,
      GPS_animal_comparsion_wide
    )  
}    



saveRDS(df,       "W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF1_step6_dist_between_animals_matrix.rds")
write.csv(matrix, "W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF1_step6_dist_between_animals_matrix.csv")
################  VF  ################ 

GPS_animal <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF2_step5c.rds")
GPS_animal$local_time <- as.POSIXct(GPS_animal$local_time,  tz = "Australia/Adelaide")          
GPS_animal$time_step <- as.POSIXct(GPS_animal$time_step,  tz = "Australia/Adelaide") 

reg_time_step <- GPS_animal %>% 
  distinct(time_step)
df <- reg_time_step %>%  arrange(time_step)


list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")


list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping
animal_list_x <- list_of_comparsions_x



animal_list_x <- as.list(animal_list_x)
animal_list_x <- c(animal_list_x$comparison)

animal_list <- animal_list_x
df <- reg_time_step %>%  arrange(time_step)

for (animal_list in animal_list){
  
  comparison_a_b <- as.data.frame(str_split(animal_list, "vs"),
                                  col.names = "animal_ID" )
  
  comparison_a <- comparison_a_b[1,1]
  comparison_b <- comparison_a_b[2,1]
  
  GPS_animal_comparsion_a <- GPS_animal %>% filter(animal == comparison_a) %>% mutate(comparison_label = "a")
  GPS_animal_comparsion_b <- GPS_animal %>% filter(animal == comparison_b) %>% mutate(comparison_label = "b")
  
  GPS_animal_comparsion <- rbind(GPS_animal_comparsion_a, GPS_animal_comparsion_b)
  
  GPS_animal_comparsion <- GPS_animal_comparsion %>% dplyr::select(time_step, X, Y,comparison_label)
  
  str(GPS_animal_comparsion)
  
  ## make it wide
  GPS_animal_comparsion_wide <- GPS_animal_comparsion %>% 
    pivot_wider(names_from = comparison_label, values_from = c(X,Y))
  GPS_animal_comparsion_wide
  
  
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>% 
    dplyr::mutate(
      dist = sqrt(  ((X_a - X_b)^ 2) + (Y_a - Y_b)^ 2))
  
  
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%  dplyr::select(time_step , dist)
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%
    rename(!!paste0(paste0("dist",comparison_a,"vs", comparison_b)) := dist)
  
  df = left_join(df, GPS_animal_comparsion_wide)
  df <- df %>% arrange(time_step)
  rm(
    comparison_a,
    comparison_b,
    comparison_a_b,
    GPS_animal_comparsion_a,
    GPS_animal_comparsion_b,
    GPS_animal_comparsion,
    GPS_animal_comparsion_wide
  )  
}    



saveRDS(df, "W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF2_step6_dist_between_animals_matrix.rds")
rm(list = ls())
################  VF3  ################ 

GPS_animal <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF3_step5c.rds")
GPS_animal$local_time <- as.POSIXct(GPS_animal$local_time,  tz = "Australia/Adelaide")          
GPS_animal$time_step <- as.POSIXct(GPS_animal$time_step,  tz = "Australia/Adelaide") 

reg_time_step <- GPS_animal %>% 
  distinct(time_step)
df <- reg_time_step %>%  arrange(time_step)


list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")


list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping
animal_list_x <- list_of_comparsions_x



animal_list_x <- as.list(animal_list_x)
animal_list_x <- c(animal_list_x$comparison)

animal_list <- animal_list_x
df <- reg_time_step %>%  arrange(time_step)

for (animal_list in animal_list){
  
  comparison_a_b <- as.data.frame(str_split(animal_list, "vs"),
                                  col.names = "animal_ID" )
  
  comparison_a <- comparison_a_b[1,1]
  comparison_b <- comparison_a_b[2,1]
  
  GPS_animal_comparsion_a <- GPS_animal %>% filter(animal == comparison_a) %>% mutate(comparison_label = "a")
  GPS_animal_comparsion_b <- GPS_animal %>% filter(animal == comparison_b) %>% mutate(comparison_label = "b")
  
  GPS_animal_comparsion <- rbind(GPS_animal_comparsion_a, GPS_animal_comparsion_b)
  
  GPS_animal_comparsion <- GPS_animal_comparsion %>% dplyr::select(time_step, X, Y,comparison_label)
  
  str(GPS_animal_comparsion)
  
  ## make it wide
  GPS_animal_comparsion_wide <- GPS_animal_comparsion %>% 
    pivot_wider(names_from = comparison_label, values_from = c(X,Y))
  GPS_animal_comparsion_wide
  
  
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>% 
    dplyr::mutate(
      dist = sqrt(  ((X_a - X_b)^ 2) + (Y_a - Y_b)^ 2))
  
  
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%  dplyr::select(time_step , dist)
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%
    rename(!!paste0(paste0("dist",comparison_a,"vs", comparison_b)) := dist)
  
  df = left_join(df, GPS_animal_comparsion_wide)
  df <- df %>% arrange(time_step)
  rm(
    comparison_a,
    comparison_b,
    comparison_a_b,
    GPS_animal_comparsion_a,
    GPS_animal_comparsion_b,
    GPS_animal_comparsion,
    GPS_animal_comparsion_wide
  )  
}    



saveRDS(df, "W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF3_step6_dist_between_animals_matrix.rds")
rm(list = ls())

################  VF4  ################ 

GPS_animal <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF4_step5c.rds")
GPS_animal$local_time <- as.POSIXct(GPS_animal$local_time,  tz = "Australia/Adelaide")          
GPS_animal$time_step <- as.POSIXct(GPS_animal$time_step,  tz = "Australia/Adelaide") 

reg_time_step <- GPS_animal %>% 
  distinct(time_step)
df <- reg_time_step %>%  arrange(time_step)


list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")


list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping
animal_list_x <- list_of_comparsions_x



animal_list_x <- as.list(animal_list_x)
animal_list_x <- c(animal_list_x$comparison)

animal_list <- animal_list_x
df <- reg_time_step %>%  arrange(time_step)

for (animal_list in animal_list){
  
  comparison_a_b <- as.data.frame(str_split(animal_list, "vs"),
                                  col.names = "animal_ID" )
  
  comparison_a <- comparison_a_b[1,1]
  comparison_b <- comparison_a_b[2,1]
  
  GPS_animal_comparsion_a <- GPS_animal %>% filter(animal == comparison_a) %>% mutate(comparison_label = "a")
  GPS_animal_comparsion_b <- GPS_animal %>% filter(animal == comparison_b) %>% mutate(comparison_label = "b")
  
  GPS_animal_comparsion <- rbind(GPS_animal_comparsion_a, GPS_animal_comparsion_b)
  
  GPS_animal_comparsion <- GPS_animal_comparsion %>% dplyr::select(time_step, X, Y,comparison_label)
  
  str(GPS_animal_comparsion)
  
  ## make it wide
  GPS_animal_comparsion_wide <- GPS_animal_comparsion %>% 
    pivot_wider(names_from = comparison_label, values_from = c(X,Y))
  GPS_animal_comparsion_wide
  
  
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>% 
    dplyr::mutate(
      dist = sqrt(  ((X_a - X_b)^ 2) + (Y_a - Y_b)^ 2))
  
  
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%  dplyr::select(time_step , dist)
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%
    rename(!!paste0(paste0("dist",comparison_a,"vs", comparison_b)) := dist)
  
  df = left_join(df, GPS_animal_comparsion_wide)
  df <- df %>% arrange(time_step)
  rm(
    comparison_a,
    comparison_b,
    comparison_a_b,
    GPS_animal_comparsion_a,
    GPS_animal_comparsion_b,
    GPS_animal_comparsion,
    GPS_animal_comparsion_wide
  )  
}    



saveRDS(df, "W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF4_step6_dist_between_animals_matrix.rds")
rm(list = ls())

################  VF5  ################ 

GPS_animal <- readRDS("W:/VF/Optimising_VF/Eden Valley/data_prep/step5c/VF5_step5c.rds")
GPS_animal$local_time <- as.POSIXct(GPS_animal$local_time,  tz = "Australia/Adelaide")          
GPS_animal$time_step <- as.POSIXct(GPS_animal$time_step,  tz = "Australia/Adelaide") 

reg_time_step <- GPS_animal %>% 
  distinct(time_step)
df <- reg_time_step %>%  arrange(time_step)


list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Eden Valley/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")


list_of_comparsions_x <- list_of_comparsions #I will use all the animal no treatment grouping
animal_list_x <- list_of_comparsions_x



animal_list_x <- as.list(animal_list_x)
animal_list_x <- c(animal_list_x$comparison)

animal_list <- animal_list_x
df <- reg_time_step %>%  arrange(time_step)

for (animal_list in animal_list){
  
  comparison_a_b <- as.data.frame(str_split(animal_list, "vs"),
                                  col.names = "animal_ID" )
  
  comparison_a <- comparison_a_b[1,1]
  comparison_b <- comparison_a_b[2,1]
  
  GPS_animal_comparsion_a <- GPS_animal %>% filter(animal == comparison_a) %>% mutate(comparison_label = "a")
  GPS_animal_comparsion_b <- GPS_animal %>% filter(animal == comparison_b) %>% mutate(comparison_label = "b")
  
  GPS_animal_comparsion <- rbind(GPS_animal_comparsion_a, GPS_animal_comparsion_b)
  
  GPS_animal_comparsion <- GPS_animal_comparsion %>% dplyr::select(time_step, X, Y,comparison_label)
  
  str(GPS_animal_comparsion)
  
  ## make it wide
  GPS_animal_comparsion_wide <- GPS_animal_comparsion %>% 
    pivot_wider(names_from = comparison_label, values_from = c(X,Y))
  GPS_animal_comparsion_wide
  
  
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>% 
    dplyr::mutate(
      dist = sqrt(  ((X_a - X_b)^ 2) + (Y_a - Y_b)^ 2))
  
  
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%  dplyr::select(time_step , dist)
  GPS_animal_comparsion_wide <- GPS_animal_comparsion_wide %>%
    rename(!!paste0(paste0("dist",comparison_a,"vs", comparison_b)) := dist)
  
  df = left_join(df, GPS_animal_comparsion_wide)
  df <- df %>% arrange(time_step)
  rm(
    comparison_a,
    comparison_b,
    comparison_a_b,
    GPS_animal_comparsion_a,
    GPS_animal_comparsion_b,
    GPS_animal_comparsion,
    GPS_animal_comparsion_wide
  )  
}    



saveRDS(df, "W:/VF/Optimising_VF/Eden Valley/data_prep/step6/VF5_step6_dist_between_animals_matrix.rds")
rm(list = ls())
