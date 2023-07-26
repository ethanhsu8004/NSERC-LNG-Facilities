#Task  VNF data around two LNG facilities Freeport (started operation in mid 2019, coordinates 28.9266713(lat),-95.3271224(long)) 
#and Corpus Christi (started operation mid 2018, coordinates 27.890013(lat),-97.271692(long)).

library(dplyr)
library(sf)
library(ggplot2)
library(dplyr)

#Importing Data
Flaring_Data <- readRDS("Data/LNG_Facilities.rds")
Flaring_Data <- Flaring_Data %>% filter(value != 0)


Flaring_Data_Corpus <- Flaring_Data %>% filter(Facility == "Corpus Christi") 
Flaring_Data_Freeport <- Flaring_Data %>% filter(Facility == "FreePort")


#Making Data Spatial Object
Flaring_Data_Corpus <- st_as_sf(Flaring_Data_Corpus)
Flaring_Data_Freeport <- st_as_sf(Flaring_Data_Freeport)

#1. Freeport (started operation in mid 2019)
Flaring_Data_Freeport <- Flaring_Data_Freeport %>% filter(date > "2019-05-01")
Freeport_coordinates <- data.frame(longitude = -95.3271224,latitude = 28.9266713)
Freeport_coordinates <- st_as_sf(Freeport_coordinates, coords = c("longitude", "latitude"), crs = 4326)

#Since the latitude is < 96, use crs - 32615
Freeport_coordinates_buffer_1km <- Freeport_coordinates  %>% st_transform(crs = 32615) %>% st_buffer(dist = 1000) %>% st_transform(crs = 4326)
Flaring_Data_Freeport_1KM <- Flaring_Data_Freeport[lengths(st_within(Flaring_Data_Freeport, Freeport_coordinates_buffer_1km )) > 0, ]

#2. Corpus Christie (started mid 2018)
Flaring_Data_Corpus <- Flaring_Data_Corpus  %>% filter(date > "2018-05-01") 
Corpus_Christi_coordinates <- data.frame(longitude = -97.271692, latitude = 27.890013)
Corpus_Christi_coordinates <- st_as_sf(Corpus_Christi_coordinates, coords = c("longitude", "latitude"), crs = 4326)
 
Corpus_Christi_coordinates_buffer_1km <- Corpus_Christi_coordinates  %>% st_transform(crs = 32613) %>% st_buffer(dist = 1000) %>% st_transform(crs = 4326)
Flaring_Data_Corpus_1KM <- Flaring_Data_Corpus[lengths(st_within(Flaring_Data_Corpus, Corpus_Christi_coordinates_buffer_1km)) > 0, ]








