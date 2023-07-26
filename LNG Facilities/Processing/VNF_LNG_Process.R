#Processing the LNG Facilities, 
#What will be processed
#Removing Temperature <= 1600k and only wanting cloud_mask == 0 and applying DBSCAN

library(sf)
library(dplyr)
library(lubridate)
library(dbscan)
library(data.table)

Freeport_coordinates <- data.frame(longitude = -95.3271224,latitude = 28.9266713)
Freeport_coordinates <- st_as_sf(Freeport_coordinates, coords = c("longitude", "latitude"), crs = 4326)

FreePort_Raw <- readRDS('../Extracting/FreePort/FP_vnf_raw.rds')
FreePort_Raw$Facility <- "FreePort"

CorpusChristi_Raw <- readRDS("../Extracting/CorpusChristi/CC_vnf_raw.rds")
CorpusChristi_Raw$Facility <- "Corpus Christi"

# FreePort_Raw$longitude <- FreePort_Raw$lon
# FreePort_Raw$latitude <- FreePort_Raw$lat

Data <- rbind(CorpusChristi_Raw, FreePort_Raw)


# Data <- CorpusChristi_Raw
Data$longitude <- Data$lon
Data$latitude <- Data$lat
Data <- Data %>% select(-file_date, -temp_bkg) %>% filter(!is.na(cloud_mask))
Data <- st_as_sf(Data, coords = c("longitude", "latitude"), crs = 4326) 
Data.clust.list <- Data %>% mutate(year = year(date))  %>% as.data.frame()
Data.clust.list <- split(Data.clust.list, list(Data.clust.list$Facility, Data.clust.list$year))

minpts.grid = 3:8 

for(c in 1:length(Data.clust.list)){
  
  curr.vnf.year.minpts = NA
  curr.vnf.year.noises = NA
  curr.vnf.year.clusts = NA
  if (c == 2){
    next
  }
  for(m in 1:length(minpts.grid)){
    curr.hdbscan = hdbscan(
      x = dplyr::select(Data.clust.list[[c]], lon, lat),
      minPts = minpts.grid[m]
    )
    print(m)
    gc(); gc(reset = TRUE)
    
    if(is.na(curr.vnf.year.noises)) {
      curr.vnf.year.minpts = minpts.grid[m]
      curr.vnf.year.noises = sum(curr.hdbscan$cluster == 0)
      curr.vnf.year.clusts = ifelse(curr.hdbscan$cluster > 0, "Yes", "No")
      curr.vnf.value = curr.hdbscan$cluster
    } else if(sum(curr.hdbscan$cluster == 0) < curr.vnf.year.noises) {
      curr.vnf.year.minpts = minpts.grid[m]
      curr.vnf.year.noises = sum(curr.hdbscan$cluster == 0)
      curr.vnf.year.clusts = ifelse(curr.hdbscan$cluster > 0, "Yes", "No")
      curr.vnf.value = curr.hdbscan$cluster
    }}
  
  Data.clust.list[[c]] = data.table(
    vnf_id = Data.clust.list[[c]]$vnf_id,
    clustered = curr.vnf.year.clusts, value = curr.vnf.value)
  rm(list = ls(pattern = "curr")); gc(); gc(reset = TRUE)
  
}

Facilities_Data <- Data.clust.list[[1]]

for (x in 2:length(Data.clust.list)){
  
  if (x == 2){
    next
  }
  print(x)
  Facilities_Data<- merge(Facilities_Data, Data.clust.list[[x]], all = TRUE)
  
}

Facilities_Data_Final <- merge(Data, Facilities_Data, by = "vnf_id" )
saveRDS(Facilities_Data_Final, "../Data/LNG_Facilities.rds")
