###################################
# GPHY 484 Jackson Hole Winter Tick Abundance and Occupancy - Climate Relationships Project 
# Author: Troy Koser
#Date of last edit: 5/9/2023
###################################

pcks <- list("dplyr", "plyr", "lubridate", "rgdal", "sp", "raster", "magrittr", "amt", "data.table", "tidyr", "stringr", "prism", "rgdal","exactextractr", "tidyverse", "terra", "MASS", "lme4")
sapply(pcks, require, char = TRUE)

vars <- c("ppt",  #precipitation
          "tavg", #avg temperature
          "vpdmin"  #Minimum Vapor Pressure Deficit
)
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM") 
get_prism_monthlys(
  type = "tmean",
  years = 2020:2022,
  mon = c(3,4,5,9,10),
  keepZip = FALSE
)

get_prism_monthlys(
  type = "ppt",
  years = 2020:2022,
  mon = c(3,4,5,9,10),
  keepZip = FALSE
)

get_prism_monthlys(
  type = "vpdmin",
  years = 2020:2022,
  mon = c(3,4,5,9,10),
  keepZip = FALSE
)

#Spring 2020 Tmean Raster
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/tmean/Spring/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/tmean/Spring/2020/PRISM_tmean_", year,".csv")
write.csv(m.df, name)
writeRaster(RS, name)

head(m.df)

minLat=43.392575
maxLat=44.083639
minLon=-111.038202
maxLon=-110.256107

m.df.nwwy<-m.df%>%filter(minLat < lat, lat < maxLat, minLon < lon, lon <maxLon)%>%
  mutate(tmean = value)%>%
  select(-value)

dim(m.df)
dim(m.df.nwwy)

ggplot()+
  geom_raster(data=m.df.nwwy, aes(x=lon, y=lat, fill=tmean))+
  scale_fill_gradient2("Temp", low='darkslateblue',mid='lightblue',high = 'red', midpoint=5)+
  labs(title="Average Temperature Spring 2020")+coord_fixed(ratio=1.3)

#Fall 2020 Tmean Raster
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/tmean/Fall/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/tmean/Fall/2020/PRISM_tmean_", year,".csv")
write.csv(m.df, name)
writeRaster(RS, name)

head(m.df)

minLat=43.392575
maxLat=44.083639
minLon=-111.038202
maxLon=-110.256107

m.df.nwwy<-m.df%>%filter(minLat < lat, lat < maxLat, minLon < lon, lon <maxLon)%>%
  mutate(tmean = value)%>%
  select(-value)

dim(m.df)
dim(m.df.nwwy)

ggplot()+
  geom_raster(data=m.df.nwwy, aes(x=lon, y=lat, fill=tmean))+
  scale_fill_gradient2("Temp", low='darkslateblue',mid='lightblue',high = 'red', midpoint=0)+
  labs(title="Average Temperature Fall 2020")+coord_fixed(ratio=1.3)

#Fall 2020 VPDMin Raster
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2020/PRISM_vpdmin_", year,".csv")
write.csv(m.df, name)
writeRaster(RS, name, overwrite=TRUE)

head(m.df)

minLat=43.392575
maxLat=44.083639
minLon=-111.038202
maxLon=-110.256107

m.df.nwwy<-m.df%>%filter(minLat < lat, lat < maxLat, minLon < lon, lon <maxLon)%>%
  mutate(vpdmin = value)%>%
  select(-value)

dim(m.df)
dim(m.df.nwwy)

ggplot()+
  geom_raster(data=m.df.nwwy, aes(x=lon, y=lat, fill=vpdmin))+
  scale_fill_gradient2("VPDMin", low='darkslateblue',high = 'grey', midpoint=5)+
  labs(title="Average Minimum Vapor Pressure Deficit Fall 2020")+coord_fixed(ratio=1.3)

#Fall 2020 Precip Raster
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/ppt/Fall/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/ppt/Fall/2020/PRISM_ppt_", year,".csv")
write.csv(m.df, name)
writeRaster(RS, name, overwrite=TRUE)

head(m.df)

minLat=43.392575
maxLat=44.083639
minLon=-111.038202
maxLon=-110.246107

m.df.nwwy<-m.df%>%filter(minLat < lat, lat < maxLat, minLon < lon, lon <maxLon)%>%
  mutate(ppt = value)%>%
  select(-value)

dim(m.df)
dim(m.df.nwwy)

ggplot()+
  geom_raster(data=m.df.nwwy, aes(x=lon, y=lat, fill=ppt))+
  scale_fill_gradient2("PPT", low='blue',high = 'grey', midpoint=100)+
  labs(title="Average Precipitation (Inches) Fall 2020")+coord_fixed(ratio=1.3)

##Tick data import
td_2020 <- read_excel("2020 Tick Drag Data.xlsx")
spdf_2020<-SpatialPointsDataFrame(coords=cbind(td_2020$x, td_2020$y), data=td_2020, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

#2020 VPDmin, tmean, and ppt values added to tick dataframe
#VPDmin Fall2020
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_vpdmin_Fall2020 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_vpdmin_Fall2020)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_vpdmin_Fall2020))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2020/PRISM_vpdmin_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_vpdmin_Fall2020, name, overwrite=TRUE)

Fall_2020_vpdmin <- raster::extract(RS_vpdmin_Fall2020,             # raster layer
                                      spdf_2020,   # SPDF with centroids for buffer 
                                      df=TRUE)         # return a dataframe? 

Fall_2020_vpdmin$Fall_2020_VPDmin <- apply(Fall_2020_vpdmin,1,mean)

td_2020<-merge(Fall_2020_vpdmin, td_2020)

#VPDmin Spring 2020
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/vpdmin/Spring/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_vpdmin_Spring2020 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_vpdmin_Spring2020)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_vpdmin_Spring2020))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2020/PRISM_vpdmin_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_vpdmin_Spring2020, name, overwrite=TRUE)

Spring_2020_vpdmin <- raster::extract(RS_vpdmin_Spring2020,             # raster layer
                                   spdf_2020,   # SPDF with centroids for buffer 
                                   df=TRUE)         # return a dataframe? 

Spring_2020_vpdmin$Spring_2020_VPDmin <- apply(Spring_2020_vpdmin,1,mean)

td_2020<-merge(Spring_2020_vpdmin, td_2020)

#Tmean Fall2020
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/tmean/Fall/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_tmean_Fall2020 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_tmean_Fall2020)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_tmean_Fall2020))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/tmean/Fall/2020/PRISM_tmean_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_tmean_Fall2020, name, overwrite=TRUE)

Fall_2020_tmean <- raster::extract(RS_tmean_Fall2020,             # raster layer
                                     spdf_2020,   # SPDF with centroids for buffer 
                                     df=TRUE)         # return a dataframe? 

Fall_2020_tmean$Fall_2020_tmean <- apply(Fall_2020_tmean,1,mean)

td_2020<-merge(Fall_2020_tmean, td_2020)

#tmean Spring 2020
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/tmean/Spring/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_tmean_Spring2020 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_tmean_Spring2020)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_tmean_Spring2020))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/tmean/Fall/2020/PRISM_tmean_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_tmean_Spring2020, name, overwrite=TRUE)

Spring_2020_tmean <- raster::extract(RS_tmean_Spring2020,             # raster layer
                                 spdf_2020,   # SPDF with centroids for buffer 
                                 df=TRUE)         # return a dataframe? 

Spring_2020_tmean$Spring_2020_tmean <- apply(Spring_2020_tmean,1,mean)

td_2020<-merge(Spring_2020_tmean, td_2020)

#PPT Fall2020
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/ppt/Fall/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_ppt_Fall2020 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_ppt_Fall2020)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_ppt_Fall2020))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/ppt/Fall/2020/PRISM_ppt_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_ppt_Fall2020, name, overwrite=TRUE)

Fall_2020_ppt <- raster::extract(RS_ppt_Fall2020,             # raster layer
                                   spdf_2020,   # SPDF with centroids for buffer 
                                   df=TRUE)         # return a dataframe? 

Fall_2020_ppt$Fall_2020_ppt <- apply(Fall_2020_ppt,1,mean)

td_2020<-merge(Fall_2020_ppt, td_2020)

#ppt Spring 2020
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/ppt/Spring/2020") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_ppt_Spring2020 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_ppt_Spring2020)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_ppt_Spring2020))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2020'
name<-paste0("~/Classes/GPHY_484/data/PRISM/ppt/Fall/2020/PRISM_ppt_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_ppt_Spring2020, name, overwrite=TRUE)

Spring_2020_ppt <- raster::extract(RS_ppt_Spring2020,             # raster layer
                                 spdf_2020,   # SPDF with centroids for buffer 
                                 df=TRUE)         # return a dataframe? 

Spring_2020_ppt$Spring_2020_ppt <- apply(Spring_2020_ppt,1,mean)

td_2020<-merge(Spring_2020_ppt, td_2020)

td_2020 <- td_2020[,-c(2,3, 4, 6, 7, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21)]
##################################################################################################
#Same w/ 2021 data
td_2021 <- read_excel("2021 Tick Drag Data.xlsx")
spdf_2021<-SpatialPointsDataFrame(coords=cbind(td_2021$x, td_2021$y), data=td_2021, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

#2021 VPDmin, tmean, and ppt values added to tick dataframe
#VPDmin Fall2021
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2021") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_vpdmin_Fall2021 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_vpdmin_Fall2021)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_vpdmin_Fall2021))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2021'
name<-paste0("~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2021/PRISM_vpdmin_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_vpdmin_Fall2021, name, overwrite=TRUE)

Fall_2021_vpdmin <- raster::extract(RS_vpdmin_Fall2021,             # raster layer
                                    spdf_2021,   # SPDF with centroids for buffer 
                                    df=TRUE)         # return a dataframe? 

Fall_2021_vpdmin$Fall_2021_VPDmin <- apply(Fall_2021_vpdmin,1,mean)

td_2021<-merge(Fall_2021_vpdmin, td_2021)

#VPDmin Spring 2021
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/vpdmin/Spring/2021") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_vpdmin_Spring2021 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_vpdmin_Spring2021)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_vpdmin_Spring2021))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2021'
name<-paste0("~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2021/PRISM_vpdmin_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_vpdmin_Spring2021, name, overwrite=TRUE)

Spring_2021_vpdmin <- raster::extract(RS_vpdmin_Spring2021,             # raster layer
                                      spdf_2021,   # SPDF with centroids for buffer 
                                      df=TRUE)         # return a dataframe? 

Spring_2021_vpdmin$Spring_2021_VPDmin <- apply(Spring_2021_vpdmin,1,mean)

td_2021<-merge(Spring_2021_vpdmin, td_2021)

#Tmean Fall2021
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/tmean/Fall/2021") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_tmean_Fall2021 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_tmean_Fall2021)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_tmean_Fall2021))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2021'
name<-paste0("~/Classes/GPHY_484/data/PRISM/tmean/Fall/2021/PRISM_tmean_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_tmean_Fall2021, name, overwrite=TRUE)

Fall_2021_tmean <- raster::extract(RS_tmean_Fall2021,             # raster layer
                                   spdf_2021,   # SPDF with centroids for buffer 
                                   df=TRUE)         # return a dataframe? 

Fall_2021_tmean$Fall_2021_tmean <- apply(Fall_2021_tmean,1,mean)

td_2021<-merge(Fall_2021_tmean, td_2021)

#tmean Spring 2021
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/tmean/Spring/2021") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_tmean_Spring2021 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_tmean_Spring2021)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_tmean_Spring2021))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2021'
name<-paste0("~/Classes/GPHY_484/data/PRISM/tmean/Fall/2021/PRISM_tmean_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_tmean_Spring2021, name, overwrite=TRUE)

Spring_2021_tmean <- raster::extract(RS_tmean_Spring2021,             # raster layer
                                     spdf_2021,   # SPDF with centroids for buffer 
                                     df=TRUE)         # return a dataframe? 

Spring_2021_tmean$Spring_2021_tmean <- apply(Spring_2021_tmean,1,mean)

td_2021<-merge(Spring_2021_tmean, td_2021)

#PPT Fall2021
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/ppt/Fall/2021") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_ppt_Fall2021 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_ppt_Fall2021)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_ppt_Fall2021))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2021'
name<-paste0("~/Classes/GPHY_484/data/PRISM/ppt/Fall/2021/PRISM_ppt_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_ppt_Fall2021, name, overwrite=TRUE)

Fall_2021_ppt <- raster::extract(RS_ppt_Fall2021,             # raster layer
                                 spdf_2021,   # SPDF with centroids for buffer 
                                 df=TRUE)         # return a dataframe? 

Fall_2021_ppt$Fall_2021_ppt <- apply(Fall_2021_ppt,1,mean)

td_2021<-merge(Fall_2021_ppt, td_2021)

#ppt Spring 2021
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/ppt/Spring/2021") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_ppt_Spring2021 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_ppt_Spring2021)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_ppt_Spring2021))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2021'
name<-paste0("~/Classes/GPHY_484/data/PRISM/ppt/Fall/2021/PRISM_ppt_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_ppt_Spring2021, name, overwrite=TRUE)

Spring_2021_ppt <- raster::extract(RS_ppt_Spring2021,             # raster layer
                                   spdf_2021,   # SPDF with centroids for buffer 
                                   df=TRUE)         # return a dataframe? 

Spring_2021_ppt$Spring_2021_ppt <- apply(Spring_2021_ppt,1,mean)

td_2021<-merge(Spring_2021_ppt, td_2021)

td_2021 <- td_2021[,-c(2,3, 4, 6, 7, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21)]
#########################################################################################
#Same w/ 2022 data
td_2022 <- read_excel("2022 Tick Drag Data.xlsx")
spdf_2022<-SpatialPointsDataFrame(coords=cbind(td_2022$x, td_2022$y), data=td_2022, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

#2022 VPDmin, tmean, and ppt values added to tick dataframe
#VPDmin Fall2022
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2022") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_vpdmin_Fall2022 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_vpdmin_Fall2022)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_vpdmin_Fall2022))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2022'
name<-paste0("~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2022/PRISM_vpdmin_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_vpdmin_Fall2022, name, overwrite=TRUE)

Fall_2022_vpdmin <- raster::extract(RS_vpdmin_Fall2022,             # raster layer
                                    spdf_2022,   # SPDF with centroids for buffer 
                                    df=TRUE)         # return a dataframe? 

Fall_2022_vpdmin$Fall_2022_VPDmin <- apply(Fall_2022_vpdmin,1,mean)

td_2022<-merge(Fall_2022_vpdmin, td_2022)

#VPDmin Spring 2022
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/vpdmin/Spring/2022") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_vpdmin_Spring2022 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_vpdmin_Spring2022)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_vpdmin_Spring2022))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2022'
name<-paste0("~/Classes/GPHY_484/data/PRISM/vpdmin/Fall/2022/PRISM_vpdmin_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_vpdmin_Spring2022, name, overwrite=TRUE)

Spring_2022_vpdmin <- raster::extract(RS_vpdmin_Spring2022,             # raster layer
                                      spdf_2022,   # SPDF with centroids for buffer 
                                      df=TRUE)         # return a dataframe? 

Spring_2022_vpdmin$Spring_2022_VPDmin <- apply(Spring_2022_vpdmin,1,mean)

td_2022<-merge(Spring_2022_vpdmin, td_2022)

#Tmean Fall2022
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/tmean/Fall/2022") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_tmean_Fall2022 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_tmean_Fall2022)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_tmean_Fall2022))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2022'
name<-paste0("~/Classes/GPHY_484/data/PRISM/tmean/Fall/2022/PRISM_tmean_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_tmean_Fall2022, name, overwrite=TRUE)

Fall_2022_tmean <- raster::extract(RS_tmean_Fall2022,             # raster layer
                                   spdf_2022,   # SPDF with centroids for buffer 
                                   df=TRUE)         # return a dataframe? 

Fall_2022_tmean$Fall_2022_tmean <- apply(Fall_2022_tmean,1,mean)

td_2022<-merge(Fall_2022_tmean, td_2022)

#tmean Spring 2022
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/tmean/Spring/2022") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_tmean_Spring2022 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_tmean_Spring2022)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_tmean_Spring2022))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2022'
name<-paste0("~/Classes/GPHY_484/data/PRISM/tmean/Fall/2022/PRISM_tmean_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_tmean_Spring2022, name, overwrite=TRUE)

Spring_2022_tmean <- raster::extract(RS_tmean_Spring2022,             # raster layer
                                     spdf_2022,   # SPDF with centroids for buffer 
                                     df=TRUE)         # return a dataframe? 

Spring_2022_tmean$Spring_2022_tmean <- apply(Spring_2022_tmean,1,mean)

td_2022<-merge(Spring_2022_tmean, td_2022)

#PPT Fall2022
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/ppt/Fall/2022") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_ppt_Fall2022 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_ppt_Fall2022)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_ppt_Fall2022))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2022'
name<-paste0("~/Classes/GPHY_484/data/PRISM/ppt/Fall/2022/PRISM_ppt_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_ppt_Fall2022, name, overwrite=TRUE)

Fall_2022_ppt <- raster::extract(RS_ppt_Fall2022,             # raster layer
                                 spdf_2022,   # SPDF with centroids for buffer 
                                 df=TRUE)         # return a dataframe? 

Fall_2022_ppt$Fall_2022_ppt <- apply(Fall_2022_ppt,1,mean)

td_2022<-merge(Fall_2022_ppt, td_2022)

#ppt Spring 2022
prism_set_dl_dir(path="~/Classes/GPHY_484/data/PRISM/ppt/Spring/2022") 
prism_archive_ls()

new_file<-1#this number corresponds to the row of the file of interest
RS_ppt_Spring2022 <- pd_stack(prism_archive_ls()) ##raster file of data
proj4string(RS_ppt_Spring2022)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

##convert raster to point data frame
df <- data.frame(rasterToPoints(RS_ppt_Spring2022))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

year<-'2022'
name<-paste0("~/Classes/GPHY_484/data/PRISM/ppt/Fall/2022/PRISM_ppt_", year,".csv")
write.csv(m.df, name)
writeRaster(RS_ppt_Spring2022, name, overwrite=TRUE)

Spring_2022_ppt <- raster::extract(RS_ppt_Spring2022,             # raster layer
                                   spdf_2022,   # SPDF with centroids for buffer 
                                   df=TRUE)         # return a dataframe? 

Spring_2022_ppt$Spring_2022_ppt <- apply(Spring_2022_ppt,1,mean)

td_2022<-merge(Spring_2022_ppt, td_2022)

td_2022 <- td_2022[,-c(2,3, 4, 6, 7, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21)]
############################end tick data import and cleaning##########################

# Histogram plot, tick data is frequently overdispersed
td<-rbind(td_2020, td_2021, td_2022)
dim(td)

ggplot(td, aes(x=Total.Ticks)) + 
  geom_histogram(colour="black", fill="lightblue", bins=100)+ggtitle("Frequency Histogram of Tick Abundance during Drag Surveys 2020-23")+theme_bw()+facet_grid(td$Year ~ .)


spdf<-SpatialPointsDataFrame(coords=cbind(td$x, td$y), data=td, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

ggplot()+
  geom_raster(data=m.df.nwwy, aes(x=lon, y=lat, fill=ppt))+
  scale_fill_gradient2("PPT", low='blue',high = 'grey', midpoint=100)+
  labs(title="Average Precipitation (Inches) Fall 2020 & Tick Drag Locations")+coord_fixed(ratio=1.3)+
  geom_point(data=spdf@data, aes(x=spdf$x, y=spdf$y), size=2)


#model time! Start with presence absence#

p_global_2021 <- glm(Presence ~ Spring_2021_VPDmin + Spring_2021_ppt + Spring_2021_tmean + Fall_2021_VPDmin + Fall_2021_ppt + Fall_2021_tmean, 
                family = binomial(link = "logit"), data = td_2021)
summary(p_global_2021)

p_global_2021_2 <- glm(Presence ~ Spring_2021_VPDmin + Spring_2021_ppt + Spring_2021_tmean + Fall_2021_VPDmin + Fall_2021_ppt + Fall_2021_tmean, 
                     family = quasibinomial(link = "logit"), data = td_2021)
summary(p_global_2021_2)

#abundance

a_global_2021 <- glmer.nb(Total.Ticks ~ Spring_2021_VPDmin + Spring_2021_ppt + Spring_2021_tmean + Fall_2021_VPDmin + Fall_2021_ppt + Fall_2021_tmean, data = td_2021)
summary(a_global_2021)




