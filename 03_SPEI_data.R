
# Packages activation ####
library("ggplot2")
library("dplyr")
library("tidyr")
library("ncdf4")
library("raster")
library("sf")
library("stringr")


# Setting working directories ####
path_datasets<-"02_Datasets/"
path_images<-"03_Images/"

# Extracting SPEI122 values per municipalities ####
file.in<-list.files(path_datasets, pattern=".nc")

#open netCDF connection
r.nc<-nc_open(paste0(path_datasets, file.in))

#extract dates
timeNC<-ncvar_get(r.nc, "time")

timeR<-convertDateNcdf2R(timeNC, unit="days", origin=as.POSIXct("1901-01-01"),
                         time.format=c("%Y-%m-%d"))
# close connection
nc_close(r.nc)

#extract data from rasterStack
r<-stack(paste0(path_datasets, file.in))

xy<-coordinates(r)

mat<-as.data.frame(r)

# uploading shapefile with municipalities
admin<-st_read(paste0(path_datasets, "Admin_boundaries/", "ago_admbnda_adm2_gadm_ine_ocha_20180904.shp")) %>% 
  filter(ADM2_EN %in% c("Camucuio", "Bibala", "Tômbwa (Porto Alexandre)", "Mocamedes", "Virei", "Namibe"))

# Extract SPEI12 values per each municipality ####
#1
admin1<-admin %>% 
  filter(ADM2_EN == "Camucuio")

admin1_spei<-raster::extract(r, admin1, fun=mean) %>% 
  as_tibble() 

names(admin1_spei)<-sapply(str_remove_all(colnames(admin1_spei),"X"),"[")

admin_spei1<-admin1_spei %>% 
  gather(Date, Value) %>% 
  mutate(Date=as.Date(Date, format="%Y.%m.%d")) %>% 
  mutate(Municipality="Camucuio")

#2
admin2<-admin %>% 
  filter(ADM2_EN == "Bibala")

admin2_spei<-raster::extract(r, admin2, fun=mean) %>% 
  as_tibble()

names(admin2_spei)<-sapply(str_remove_all(colnames(admin2_spei),"X"),"[")

admin_spei2<-admin2_spei %>% 
  gather(Date, Value) %>% 
  mutate(Date=as.Date(Date, format="%Y.%m.%d")) %>% 
  mutate(Municipality="Bibala")

#3
admin3<-admin %>% 
  filter(ADM2_EN == "Tômbwa (Porto Alexandre)")

admin3_spei<-raster::extract(r, admin3, fun=mean) %>% 
  as_tibble()

names(admin3_spei)<-sapply(str_remove_all(colnames(admin3_spei),"X"),"[")

admin_spei3<-admin3_spei %>% 
  gather(Date, Value) %>% 
  mutate(Date=as.Date(Date, format="%Y.%m.%d")) %>% 
  mutate(Municipality="Tômbwa (Porto Alexandre)")

#4
admin4<-admin %>% 
  filter(ADM2_EN == "Namibe")

admin4_spei<-raster::extract(r, admin4, fun=mean) %>% 
  as_tibble()

names(admin4_spei)<-sapply(str_remove_all(colnames(admin4_spei),"X"),"[")

admin_spei4<-admin4_spei %>% 
  gather(Date, Value) %>% 
  mutate(Date=as.Date(Date, format="%Y.%m.%d")) %>% 
  mutate(Municipality="Namibe")

# binding datasets into one
admin_spei_namibe<-rbind(admin_spei1, admin_spei2, 
                         admin_spei3, admin_spei4) %>% 
  filter(Date>"1991-12-01")

saveRDS(admin_spei_namibe, paste0(path_datasets, "SPEI_Namibe.rds"))


