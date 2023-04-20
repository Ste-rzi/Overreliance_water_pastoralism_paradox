

# Packages activation ####
library("dplyr")
library("tidyr")
library("ggplot2")

# Setting working directories ####
path_datasets<-"02_Datasets/"
path_images<-"03_Images/"

# functions
'%!in%' <- function(x,y)!('%in%'(x,y))

# uploading data
dt_geo<-read.csv(paste0(path_datasets, "Namibe_GeoQuery.csv"),
                 header=T, sep = ";") %>% 
  gather(Variable, Value, -c("id", "Municipio")) %>% 
  rename(Municipality=Municipio)

dt_temp<-do.call(rbind.data.frame, strsplit(dt_geo$Variable, "[_.]")) %>% 
  `colnames<-`(c("Var1", "Var2", "Var3", "Var4", "Var5",
                 "Var6", "Var7", "Var8"))

dt_geo2<-cbind(dt_geo, dt_temp) %>% 
  dplyr::select(-c("Variable"))

# checking total area value
dt_tot_esa<-dt_geo2 %>% 
  filter(Var1=="esa") %>% 
  rename("Years"=Var3) %>% 
  group_by(Years) %>% 
  summarise(Value_tot_cell=sum(Value))

reference_pixels<-unique(dt_tot_esa$Value_tot_cell)

# Dataset for cropland only ####
dt_esa<-dt_geo2 %>% 
  filter(Var1=="esa",
         Var6=="cropland") %>% 
  rename("Years"=Var3,
         "Variable"=Var6) %>% 
  group_by(Years, Municipality, Variable) %>% 
  summarise(Value_sum=sum(Value)) %>% 
  mutate(Value_perc=(Value_sum/reference_pixels)*100) %>% 
  as_tibble()


total_land_mun<-dt_geo2 %>% 
  filter(Var1=="esa",
         Var5 %!in% c("no", "count")) %>% 
  rename("Years"=Var3,
         "Variable"=Var5) %>% 
  group_by(Years, Municipality) %>% 
  summarise(Value_sum=sum(Value)) %>% 
  as_tibble() %>% 
  distinct(Municipality, Value_sum)


# Dataset for grassland only ####
dt_grass<-dt_geo2 %>% 
  filter(Var5=="grassland") %>% 
  rename("Years"=Var3,
         "Variable"=Var5) %>% 
  group_by(Years, Municipality, Variable) %>% 
  summarise(Value_sum=sum(Value)) %>% 
  mutate(Value_perc=(Value_sum/reference_pixels)*100) %>% 
  as_tibble()

dt_test<-rbind(dt_esa, dt_grass) %>% 
  mutate(Legend="Legend") 


# Adding water infrastructure ####
dt1<-readRDS(paste0(path_datasets, "Pozzi_Namibe.rds")) %>% 
  rename("Years"=Anno) %>% 
  mutate(Legend="Legend",
         Value=Value) %>% 
  mutate(Municipality=ifelse(Municipio=="Mocamedes", "Namibe", Municipio)) %>% 
  ungroup() %>% 
  dplyr::select(-c(Municipio))

dt_test2<-dt_test %>% 
  dplyr::select(Municipality, Years, Variable, Value_sum, Legend) %>% 
  mutate(Value=Value_sum) %>% 
  dplyr::select(-c(Value_sum))

dt_infrastructure_esa<-rbind(dt_test2, dt1) %>% 
  filter(Municipality!="Virei")


