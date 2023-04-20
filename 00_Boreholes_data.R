
# This script extracts information on irrigation water expansion, migration and 
# settling of pastoralists in different AoI around the global drylands


# Packages activation ####
library("dplyr")
library("rgdal")
library("tidyr")
library("lubridate")


# Clearing workspace
rm(list=ls(all=TRUE))
graphics.off()

# Setting working directories ####
path_datasets<-"02_Datasets/"
path_images<-"03_Images/"

# Projection variables ####
wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
utm <- "+proj=utm +zone=32 +south=S ellps=WGS84"

# Upload data #### 
Municipios_Angola <- readOGR(paste0(path_datasets, "MUNICIPIOS_NAMIBE.shp"))

All_pozzi<- read.csv(paste0(path_datasets, "Puntos_Agua_COORDINATE_ANNO_Bibala_Virei_Namibe_Tombwa_Camucuio.csv"), 
                            header = T, sep = ";") %>% 
  rename(Anno=Construção) %>% 
  mutate(Profun.= as.numeric(Profun.)) %>% 
  as_tibble()

# selecting only those boreholes having information on the year of construction
Pozzi_anno<-filter(All_pozzi, !is.na(All_pozzi$Anno))

# Cumulative long boreholes dataset ####
dt1<-Pozzi_anno %>% 
  arrange(Nr) %>% 
  group_by(Municipio, Anno) %>% 
  summarize(n_pozzi_anno=n()) %>%
  mutate(Anno=as.Date(as.character(Anno), "%Y")) %>%
  group_by(Municipio) %>% 
  complete(Anno = seq.Date(min(Anno), as.Date("2019-01-20"), by="year")) %>%
  replace(is.na(.), 0) %>%
  group_by(Municipio) %>% 
  mutate(Boreholes=cumsum(n_pozzi_anno)) %>% 
  tidyr::gather(Variable, Value, "Boreholes") %>% 
  dplyr::select(-c(n_pozzi_anno)) %>% 
  mutate(Anno=year(Anno))

saveRDS(dt1, paste0(path_datasets, "Pozzi_Namibe.rds"))
