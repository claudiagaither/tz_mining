## Malaria in TZ national surveys from 2011-2022, predicted by proximity to artisanal and industrial mines----

## part one:    data extract and organization ----

set.seed(321)
source("C:/Users/cgait/OneDrive/Desktop/TZ mining/mine_functions.R")

library(broom)
library(broom.mixed)
library(dplyr)
library(geosphere)
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(ggspatial)
library(haven)
library(INLA)
library(jsonlite)
library(lwgeom)
library(MASS)
library(Matrix)
library(mice)
library(mitools)
library(openxlsx)
library(patchwork)
library(pscl)
library(purrr)
library(readr)
library(readxl)
library(scico)
library(sf)
library(statmod)
library(stars)
library(stats)
library(survey)
library(terra)
library(tibble)
library(tidyverse)
library(viridis)
library(writexl)


#DHS 2022 household member recode
TZ22_pr <- read_sas("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS22/TZPR82SD/TZPR82FL.SAS7BDAT")
#MIS 2017 household member recode
TZ17_pr <- read_sas("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_MIS17/TZPR7ISD/TZPR7IFL.SAS7BDAT")
#DHS 2015-2016 household member recode
TZ15_pr <- read_sas("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS15/TZPR7BSD/TZPR7BFL.SAS7BDAT")
#AIS 2011-12 household member recode
TZ11_pr <- read_sas("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_AIS11/TZPR6ASD/TZPR6AFL.SAS7BDAT")

#geography data & geocovariates for all surveys
geo_22 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS22/TZGE81FL/TZGE81FL.shp")
geo_cov22 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS22/TZGC81FL_geocov/TZGC81FL.csv")
geo_17 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_MIS17/TZGE7IFL/TZGE7IFL.shp")
geo_cov17 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_MIS17/TZGC7JFL/TZGC7JFL.csv")
geo_15 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS15/TZGE7AFL/TZGE7AFL.shp")
geo_cov15 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS15/TZGC7BFL/TZGC7BFL.csv")
geo_11 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_AIS11/TZGE6AFL/TZGE6AFL.shp")
geo_cov11 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_AIS11/TZGC6BFL/TZGC6BFL.csv")

#artisinal mines in north west Tanzania
mines <- fromJSON("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/tza_mines_curated_all_opendata_p_ipis.json")
mines <- as.data.frame(mines)
#bigger mines in north west TZ manually compiled from mention in the IPIS report 
big_mines <- read_excel("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/big_mines.xlsx")

#ecology data from CHIRPS & MODIS 
vegi22 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi22.shp")
vegi17 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi17.shp")
vegi16 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi16.shp")
vegi15 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi15.shp")
vegi12 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi12.shp")
vegi11 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi11.shp")
admin <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/regions/tza_admbnda_adm2_20181019/tza_admbnda_adm2_20181019.shp")
rain <- read_excel("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/avg_precip.xlsx")

#bordering countries in Africa
africa <- st_read("C:/Users/cgait/OneDrive/Desktop/IDEEL/Rwanda nonpf/data/data downloads/Africa_Boundaries-shp/Africa_Boundaries.shp")
#regional boundaries
district <- st_read("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/districts/Districts.shp")
#rivers 
#water <- st_read("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/Water_Supply_Control-Rivers-shp/Water_Supply_Control-Rivers.shp")
#Lake Victoria
Victoria <- st_read("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/Lake_Victoria_Shapefile/Lake_Victoria_Shapefile.shp")

#subset bounds (Uganda, Rwanda, Burundi)
uga<-subset(africa, NAME_0=="Uganda")
rwa<-subset(africa, NAME_0=="Rwanda")
bur<-subset(africa, NAME_0=="Burundi")
tza<-subset(africa, NAME_0=="Tanzania")
bounds<-rbind(bur, rwa, tza, uga)

# site names for artisinal mine sites
mines$name <- mines$features.properties$sitename
#number of workers
mines$workers <- mines$features.properties$worker
mines$fem_workers <- mines$features.properties$workerwomen
# building structure permanence 
mines$building <- mines$features.properties$buildingtype
#other mine features
mines$mineral <- mines$features.properties$mineral1name
mines$num_pits <- mines$features.properties$pits
mines$cyanide <- mines$features.properties$cyanide
mines$mercury <- mines$features.properties$mercury
#indicator for any chemical in use
mines <- mines %>% mutate(cyanide = case_when(cyanide=="Yes" ~ 1, cyanide=="No" ~ 0))
mines <- mines %>% mutate(mercury = case_when(mercury=="Yes" ~ 1, mercury=="No" ~ 0))
mines$chem_sum <- mines$cyanide + mines$mercury              
mines$site_type <- mines$features.properties$sitetype
mines$facilities <- mines$features.properties$facilities
mines$closetores <- mines$features.properties$closetoresidential

# ~minimum years operational based on building type permanence
mines <- mines %>% mutate(min_years = case_when(building=="None" ~ 0,
                   building=="Permanent" ~ 3, building=="Temporary/makeshift" ~ 1,
                   building=="Temporary/makeshift, Permanent" ~ 2))
# ~year mine has been operational since, y_open
mines$year_open <- 2018-mines$min_years
mines$year_close <- 2024   # assume all mines mapped in 2018 are still open in 2022..


## DHS or other survey geography & ecology data at cluster level, for each survey year
geo_vars22 <- geo_cov22[,c("DHSCLUST","Temperature_February","Temperature_March","Temperature_April","Temperature_May","Temperature_June","Temperature_July")]
geo_22 <- geo_22[,c("ALT_GPS","ALT_DEM","DHSREGNA","URBAN_RURA","LATNUM","LONGNUM")]
geo_22 <- bind_cols(geo_22, geo_vars22)
geo_22$HV001 <- geo_22$DHSCLUST

geo_vars17 <- geo_cov17[,c("DHSCLUST","Temperature_October","Temperature_November","Temperature_December")]
geo_17 <- geo_17[,c("ALT_GPS","ALT_DEM","DHSREGNA","URBAN_RURA","LATNUM","LONGNUM")]
geo_17 <- bind_cols(geo_17, geo_vars17)
geo_17$HV001 <- geo_17$DHSCLUST

geo_vars15 <- geo_cov15[,c("DHSCLUST","Temperature_January","Temperature_February","Temperature_August","Temperature_September","Temperature_October","Temperature_November","Temperature_December")]
geo_15 <- geo_15[,c("ALT_GPS","ALT_DEM","DHSREGNA","URBAN_RURA","LATNUM","LONGNUM")]
geo_15 <- bind_cols(geo_15, geo_vars15)
geo_15$HV001 <- geo_15$DHSCLUST

geo_vars11 <- geo_cov11[,c("DHSCLUST","Temperature_January","Temperature_February","Temperature_March","Temperature_April","Temperature_May","Temperature_December")]
geo_11 <- geo_11[,c("ALT_GPS","ALT_DEM","DHSREGNA","URBAN_RURA","LATNUM","LONGNUM")]
geo_11 <- bind_cols(geo_11, geo_vars11)
geo_11$HV001 <- geo_11$DHSCLUST

remove(geo_vars11,geo_vars15,geo_vars17,geo_vars22,geo_cov11,geo_cov15,geo_cov17,geo_cov22)

#rename latitude and longitude
geo_list <- list(geo_22 = geo_22, geo_17 = geo_17, geo_15 = geo_15, geo_11 = geo_11)
geo_list <- map(geo_list, ~ .x %>% mutate(lat = LATNUM, long = LONGNUM))
geo_22 <- geo_list$geo_22
geo_17 <- geo_list$geo_17
geo_15 <- geo_list$geo_15
geo_11 <- geo_list$geo_11

#add HV001 for future merging
geo_22$HV001 <- geo_22$DHSCLUST
geo_17$HV001 <- geo_17$DHSCLUST
geo_15$HV001 <- geo_15$DHSCLUST
geo_11$HV001 <- geo_11$DHSCLUST

# pull coordinates and dates for all survey years 
ccoord22 <- as.data.frame(st_drop_geometry(geo_22))[, c("HV001", "lat", "long")]
ccoord17 <- as.data.frame(st_drop_geometry(geo_17))[, c("HV001", "lat", "long")]
ccoord15 <- as.data.frame(st_drop_geometry(geo_15))[, c("HV001", "lat", "long")]
ccoord16 <- ccoord15
ccoord11 <- as.data.frame(st_drop_geometry(geo_11))[, c("HV001", "lat", "long")]
ccoord12 <- ccoord11


## rdt positivity and other relevant variables extracted from household member recode survey data
tz_dhs22 <- TZ22_pr[,c("HML35","HV104","HV105","HV106","HML10","HV001","HV002","HV003","HV005","HV006","HV012","HV013",
                       "HV040","HV201","HV201B","HV246","HV270","HV023","HV021")]
tz_mis17 <- TZ17_pr[,c("HML35","HV104","HV105","HML10","HV001","HV002","HV003","HV005","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270","HV023","HV021")]
tz_mis17$HV201B <- NA
tz_mis17$HV106 <- NA
tz_dhs15 <- TZ15_pr[,c("HML35","HV104","HV105","HV106","HML10","HV001","HV002","HV003","HV005","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270","HV023","HV021")]
tz_dhs15$HV201B <- NA
tz_ais11 <- TZ11_pr[,c("HML35","HV104","HV105","HV106","HML10","HV001","HV002","HV003","HV005","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270","HV023","HV021")]
tz_ais11$HV201B <- NA 

#join DHS geography data for clusters
tz_dhs22 <- left_join(tz_dhs22, geo_22, by = "HV001")
tz_mis17 <- left_join(tz_mis17, geo_17, by = "HV001")
tz_dhs15 <- left_join(tz_dhs15, geo_15, by = "HV001")
tz_ais11 <- left_join(tz_ais11, geo_11, by = "HV001")


## attach monthly ecology data to survey datasets based on survey month
#re-defining survey years because vegetation is current not 1 month lagged
tz_dhs16 <- subset(tz_dhs15, HV006 == 1| HV006 == 2)
tz_dhs15 <- subset(tz_dhs15, HV006 == 8| HV006 == 9| HV006 == 10| HV006 == 11| HV006 == 12)
tz_ais12 <- subset(tz_ais11, HV006 == 1| HV006 == 2| HV006 == 3| HV006 == 4| HV006 == 5)
tz_ais11 <- subset(tz_ais11, HV006 == 12)

tz_dhs22 <- left_join(tz_dhs22, vegi22, by = "HV001")
tz_mis17 <- left_join(tz_mis17, vegi17, by = "HV001")
tz_dhs16 <- left_join(tz_dhs16, vegi16, by = "HV001")
tz_dhs15 <- left_join(tz_dhs15, vegi15, by = "HV001")
tz_ais12 <- left_join(tz_ais12, vegi12, by = "HV001")
tz_ais11 <- left_join(tz_ais11, vegi11, by = "HV001")

#add avg_vim to each survey based on month!
tz_dhs22 <- tz_dhs22 %>% mutate(veg = case_when(HV006 == 2 ~ vim_02,
                                                HV006 == 3 ~ vim_03, HV006 == 4 ~ vim_04, HV006 == 5 ~ vim_05,
                                                HV006 == 6 ~ vim_06, HV006 == 7 ~ vim_07))
tz_mis17 <- tz_mis17 %>% mutate(veg = case_when(HV006 == 10 ~ vim_10,
                                                HV006 == 11 ~ vim_11, HV006 == 12 ~ vim_12))
tz_dhs16 <- tz_dhs16 %>% mutate(veg = case_when(HV006 == 1 ~ vim_01, HV006 == 2 ~ vim_02))
tz_dhs15 <- tz_dhs15 %>% mutate(veg = case_when(HV006 == 8 ~ vim_08,
                                                HV006 == 9 ~ vim_09, HV006 == 10 ~ vim_10, HV006 == 11 ~ vim_11,
                                                HV006 == 12 ~ vim_12))
tz_ais12 <- tz_ais12 %>% mutate(veg = case_when(HV006 == 1 ~ vim_01,
                                                HV006 == 2 ~ vim_02, HV006 == 3 ~ vim_03, HV006 == 4 ~ vim_04,
                                                HV006 == 5 ~ vim_05)) 
tz_ais11 <- tz_ais11 %>% mutate(veg = case_when(HV006 == 11 ~ vim_11, HV006 == 12 ~ vim_12)) 

#re-attach separated survey years ...
tz_dhs15 <- rbind(tz_dhs15, tz_dhs16)
tz_ais11 <- rbind(tz_ais11, tz_ais12)
remove(tz_dhs16,tz_ais12)


#CHIRPS prior monthly average precipitation 
#subset rain into survey years..
rain$HV001 <- rain$id
rain <- rain[,c("HV001","year","month","avg_chirps")]

#subset into survey years 
rain22 <- subset(rain, year == 2022)
rain17 <- subset(rain, year == 2017)
rain16 <- subset(rain, year == 2016 & month %in% c("01"))
rain15 <- subset(rain, year == 2015)
rain12 <- subset(rain, year == 2012)
rain11 <- subset(rain, year == 2011 & month %in% c("11","12"))

#convert precipitation average into columns for each month
rain22 <- rain22 %>% pivot_wider(names_from = month, values_from = avg_chirps)
rain22 <- rain22 %>% rename_with(~ paste0("chirps_", .), .cols = matches("^\\d{2}$"))
rain17 <- rain17 %>% pivot_wider(names_from = month, values_from = avg_chirps)
rain17 <- rain17 %>% rename_with(~ paste0("chirps_", .), .cols = matches("^\\d{2}$"))
rain16 <- rain16 %>% pivot_wider(names_from = month, values_from = avg_chirps)
rain16 <- rain16 %>% rename_with(~ paste0("chirps_", .), .cols = matches("^\\d{2}$"))
rain15 <- rain15 %>% pivot_wider(names_from = month, values_from = avg_chirps)
rain15 <- rain15 %>% rename_with(~ paste0("chirps_", .), .cols = matches("^\\d{2}$"))
rain12 <- rain12 %>% pivot_wider(names_from = month, values_from = avg_chirps)
rain12 <- rain12 %>% rename_with(~ paste0("chirps_", .), .cols = matches("^\\d{2}$"))
rain11 <- rain11 %>% pivot_wider(names_from = month, values_from = avg_chirps)
rain11 <- rain11 %>% rename_with(~ paste0("chirps_", .), .cols = matches("^\\d{2}$"))

#join rain data to each survey year
#right now it's too confusing to keep them in original surveys and match by month and year
#so will maybe condense in the future but for now this is easier
tz_dhs16 <- subset(tz_dhs15, HV006 == 2)
tz_dhs15 <- subset(tz_dhs15, HV006 == 8| HV006 == 9| HV006 == 10| HV006 == 11| HV006 == 12| HV006 == 1)
tz_ais12 <- subset(tz_ais11, HV006 == 2| HV006 == 3| HV006 == 4| HV006 == 5)
tz_ais11 <- subset(tz_ais11, HV006 == 12| HV006 == 1)

tz_dhs22 <- left_join(tz_dhs22, rain22, by = "HV001")
tz_mis17 <- left_join(tz_mis17, rain17, by = "HV001")
tz_dhs16 <- left_join(tz_dhs16, rain16, by = "HV001")
tz_dhs15 <- left_join(tz_dhs15, rain15, by = "HV001")
tz_ais12 <- left_join(tz_ais12, rain12, by = "HV001")
tz_ais11 <- left_join(tz_ais11, rain11, by = "HV001")

#finally match based on survey month!
tz_dhs22 <- tz_dhs22 %>% mutate(rain = case_when(HV006 == 2 ~ chirps_01,
                                                 HV006 == 3 ~ chirps_02, HV006 == 4 ~ chirps_03,
                                                 HV006 == 5 ~ chirps_04, HV006 == 6 ~ chirps_05, HV006 == 7 ~ chirps_06))
tz_mis17 <- tz_mis17 %>% mutate(rain = case_when(HV006 == 10 ~ chirps_09,
                                                 HV006 == 11 ~ chirps_10, HV006 == 12 ~ chirps_11))
tz_dhs16 <- tz_dhs16 %>% mutate(rain = case_when(HV006 == 2 ~ chirps_01))
tz_dhs15 <- tz_dhs15 %>% mutate(rain = case_when(HV006 == 1 ~ chirps_12,
                                                 HV006 == 8 ~ chirps_07, HV006 == 9 ~ chirps_08,
                                                 HV006 == 10 ~ chirps_09, HV006 == 11 ~ chirps_10, HV006 == 12 ~ chirps_11))
tz_ais12 <- tz_ais12 %>% mutate(rain = case_when(HV006 == 2 ~ chirps_01,
                                                 HV006 == 3 ~ chirps_02, HV006 == 4 ~ chirps_03, HV006 == 5 ~ chirps_04)) 
tz_ais11 <- tz_ais11 %>% mutate(rain = case_when(HV006 == 1 ~ chirps_12, HV006 == 12 ~ chirps_11)) 

#empty columns so we can merge :{
tz_ais11$chirps_01 <- NA
tz_ais11$chirps_02 <- NA
tz_ais11$chirps_03 <- NA
tz_ais11$chirps_04 <- NA
tz_ais12$chirps_11 <- NA
tz_ais12$chirps_12 <- NA
tz_dhs15$chirps_01 <- NA
tz_dhs16$chirps_07 <- NA
tz_dhs16$chirps_08 <- NA
tz_dhs16$chirps_09 <- NA
tz_dhs16$chirps_10 <- NA
tz_dhs16$chirps_11 <- NA
tz_dhs16$chirps_12 <- NA

#re-attach separated survey years ...
tz_dhs15 <- rbind(tz_dhs15, tz_dhs16)
tz_ais11 <- rbind(tz_ais11, tz_ais12)
remove(tz_dhs16,tz_ais12)


#DHS monthly average temp (month of survey, not prior month)
tz_dhs22 <- tz_dhs22 %>% mutate(dhs_temp = case_when(HV006 == 2 ~ Temperature_February,
            HV006 == 3 ~ Temperature_March, HV006 == 4 ~ Temperature_April,
            HV006 == 5 ~ Temperature_May, HV006 == 6 ~ Temperature_June,  HV006 == 7 ~ Temperature_July))
tz_mis17 <- tz_mis17 %>% mutate(dhs_temp = case_when(HV006 == 10 ~ Temperature_October,
            HV006 == 11 ~ Temperature_November, HV006 == 12 ~ Temperature_December))
tz_dhs15 <- tz_dhs15 %>% mutate(dhs_temp = case_when(HV006 == 1 ~ Temperature_January,
            HV006 == 2 ~ Temperature_February, HV006 == 8 ~ Temperature_August,
            HV006 == 9 ~ Temperature_September, HV006 == 10 ~ Temperature_October,
            HV006 == 11 ~ Temperature_November, HV006 == 12 ~ Temperature_December))
tz_ais11 <- tz_ais11 %>% mutate(dhs_temp = case_when(HV006 == 1 ~ Temperature_January,
            HV006 == 2 ~ Temperature_February, HV006 == 3 ~ Temperature_March,
            HV006 == 4 ~ Temperature_April, HV006 == 5 ~ Temperature_May, HV006 == 12 ~ Temperature_December))

#remove original monthly variables
tz_dhs22 <- tz_dhs22 %>% dplyr::select(-matches("^Temperature_|chirps|vim"))
tz_mis17 <- tz_mis17 %>% dplyr::select(-matches("^Temperature_|chirps|vim"))
tz_dhs15 <- tz_dhs15 %>% dplyr::select(-matches("^Temperature_|chirps|vim"))
tz_ais11 <- tz_ais11 %>% dplyr::select(-matches("^Temperature_|chirps|vim"))

#clear work space
remove(TZ22_pr, TZ17_pr, TZ15_pr, TZ11_pr, rain, rain22, rain17, rain15, rain16, rain12, rain11, admin,
       geo_11, geo_15, geo_17, geo_22, vegi11, vegi12, vegi15, vegi16, vegi17, vegi22)



## part two:    create mine exposure data ----

#add indicators for survey years and months
tz_dhs22$svy <- "dhs_2022"
tz_dhs22$svy_year <- 2022
tz_dhs22$svy_month <- tz_dhs22$HV006
tz_dhs22$svy_year_dec <- tz_dhs22$svy_year + ((tz_dhs22$svy_month)/12)

tz_mis17$svy <- "mis_2017"
tz_mis17$svy_year <- 2017
tz_mis17$svy_month <- tz_mis17$HV006
tz_mis17$svy_year_dec <- tz_mis17$svy_year + ((tz_mis17$svy_month)/12)

tz_dhs15$svy <- "dhs_2015"
tz_dhs15 <- tz_dhs15 %>% mutate(svy_year = case_when(HV006>6 ~ 2015, HV006<6 ~ 2016))
tz_dhs15$svy_month <- tz_dhs15$HV006
tz_dhs15 <- tz_dhs15 %>% mutate(svy_year_dec = case_when(HV006>6 ~ svy_year+((svy_month/12)-1/12),
                                                         HV006<6 ~ svy_year+svy_month/12))
#right now this makes sense for me to do....will maybe change later but keeping for now
tz_dhs16 <- tz_dhs15 %>% filter(svy_year == 2016)
tz_dhs15 <- tz_dhs15 %>% filter(svy_year == 2015)

tz_ais11$svy <- "ais_2011"
tz_ais11 <- tz_ais11 %>% mutate(svy_year = case_when(HV006==12 ~ 2011, HV006<6 ~ 2012))
tz_ais11$svy_month <- tz_ais11$HV006
tz_ais11 <- tz_ais11 %>% mutate(svy_year_dec = case_when(HV006==12 ~ svy_year+((svy_month/12)-1/12),
                                                         HV006<6 ~ svy_year+svy_month/12))
tz_ais12 <- tz_ais11 %>% filter(svy_year == 2012)
tz_ais11 <- tz_ais11 %>% filter(svy_year == 2011)

#combine big and regular mine data
mines$long <- mines$features.properties$longitude
mines$lat <- mines$features.properties$latitude

#subset mines data to years operational and coords
mines_edit <- mines[,c("lat", "long", "year_open", "year_close", "mineral", "workers", "fem_workers",
                       "building", "num_pits","cyanide","mercury","site_type","facilities","closetores" )]
mines_edit$size <- "artisanal"
bmines_edit <- big_mines[,c("lat", "long", "year_open", "year_close","mineral", "workers", "fem_workers",
                        "building", "num_pits","cyanide","mercury","site_type","facilities","closetores")]
bmines_edit$size <- "industrial"
mine_times <- rbind(mines_edit, bmines_edit)
remove(mines_edit, bmines_edit, geo_list)


## imputation for missing number of pits using MICE
mines_missing <- mine_times[,c("workers","fem_workers","building","mineral","num_pits", 
                               "cyanide","mercury","site_type","facilities","closetores")]
## missing data patterns 
#md.pattern(mines_missing)
#md.pattern(mine_times)

## by default it does 5 imputations using all variables for the missing data model
mines_imp <- mice(mines_missing, m = 5)

# completed dataset for all imputations stacked together
all_complete <- complete(mines_imp, "long")  # adds a .imp column

# pool values (average) across the 5 imputations
pooled_pits <- all_complete %>% group_by(.id) %>% summarise(num_pits_avg = mean(num_pits))

## merge back into original data
mine_times$num_pits_avg <- pooled_pits$num_pits_avg

#specify years to run proximity calculations
survey_years <- c(2011, 2012, 2015, 2016, 2017, 2022)

#list coordinates
survey_coords <- list(`2022` = ccoord22,`2017` = ccoord17,`2016` = ccoord16,`2015` = ccoord15,
                 `2012` = ccoord12,`2011` = ccoord11)

#apply function for every year for which we have data
tz_geo_prox <- reduce(survey_years, function(data, year) {
  # Retrieve coordinates for the current year
  year_coords <- survey_coords[[as.character(year)]]
  # Calculate proximity and combine results
  year_proximity <- calculate_proximity(year, mine_times, year_coords)
  if (nrow(data) == 0) {
    year_proximity  # First iteration
  } else {
    left_join(data, year_proximity, by = "HV001")  # Merge using HV001
  }
}, .init = tibble())

#extract proximity data for each year
proximity_data <- lapply(survey_years, function(year) {
  tz_geo_prox %>% dplyr::select(HV001,
      !!paste0("close_mine", year),
      !!paste0("close_big", year),
      !!paste0("nearest_dist", year),
      !!paste0("total_distances", year),
      !!paste0("mean_distances", year),
      !!paste0("big_dist", year),
      !!paste0("num_mines_rho", year),
      !!paste0("num_bmines_rho", year))
})
names(proximity_data) <- paste0("prox", survey_years)

#join proximity data with DHS datasets 
tz_dhs22 <- left_join(tz_dhs22, proximity_data[["prox2022"]], by = "HV001")
tz_mis17 <- left_join(tz_mis17, proximity_data[["prox2017"]], by = "HV001")
tz_dhs16 <- left_join(tz_dhs16, proximity_data[["prox2016"]], by = "HV001")
tz_dhs15 <- left_join(tz_dhs15, proximity_data[["prox2015"]], by = "HV001")
tz_ais12 <- left_join(tz_ais12, proximity_data[["prox2012"]], by = "HV001")
tz_ais11 <- left_join(tz_ais11, proximity_data[["prox2011"]], by = "HV001")

#remove trailing years from proximity variable names
names(tz_dhs22) <- sub("2022$", "", names(tz_dhs22))
names(tz_mis17) <- sub("2017$", "", names(tz_mis17))
names(tz_dhs16) <- sub("2016$", "", names(tz_dhs16))
names(tz_dhs15) <- sub("2015$", "", names(tz_dhs15))
names(tz_ais12) <- sub("2012$", "", names(tz_ais12))
names(tz_ais11) <- sub("2011$", "", names(tz_ais11))

#combine all years of survey data...
tz_dhs <- rbind(tz_dhs22, tz_mis17, tz_dhs16, tz_dhs15, tz_ais12, tz_ais11)

#subset participants with non-missing RDT data
tz_rdt <- tz_dhs %>% filter(!is.na(HML35))
#remove extra year column that comes from previous joins
tz_rdt$year <- tz_rdt$year.x

#clean workspace againnn
remove(tz_geo_prox,big_mines,mines,proximity_data,
       tz_ais11, tz_ais12, tz_dhs15, tz_dhs16, tz_mis17, tz_dhs22,
       ccoord11, ccoord12, ccoord15, ccoord16, ccoord17, ccoord22)


# recode RDT positivity indicator (rdt)
tz_rdt <- tz_rdt %>% mutate(rdt = case_when(HML35==0 ~ 0, HML35 >0 ~ 1, HML35==NA ~ NA))
# check what values >1 indicate ???

#binary variable for water source (water_cat, 1 = piped, 0 = unpiped)
tz_rdt$water_cat <- cut(tz_rdt$HV201, breaks=c(0, 12, Inf), labels=c(0,1), include.lowest = TRUE) 

#rename a couple of cluster level variables
tz_rdt$cluster <- tz_rdt$HV001
tz_rdt <- tz_rdt %>% mutate(urban = case_when(URBAN_RURA=="U" ~ 1, URBAN_RURA=="R" ~ 0))

#binary elevation where 1 = elevated above mosquito habitat
tz_rdt$elevation <- tz_rdt$ALT_GPS
tz_rdt <- tz_rdt %>% mutate(elevationb = case_when(HV040 > 1500 ~ 1, HV040 <= 1500 ~ 0))

#are these missing or is it really 9999 ???
#tz_rdt <- tz_rdt %>% mutate(across(c(elevation), ~ na_if(., 9999)))

#rename more variables for upcoming models
tz_rdt$age <- tz_rdt$HV105
tz_rdt <- tz_rdt %>% mutate(female = case_when(HV104==2 ~ 1, TRUE ~ 0))
tz_rdt <- tz_rdt %>% mutate(hml1_cat = case_when(HML10 == 0 ~ 0, HML10 > 0 ~ 1))

# subset to north western clusters (within 60 km of any mapped mines) and add new variables 
nw_rdt <- tz_rdt %>% filter(nearest_dist<=60)

#center all continuous variables around means
#recode nearest_dist to represent proximity (larger values = closer distance)
max_dist <- max(nw_rdt$nearest_dist)
nw_rdt$nearest_dist <- max_dist-nw_rdt$nearest_dist 
mean_dist <- mean(nw_rdt$nearest_dist)
nw_rdt$nearest_distc <- nw_rdt$nearest_dist - mean_dist

#age centered (agec) (range is 0-5 for rdt test results)
mean_age <- mean(nw_rdt$age)
nw_rdt$agec <- nw_rdt$age - mean_age

#wealth quintile centered (wealthc)
mean_wealth <- mean(nw_rdt$HV270)
nw_rdt$wealthc <- nw_rdt$HV270 - mean_wealth

#indicator variables for svy waves
nw_rdt <- nw_rdt %>% mutate(mis_2017 = case_when(svy=="mis_2017" ~ 1, TRUE ~ 0))
nw_rdt <- nw_rdt %>% mutate(dhs_2015 = case_when(svy=="dhs_2015" ~ 1, TRUE ~ 0))
nw_rdt <- nw_rdt %>% mutate(ais_2011 = case_when(svy=="ais_2011" ~ 1, TRUE ~ 0))

#proximity to an industrial (big) mine
max_indust <- max(nw_rdt$big_dist)
nw_rdt$nearest_indust <- max_indust - nw_rdt$big_dist
#centered proximity 
mean_indust <- mean(nw_rdt$nearest_indust)
nw_rdt$nearest_industc <- nw_rdt$nearest_indust - mean_indust

remove(rwa, bur, africa, uga, tza)



## part three:  risk factor analysis ----

## DHS survey design weight = household survey weight (HV005)
## should double check that it is fine to combine across surveys like this?
nw_rdt$wt <- nw_rdt$HV005

#factor all categorical variables for glms
nw_rdt$urban <- as.factor(nw_rdt$urban)
nw_rdt$water_cat <- as.factor(nw_rdt$water_cat)
nw_rdt$livestock <- as.factor(nw_rdt$HV246)
nw_rdt$elevationb <- as.factor(nw_rdt$elevationb)
nw_rdt$female <- as.factor(nw_rdt$female)
nw_rdt$close_mine <- as.factor(nw_rdt$close_mine)
nw_rdt$close_big <- as.factor(nw_rdt$close_big)

## survey design to do glms for non-imputed variables
DHS <- svydesign(id = nw_rdt$HV021, strata = nw_rdt$HV023, weights = nw_rdt$wt, data = nw_rdt, nest = TRUE)
options(survey.lonely.psu = "adjust")

## unadjusted glms for non-pits variables (original rdt_svy function)
studyvars <- c("female", "age", "close_big", "close_mine", "HV270", 
               "urban", "livestock", "water_cat", "elevationb")
nwsvy_glm <- map_dfr(studyvars, rdt_svy)
colnames(nwsvy_glm) <- c("term", "estimate", "std.error", "statistic", "p.value", "CIL_95", "CIU_95")

## imputed pits glm (using just one seed, set at the beginning)
svy_year_map <- c("dhs_2022" = 2022, "mis_2017" = 2017, "dhs_2015" = 2015,
                  "dhs_2015" = 2016, "ais_2011" = 2011, "ais_2011" = 2012)

imp_list <- lapply(1:5, function(i) {
  completed <- complete(mines_imp, i)
  mine_times_i <- mine_times
  mine_times_i$num_pits <- completed$num_pits
  
  pits_prox_i <- reduce(survey_years, function(data, year) {
    year_coords <- survey_coords[[as.character(year)]]
    year_pits <- pits_proximity(year, mine_times_i, year_coords)
    if (nrow(data) == 0) year_pits
    else left_join(data, year_pits, by = "HV001")
  }, .init = tibble())
  
  nw_rdt_i <- nw_rdt
  nw_rdt_i$nearest_num_pits <- NA_real_
  nw_rdt_i$avg_pits_rho <- NA_real_
  
  for (j in seq_along(svy_year_map)) {
    svy_label <- names(svy_year_map)[j]
    yr <- svy_year_map[[j]]            # index by position, not name
    rows <- which(nw_rdt_i$svy == svy_label & nw_rdt_i$svy_year == yr)
    lookup <- pits_prox_i[match(nw_rdt_i$HV001[rows], pits_prox_i$HV001), ]
    nw_rdt_i$nearest_num_pits[rows] <- lookup[[paste0("nearest_num_pits", yr)]]
    nw_rdt_i$avg_pits_rho[rows]     <- lookup[[paste0("avg_pits_rho", yr)]]
  }
  
  nw_rdt_i %>% mutate(
    nnpits_2level = factor(case_when(nearest_num_pits <= 18 ~ 0, nearest_num_pits > 18 ~ 1)))
})


imp_data <- imputationList(imp_list)
#surveydesign to use imputed data
DHS_imp <- svydesign(id = ~HV021, strata = ~HV023, weights = ~wt, data = imp_data, nest = TRUE)
#fit model using all imputed datasets, then pool for final coefficient estimate
fit_pits <- with(DHS_imp, svyglm(rdt ~ nnpits_2level, family = quasibinomial("identity")))
pool_pits <- MIcombine(fit_pits)

## format pooled pits result to match nwsvy_glm structure
ci_pits <- confint(pool_pits)
pits_row <- data.frame(term = "nnpits_2level1",
  estimate = pool_pits$coefficients[-1],
  std.error = sqrt(diag(pool_pits$variance))[-1],
  statistic = pool_pits$coefficients[-1] / sqrt(diag(pool_pits$variance))[-1],
  p.value = 2 * pt(-abs(pool_pits$coefficients[-1] / sqrt(diag(pool_pits$variance))[-1]),
                   df = pool_pits$df[-1]),
  CIL_95 = ci_pits[-1, 1],
  CIU_95 = ci_pits[-1, 2])

## merge into one table
nwsvy_glm <- bind_rows(nwsvy_glm, pits_row)

## forest plot
nwsvy_glm_plot <- nwsvy_glm %>% filter(!grepl("Intercept", term)) %>%
  mutate(label = case_when(term == "female1" ~ "Female (vs male)", term == "age" ~ "Age (years)",
      term == "close_mine1" ~ "Close (within 15 km) to any mine",
      term == "close_big1" ~ "Close (within 15 km) to industrial mine",
      term == "HV270" ~ "Wealth quintile", term == "urban1" ~ "Urban (vs rural)",
      term == "livestock1" ~ "Owns livestock",
      term == "nnpits_2level1" ~ ">18 pits at nearest mine (vs 0-18 pits)",
      term == "water_cat1" ~ "Piped water source (vs unpiped)",
      term == "elevationb1" ~ "Elevation > 1500 m (vs < 1500 m)", TRUE ~ term),
    significant = p.value < 0.05)

bivar_forest <- ggplot(nwsvy_glm_plot, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = CIL_95, xmax = CIU_95, color = significant), height = 0.3, linewidth = 0.8) +
  geom_point(aes(color = significant), size = 3, shape = 18) +
  scale_color_manual(values = c("FALSE" = "pink3", "TRUE" = "maroon4"), labels = c("p > 0.05", "p < 0.05")) +
  labs(title = "Risk Factors for RDT Positivity", subtitle = "Unadjusted GLM",
       x = "Prevalence Difference (95% CI)", y = "", color = "Significance") +
  theme_classic(base_size = 18) + 
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18),
        plot.subtitle = element_text(color = "gray30"), legend.position = "bottom")
#bivar_forest
#ggsave("C:/Users/cgait/OneDrive/Desktop/bivar_forest.jpeg",width=30,height=20,units=c("cm"),bivar_forest)


## chemical use in mine sites (and missing data for chemical use)
#table(mine_times$cyanide, useNA = "always")
#table(mine_times$mercury, useNA = "always")
#table(mine_times$cyanide, mine_times$mercury, useNA = "always")

## attach averaged nearest_num_pits & avg_pits_rho to nw_rdt for use in INLA
nw_rdt$nearest_num_pits <- rowMeans(sapply(imp_list, function(df) df$nearest_num_pits))
nw_rdt$avg_pits_rho <- rowMeans(sapply(imp_list, function(df) df$avg_pits_rho))

#centered nearest number of pits
mean_pits <- mean(nw_rdt$nearest_num_pits)
nw_rdt$nearest_num_pitsc <- nw_rdt$nearest_num_pits - mean_pits
#centered number of mines within rho
mean_mines <- mean(nw_rdt$num_mines_rho)
nw_rdt$num_mines_rhoc <- nw_rdt$num_mines_rho - mean_mines
#centered average number of pits at mines within rho
mean_avgnumpits <- mean(nw_rdt$avg_pits_rho)
nw_rdt$avg_pits_rhoc <- nw_rdt$avg_pits_rho - mean_avgnumpits

## table 1: study population characteristics (northwest subset and overall)

#non-weight to use svy for unweighted counts
nw_rdt$wt1 <- 1
nw_rdt$select <- 1
tz_rdt$wt1 <- 1
tz_rdt$select <- 1

#clusters with only one participant (lonely psu)
options(survey.lonely.psu="adjust")

#code nearest number of pits 2 levels
nw_rdt <- nw_rdt %>% mutate(nnpits_2level = case_when(nearest_num_pits>=18 ~ "more pits",
                                                      nearest_num_pits<18 ~ "fewer pits"))
#code average number of pits at mines within rho 2 levels
nw_rdt <- nw_rdt %>% mutate(avgpits_2level = case_when(avg_pits_rho>=2.72 ~ 1,
                                                      avg_pits_rho<2.72 ~ 0))
#survey designs for both datasets and non-weights for just counts ?
nw_svy <- svydesign(ids=nw_rdt$HV021, strata=nw_rdt$HV023, weights=nw_rdt$wt1, data=nw_rdt, nest = TRUE)
tz_svy <- svydesign(ids=tz_rdt$HV021, strata=tz_rdt$HV023, weights=tz_rdt$wt1, data=tz_rdt, nest = TRUE)
DHS <- svydesign(id = nw_rdt$HV021, strata = nw_rdt$HV023, weights = nw_rdt$wt, data = nw_rdt, nest = TRUE)

#variables to pull counts for
nwsvy_vars <- c("urban","female","HV105","svy","water_cat","HV270","HV246", "elevationb","svy",
                "close_big","close_mine","nnpits_2level","avgpits_2level")
## no pit data for overall because we don't have mine data for the whole country
svy_vars <- c("urban","female","HV105","svy","water_cat","HV270","HV246","elevationb","svy")

## map variables to pull total by rdt outcome status (table 1 counts)
nw_rdt_count <- nw_counts(nwsvy_vars) %>% bind_rows(.id = "variable")
nw_total_count <- nw_total(nwsvy_vars) %>% bind_rows(.id = "variable")
## overall tz_dhs population characteristics 
#tz_rdt_count <- tz_counts(svy_vars) %>% bind_rows(.id = "variable")
tz_total_count <- tz_total(svy_vars) %>% bind_rows(.id = "variable")



## part four:   spatial models using INLA ------

## interaction terms for future EMM models
nw_rdt$npitxprox <- nw_rdt$nearest_num_pits*nw_rdt$nearest_dist
nw_rdt$rho_pitxprox <- nw_rdt$avg_pits_rho*nw_rdt$nearest_dist
nw_rdt$nmines_rho_x_prox <-nw_rdt$num_mines_rho*nw_rdt$nearest_dist
nw_rdt$avg_pits_x_prox <- nw_rdt$avg_pits_rho*nw_rdt$nearest_dist

## add SPDE to binomial logit to estimate spatial effects (range parameter and variance)
dat <- nw_rdt
dat$urban <- as.numeric(as.character(dat$urban))

# 1) project lon/lat to km
R <- 6371 
lat0 <- mean(dat$lat, na.rm=TRUE) * pi/180
x_km <- R * (dat$long * pi/180) * cos(lat0)
y_km <- R * (dat$lat * pi/180)
coords <- cbind(x_km, y_km)

# 2) make new mesh and SPDE for that subset
# Build mesh & spde once (outside the function)
mesh <- inla.mesh.2d(loc = coords, max.edge = c(5, 20), cutoff = 1)
spde  <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                             prior.range = c(10, 0.01), prior.sigma = c(5, 0.9))

# 3) prediction grid stuff (can be computed once)
grid_extent = c(29.4, 35, -5.5, -0.5)
pred_grid <- expand.grid(long = seq(grid_extent[1], grid_extent[2], length.out = 200),
                         lat  = seq(grid_extent[3], grid_extent[4], length.out = 200))
pred_grid_sf <- st_as_sf(pred_grid, coords = c("long", "lat"), crs = 4326)
pred_coords <- st_coordinates(pred_grid_sf)
A_pred <- inla.spde.make.A(mesh, loc = pred_coords)
n_pred <- nrow(pred_coords)

# 4) Compute nearest mine distance for prediction grid
## using nearest industrial mine in nearest distance calculations
#indust_times <- mine_times %>% filter(size=="industrial")
#mine_times_sf <- st_as_sf(indust_times, coords = c("long", "lat"), crs = 4326)
## restricting mine exposure to mines with chemicals reported
#mercury_times <- mine_times %>% filter(mercury==1)
#cyanide_times <- mine_times %>% filter(cyanide==1)
#chem_times <- unique(bind_rows(mercury_times, cyanide_times))
#mine_times_sf <- st_as_sf(chem_times, coords = c("long", "lat"), crs = 4326)
mine_times_sf <- st_as_sf(mine_times, coords = c("long", "lat"), crs = 4326)
nearest_distances <- st_distance(pred_grid_sf, mine_times_sf)
pred_grid_sf$nearest_dist <- apply(nearest_distances, 1, min) / 1000  # convert to km

#centered variable for nearest distance
grid_mean_dist <- mean(pred_grid_sf$nearest_dist)
pred_grid_sf$nearest_distc <- pred_grid_sf$nearest_dist - grid_mean_dist
#if we are using industrial mines above
#pred_grid_sf$nearest_industc <- pred_grid_sf$nearest_distc

# 5) A for observation stack 
A_obs_sub <- inla.spde.make.A(mesh, loc = coords) 
# separate As for observations and spatial slope?
A_obs_slope <- Matrix::Diagonal(n = nrow(A_obs_sub), x = dat$nearest_distc) %*% A_obs_sub   # centered nearest distance, change to indust if necessary 

# 6) two separate SPDE indices (same mesh/spde, different names) 
spde_idx0 <- inla.spde.make.index("spatial0", n.spde = spde$n.spde) 
spde_idx1 <- inla.spde.make.index("spatial1", n.spde = spde$n.spde) 

# 7) stack_obs: 3 A blocks: intercept/covariates + 2 spatial fields 
stack_obs <- inla.stack(data = list(y = dat$rdt), 
                        A = list(A_obs_sub, A_obs_slope, 1), 
                        effects = list( spatial0 = spde_idx0, 
                                        spatial_slope = spde_idx1, data.frame(Intercept = 1, 
                                        nearest_dist = dat$nearest_distc, # centered
                                        elevation = dat$elevationb, age = dat$agec, sex = dat$female, 
                                        mis_2017 = dat$mis_2017, dhs_2015 = dat$dhs_2015, 
                                        ais_2011 = dat$ais_2011, urban = dat$urban, 
                                        indust = dat$nearest_industc, wealth = dat$wealthc, 
                                        nearest_num_pits = dat$nearest_num_pits, avg_pits_rho = dat$avg_pits_rhoc,
                                        npitxprox = dat$npitxprox, num_mines_rho = dat$num_mines_rho, 
                                        nmines_rho_x_prox = dat$nmines_rho_x_prox, 
                                        avg_pits_x_prox = dat$avg_pits_x_prox,
                                        avgpits_2level = dat$avgpits_2level,
                                        dhs_temp = dat$dhs_temp, rain = dat$rain, veg = dat$veg)), tag = "obs") 

# 8) stack_pred: must mirror the SAME 3 A blocks as observation stack
# using the same centered/scaled nearest_dist variable as in training 
pred_nearest_dist <- pred_grid_sf$nearest_distc # centered, change to indust if necessary 
A_pred_slope <- Matrix::Diagonal(n = nrow(A_pred), x = pred_nearest_dist) %*% A_pred 

# 9) prediction stack for all data
stack_pred <- inla.stack(data = list(y = NA), A = list(A_pred, A_pred_slope, 1), effects = list(
                spatial0 = spde_idx0, spatial_slope = spde_idx1, data.frame(Intercept = rep(1, n_pred), # n_pred rows
                nearest_dist = pred_nearest_dist,    # prediction grid values, unless we are using indust
        elevation = NA, age = NA, sex = NA, mis_2017 = NA,  dhs_2015 = NA, ais_2011 = NA, 
        urban = NA, indust = NA, wealth = NA, nearest_num_pits = NA, npitxprox = NA, num_mines_rho = NA, 
        nmines_rho_x_avg_pits = NA, avg_pits_x_prox = NA, avg_pits_rho = NA, dhs_temp = NA,
        rain = NA, veg = NA)), tag = "pred")

# 10) Combine observed + prediction stacks
stack_full <- inla.stack(stack_obs, stack_pred)

#Define formulas (use the same naming as before, spde object will be built inside the function)
formulas <- list(
                formula1 <- y ~ 0 + Intercept + nearest_dist + f(spatial0, model = spde),
                formula2 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + 
                          mis_2017 + dhs_2015 + ais_2011 + urban + wealth,
  formula3a <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde),
# formula3b <- y ~ 0 + Intercept + avg_pits_rho + elevation + age + sex +
#    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde))
               formula4 <- y ~ 0 + Intercept + nearest_dist + indust + f(spatial0, model = spde), 
                formula5 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +  
                         mis_2017 + dhs_2015 + ais_2011 + urban + wealth + indust + f(spatial0, model = spde),
                formula6 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + dhs_temp + rain + veg +  
                         mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde))

## run and export overall results (un-stratified)
#out_all <- run_inla_overall(nw_rdt, mine_times, formulas, mesh, spde, pred_grid_sf, A_pred, n_pred)

# Extract WAIC, DIC, and hyperpar means for all 6 models
#results <- dplyr::bind_rows(lapply(1:1, function(i) {
#  mod <- out_all[["models"]][[paste0("model_", i)]]
#  hyper_means <- mod[["summary.hyperpar"]][["mean"]]
#  base <- data.frame(model = paste0("model_", i),
#    waic  = mod[["waic"]][["waic"]],
#    dic   = mod[["dic"]][["dic"]])
#  if (!is.null(hyper_means)) {
#    hyper_df <- as.data.frame(t(setNames(hyper_means, paste0("hyperpar_mean_", seq_along(hyper_means)))))
#    base <- cbind(base, hyper_df)
#  }
#  base
#}))
#write_xlsx(results, "inla_overall_results.xlsx")

## model predictions 
#pred_grid_sf$model3_prob <-   out_all$predictions$pred1_prob
#pred_grid_sf$pits_prob <-   out_all$predictions$pred1_prob

# stratified model 3 (less and more pits on average at mines within rho)
formulas <- list(formula3 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +  
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde))

nw_lpits <- nw_rdt %>% filter(avg_pits_rho <= 2.72)
nw_mpits <- nw_rdt %>% filter(avg_pits_rho > 2.72)
#lpits_out <- run_inla_overall(nw_lpits, mine_times, formulas, mesh, spde, pred_grid_sf, A_pred, n_pred)
#mpits_out <- run_inla_overall(nw_mpits, mine_times, formulas, mesh, spde, pred_grid_sf, A_pred, n_pred)

#extract predicted probabilities
#pred_grid_sf$lpits_prob <- lpits_out$predictions$pred1_prob
#pred_grid_sf$mpits_prob <- mpits_out$predictions$pred1_prob


## part five:   number of pits -----

## pits as predictor/EMM assessed using models with interaction terms and unadjusted beta 1s
pits_formulas <- list(
  #Model 1: number of pits at the nearest mine + spatial random effect
  formula1 <- y ~ 0 + Intercept + nearest_num_pits + f(spatial0, model = spde),
  #Model 2: number of pits at the nearest mine + survey covariates
 formula2 <- y ~ 0 + Intercept + nearest_num_pits + elevation + age + sex + mis_2017 + dhs_2015 + 
    ais_2011 + urban + wealth + f(spatial0, model = spde),
  #Model 3: number of mines within rho 
  formula3 <- y ~ 0 + Intercept + num_mines_rho + f(spatial0, model = spde),
  #Model 4: number of mines within rho + survey covariates
  formula4 <- y ~ 0 + Intercept + num_mines_rho + elevation + age + sex + mis_2017 + dhs_2015 + 
    ais_2011 + urban + wealth + f(spatial0, model = spde),
  #Model 5: average number of pits at mines within rho (21 km) 
   formula5 <- y ~ 0 + Intercept + avg_pits_rho + f(spatial0, model = spde),
  #Model 6: average number of pits + survey covariates
  formula6 <- y ~ 0 + Intercept + avg_pits_rho + elevation + age + sex + mis_2017 + dhs_2015 + 
    ais_2011 + urban + wealth + f(spatial0, model = spde),
  #Model 7 formula (DHS covariates + proximity + number of pits at nearest mine + nearest number of pits x proximity)
  formula7 <- y ~ 0 + Intercept + nearest_dist + nearest_num_pits + elevation + age + sex + 
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + npitxprox + f(spatial0, model = spde),
  #Model 8 formula (DHS covariates + proximity + average pits at mines w/in rho + average pits x proximity)
  formula8 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + mis_2017 + dhs_2015 + 
    ais_2011 + urban + wealth + avg_pits_rho + avg_pits_x_prox + f(spatial0, model = spde),
#Model 9 formula (DHS covariates + proximity, regular model 3!)
formula9 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + mis_2017 + dhs_2015 + 
  ais_2011 + urban + wealth + avg_pits_rho + f(spatial0, model = spde))

#pits_out <- run_inla_overall(nw_rdt, mine_times, pits_formulas, mesh, spde, pred_grid_sf, A_pred, n_pred)

## model predictions 
#pred_grid_sf$pitsmodel_prob <- pits_out$predictions$pred1_prob

# define which coefficient to pull from each model 
beta1_map <- list(model_1 = "nearest_num_pits", model_2 = "nearest_num_pits",
  model_3 = "num_mines_rho", model_4 = "num_mines_rho",
  model_5 = "avg_pits_rho", model_6 = "avg_pits_rho", 
  model_7 = "npitxprox", model_8 = "avg_pits_x_prox", model_9 = "nearest_dist")

# extract coefficients, WAIC, and DIC in one loop 
#results <- do.call(rbind, lapply(seq_along(beta1_map), function(i) {
#  mod_name  <- names(beta1_map)[i]
#  param     <- beta1_map[[i]]
#  mod       <- pits_out$models[[mod_name]]
#  fix       <- mod$summary.fixed[param, ]
#  data.frame(
#    model     = paste("Model", i),
#    parameter = param,
#    mean      = fix[["mean"]],
#    lower_95  = fix[["0.025quant"]],
#    upper_95  = fix[["0.975quant"]],
#    WAIC      = mod$waic$waic,
#    p_WAIC    = mod$waic$p.eff,
#    DIC       = mod$dic$dic,
#    p_DIC     = mod$dic$p.eff,
#    stringsAsFactors = FALSE)
#}))
#rownames(results) <- NULL

## write to Excel 
#wb <- createWorkbook()
#addWorksheet(wb, "Model Comparison")
#writeDataTable(wb, "Model Comparison", results, tableStyle = "TableStyleMedium9")
## styling
#headerStyle <- createStyle(textDecoration = "bold", halign = "center")
#addStyle(wb, "Model Comparison", headerStyle, rows = 1, cols = 1:ncol(results))
#saveWorkbook(wb, "pits_models.xlsx", overwrite = TRUE)

## pull coefficients for plotting
#pits_coeffs <- results[,c("model","mean","lower_95","upper_95")]

# Add a more descriptive label column
#pits_coeffs$label <- c("Model 1: RDT ~ Number of pits at nearest mine (unadjusted)",
#                       "Model 2: RDT ~ Number of pits at nearest mine (adjusted)",
#                       "Model 3: RDT ~ Number of mines within rho (unadjusted)",
#                       "Model 4: RDT ~ Number of mines within rho (adjusted)",
#                       "Model 5: RDT ~ Avg # of pits at mines within rho (unadjusted)",
#                       "Model 6: RDT ~ Avg # of pits at mines within rho (adjusted)",
#                       "Model 7: Number of pits × Proximity interaction coefficient (adjusted)", 
#                       "Model 8: Avg # of pits × Proximity interaction coefficient (adjusted)",
#                       "Model 9: RDT ~ Proximity to nearest mine (adjusted)")

# Reorder for plotting (bottom to top)
#pits_coeffs$label <- factor(pits_coeffs$label, levels = rev(pits_coeffs$label))

## add colors for different predictors!
model_colors <- c("skyblue","deepskyblue4","pink","pink4", "orange","darkorange4",
                  "orchid1","orchid4", "gold")

# Create forest plot
#pits_forest <- ggplot(pits_coeffs, aes(x = mean, y = label, color = label)) +
#  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
#  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95), height = 0.2, linewidth = 0.9) +
#  geom_point(size = 3) +
#  scale_color_manual(values = setNames(rev(model_colors), levels(pits_coeffs$label))) +
#  labs(subtitle = "Coefficients from INLA spatial models (95% Credible Intervals)",
#    x = "Coefficient estimate", y = NULL) +
#  theme_classic(base_size = 18) + theme(panel.grid.major.y = element_blank(),
#    panel.grid.minor = element_blank(), axis.text.y = element_text(size = 16),
#    legend.position = "none")
#pits_forest
#ggsave("C:/Users/cgait/OneDrive/Desktop/pits_forest.jpeg",width=40,height=20,units=c("cm"),pits_forest)


## part six:    sensitivity analysis for GLM ----

## for imputation of number of pits and resulting glm
#seeds <- c(111, 222, 333, 444, 555, 666, 777, 888, 999,
#           123, 234, 345, 456, 567, 678, 789, 890, 901,
#           987, 876, 765, 654, 543, 432, 321, 210, 109)

#all_results <- map_dfr(seeds, ~ run_imputed_glm(seed = .x, 
#  mines_missing = mines_missing, mine_times = mine_times, nw_rdt = nw_rdt, 
#  survey_years = survey_years, survey_coords = survey_coords, svy_year_map = svy_year_map))

# Add directionality variable
#all_results <- all_results %>%
#  mutate(direction = ifelse(estimate >= 0, "Positive", "Negative"))

#imp_forest <- ggplot(all_results, aes(x = estimate, y = term, 
#                                      color = direction, 
#                                      group = factor(seed))) +
#  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
#  geom_errorbarh(aes(xmin = CIL_95, xmax = CIU_95), 
#                 height = 0.2, position = position_dodge(width = 0.8)) +
#  geom_point(size = 3, position = position_dodge(width = 0.8)) +
#  scale_color_manual(values = c("Negative" = "#E63946", "Positive" = "#457B9D")) +
#  labs(x = "Prevalence Difference (95% CI)", y = "", color = "Direction") +
#  ggtitle("Coefficient estimates for unadjusted GLM: Sensitivity analysis", 
#          sub = "RDT ~ More pits (> 18) at nearest mine") +
#  theme_classic(base_size = 16)
#imp_forest



## part seven:  sensitivity analysis for range ----

## range parameter prior values
range_values <- c(1, 5, 10, 15)
range_probs <- c(0.01, 0.10, 0.25, 0.5, 0.75, 0.9)

var_values <- c(0.1, 1, 5, 10)
var_probs <- c(0.01, 0.10, 0.25, 0.5, 0.75, 0.9)

## import previously run models bc it takes 3 hours
range_sensitivity <- read_excel("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/range_sensitivity.xlsx")
variance_sensitivity <- read_excel("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/variance_sensitivity.xlsx")

# 4 x 6 = 24 fits
range_grid <- expand.grid(range_val = range_values, range_prob = range_probs)
variance_grid <- expand.grid(var_val = var_values, var_prob = var_probs)

#range_sensitivity <- pmap_dfr(range_grid, function(range_val, range_prob) {
#  message("Running range=", range_val, ", prob=", range_prob)
#  run_range_sensitivity(range_val, range_prob, mesh, stack_full)
#})

#export range sensitivity results
#write_xlsx(range_sensitivity, "C:/Users/cgait/OneDrive/Desktop/range_sensitivity.xlsx")

## Display results
## 1a) Heatmap of WAIC (variance held constant)
dic_heat_range <- ggplot(range_sensitivity, aes(x = factor(range_val), y = factor(range_prob), 
                                                 fill = waic)) +
  geom_tile(color = "white") + geom_text(aes(label = round(waic, 1)), size = 4, color="goldenrod") +
  scale_fill_viridis(option = "mako", direction = 1) +
  labs(x = "Range prior value (km)", y = "P(range < value)", fill = "WAIC", 
       title = "Range prior sensitivity", subtitle = "Variance prior fixed: σ ~ PC(1, 0.01)") + 
  theme_classic(base_size = 16)
#dic_heat_range

## 2) Heatmap of range parameter values (variance held constant)
mean_heat_range <- ggplot(range_sensitivity, aes(x = factor(range_val), y = factor(range_prob), 
                                                 fill = range_post_mean)) +
  geom_tile(color = "white") + geom_text(aes(label = round(range_post_mean, 1)), size = 4, color="white") +
  scale_fill_viridis(option = "turbo", direction = 1) +
  labs(x = "Range prior value (km)", y = "P(range < value)", fill = "Range", 
       title = "Range prior sensitivity", subtitle = "Variance prior fixed: σ ~ PC(1, 0.01)") + 
  theme_classic(base_size = 16)
#mean_heat_range 


## select pair of values with lowest values for both/on average


## using optimal range parameter values, repeat formula 5 using each combination of variance values & probabilities
#variance_sensitivity <- pmap_dfr(variance_grid, function(var_val, var_prob) {
# message("Running variance=", var_val, ", prob=", var_prob)
#  run_var_sensitivity(var_val, var_prob, formula, mesh, stack_full)
#})

#export variance sensitivity results
#write_xlsx(variance_sensitivity, "C:/Users/cgait/OneDrive/Desktop/variance_sensitivity.xlsx")

# 1) Heatmap of WAIC (range priors held constant)
waic_heat_var <- ggplot(variance_sensitivity, aes(x = factor(var_val), y = factor(var_prob), fill = dic)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(dic, 1)), size = 4, color = "goldenrod") +
  scale_fill_viridis(option = "viridis", direction = 1) +
  labs(x = "Variance prior value (km)", y = "P(variance < value)",
       fill = "DIC", title = "Variance prior sensitivity",
       subtitle = "Range priors fixed: ρ ~ PC(10, 0.01)") +
  theme_classic(base_size = 16)
#waic_heat_var

# 2) heatmap of range values based on varying the variance priors
range_heat_var <- ggplot(variance_sensitivity, aes(x = factor(var_val), y = factor(var_prob), 
                        fill = range_post_mean)) + geom_tile(color = "white") + 
  geom_text(aes(label = round(range_post_mean, 1)), size = 4, color = "white") +
  scale_fill_viridis(option = "turbo", direction = 1) +
  labs(x = "Variance prior value (km)", y = "P(variance < value)",
       fill = "Range", title = "Variance prior sensitivity",
       subtitle = "Range priors fixed: ρ ~ PC(10, 0.01)") +
  theme_classic(base_size = 16)
#range_heat_var


## part eight:  maps (predictions and observed) ----

# Pull DHS clusters for map (include HV001 and svy_year for proximity lookup)
nw_cluster <- nw_rdt[, c("HV001", "lat", "long", "svy", "svy_year", "rdt")]
cluster_prev <- nw_cluster %>% group_by(lat, long, svy) %>%
  summarise(n = n(), positives = sum(rdt, na.rm = TRUE),
            prevalence = mean(rdt, na.rm = TRUE)) %>% ungroup()

## Re-calculate proximity using only chemical mines (chem_times, created in part 4)
#chem_prox <- reduce(survey_years, function(data, year) {
#  year_coords <- survey_coords[[as.character(year)]]
#  year_proximity <- calculate_proximity(year, chem_times, year_coords)
#  if (nrow(data) == 0) year_proximity
#  else left_join(data, year_proximity, by = "HV001")
#}, .init = tibble())

# Build long-format lookup: one row per HV001 x survey year with nearest_dist
#chem_nearest <- bind_rows(lapply(survey_years, function(yr) {
#  chem_prox %>%
#    dplyr::select(HV001, nearest_dist = !!paste0("nearest_dist", yr)) %>%
#    mutate(svy_year = yr)
#}))

# Subset to clusters within 60 km of a chemical mine active during their survey year
#nw_cluster <- nw_cluster %>%
#  left_join(chem_nearest, by = c("HV001", "svy_year")) %>%
#  filter(nearest_dist <= 60) %>%
#  dplyr::select(-nearest_dist, -svy_year)

# Recompute cluster_prev on the filtered clusters
cluster_prev <- nw_cluster %>% group_by(lat, long, svy) %>%
  summarise(n = n(), positives = sum(rdt, na.rm = TRUE),
            prevalence = mean(rdt, na.rm = TRUE)) %>% ungroup()

## Subset chemical mines to those active during each survey wave
mine_times_11 <- mine_times_sf %>% filter(year_open < 2011)
mine_times_15 <- mine_times_sf %>% filter(year_open <= 2016, year_close >= 2016)
mine_times_17 <- mine_times_sf %>% filter(year_open <= 2017, year_close >= 2017)
mine_times_22 <- mine_times_sf %>% filter(year_open <= 2022, year_close >= 2022)


## clip effects grid to TZ district boundaries, after fixing invalid geometries
district_valid <- st_make_valid(district)
# Now union the,
tanzania_boundary <- st_union(district_valid)
# Re-project prediction grid to match boundary CRS (or vice versa)
pred_grid_sf <- st_transform(pred_grid_sf, st_crs(tanzania_boundary))
# Clip the prediction grid to the Tanzania boundary
pred_grid_sf <- pred_grid_sf[st_within(st_centroid(pred_grid_sf), tanzania_boundary, sparse = FALSE), ]

## then clip effects (within TZ boundary) to a convex polygon using cluster locations
#pull both cluster and mine coordinates into one list
cluster_coord <- nw_cluster[,c("lat","long")]
#mines_coord <- mine_times[,c("lat","long")]
mines_coord <- chem_times[,c("lat","long")]
all_coords <- bind_rows(cluster_coord, mines_coord)
cluster_sf <- st_as_sf(all_coords, coords = c("long", "lat"), crs = 4326)
# convex hull polygon
convex_poly <- cluster_sf %>% summarise() %>% st_convex_hull()
# reproject back to lon/lat if needed
convex_poly <- st_transform(convex_poly, 4210)
convex_poly_buff <- st_buffer(convex_poly, dist = 0.075)  # buffer in degrees; adjust as needed
pred_overall_clipped <- st_intersection(pred_grid_sf, convex_poly_buff)

remove(africa, all_complete, cluster_coord, clusters, clusters_sf, cluster_sf, complete1, convex_poly, 
       convex_poly_buff, coords, district, pred_grid, mines_coord, mine_times_imp1, mines_imp, mines_missing, 
       pred_nearest_dist)

## malaria prevalence (RDT) in each survey wave
# Create a list of survey specifications
survey_specs <- list(list(svy = "dhs_2022", mine_data = mine_times_22, title = "2022 DHS"),
  list(svy = "mis_2017", mine_data = mine_times_17, title = "2017 MIS"),
  list(svy = "dhs_2015", mine_data = mine_times_15, title = "2015-16 DHS"),
  list(svy = "ais_2011", mine_data = mine_times_11, title = "2011-12 AIS"))

# Generate prevalence maps (top row)
#prev_maps <- lapply(survey_specs, function(spec) {
 # cluster_data <- cluster_prev %>% filter(svy == spec$svy)
#  create_prev_map(cluster_data, spec$title)
#})

# Generate mine maps (bottom row)
#mine_maps <- lapply(survey_specs, function(spec) {
#  create_mine_map(spec$mine_data, spec$title)
#})

# Combine all maps: prevalence on top row, mines on bottom row
#all_maps <- c(prev_maps, mine_maps)

# Arrange the maps: 2 rows (prevalence, then mines) x 4 columns (surveys)
#svys_prev <- ggarrange(plotlist = all_maps, nrow = 2, ncol = 4)
#svys_prev
#ggsave("C:/Users/cgait/OneDrive/Desktop/svys_prev.jpeg",width=45,height=15,units=c("cm"),svys_prev)


## predicted prevalence for model 3, full study population
#pred_3 <- ggplot() + geom_sf(data = bounds, fill = "grey90") + geom_sf_text(data = bounds,
#                        aes(label = NAME_0),color = "grey35",size = 3, fontface = "italic") +
#  geom_sf(data = pred_overall_clipped, aes(color = model3_prob)) +
#  geom_sf(data = Victoria, fill = "skyblue", color = "skyblue") +
#  scale_color_viridis_c(option = "inferno", name = "Predicted prevalence", direction = -1, alpha = 0.7) + 
#  ggnewscale::new_scale("color") +
#  geom_sf(data = mine_times_sf, aes(shape = size, color = size), size = 2, alpha = 0.7) +
#  scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
#  scale_color_manual(values = c("artisanal" = "plum1", "industrial" = "darkorchid4"), name = "Mine type") +
#  geom_point(data = nw_cluster, aes(x = long, y = lat), color = "grey75",shape = 21, size = 2.5) +
#  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) +
#  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7) +
#  theme(legend.title = element_text(size = 12), legend.text  = element_text(size = 12),
#        axis.text   = element_blank(), axis.ticks  = element_blank(),
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#  labs(x = "", y = "") + ggtitle("RDT positivity predicted by Model 3")
#pred_3
#ggsave("C:/Users/cgait/OneDrive/Desktop/model3_pred.jpeg",width=25,height=15,units=c("cm"),pred_3)


## predicted prevalence for avg pits rho model, full study population
#pred_pits <- ggplot() + geom_sf(data = bounds, fill = "grey90") + geom_sf_text(data = bounds,
#                        aes(label = NAME_0),color = "grey35",size = 3, fontface = "italic") +
#  geom_sf(data = pred_overall_clipped, aes(color = pits_prob)) +
#  geom_sf(data = Victoria, fill = "skyblue", color = "skyblue") +
#  scale_color_viridis_c(option = "inferno", name = "Predicted prevalence", direction = -1, alpha = 0.7) + 
#  ggnewscale::new_scale("color") +
#  geom_sf(data = mine_times_sf, aes(shape = size, color = size), size = 2, alpha = 0.7) +
#  scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
#  scale_color_manual(values = c("artisanal" = "plum1", "industrial" = "darkorchid4"), name = "Mine type") +
#  geom_point(data = nw_cluster, aes(x = long, y = lat), color = "grey75",shape = 21, size = 2.5) +
#  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) +
#  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7) +
#  theme(legend.title = element_text(size = 12), legend.text  = element_text(size = 12),
#        axis.text   = element_blank(), axis.ticks  = element_blank(),
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#  labs(x = "", y = "") + ggtitle("RDT positivity predicted by proximity to industrial mining")
#pred_pits
#ggsave("C:/Users/cgait/OneDrive/Desktop/pitsmodel_pred.jpeg",width=25,height=15,units=c("cm"),pred_pits)


## overall/combined plot across all survey waves, stratified by exposure to puts

# shared color limits across both strata
shared_limits <- range(c(pred_overall_clipped$mpits_prob, pred_overall_clipped$lpits_prob), na.rm = TRUE)

# exposed to more pits
pred_mpits <- ggplot() + geom_sf(data = bounds, fill = "grey90") + geom_sf_text(data = bounds,
           aes(label = NAME_0),color = "grey35",size = 3, fontface = "italic") +
  geom_sf(data = pred_overall_clipped, aes(color = mpits_prob)) +
  geom_sf(data = Victoria, fill = "skyblue", color = "skyblue") +
  scale_color_scico(palette = "managua", name = "Predicted prevalence", direction = -1, alpha = 0.7,
                        limits = shared_limits) + 
  ggnewscale::new_scale("color") +
  geom_sf(data = mine_times_sf, aes(shape = size, color = size), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
  scale_color_manual(values = c("artisanal" = "plum1", "industrial" = "darkorchid4"), name = "Mine type") +
  geom_point(data = nw_cluster, aes(x = long, y = lat), color = "grey95",shape = 21, size = 1.5) +
  coord_sf(xlim = c(30.7, 34.8), ylim = c(-4.8, -1)) +
  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7) +
  theme(legend.title = element_text(size = 12), legend.text  = element_text(size = 12),
        axis.text   = element_blank(), axis.ticks  = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "", y = "") + ggtitle("Clusters exposed to more pits (>2.7) on average 
  at mines using chemicals within 25.5 km")

#exposed to fewer pits
pred_lpits <- ggplot() + geom_sf(data = bounds, fill = "grey90") + geom_sf_text(data = bounds,
         aes(label = NAME_0),color = "grey35",size = 3, fontface = "italic") +
  geom_sf(data = pred_overall_clipped, aes(color = lpits_prob)) +
  geom_sf(data = Victoria, fill = "skyblue", color = "skyblue") +
  scale_color_scico(palette = "managua", name = "Predicted prevalence", direction = -1, alpha = 0.7,
                        limits = shared_limits) + 
  ggnewscale::new_scale("color") +
  geom_sf(data = mine_times_sf, aes(shape = size, color = size), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
  scale_color_manual(values = c("artisanal" = "plum1", "industrial" = "darkorchid4"), name = "Mine type") +
  geom_point(data = nw_cluster, aes(x = long, y = lat), color = "grey95",shape = 21, size = 1.5) +
  coord_sf(xlim = c(30.7, 34.8), ylim = c(-4.8, -1)) +
  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7) +
  theme(legend.title = element_text(size = 12), legend.text  = element_text(size = 12),
        axis.text   = element_blank(), axis.ticks  = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "", y = "") + ggtitle("Clusters exposed to fewer pits (<2.7) on average 
  at mines using chemicals within 25.5 km")

#model3_pits <- ggarrange(pred_mpits, pred_lpits, nrow = 1, ncol = 2)
#ggsave("C:/Users/cgait/OneDrive/Desktop/model3_chemical_pits.jpeg",width=40,height=15,units=c("cm"),model3_pits)

