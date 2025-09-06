## Malaria in TZ national surveys from 2011-2022, predicted by proximity to artisanal and industrial mines----

#part one: data extract and organization----

#load packages
library(broom)
library(broom.mixed)
library(dplyr)
library(geosphere)
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(haven)
library(INLA)
library(jsonlite)
library(lwgeom)
library(MASS)
library(Matrix)
library(pscl)
library(purrr)
library(readr)
library(readxl)
library(sf)
library(statmod)
library(stars)
library(stats)
library(terra)
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
#DHS 2010 household member recode
#TZ10_pr <- read_sas("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS10/TZPR63SD/TZPR63FL.SAS7BDAT")

#geography data & geocovariates for all surveys
geo_22 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS22/TZGE81FL/TZGE81FL.shp")
geo_cov22 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS22/TZGC81FL_geocov/TZGC81FL.csv")
geo_17 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_MIS17/TZGE7IFL/TZGE7IFL.shp")
geo_cov17 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_MIS17/TZGC7JFL/TZGC7JFL.csv")
geo_15 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS15/TZGE7AFL/TZGE7AFL.shp")
geo_cov15 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS15/TZGC7BFL/TZGC7BFL.csv")
geo_11 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_AIS11/TZGE6AFL/TZGE6AFL.shp")
geo_cov11 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_AIS11/TZGC6BFL/TZGC6BFL.csv")
#geo_10 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS10/TZ_DHS10_geo/TZGE61FL/TZGE61FL.shp")
#geo_cov10 <- read.csv("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/TZ_DHS10/TZ_DHS10_geo/TZGC62FL/TZGC62FL.csv")

#artisinal mines in north west Tanzania
mines <- fromJSON("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/tza_mines_curated_all_opendata_p_ipis.json")
mines <- as.data.frame(mines)
#bigger mines in north west TZ manually compiled from mention in the IPIS report 
big_mines <- read_excel("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/big_mines.xlsx")

#and roads
#roads <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/hotosm_tza_roads_lines_shp/hotosm_tza_roads_lines_shp.shp")
#subset road data 
#roi <- st_bbox(c(xmin = 29.4, xmax = 35, ymin = -5.5, ymax = -0.5), crs = st_crs(roads)) |> st_as_sfc()
#roads_crop <- st_crop(roads, roi)
#roads_path <- roads_crop %>% filter(highway=="path")
#roads_res <- roads_crop %>% filter(highway=="residential")
#roads_foot <- roads_crop %>% filter(highway=="footway")
#roads_123 <- roads_crop %>% filter(highway %in% c("primary", "secondary", "tertiary"))
# Export as shapefile (creates multiple files: .shp, .shx, .dbf, etc.)
#st_write(roads_path, "C:/Users/cgait/OneDrive/Desktop/TZ mining/data/roads_shp/roads_path.shp")

#read back in subsetted roads
roads_123 <- st_read("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/roads_shp/roads_123.shp")
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
bounds<-rbind(bur, rwa, uga)

# site names for artisinal mine sites
mines$name <- mines$features.properties$sitename
#number of workers
mines$workers <- mines$features.properties$worker
mines$fem_workers <- mines$features.properties$workerwomen
# building structure permanence 
mines$building <- mines$features.properties$buildingtype
# ~minimum years operational based on building type permanence
mines <- mines %>% mutate(min_years = case_when(building=="None" ~ 0,
                   building=="Permanent" ~ 3, building=="Temporary/makeshift" ~ 1,
                   building=="Temporary/makeshift, Permanent" ~ 2))
# ~year mine has been operational since, y_open
mines$year_open <- 2018-mines$min_years
mines$year_close <- 2024   # assume all mines mapped in 2018 are still open in 2022..
# re-examine at some point critically


#ecology data from CHIRPS & MODIS 
vegi22 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi22.shp")
vegi17 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi17.shp")
vegi16 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi16.shp")
vegi15 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi15.shp")
vegi12 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi12.shp")
vegi11 <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/vegi_sf/vegi11.shp")
admin <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/regions/tza_admbnda_adm2_20181019/tza_admbnda_adm2_20181019.shp")
rain <- read_excel("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/avg_precip.xlsx")
#temp <- read_excel("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/eco_data/avg_tmax.xlsx")

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
TZ22_pr$hml
tz_dhs22 <- TZ22_pr[,c("HML35","HV104","HV105","HV106","HML10","HV001","HV002","HV003","HV006","HV012","HV013",
                       "HV040","HV201","HV201B","HV246","HV270")]
tz_mis17 <- TZ17_pr[,c("HML35","HV104","HV105","HML10","HV001","HV002","HV003","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270")]
tz_mis17$HV201B <- NA
tz_mis17$HV106 <- NA
tz_dhs15 <- TZ15_pr[,c("HML35","HV104","HV105","HV106","HML10","HV001","HV002","HV003","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270")]
tz_dhs15$HV201B <- NA
tz_ais11 <- TZ11_pr[,c("HML35","HV104","HV105","HV106","HML10","HV001","HV002","HV003","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270")]
tz_ais11$HV201B <- NA 
#tz_dhs10 <- TZ10_pr %>% select(HV104,HV105,HV106,HML10,HV001,HV002,HV003,HV006,HV012,HV013,
#                        HV040,HV201,HV246,HV270)
#no HML35 in DHS10 ? Is there another coding or maybe not

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
            HV006 == 5 ~ Temperature_May, HV006 == 6 ~ Temperature_June, 
            HV006 == 7 ~ Temperature_July))
tz_mis17 <- tz_mis17 %>% mutate(dhs_temp = case_when(HV006 == 10 ~ Temperature_October,
            HV006 == 11 ~ Temperature_November, HV006 == 12 ~ Temperature_December))
tz_dhs15 <- tz_dhs15 %>% mutate(dhs_temp = case_when(HV006 == 1 ~ Temperature_January,
            HV006 == 2 ~ Temperature_February, HV006 == 8 ~ Temperature_August,
            HV006 == 9 ~ Temperature_September, HV006 == 10 ~ Temperature_October,
            HV006 == 11 ~ Temperature_November, HV006 == 12 ~ Temperature_December))
tz_ais11 <- tz_ais11 %>% mutate(dhs_temp = case_when(HV006 == 1 ~ Temperature_January,
            HV006 == 2 ~ Temperature_February, HV006 == 3 ~ Temperature_March,
            HV006 == 4 ~ Temperature_April, HV006 == 5 ~ Temperature_May,
            HV006 == 12 ~ Temperature_December))

#remove original monthly variables
tz_dhs22 <- tz_dhs22 %>% dplyr::select(-matches("^Temperature_|chirps|vim"))
tz_mis17 <- tz_mis17 %>% dplyr::select(-matches("^Temperature_|chirps|vim"))
tz_dhs15 <- tz_dhs15 %>% dplyr::select(-matches("^Temperature_|chirps|vim"))
tz_ais11 <- tz_ais11 %>% dplyr::select(-matches("^Temperature_|chirps|vim"))

#clear work space
remove(TZ22_pr, TZ17_pr, TZ15_pr, TZ11_pr, rain, rain22, rain17, rain15, rain16, rain12, rain11, admin,
       geo_11, geo_15, geo_17, geo_22, vegi11, vegi12, vegi15, vegi16, vegi17, vegi22)



#part two: create mine exposure data over time ----
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
mines_edit <- mines[,c("lat", "long", "year_open", "year_close")]
mines_edit$size <- "artisanal"
bmines_edit <- big_mines[,c("lat", "long", "year_open", "year_close")]
bmines_edit$size <- "industrial"
mine_times <- rbind(mines_edit, bmines_edit)
remove(mines_edit, bmines_edit, geo_list)


calculate_proximity <- function(year, mine_data, cluster_coords) {
  # Filter active mines for the given year
  mines <- mine_data %>% filter(year_open <= year & year_close > year)
  big_mines <- mines %>% filter(size == "industrial")
  
  # Pull coordinates for mines
  mine_coords <- mines[, c("lat", "long")]
  big_mine_coords <- big_mines[, c("lat", "long")]
  
  # Pull cluster coordinates and ensure HV001 is retained
  cluster_coords <- cluster_coords %>% dplyr::select(HV001, lat, long)
  
  # Calculate distances
  distances <- apply(cluster_coords[, c("lat", "long")], 1, function(point) distHaversine(point, mine_coords))
  big_distances <- apply(cluster_coords[, c("lat", "long")], 1, function(point) distHaversine(point, big_mine_coords))
  
  # Count mines within 15 km
  mines_within_15km <- apply(distances, 2, function(d) sum(d < 15000))
  big_mines_within_15km <- apply(big_distances, 2, function(d) sum(d < 15000))
  
  # Find nearest distances
  nearest_distances <- apply(distances, 2, min) / 1000 # Convert to km
  nearest_big_distances <- apply(big_distances, 2, min) / 1000 # Convert to km
  
  # Add proximity calculations to cluster data
  cluster_data <- cluster_coords %>%
    mutate(
      !!paste0("num_mines", year) := mines_within_15km,
      !!paste0("total_distances", year) := apply(distances, 2, sum) / 1000,
      !!paste0("mean_distances", year) := apply(distances, 2, mean) / 1000,
      !!paste0("num_bmines", year) := big_mines_within_15km,
      !!paste0("nearest_dist", year) := nearest_distances,
      !!paste0("big_dist", year) := nearest_big_distances,
      !!paste0("close_mine", year) := if_else(nearest_distances <= 15, 1, 0),
      !!paste0("close_big", year) := if_else(nearest_big_distances <= 15, 1, 0)
    )
  
  # Return updated cluster data
  return(cluster_data)
}

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
      !!paste0("num_mines", year),
      !!paste0("num_bmines", year)
    )
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
remove(tz_geo_prox,survey_coords,big_mines,mines,survey_years,proximity_data,
       tz_ais11, tz_ais12, tz_dhs15, tz_dhs16, tz_mis17, tz_dhs22, tz_dhs,
       ccoord11, ccoord12, ccoord15, ccoord16, ccoord17, ccoord22)

# subset to north western clusters (within 60 km of any mapped mines) and add new variables 
nw_rdt <- tz_rdt %>% filter(nearest_dist<=60)

# recode RDT positivity indicator (rdt)
nw_rdt <- nw_rdt %>% mutate(rdt = case_when(HML35==0 ~ 0, HML35 >0 ~ 1, HML35==NA ~ NA))
# check what values >1 indicate ???

#categorical variables recode
nw_rdt <- nw_rdt %>% mutate(age_cat = case_when(HV105 < 15 ~ "15 years or younger",
                     HV105 <= 25 & HV105 > 15 ~ "16-25 years old",
                     HV105 <= 35 & HV105 > 25 ~ "26-35 years old",
                     HV105 <= 45 & HV105 > 35 ~ "36-45 years old",
                     HV105 <= 55 & HV105 > 45 ~ "46-55 years old",
                     HV105 <= 65 & HV105 > 55 ~ "56-65 years old"))
nw_rdt$edu_cat <- as.factor(nw_rdt$HV106)
nw_rdt <- nw_rdt %>% mutate(edu_catb = case_when(HV106==0 ~ "Primary or no education",
                     HV106==1 ~ "Primary or no education",
                     HV106==2 ~ "Secondary education",
                     HV106==3 ~ "Secondary education"))

#binary variable for water source (water_cat, 1 = piped, 0 = unpiped)
nw_rdt$water_cat <- cut(nw_rdt$HV201, breaks=c(0, 12, Inf), labels=c(0,1), include.lowest = TRUE) 
#nw_rdt <- nw_rdt %>% mutate(wealthb = case_when(HV270==1 ~ "Wealth quintiles 1 & 2",
#                     HV270==2 ~ "Wealth quintiles 1 & 2", HV270==3 ~ "Wealth quintiles 3-5",
#                     HV270==4 ~ "Wealth quintiles 3-5", HV270==5 ~ "Wealth quintiles 3-5"))

#rename a couple of cluster level variables
nw_rdt$cluster <- nw_rdt$HV001
nw_rdt <- nw_rdt %>% mutate(urban = case_when(URBAN_RURA=="U" ~ 1, URBAN_RURA=="R" ~ 0))
nw_rdt$urban <- as.factor(nw_rdt$urban)

#binary elevation where 1 = elevated above mosquito habitat
nw_rdt$elevation <- nw_rdt$ALT_GPS
nw_rdt <- nw_rdt %>% mutate(elevationb = case_when(HV040 > 1500 ~ 1, HV040 <= 1500 ~ 0))

#replace -9999 in rain and temp with missing
nw_rdt <- nw_rdt %>% mutate(across(c(rain, dhs_temp), ~ na_if(., -9999)))
#are these missing or is it really 9999 ???
#nw_rdt <- nw_rdt %>% mutate(across(c(elevation), ~ na_if(., 9999)))

#rename more variables for upcoming models
nw_rdt$age <- nw_rdt$HV105
nw_rdt <- nw_rdt %>% mutate(female = case_when(HV104==2 ~ 1, TRUE ~ 0))

#center all continuous variables around means
#recode nearest_dist to represent proximity (larger values = closer distance)
nw_rdt$nearest_dist <- 60-nw_rdt$nearest_dist 
mean_dist <- mean(nw_rdt$nearest_dist)
nw_rdt$nearest_distc <- nw_rdt$nearest_dist - mean_dist

#age centered (agec) (range is 0-5 for rdt test results)
mean_age <- mean(nw_rdt$age)
nw_rdt$agec <- nw_rdt$age - mean_age

#wealth quintile centered (wealthc)
mean_wealth <- mean(nw_rdt$HV270)
nw_rdt$wealthc <- nw_rdt$HV270 - mean_wealth

#indicator variables for svy wave
nw_rdt <- nw_rdt %>% mutate(dhs_2022 = case_when(svy=="dhs_2022" ~ 1, TRUE ~ 0))
nw_rdt <- nw_rdt %>% mutate(mis_2017 = case_when(svy=="mis_2017" ~ 1, TRUE ~ 0))
nw_rdt <- nw_rdt %>% mutate(dhs_2015 = case_when(svy=="dhs_2015" ~ 1, TRUE ~ 0))
nw_rdt <- nw_rdt %>% mutate(ais_2011 = case_when(svy=="ais_2011" ~ 1, TRUE ~ 0))



## part three : model structures using INLA------

## add SPDE to binomial logit to estimate spatial effects (range parameter and variance)
dat <- nw_rdt

# 1) Project lon/lat to km (simple equirectangular; good enough for moderate extents)
R <- 6371  # Earth radius in km
lat0 <- mean(dat$lat, na.rm=TRUE) * pi/180
x_km <- R * (dat$long * pi/180) * cos(lat0)
y_km <- R * (dat$lat * pi/180)
coords <- cbind(x_km, y_km)

# 2) Mesh
mesh <- inla.mesh.2d(loc = coords,
        max.edge = c(5, 20),   # inner/outer triangle sizes in km (tune to your domain)
        cutoff   = 1)           # merge near-duplicate points (km)

# 3) SPDE with PC priors (set range scale to your domain)
spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
  prior.range = c(15, 0.5),  # P(range < 15 km) = 0.5  
  prior.sigma = c(1, 0.01))   # P(sigma > 1) = 0.01

# 4) Index + projector matrix
spde_idx <- inla.spde.make.index("spatial", n.spde = spde$n.spde)
A_obs <- inla.spde.make.A(mesh, loc = coords)

# 5) Stack (observations only)
stack_obs <- inla.stack(
  data = list(y = dat$rdt),
  A    = list(A_obs, 1),
  effects = list(
    spatial = spde_idx,
    data.frame(Intercept   = 1,
      nearest_dist = dat$nearest_distc,
      elevation    = dat$elevationb,
      age          = dat$agec,
      sex          = dat$female,
      dhs_2022     = dat$dhs_2022,
      mis_2017     = dat$mis_2017,
      dhs_2015     = dat$dhs_2015,
      ais_2011     = dat$ais_2011,
      urban        = dat$urban,
      wealth       = dat$wealthc)),tag = "obs")

# 6a) Model 1 formula
formula1 <- y ~ 0 + Intercept + nearest_dist + f(spatial, model = spde)

# 6b) Model 2 formula
formula2 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + 
            dhs_2022 + mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial, model = spde)

# 7) Prediction grid
pred_grid <- expand.grid(long = seq(29.4, 35, length.out = 200),
                         lat  = seq(-5.5, -0.5, length.out = 200))

# Convert to sf with WGS84 CRS
pred_grid_sf <- st_as_sf(pred_grid, coords = c("long", "lat"), crs = 4326)
# Also convert observed DHS clusters to sf
nw_rdt_sf <- st_as_sf(nw_rdt, coords = c("long", "lat"), crs = 4326)
# extract coordinates for INLA
pred_coords <- st_coordinates(pred_grid_sf)
A_pred <- inla.spde.make.A(mesh, loc = pred_coords)

# Ensure mines are sf
mine_times_sf <- st_as_sf(mine_times, coords = c("long", "lat"), crs = 4326)
# Compute nearest mine distance for each grid cell
nearest_distances <- st_distance(pred_grid_sf, mine_times_sf)
pred_grid_sf$nearest_dist <- apply(nearest_distances, 1, min)
# Convert from units object to numeric (meters → km if you want)
pred_grid_sf$nearest_dist <- as.numeric(pred_grid_sf$nearest_dist) / 1000  # in km

# 8) Prediction stack
pred_coords <- st_coordinates(pred_grid_sf)
n_pred <- nrow(pred_coords)
A_pred <- inla.spde.make.A(mesh, loc = pred_coords)

stack_pred <- inla.stack(
  data = list(y = NA),
  A    = list(A_pred, 1),
  effects = list(
    spatial = 1:spde$n.spde,
    data.frame(Intercept    = rep(1, n_pred),
      nearest_dist = pred_grid_sf$nearest_dist,
      elevation    = NA,
      age          = NA,
      sex          = NA,
      dhs_2022     = NA,
      mis_2017     = NA,
      dhs_2015     = NA,
      ais_2011     = NA,
      urban        = NA,
      wealth       = NA)),tag = "pred")

# Combine observed + prediction stacks for models
stack_full <- inla.stack(stack_obs, stack_pred)


## fit model 1 (only proximity as predictor)
#model_1 <- inla(formula1, data = inla.stack.data(stack_full),
#                family = "binomial", control.family = list(link = "logit"),
#                control.fixed = list(mean.intercept = 0, prec.intercept = 1e-4,
#                mean = 0, prec = 1e-4),
#                control.predictor = list(A = inla.stack.A(stack_full), compute = TRUE),
#                control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE))

## fit model 2 (proximity + DAG adjustment DHS variables)
model_2 <- inla(formula2, data = inla.stack.data(stack_full),
                family = "binomial",
                control.family = list(link = "logit"),
                control.fixed = list(mean.intercept = 0, prec.intercept = 1e-4,
                  mean = 0, prec = 1e-4),
                control.predictor = list(A = inla.stack.A(stack_full), compute = TRUE),
                control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE))

# Extract predictions
#idx_pred1 <- inla.stack.index(stack_full, "pred")$data
#pred1_mean <- model_1$summary.fitted.values[idx_pred1, "mean"]

idx_pred2 <- inla.stack.index(stack_full, "pred")$data
pred2_mean <- model_2$summary.fitted.values[idx_pred2, "mean"]

# Attach to grid sf object
#pred_grid_sf$pred1 <- pred1_mean
pred_grid_sf$pred2 <- pred2_mean

# Back-transform from logit to probability
#pred_grid_sf$pred1_prob <- plogis(pred_grid_sf$pred1)
pred_grid_sf$pred2_prob <- plogis(pred_grid_sf$pred2)

##summary of model parameters
#model_1[["summary.fixed"]]
#model_1[["summary.hyperpar"]]
model_2[["summary.fixed"]]
model_2[["summary.hyperpar"]]



## part four: map effects from models -----

# Fix invalid geometries
district_valid <- st_make_valid(district)
# Now union them
tanzania_boundary <- st_union(district_valid)

# Re-project prediction grid to match boundary CRS (or vice versa)
pred_grid_sf <- st_transform(pred_grid_sf, st_crs(tanzania_boundary))

# Clip the prediction grid to the Tanzania boundary
pred_grid_clipped <- pred_grid_sf[tanzania_boundary, ]  
pred_grid_clipped <- pred_grid_sf[st_within(st_centroid(pred_grid_sf), tanzania_boundary, sparse = FALSE), ]

#pull DHS clusters for map
nw_cluster <- nw_rdt[,c("lat", "long", "svy", "rdt")]
cluster_prev <- nw_cluster %>% group_by(lat, long, svy) %>% summarise(n = n(), positives = sum(rdt, na.rm = TRUE),
    prevalence = mean(rdt, na.rm = TRUE)) %>% ungroup()

#subset clusters into survey waves
#prev_22 <- cluster_prev %>% filter(svy=="dhs_2022")
#prev_17 <- cluster_prev %>% filter(svy=="mis_2017")
#prev_15 <- cluster_prev %>% filter(svy=="dhs_2015")
#prev_11 <- cluster_prev %>% filter(svy=="ais_2011")

## map effects, this is predicted prevalence(?) at all pixels and over a 4 waves..
#model1_map <- ggplot() +
#  geom_sf(data = bounds, fill = "grey95") +
#  geom_sf(data = Victoria, fill = "skyblue", color = "skyblue")+
#  geom_sf(data = district, color="black") +
#  geom_sf(data = pred_grid_clipped, aes(color = pred1_prob)) +
#  scale_color_viridis_c(option = "magma", name = "Predicted prevalence", direction = 1) +
#  new_scale("fill") + # Reset fill scale for mines
#  geom_sf(data = mine_times_sf,
#    aes(shape = size, fill = size),
#    size = 2, alpha = 0.5) +
#  scale_shape_manual(values = c("artisanal" = 24, "industrial" = 22),
#    name   = "Mine type") +
#  scale_fill_manual(values = c("artisanal" = "pink3", "industrial" = "maroon4"),
#    name   = "Mine type") +
#  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) + theme_void() + 
#  ggtitle("Model 1, proximity as only predictor")
#  coord_sf(xlim = c(30.0, 34.4), ylim = c(-4.6, -2.6)) 
#model1_map

model2_map <- ggplot() +
  geom_sf(data = bounds, fill = "grey95") +
  geom_sf(data = Victoria, fill = "skyblue", color = "skyblue")+
#  geom_sf(data = district, color="black") +
  geom_point(data = nw_cluster, aes(x=long, y=lat), color="white") +
  geom_sf(data = pred_grid_clipped, aes(color = pred2_prob)) +
  scale_color_viridis_c(option = "magma", name = "Predicted prevalence", direction = -1) +
  new_scale("fill") + # Reset fill scale for mines
  geom_sf(data = mine_times_sf,
         aes(shape = size, fill = size),
          size = 2, alpha = 0.5) +
  scale_shape_manual(values = c("artisanal" = 24, "industrial" = 22),
                     name   = "Mine type") +
  scale_fill_manual(values = c("artisanal" = "pink3", "industrial" = "maroon4"),
                    name   = "Mine type") +
  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) + theme_void() + 
  ggtitle("Model 2: proximity + DHS covariates in DAG adjustment set")
#model2_map

#ggsave("C:/Users/cgait/OneDrive/Desktop/model2.jpeg", 
#       width = 20, height = 15, units = c("cm"),  model2_map)

#prev22_map <- ggplot() +
#  geom_sf(data = bounds, fill = "grey95") +
#  geom_sf(data = Victoria, fill = "skyblue", color = "skyblue")+
#    geom_sf(data = district, color="gray5") + new_scale("fill") +
#  geom_point(data = prev_22, aes(x=long, y=lat, fill=prevalence), shape=21, size=3.5) +
#  scale_fill_viridis_c(option = "viridis", name = "Prevalence", direction=-1)+
#  new_scale("fill") +
#  geom_sf(data = mine_times_sf,
#          aes(shape = size, fill = size),
#          size = 2.9, alpha = 0.5) +
#  scale_shape_manual(values = c("artisanal" = 24, "industrial" = 22),
#                     name   = "Mine type") +
#  scale_fill_manual(values = c("artisanal" = "pink3", "industrial" = "maroon4"),
#                    name   = "Mine type") +
#  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) + theme_void() +
#  ggtitle("2022 MIS")
#svys_prev <- ggarrange(prev22_map, prev17_map, prev15_map, prev11_map, nrow=2, ncol=2)
