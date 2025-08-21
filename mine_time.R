## Malaria in national surveys from 2011-2022, predicted by proximity to artisanal and industrial mines----

#part one: data extract and organization----

#load packages
library(broom)
library(broom.mixed)
library(dplyr)
library(geosphere)
library(ggplot2)
library(ggpubr)
library(haven)
library(jsonlite)
library(lwgeom)
library(MASS)
library(pscl)
library(purrr)
library(readxl)
library(sf)
library(statmod)
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
roads <- read_sf("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/hotosm_tza_roads_lines_shp/hotosm_tza_roads_lines_shp.shp")


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

#clean workspace againnn
remove(tz_geo_prox,survey_coords,big_mines,mines,survey_years,proximity_data, mine_times,
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
nw_rdt$wealthq<-as.factor(nw_rdt$HV270)
#nw_rdt <- nw_rdt %>% mutate(wealthb = case_when(HV270==1 ~ "Wealth quintiles 1 & 2",
#                     HV270==2 ~ "Wealth quintiles 1 & 2", HV270==3 ~ "Wealth quintiles 3-5",
#                     HV270==4 ~ "Wealth quintiles 3-5", HV270==5 ~ "Wealth quintiles 3-5"))

#rename a couple of cluster level variables
nw_rdt$cluster <- nw_rdt$HV001
nw_rdt <- nw_rdt %>% mutate(urban = case_when(URBAN_RURA=="U" ~ 1, URBAN_RURA=="R" ~ 0))
nw_rdt$urban <- as.factor(nw_rdt$urban)
#recode elevation..
nw_rdt$elevation <- nw_rdt$ALT_GPS
#binary elevation where 1 = elevated above mosquito habitat
nw_rdt <- nw_rdt %>% mutate(elevationb = case_when(HV040 > 1500 ~ 1, HV040 <= 1500 ~ 0))
#replace -9999 in rain and temp with missing
nw_rdt <- nw_rdt %>% mutate(across(c(rain, dhs_temp), ~ na_if(., -9999)))

#exclude missing values if we end up using these
#nw_rdt <- nw_rdt[complete.cases(nw_rdt[c("dhs_temp", "rain", "veg")]), ]
# subset clusters for doubly exposed
#both_rdt <- nw_rdt %>% filter(close_big==1 & close_mine==1)
#summary(nw_rdt$nearest_dist)



## part three : functions and starting values for sampling model parameters------

#log-posterior function, this will compute posterior distribution(s?)
#from which we will sample parameter values

log_model1 <- function(params, y, x, cluster_ids, Dmat) {
  n_clusters <- nrow(Dmat)
  beta0 <- params[1]
  beta1 <- params[2]
  rho <- exp(params[3])  # enforce positive
  s <- params[4:(3 + n_clusters)]  # spatial effects
  
  # Covariance matrix
  Sigma <- exp(-Dmat / rho) + diag(1e-6, n_clusters)
  Sigma_inv <- tryCatch(solve(Sigma), error = function(e) return(matrix(NA, nrow = n_clusters, ncol = n_clusters)))
  det_Sigma <- tryCatch(determinant(Sigma, logarithm = TRUE)$modulus, error = function(e) return(NA))
  
  # Check if inversion failed
  if (any(is.na(Sigma_inv)) || is.na(det_Sigma)) {
    cat("Matrix inversion or determinant failed\n")
    return(NA)}
  
  # Linear predictor
  eta <- beta0 + beta1 * x + s[cluster_ids]
  if (any(is.na(eta))) {
    cat("NA in eta\n")
    return(NA)}
  
  loglik <- sum(y * eta - log1p(exp(eta)))  # log-likelihood
  if (is.na(loglik)) {
    cat("NA in log-likelihood\n")
    return(NA)}
  
  quad_form <- t(s) %*% Sigma_inv %*% s
  log_prior_s <- -0.5 * (quad_form + det_Sigma)
  
  log_prior_beta <- dnorm(beta0, 0, 5, log = TRUE) + dnorm(beta1, 0, 5, log = TRUE)
  log_prior_rho <- dnorm(params[3], 0, 1, log = TRUE)
  
  total <- loglik + log_prior_s + log_prior_beta + log_prior_rho
  if (is.na(total)) cat("NA in total log-posterior\n")
  return(total)
}


## Metropolis-Hastings loop to sample from posterior distribution we just assigned
run_mcmc <- function(start, n_iter, burn_in = 1000, thin = 1,
                     log_post_fun, proposal_sd, data_args) {
  
  n_params <- length(start)
  chain_full <- matrix(NA, nrow = n_iter, ncol = 3)  # beta0, beta1, rho
  colnames(chain_full) <- c("beta0", "beta1", "rho")
  
  current_params <- start
  accept_count <- 0
  
  cat("Initial log-posterior:", 
      do.call(log_post_fun, c(list(current_params), data_args)), "\n")
  
  for (i in 1:n_iter) {
    # Step 1: propose new parameters
    proposal <- rnorm(n_params, mean = current_params, sd = proposal_sd)
    
    # Step 2: evaluate log-posteriors
    log_post_current <- do.call(log_post_fun, c(list(params = current_params), data_args))
    log_post_proposal <- do.call(log_post_fun, c(list(params = proposal), data_args))
    
    # Step 3: compute acceptance probability
    log_accept_ratio <- log_post_proposal - log_post_current
    accept_prob <- exp(log_accept_ratio)
    if (is.na(accept_prob) || is.nan(accept_prob)) accept_prob <- 0
    
    # Step 4: accept/reject
    if (runif(1) < accept_prob) {
      current_params <- proposal
      accept_count <- accept_count + 1}
    
    # Save only key parameters
    chain_full[i, ] <- c(current_params[1],      # beta0
                         current_params[2],      # beta1
                         exp(current_params[3]))} # rho (on original scale)
  
  cat("Acceptance rate:", accept_count / n_iter, "\n")
  
  # Apply burn-in and thinning
  kept_iters <- seq(burn_in + 1, n_iter, by = thin)
  chain_final <- chain_full[kept_iters, , drop = FALSE]
  
  return(chain_final)}


#plots to review traces and posterior density from MCMC sampling
plot_mcmc_diagnostics <- function(chain) {
  par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
  
  param_names <- colnames(chain)
  
  for (j in seq_along(param_names)) {
    param <- chain[, j]
    
    # Trace plot
    plot(param, type = "l", col = "steelblue",
         main = paste("Trace:", param_names[j]),
         xlab = "Iteration", ylab = param_names[j])
    
    # Posterior density
    dens <- density(param)
    plot(dens, main = paste("Density:", param_names[j]),
         xlab = param_names[j], ylab = "Density", col = "darkorange", lwd = 2)
    abline(v = mean(param), col = "red", lty = 2) # posterior mean
  }
  
  par(mfrow = c(1, 1))}


#compute starting values for model 1 chain
# x: mine proximity (vector), y: 0/1 RDT outcome, cluster_id: factor or integer per row
# Keep only complete cases for y, x, and cluster_id
# ---- 1) Clean, keep only rows needed for the model
model_df <- nw_rdt[, c("nearest_dist", "HML35", "HV001", "long", "lat", "year.x")]


# Ensure y is 0/1 numeric (not factor/labels)
y <- as.integer(model_df$HML35)
# If HML35 isn’t already 0/1, map here, e.g.:
# y <- ifelse(model_df$HML35 %in% c(1, "positive", "Yes"), 1, 0)

# Predictor
x <- model_df$nearest_dist
x_std <- as.numeric(scale(x))

# ---- 2) Make a cluster-level table with unique coords
cluster_tbl <- model_df %>%
  distinct(year.x, HV001, long, lat) %>%   # include survey year
  arrange(year.x, HV001) %>%
  mutate(cluster_id = row_number())

cluster_tbl_unique <- cluster_tbl %>%
  distinct(HV001, year.x, cluster_id, .keep_all = FALSE)


K <- nrow(cluster_tbl)

# ----- 3) Build Dmat at the CLUSTER level (K x K)
coords_mat <- as.matrix(cluster_tbl[, c("long", "lat")])
Dmat <- distm(coords_mat) / 1000  # km

# ---- 4) Map each row to its cluster index 1..K
model_df <- model_df %>%
  left_join(
    dplyr::select(cluster_tbl_unique, HV001, year.x, cluster_id),
    by = c("HV001", "year.x"))

cluster_ids <- model_df$cluster_id  # integers in 1..K

# Safety checks
stopifnot(length(x_std) == length(y),
          length(cluster_ids) == length(y),
          all(cluster_ids >= 1 & cluster_ids <= K),
          nrow(Dmat) == K, ncol(Dmat) == K)


length(unique(model_df$HV001))
length(unique(nw_rdt$HV001))


# ---- 5) Starting values
eps <- 1e-6
p_bar <- mean(y)
p_bar <- min(max(p_bar, eps), 1 - eps)
beta0_start <- qlogis(p_bar)

beta1_start <- 0
suppressWarnings({
  fit0 <- try(glm(y ~ x_std, family = binomial()), silent = TRUE)
})
if (!inherits(fit0, "try-error")) {
  b <- suppressWarnings(coef(fit0))
  if (!any(is.na(b))) beta1_start <- unname(b[2])
}

dvals <- Dmat[upper.tri(Dmat)]
dvals <- dvals[dvals > 0]
rho_start <- if (length(dvals)) quantile(dvals, 0.75, na.rm = TRUE) else 10
log_rho_start <- log(rho_start)

s_start <- rep(0, K)

start_vals1 <- c(beta0_start, beta1_start, log_rho_start, s_start)
proposal_sd1 <- c(0.15, 0.15, 0.08, rep(0.10, K))

data_args <- list(
  y = y,
  x = x_std,
  cluster_ids = cluster_ids,
  Dmat = Dmat)

# Optional: quick preflight to fail fast if sizes don’t match
preflight <- function(params, data_args) {
  K <- nrow(data_args$Dmat)
  stopifnot(length(params) == 3 + K)
  stopifnot(all(is.finite(unlist(data_args))))
  TRUE
}
preflight(start_vals1, data_args)


## run chain using Dmat, RDT outcomes (y), mine proximity (x), cluster ids.. 
pilot_chain <- run_mcmc(
  start = start_vals1,
  n_iter = 4000, burn_in = 1000, thin = 5,
  log_post_fun = log_model1,
  proposal_sd = proposal_sd1,
  data_args = data_args)

plot_mcmc_diagnostics(pilot_chain)

