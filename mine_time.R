## Malaria in TZ national surveys from 2011-2022, predicted by proximity to artisanal and industrial mines----

## part one: data extract and organization----

#load packages
library(broom)
library(broom.mixed)
#library(concaveman)
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
library(mice)
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

#read back in subsetted roads
#roads_123 <- st_read("C:/Users/cgait/OneDrive/Desktop/TZ mining/data/roads_shp/roads_123.shp")
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

#join DHS geography data for clusters
tz_dhs22 <- left_join(tz_dhs22, geo_22, by = "HV001")
tz_mis17 <- left_join(tz_mis17, geo_17, by = "HV001")
tz_dhs15 <- left_join(tz_dhs15, geo_15, by = "HV001")
tz_ais11 <- left_join(tz_ais11, geo_11, by = "HV001")

#removed CHIRPS rain and vegetation data attachment here
#only keeping survey temperatures because it is so short

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
mines_edit <- mines[,c("lat", "long", "year_open", "year_close", "mineral", "workers", "fem_workers",
                       "building", "num_pits","cyanide","mercury","site_type","facilities","closetores" )]
mines_edit$size <- "artisanal"
bmines_edit <- big_mines[,c("lat", "long", "year_open", "year_close")]
bmines_edit$size <- "industrial"
bmines_edit$mineral <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$workers <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$fem_workers <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$building <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$num_pits <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$cyanide <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$mercury <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$site_type <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$facilities <- c(NA, NA, NA, NA, NA, NA)
bmines_edit$closetores <- c(NA, NA, NA, NA, NA, NA)

mine_times <- rbind(mines_edit, bmines_edit)
remove(mines_edit, bmines_edit, geo_list)


## imputation for missing number of pits 
## using R MICE
mines_missing <- mine_times[,c("workers","fem_workers","building","mineral","num_pits", 
                               "cyanide","mercury","site_type","facilities","closetores")]
## missing data patterns 
#md.pattern(mines_missing)
#md.pattern(mine_times)

## by default it does 5 imputations using all variables for the missing data model
mines_imp <- mice(mines_missing, m = 30)

# completed dataset for the 1st imputation
complete1 <- complete(mines_imp, 1)

# completed dataset for all imputations stacked together
all_complete <- complete(mines_imp, "long")  # adds a .imp column

#hist(mine_times$num_pits)
#densityplot(mines_imp, ~ num_pits)
mine_times_imp1 <- complete(mines_imp, 1)   # dataset with imputed values filled
mine_times$num_pits_imp1 <- mine_times_imp1$num_pits


#function to calculate proximity measures for each cluster based on mines active each year
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
  distances <- apply(cluster_coords[, c("lat", "long")], 1,
                     function(point) distHaversine(point, mine_coords))
  big_distances <- apply(cluster_coords[, c("lat", "long")], 1,
                         function(point) distHaversine(point, big_mine_coords))
  
  # Count mines within 21.08 km
  mines_within_rho <- apply(distances, 2, function(d) sum(d < 21080))
  big_mines_within_rho <- apply(big_distances, 2, function(d) sum(d < 21080))
  
  # Find nearest distances
  nearest_distances <- apply(distances, 2, min) / 1000 # km
  nearest_big_distances <- apply(big_distances, 2, min) / 1000 # km
  
  # Find nearest mine indices and get num_pits
  nearest_indices <- apply(distances, 2, which.min)
  nearest_num_pits <- mines$num_pits_imp1[nearest_indices]
  
  # Average num_pits within 21 km (estimated rho using Model 3)
  avg_pits_rho <- apply(distances, 2, function(d) {
    pits <- mines$num_pits_imp1[d < 21080]
    if (length(pits) > 0) mean(pits, na.rm = TRUE) else 0})
  
  # Add proximity calculations to cluster data
  cluster_data <- cluster_coords %>%
    mutate(
      !!paste0("num_mines_rho", year) := mines_within_rho,
      !!paste0("total_distances", year) := apply(distances, 2, sum) / 1000,
      !!paste0("mean_distances", year) := apply(distances, 2, mean) / 1000,
      !!paste0("num_bmines_rho", year) := big_mines_within_rho,
      !!paste0("nearest_dist", year) := nearest_distances,
      !!paste0("big_dist", year) := nearest_big_distances,
      !!paste0("close_mine", year) := if_else(nearest_distances <= 15, 1, 0),
      !!paste0("close_big", year) := if_else(nearest_big_distances <= 15, 1, 0),
      !!paste0("nearest_num_pits", year) := nearest_num_pits,
      !!paste0("avg_pits_rho", year) := avg_pits_rho)
  
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
      !!paste0("num_mines_rho", year),
      !!paste0("num_bmines_rho", year),
      !!paste0("nearest_num_pits", year),
      !!paste0("avg_pits_rho", year))
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
                     HV106==1 ~ "Primary or no education", HV106==2 ~ "Secondary education",
                     HV106==3 ~ "Secondary education"))

#binary variable for water source (water_cat, 1 = piped, 0 = unpiped)
nw_rdt$water_cat <- cut(nw_rdt$HV201, breaks=c(0, 12, Inf), labels=c(0,1), include.lowest = TRUE) 

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

#indicator variables for svy wave
nw_rdt <- nw_rdt %>% mutate(dhs_2022 = case_when(svy=="dhs_2022" ~ 1, TRUE ~ 0))
nw_rdt <- nw_rdt %>% mutate(mis_2017 = case_when(svy=="mis_2017" ~ 1, TRUE ~ 0))
nw_rdt <- nw_rdt %>% mutate(dhs_2015 = case_when(svy=="dhs_2015" ~ 1, TRUE ~ 0))
nw_rdt <- nw_rdt %>% mutate(ais_2011 = case_when(svy=="ais_2011" ~ 1, TRUE ~ 0))

#proximity to an industrial (big) mine
max_indust <- max(nw_rdt$big_dist)
nw_rdt$nearest_indust <- max_indust - nw_rdt$big_dist
mean_indust <- mean(nw_rdt$nearest_indust)
nw_rdt$nearest_industc <- nw_rdt$nearest_indust - mean_indust

#interactions between number of pits at nearest mine, 
#and average number of pits at miens within 21 km (model 3 rho estimate)
table(nw_rdt$avg_pits_rho, useNA = "always")
nw_rdt$npitxprox <- nw_rdt$nearest_num_pits*nw_rdt$nearest_dist
nw_rdt$rho_pitxprox <- nw_rdt$avg_pits_rho*nw_rdt$nearest_dist
nw_rdt$nmines_rho_x_avg_pits <- nw_rdt$avg_pits_rho*nw_rdt$num_mines_rho
nw_rdt$avg_pits_x_prox <- nw_rdt$avg_pits_rho*nw_rdt$nearest_dist



## part three : model structures using INLA------
## add SPDE to binomial logit to estimate spatial effects (range parameter and variance)
dat <- nw_rdt

# 1) Project lon/lat to km
R <- 6371
lat0 <- mean(dat$lat, na.rm=TRUE) * pi/180
x_km <- R * (dat$long * pi/180) * cos(lat0)
y_km <- R * (dat$lat * pi/180)
coords <- cbind(x_km, y_km)

# 2) Mesh
mesh <- inla.mesh.2d(loc = coords, max.edge = c(5, 20), cutoff   = 1)

# 3) SPDE
spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                            prior.range = c(15, 0.5), prior.sigma = c(1, 0.01))
# 4) Index + projector matrix
spde_idx <- inla.spde.make.index("spatial", n.spde = spde$n.spde)
A_obs <- inla.spde.make.A(mesh, loc = coords)

# 5) Stack (observations only) – adjust to your covariates
stack_obs <- inla.stack(
  data = list(y = dat$rdt),
  A    = list(A_obs, 1),
  effects = list(spatial = spde_idx,
                 data.frame(Intercept = 1,
                            nearest_dist = dat$nearest_distc,
                            elevation = dat$elevationb,
                            age = dat$agec,
                            sex = dat$female,
                            dhs_2022 = dat$dhs_2022, mis_2017 = dat$mis_2017,
                            dhs_2015 = dat$dhs_2015, ais_2011 = dat$ais_2011,
                            urban = dat$urban,
                            indust = dat$nearest_industc,
                            wealth = dat$wealthc,
                            nearest_num_pits = dat$nearest_num_pits,
                            npitxprox = dat$npitxprox,
                            num_mines_rho = dat$num_mines_rho,
                            nmines_rho_x_avg_pits = dat$nmines_rho_x_avg_pits,
                            avg_pits_x_prox = dat$avg_pits_x_prox)),tag = "obs")

# 6) Prediction grid
grid_extent = c(29.4, 35, -5.5, -0.5)
pred_grid <- expand.grid(long = seq(grid_extent[1], grid_extent[2], length.out = 200),
                         lat  = seq(grid_extent[3], grid_extent[4], length.out = 200))
pred_grid_sf <- st_as_sf(pred_grid, coords = c("long", "lat"), crs = 4326)

# Compute nearest mine distance
mine_times_sf <- st_as_sf(mine_times, coords = c("long", "lat"), crs = 4326)
nearest_distances <- st_distance(pred_grid_sf, mine_times_sf)
pred_grid_sf$nearest_dist <- apply(nearest_distances, 1, min) / 1000  # km

# Prediction coords
pred_coords <- st_coordinates(pred_grid_sf)
n_pred <- nrow(pred_coords)
A_pred <- inla.spde.make.A(mesh, loc = pred_coords)

stack_pred <- inla.stack(
  data = list(y = NA),
  A    = list(A_pred, 1),
  effects = list(
    spatial = 1:spde$n.spde,
    data.frame(Intercept = rep(1, n_pred),
               nearest_dist = pred_grid_sf$nearest_dist,
               elevation = NA, age = NA, sex = NA,
               dhs_2022 = NA, mis_2017 = NA, dhs_2015 = NA, ais_2011 = NA,
               urban = NA, indust = NA, wealth = NA,
               nearest_num_pits = NA, npitxprox = NA,
               num_mines_rho = NA, avg_pits_rho = NA,
               nmines_rho_x_avg_pits = NA, avg_pits_x_prox = NA)), tag = "pred")

# Combine observed + prediction stacks
stack_full <- inla.stack(stack_obs, stack_pred)


#function to run INLA models with different formulas within the same framework
run_inla_models <- function(dat, mine_times, formulas, grid_extent = c(29.4, 35, -5.5, -0.5)) {
  
  results <- list()
  for (i in seq_along(formulas)) {
    cat("Running model", i, "...\n")
    fit <- inla(formulas[[i]], 
            data = inla.stack.data(stack_full),
            family = "binomial",
            control.family = list(link = "logit"),
            control.fixed = list(mean.intercept = 0, prec.intercept = 1e-4,
                            mean = 0, prec = 1e-4),
            control.predictor = list(A = inla.stack.A(stack_full), compute = TRUE),
            control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE))
    
    idx_pred <- inla.stack.index(stack_full, "pred")$data
    pred_mean <- fit$summary.fitted.values[idx_pred, "mean"]
    pred_grid_sf[[paste0("pred", i)]] <- pred_mean
    pred_grid_sf[[paste0("pred", i, "_prob")]] <- plogis(pred_mean)
    results[[paste0("model_", i)]] <- fit}
  
  return(list(models = results, predictions = pred_grid_sf))
}

# Define formulas (use the same naming as before, spde object will be built inside the function)
formulas <- list(
                #formula1 <- y ~ 0 + Intercept + nearest_dist + f(spatial, model = spde),
                #formula2 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + 
                #          dhs_2022 + mis_2017 + dhs_2015 + ais_2011 + urban + wealth,
                formula3 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + 
                         dhs_2022 + mis_2017 + dhs_2015 + ais_2011 + urban + wealth + 
                         f(spatial, model = spde))#,
                #formula4 <- y ~ 0 + Intercept + nearest_dist + indust + f(spatial, model = spde), 
                #formula5 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + dhs_2022 + 
                #         mis_2017 + dhs_2015 + ais_2011 + urban + wealth + indust + 
                #         f(spatial, model = spde))

#out <- run_inla_models(nw_rdt, mine_times, formulas)

# attach predictions to the preset grid
#pred_grid_sf$pred3_prob <- out$predictions$pred1_prob



## part 4: EMM by mine characteristics

## EMM assessed using models with interaction terms 
## based on model 3 from above
emm_formulas <- list(
         # Model 6 formula (proximity + DHS covariates + number of pits + pitsxproximity)
         formula6 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + 
            dhs_2022 + mis_2017 + dhs_2015 + ais_2011 + urban + wealth + 
            nearest_num_pits + npitxprox + f(spatial, model = spde),
         # Model 7 formula (proximity + DHS covariates + number of mines within rho (21.08 km))
         formula7 <- y ~ 0 + Intercept + num_mines_rho + elevation + age + sex + 
            dhs_2022 + mis_2017 + dhs_2015 + ais_2011 + urban + wealth + avg_pits_rho +
            nmines_rho_x_avg_pits + f(spatial, model = spde),
         # Model 8 formula 
         formula8 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + 
            dhs_2022 + mis_2017 + dhs_2015 + ais_2011 + urban + wealth + 
            avg_pits_rho + avg_pits_x_prox + f(spatial, model = spde))

emm_out <- run_inla_models(nw_rdt, mine_times, emm_formulas)

exp(emm_out$models$model_1$summary.fixed)
exp(emm_out$models$model_2$summary.fixed)
exp(emm_out$models$model_3$summary.fixed)


## model 3 within strata of average number of pits within rho 
#strata could be just 0, 0-6.308(mean), 6.308-8.4, 8.4-78.5, for quartiles....
nearest0 <- nw_rdt %>% filter(nearest_num_pits<=2)
nearest1 <- nw_rdt %>% filter(nearest_num_pits>2 & nearest_num_pits<=11.36)
nearest2 <- nw_rdt %>% filter(nearest_num_pits>11.36 & nearest_num_pits<=50)
nearest3 <- nw_rdt %>% filter(nearest_num_pits>50 & nearest_num_pits<=330)
#pits0 <- nw_rdt %>% filter(avg_pits_rho==0)
#pits1 <- nw_rdt %>% filter(avg_pits_rho>0 & avg_pits_rho<=1.5)
#pits2 <- nw_rdt %>% filter(avg_pits_rho>1.5 & avg_pits_rho<=6.308)
#pits3 <- nw_rdt %>% filter(avg_pits_rho>6.308)

#outcome prevalence within strata
pits0_prev <- sum(pits0$rdt)/nrow(pits0)
pits1_prev <- sum(pits1$rdt)/nrow(pits1)
pits2_prev <- sum(pits2$rdt)/nrow(pits2)
pits3_prev <- sum(pits3$rdt)/nrow(pits3)
#highest prevalence among pits1....next for pits0 but pits2 and 3 are not far off 
nearest0_prev <- sum(nearest0$rdt)/nrow(nearest0)
nearest1_prev <- sum(nearest1$rdt)/nrow(nearest1)
nearest2_prev <- sum(nearest2$rdt)/nrow(nearest2)
nearest3_prev <- sum(nearest3$rdt)/nrow(nearest3)
#highest prevalence among strata 0 and 1 compared to 2 and 3...
#hm why is there overall less prevalence with more pits ?

#run model 3 within pit strata
#pits0_out <- run_inla_models(pits0, mine_times, formulas)
#pits1_out <- run_inla_models(pits1, mine_times, formulas)
#pits2_out <- run_inla_models(pits2, mine_times, formulas)
#pits3_out <- run_inla_models(pits3, mine_times, formulas)
#nearest0_out <- run_inla_models(nearest0, mine_times, formulas)
#nearest1_out <- run_inla_models(nearest1, mine_times, formulas)
#nearest2_out <- run_inla_models(nearest2, mine_times, formulas)
#nearest3_out <- run_inla_models(nearest3, mine_times, formulas)

#exp(pits0_out$models$model_1$summary.fixed)
#exp(pits1_out$models$model_1$summary.fixed)
#exp(pits2_out$models$model_1$summary.fixed)
#exp(pits3_out$models$model_1$summary.fixed)

#exp(nearest0_out$models$model_1$summary.fixed)
#exp(nearest1_out$models$model_1$summary.fixed)
#exp(nearest2_out$models$model_1$summary.fixed)
#exp(nearest3_out$models$model_1$summary.fixed)



## part five: map effects from models -----
#pull DHS clusters for map
nw_cluster <- nw_rdt[,c("lat", "long", "svy", "rdt")]
cluster_prev <- nw_cluster %>% group_by(lat, long, svy) %>% summarise(n = n(), positives = sum(rdt, na.rm = TRUE),
                  prevalence = mean(rdt, na.rm = TRUE)) %>% ungroup()

## clip effects grid to TZ district boundaries, first fix invalid geometries
district_valid <- st_make_valid(district)
# Now union them
tanzania_boundary <- st_union(district_valid)

# Re-project prediction grid to match boundary CRS (or vice versa)
pred_grid_sf <- st_transform(pred_grid_sf, st_crs(tanzania_boundary))

# Clip the prediction grid to the Tanzania boundary
pred_grid_clipped <- pred_grid_sf[tanzania_boundary, ]  
pred_grid_clipped <- pred_grid_sf[st_within(st_centroid(pred_grid_sf), tanzania_boundary, sparse = FALSE), ]


#map effects! rewrite as function ?
model3_map <- ggplot() +
  geom_sf(data = bounds, fill = "grey95") +
  geom_sf(data = Victoria, fill = "skyblue", color = "skyblue")+
  geom_sf(data = pred_grid_clipped, aes(color = pred3_prob)) +
  scale_color_viridis_c(option = "viridis", name = "Predicted prevalence", direction = -1) +
  new_scale("fill") +
  geom_sf(data = mine_times_sf, aes(shape = size, fill = size), size = 2, alpha = 0.6) +
  scale_shape_manual(values = c("artisanal" = 24, "industrial" = 22), name = "Mine type") +
  scale_fill_manual(values = c("artisanal" = "plum1", "industrial" = "tomato3"),name = "Mine type") +
  geom_point(data = nw_cluster, aes(x=long, y=lat), color="grey45", shape=21, size=1.5) +
  geom_sf(data = district, color="grey35", fill=NA) +
  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) + theme_void() + 
  ggtitle("Model 3: proximity + survey covariates")
model3_map

#ggsave("C:/Users/cgait/OneDrive/Desktop/pits2.jpeg", width=20, height=15, units=c("cm"), pits2_map)


## DHS cluster level covariates

#subset clusters into survey waves
#prev_22 <- cluster_prev %>% filter(svy=="dhs_2022")
#prev_17 <- cluster_prev %>% filter(svy=="mis_2017")
#prev_15 <- cluster_prev %>% filter(svy=="dhs_2015")
#prev_11 <- cluster_prev %>% filter(svy=="ais_2011")

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
