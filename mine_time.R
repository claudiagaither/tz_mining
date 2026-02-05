## Malaria in TZ national surveys from 2011-2022, predicted by proximity to artisanal and industrial mines----

## part one: data extract and organization----
#load packages
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
library(patchwork)
library(pscl)
library(purrr)
library(readr)
library(readxl)
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
# re-examine at some point critically


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
                       "HV040","HV201","HV201B","HV246","HV270","HV023","HV021")]
tz_mis17 <- TZ17_pr[,c("HML35","HV104","HV105","HML10","HV001","HV002","HV003","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270","HV023","HV021")]
tz_mis17$HV201B <- NA
tz_mis17$HV106 <- NA
tz_dhs15 <- TZ15_pr[,c("HML35","HV104","HV105","HV106","HML10","HV001","HV002","HV003","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270","HV023","HV021")]
tz_dhs15$HV201B <- NA
tz_ais11 <- TZ11_pr[,c("HML35","HV104","HV105","HV106","HML10","HV001","HV002","HV003","HV006","HV012","HV013",
                       "HV040","HV201","HV246","HV270","HV023","HV021")]
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
bmines_edit <- big_mines[,c("lat", "long", "year_open", "year_close","mineral", "workers", "fem_workers",
                        "building", "num_pits","cyanide","mercury","site_type","facilities","closetores")]
bmines_edit$size <- "industrial"
mine_times <- rbind(mines_edit, bmines_edit)
remove(mines_edit, bmines_edit, geo_list)


## imputation for missing number of pits 
## using R MICE
set.seed(124)
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
  distances <- apply(cluster_coords[, c("lat", "long")], 1, function(point) distHaversine(point, mine_coords))
  big_distances <- apply(cluster_coords[, c("lat", "long")], 1, function(point) distHaversine(point, big_mine_coords))
  
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
  cluster_data <- cluster_coords %>% mutate(
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
mean_indust <- mean(nw_rdt$nearest_indust)
nw_rdt$nearest_industc <- nw_rdt$nearest_indust - mean_indust

#nearest number of pits recode
nw_rdt <- nw_rdt %>% mutate(nnpits_2level = case_when(nearest_num_pits <= 18 ~ 0,
                            nearest_num_pits > 18  ~ 1))

#interactions between number of pits at nearest mine, 
#and average number of pits at mines within 21 km (model 3 rho estimate)
nw_rdt$npitxprox <- nw_rdt$nearest_num_pits*nw_rdt$nearest_dist
nw_rdt$rho_pitxprox <- nw_rdt$avg_pits_rho*nw_rdt$nearest_dist
nw_rdt$nmines_rho_x_prox <-nw_rdt$num_mines_rho*nw_rdt$nearest_dist
nw_rdt$avg_pits_x_prox <- nw_rdt$avg_pits_rho*nw_rdt$nearest_dist

remove(rwa, bur, africa, uga, tza, mines_missing, complete1, mine_times_imp1, mines_imp)

## part three: risk factor analysis ----
## still need to define DHS survey design (weight = individual survey weight??)
nw_rdt$wt <- 1
#factor all categorical variables for glms
nw_rdt$urban <- as.factor(nw_rdt$urban)
nw_rdt$water_cat <- as.factor(nw_rdt$water_cat)
nw_rdt$livestock <- as.factor(nw_rdt$HV246)
nw_rdt$pits_2level <- as.factor(nw_rdt$nnpits_2level)
nw_rdt$elevationb <- as.factor(nw_rdt$elevationb)
nw_rdt$female <- as.factor(nw_rdt$female)
nw_rdt$close_mine <- as.factor(nw_rdt$close_mine)
nw_rdt$close_big <- as.factor(nw_rdt$close_big)

DHS<-svydesign(id=nw_rdt$HV021, strata=nw_rdt$HV023, weights=nw_rdt$wt, data=nw_rdt, nest=TRUE)
#clusters with only one participant (lonely psu)
options(survey.lonely.psu="adjust")

rdt_svy<- function(var) {m <- svyglm(as.formula(paste0('rdt ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}

adj_svy<- function(var) {m <- svyglm(as.formula(paste0('rdt ~',var,'+ nearest_num_pits')),DHS,family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}

#variables for glms
studyvars <-c("female","age","close_big","HV270","urban","livestock","pits_2level", "water_cat","elevationb")

#map across study variables
#nwsvy_glm<- map_dfr(studyvars, rdt_svy)
#colnames(nwsvy_glm) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
#nwsvy_glm %>% print(noSpaces=T) 

nwsvy_adj<- map_dfr(studyvars, adj_svy)
colnames(nwsvy_adj) <- c('term','estimate','std.error','statistic','p.value','CIL_95','CIU_95')
nwsvy_adj %>% print(noSpaces=T) 

# Create better labels for the variables
#nwsvy_glm_plot <- nwsvy_glm %>% filter(!grepl("Intercept", term)) %>%  # Remove intercept terms
#  mutate(label = case_when(term == "female1" ~ "Female (vs male)", term == "age" ~ "Age (years)",
#      term == "close_mine1" ~ "Close (within 15 km) to any mine", term == "close_big1" ~ "Close (within 15 km) to industrial mine",
#      term == "HV270" ~ "Wealth quintile", term == "urban1" ~ "Urban (vs rural)", term == "livestock1" ~ "Owns livestock",
#      term == "pits_2level1" ~ "0-18 pits at nearest mine (vs 19-330 pits)", term == "water_cat1" ~ "Piped water source (vs unpiped)",
#      term == "elevationb1" ~ "Elevation > 1500 m (vs < 1500 m)", TRUE ~ term), significant = p.value < 0.05)

nwsvy_adj_plot <- nwsvy_adj %>% filter(!grepl("Intercept", term)) %>% filter(!grepl("nearest_num_pits", term)) %>% 
  mutate(label = case_when(term == "female1" ~ "Female (vs male)", term == "age" ~ "Age (years)",
    term == "close_big1" ~ "Close (within 15 km) to industrial mine",
    term == "HV270" ~ "Wealth quintile", term == "urban1" ~ "Urban (vs rural)", term == "livestock1" ~ "Owns livestock",
    term == "pits_2level1" ~ "0-18 pits at nearest mine (vs 19-330 pits)", term == "water_cat1" ~ "Piped water source (vs unpiped)",
    term == "elevationb1" ~ "Elevation > 1500 m (vs < 1500 m)", TRUE ~ term), significant = p.value < 0.05)

# Create forest plots
#bivar_forest <- ggplot(nwsvy_glm_plot, aes(x = estimate, y = reorder(label, estimate))) +
#  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80", linewidth = 0.8) +
#  geom_errorbarh(aes(xmin = CIL_95, xmax = CIU_95, color = significant), height = 0.3, linewidth = 0.8) +
#  geom_point(aes(color = significant), size = 3, shape = 18) +
#  scale_color_manual(values = c("FALSE" = "pink3", "TRUE" = "maroon4"), labels = c("p > 0.05", "p < 0.05")) +
#  labs(title = "Risk Factors for RDT Positivity", subtitle = "Unadjusted GLM", 
#       x = "Prevalence Difference (95% CI)", y = "", color = "Significance") +
#  theme_classic(base_size = 16) + theme(plot.title = element_text(face = "bold", size = 18),
#    plot.subtitle = element_text(color = "gray30"), panel.grid.minor = element_blank(),
#    panel.grid.major.y = element_blank(), legend.position = "bottom")
#bivar_forest

adjusted_forest <- ggplot(nwsvy_adj_plot, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = CIL_95, xmax = CIU_95, color = significant), height = 0.3, linewidth = 0.8) +
  geom_point(aes(color = significant), size = 3, shape = 18) +
  scale_color_manual(values = c("FALSE" = "pink3", "TRUE" = "maroon4"), labels = c("p > 0.05", "p < 0.05")) +
  labs(title = "Risk Factors for RDT Positivity", subtitle = "Adjusted (for number of pits at the nearest mine) GLM", 
       x = "Prevalence Difference (95% CI)", y = "", color = "Significance") +
  theme_classic(base_size = 16) + theme(plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(color = "gray30"), panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(), legend.position = "bottom")
#adjusted_forest



## part four : spatial model structures using INLA------
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
                             prior.range = c(15, 0.5), prior.sigma = c(1, 0.01))
spde_idx <- inla.spde.make.index("spatial", n.spde = spde$n.spde)

# 3) prediction grid stuff (can be computed once)
grid_extent = c(29.4, 35, -5.5, -0.5)
pred_grid <- expand.grid(long = seq(grid_extent[1], grid_extent[2], length.out = 200),
                         lat  = seq(grid_extent[3], grid_extent[4], length.out = 200))
pred_grid_sf <- st_as_sf(pred_grid, coords = c("long", "lat"), crs = 4326)
pred_coords <- st_coordinates(pred_grid_sf)
A_pred <- inla.spde.make.A(mesh, loc = pred_coords)
n_pred <- nrow(pred_coords)

# 4) Compute nearest mine distance for prediction grid
mine_times_sf <- st_as_sf(mine_times, coords = c("long", "lat"), crs = 4326)
nearest_distances <- st_distance(pred_grid_sf, mine_times_sf)
pred_grid_sf$nearest_dist <- apply(nearest_distances, 1, min) / 1000  # convert to km

#centered variable for nearest distance
grid_mean_dist <- mean(pred_grid_sf$nearest_dist)
pred_grid_sf$nearest_distc <- pred_grid_sf$nearest_dist - grid_mean_dist

# 5) A for observation stack 
A_obs_sub <- inla.spde.make.A(mesh, loc = coords) 
# separate As for observations and spatial slope?
A_obs_slope <- Matrix::Diagonal(n = nrow(A_obs_sub), x = dat$nearest_distc) %*% A_obs_sub   # centered nearest distance 

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
                                        nearest_num_pits = dat$nearest_num_pits, avg_pits_rho = dat$avg_pits_rho,
                                        npitxprox = dat$npitxprox, num_mines_rho = dat$num_mines_rho, 
                                        nmines_rho_x_prox = dat$nmines_rho_x_prox, 
                                        avg_pits_x_prox = dat$avg_pits_x_prox)), tag = "obs") 

# 8) stack_pred: must mirror the SAME 3 A blocks as observation stack
# using the same centered/scaled nearest_dist variable as in training 
  pred_nearest_dist <- pred_grid_sf$nearest_distc # centered 
  A_pred_slope <- Matrix::Diagonal(n = nrow(A_pred), x = pred_nearest_dist) %*% A_pred 

# 9) prediction stack for all data
  stack_pred <- inla.stack(data = list(y = NA), A = list(A_pred, A_pred_slope, 1), effects = list(
                spatial0 = spde_idx0, spatial_slope = spde_idx1, data.frame(Intercept = rep(1, n_pred), # n_pred rows
                nearest_dist = pred_nearest_dist,    # prediction grid values
        elevation = NA, age = NA, sex = NA, mis_2017 = NA,  dhs_2015 = NA, ais_2011 = NA, 
        urban = NA, indust = NA, wealth = NA, nearest_num_pits = NA, npitxprox = NA, num_mines_rho = NA, 
        nmines_rho_x_avg_pits = NA, avg_pits_x_prox = NA)), tag = "pred")

# 10) Combine observed + prediction stacks
stack_full <- inla.stack(stack_obs, stack_pred)

#function to run INLA models with different formulas within the same framework
run_inla_overall <- function(dat, mine_times, formulas, grid_extent = c(29.4, 35, -5.5, -0.5)) {
  
  results <- list()
  for (i in seq_along(formulas)) {
    cat("Running model", i, "...\n")
    fit <- inla(formulas[[i]], data = inla.stack.data(stack_full),
                family = "binomial", control.family = list(link = "logit"),
                control.fixed = list(mean.intercept = 0, prec.intercept = 1e-4, mean = 0, prec = 1e-4),
                control.predictor = list(A = inla.stack.A(stack_full), compute = TRUE),
                control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE))
    idx_pred <- inla.stack.index(stack_full, "pred")$data
    pred_mean <- fit$summary.fitted.values[idx_pred, "mean"]
    pred_grid_sf[[paste0("pred", i)]] <- pred_mean
    pred_grid_sf[[paste0("pred", i, "_prob")]] <- plogis(pred_mean)
    results[[paste0("model_", i)]] <- fit}
  
  return(list(models = results, predictions = pred_grid_sf))
}

#Define formulas (use the same naming as before, spde object will be built inside the function)
formulas <- list(
                #formula1 <- y ~ 0 + Intercept + nearest_dist + f(spatial, model = spde),
                #formula2 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + 
                #          mis_2017 + dhs_2015 + ais_2011 + urban + wealth,
  formula3a <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde))
#  formula3b <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +
#    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde) + f(spatial1, model = spde))
                #formula4 <- y ~ 0 + Intercept + nearest_dist + indust + f(spatial, model = spde), 
                #formula5 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +  
                #         mis_2017 + dhs_2015 + ais_2011 + urban + wealth + indust + 
                #         f(spatial, model = spde))

## run and export overall results (un-stratified)
#out_all <- run_inla_overall(nw_rdt, mine_times, formulas)
##view range parameters?
#range_3a <- out_all$models$model_1$summary.hyperpar$mean
#range_3b <- out_all$models$model_2$summary.hyperpar$mean
## model predictions 
#pred_grid_sf$model3a_prob <-   out_all$predictions$pred1_prob
#pred_grid_sf$model3b_prob <-  out_all$predictions$pred2_prob

# stratified model 3 (less and more pits on average at mines within rho)
#nw_lpits <- nw_rdt %>% filter(avg_pits_rho <= 1.5)
#nw_mpits <- nw_rdt %>% filter(avg_pits_rho > 1.5)
#lpits_out <- run_inla_overall(nw_lpits, mine_times, formulas)
#mpits_out <- run_inla_overall(nw_mpits, mine_times, formulas)
#view range parameters? 
#range_lpits <- lpits_out$models$model_1$summary.hyperpar$mean
#range_mpits <- mpits_out$models$model_1$summary.hyperpar$mean
#extract predicted probabilities
#pred_grid_sf$lpits_prob <- lpits_out$predictions$pred1_prob
#pred_grid_sf$mpits_prob <- mpits_out$predictions$pred1_prob



## part five: EMM by number of pits-----

## EMM assessed using models with interaction terms
## could also export beta 1s for unadjusted models? just for comparison
emm_formulas <- list(
  #Model 1: nearest distance (proximity) (+ spatial random effect for all models)
  formula1 <- y ~ 0 + Intercept + nearest_dist + f(spatial0, model = spde),
  #Model 2: number of pits at the nearest mine 
  formula2 <- y ~ 0 + Intercept + nearest_num_pits + f(spatial0, model = spde),
  #Model 3: average number of pits at mines within rho (21 km) 
   formula3 <- y ~ 0 + Intercept + avg_pits_rho + f(spatial0, model = spde),
  #Model 4 formula (DHS covariates + proximity + number of pits at nearest mine + nearest number of pits x proximity)
  formula4 <- y ~ 0 + Intercept + nearest_dist + nearest_num_pits + elevation + age + sex + 
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + npitxprox + f(spatial0, model = spde),
  #Model 5 formula (DHS covariates + proximity + average pits at mines w/in rho + average pits x proximity)
  formula5 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex + mis_2017 + dhs_2015 + 
    ais_2011 + urban + wealth + avg_pits_rho + avg_pits_x_prox + f(spatial0, model = spde))

#emm_out <- run_inla_overall(nw_rdt, mine_times, emm_formulas)

#extract beta 1 for models 1-3, then interaction term betas for models 4 & 5, along with all credible intervals
# Extract coefficients and credible intervals from EMM models
#emm_coeffs <- data.frame(model = character(), parameter = character(),mean = numeric(),
#  lower_95 = numeric(),upper_95 = numeric(), stringsAsFactors = FALSE)

# Model 1: nearest_dist coefficient
#emm_coeffs <- rbind(emm_coeffs, data.frame(model = "Model 1",
#  parameter = "nearest_dist",mean = emm_out$models$model_1$summary.fixed["nearest_dist", "mean"],
#  lower_95 = emm_out$models$model_1$summary.fixed["nearest_dist", "0.025quant"],
#  upper_95 = emm_out$models$model_1$summary.fixed["nearest_dist", "0.975quant"]))

# Model 2: nearest_num_pits coefficient
#emm_coeffs <- rbind(emm_coeffs, data.frame(model = "Model 2",
#  parameter = "nearest_num_pits", mean = emm_out$models$model_2$summary.fixed["nearest_num_pits", "mean"],
#  lower_95 = emm_out$models$model_2$summary.fixed["nearest_num_pits", "0.025quant"],
#  upper_95 = emm_out$models$model_2$summary.fixed["nearest_num_pits", "0.975quant"]))

# Model 3: avg_pits_rho coefficient
#emm_coeffs <- rbind(emm_coeffs, data.frame(model = "Model 3",
#  parameter = "avg_pits_rho", mean = emm_out$models$model_3$summary.fixed["avg_pits_rho", "mean"],
#  lower_95 = emm_out$models$model_3$summary.fixed["avg_pits_rho", "0.025quant"],
#  upper_95 = emm_out$models$model_3$summary.fixed["avg_pits_rho", "0.975quant"]))

# Model 4: interaction term (npitxprox) coefficient
#emm_coeffs <- rbind(emm_coeffs, data.frame(model = "Model 4",
#  parameter = "npitxprox", mean = emm_out$models$model_4$summary.fixed["npitxprox", "mean"],
#  lower_95 = emm_out$models$model_4$summary.fixed["npitxprox", "0.025quant"],
#  upper_95 = emm_out$models$model_4$summary.fixed["npitxprox", "0.975quant"]))

# Model 5: interaction term (avg_pits_x_prox) coefficient
#emm_coeffs <- rbind(emm_coeffs, data.frame(model = "Model 5",
#  parameter = "avg_pits_x_prox", mean = emm_out$models$model_5$summary.fixed["avg_pits_x_prox", "mean"],
#  lower_95 = emm_out$models$model_5$summary.fixed["avg_pits_x_prox", "0.025quant"],
#  upper_95 = emm_out$models$model_5$summary.fixed["avg_pits_x_prox", "0.975quant"]))

# Add a more descriptive label column
#emm_coeffs$label <- c("Model 2: RDT ~ Number of pits at nearest mine",
#  "Model 3: RDT ~ Avg # of pits at mines within 21km", "Model 4 (adjusted): Number of pits × Proximity interaction coefficient",
#  "Model 5 (adjusted): Avg # of pits × Proximity interaction coefficient")

# Reorder for plotting (bottom to top)
#emm_coeffs$label <- factor(emm_coeffs$label, levels = rev(emm_coeffs$label))

# Create forest plot
#emm_forest <- ggplot(emm_coeffs, aes(x = mean, y = label)) +
#  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
#  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95), height = 0.2, linewidth = 0.8, color = "orchid4") +
#  geom_point(size = 3, color = "orchid4") + labs(title = "Effect Modification by Mining Intensity",
#    subtitle = "Coefficients from INLA spatial models (95% Credible Intervals)",
#    x = "Coefficient estimate", y = NULL) + theme_minimal(base_size = 14) +
#  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
#    plot.title = element_text(face = "bold", size = 16), axis.text.y = element_text(size = 11))
#emm_forest



## part six: maps (predictions and observed) ----
#pull DHS clusters for map
nw_cluster <- nw_rdt[,c("lat", "long", "svy", "rdt")]
cluster_prev <- nw_cluster %>% group_by(lat, long, svy) %>% summarise(n = n(), 
                positives = sum(rdt, na.rm = TRUE), prevalence = mean(rdt, na.rm = TRUE)) %>% ungroup()

## subset mines to active mines during each survey wave
mine_times_11 <- mine_times_sf %>% filter(year_open<2011)
mine_times_15 <- mine_times_sf %>% filter(year_open<=2016)
mine_times_15 <- mine_times_15 %>% filter(year_close>=2016)
mine_times_17 <- mine_times_sf %>% filter(year_open<=2017)
mine_times_17 <- mine_times_17 %>% filter(year_close>=2017)
mine_times_22 <- mine_times_sf %>% filter(year_open<=2022)
mine_times_22 <- mine_times_22 %>% filter(year_close>=2022)

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
mines_coord <- mine_times[,c("lat","long")]
all_coords <- bind_rows(cluster_coord, mines_coord)
cluster_sf <- st_as_sf(all_coords, coords = c("long", "lat"), crs = 4326)
# convex hull polygon
convex_poly <- cluster_sf %>% summarise() %>% st_convex_hull()
# reproject back to lon/lat if needed
convex_poly <- st_transform(convex_poly, 4210)
convex_poly_buff <- st_buffer(convex_poly, dist = 0.075)  # buffer in degrees; adjust as needed
pred_overall_clipped <- st_intersection(pred_grid_sf, convex_poly_buff)

remove(africa, all_complete, cluster_coords, cluster_coord, clusters, clusters_sf, cluster_sf, complete1, convex_poly, 
       convex_poly_buff, coords, district, pred_grid, pred_grid_sf, mines_coord, mine_times_imp1, mines_imp, mines_missing, 
       pred_nearest_dist, rwa, bur, uga, tza)


## overall/combined plot across all survey waves, stratified by number of pits exposed to 
#pred_mpits <- ggplot() + geom_sf(data = bounds, fill = "grey90") + geom_sf_text(data = bounds,
#           aes(label = NAME_0),color = "grey35",size = 3, fontface = "italic") +
#  geom_sf(data = pred_overall_clipped, aes(color = mpits_prob)) +
#  scale_color_viridis_c(option = "viridis", name = "Predicted prevalence", direction = -1, alpha = 0.7) +
#  ggnewscale::new_scale("color") +
#  geom_sf(data = mine_times_sf, aes(shape = size, color = size), size = 2, alpha = 0.7) +
#  scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
#  scale_color_manual(values = c("artisanal" = "plum1", "industrial" = "darkorchid4"), name = "Mine type") +
#  geom_point(data = nw_cluster, aes(x = long, y = lat), color = "grey95",shape = 21, size = 1.5) +
#  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) +
#  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7) +
#  theme(legend.title = element_text(size = 12), legend.text  = element_text(size = 12),
#        axis.text   = element_blank(), axis.ticks  = element_blank(),
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#  labs(x = "", y = "") + ggtitle("Predicted RDT positivity prevalence
#among clusters exposed to more pits (>1.5 (median) average at mines within 21 km")
#pred_mpits
#pred_lpits <- ggplot() + geom_sf(data = bounds, fill = "grey90") + geom_sf_text(data = bounds,
#         aes(label = NAME_0),color = "grey35",size = 3, fontface = "italic") +
#  geom_sf(data = pred_overall_clipped, aes(color = lpits_prob)) +
#  scale_color_viridis_c(option = "viridis", name = "Predicted prevalence", direction = -1, alpha = 0.7) +
#  ggnewscale::new_scale("color") +
#  geom_sf(data = mine_times_sf, aes(shape = size, color = size), size = 2, alpha = 0.7) +
#  scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
#  scale_color_manual(values = c("artisanal" = "plum1", "industrial" = "darkorchid4"), name = "Mine type") +
#  geom_point(data = nw_cluster, aes(x = long, y = lat), color = "grey95",shape = 21, size = 1.5) +
#  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) +
#  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7) +
#  theme(legend.title = element_text(size = 12), legend.text  = element_text(size = 12),
#        axis.text   = element_blank(), axis.ticks  = element_blank(),
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#  labs(x = "", y = "") + ggtitle("Predicted RDT positivity prevalence
#among clusters exposed to fewer pits (<1.5 (median) average at mines within 21 km)")
#pred_lpits

#model3b_pits <- ggarrange(pred_mpits, pred_lpits, nrow = 1, ncol = 2)
#ggsave("C:/Users/cgait/OneDrive/Desktop/model3b_pits.jpeg",width=20,height=15,units=c("cm"),model3b_pits)


## malaria prevalence (RDT) in each survey wave
# Create a list of survey specifications
survey_specs <- list(list(svy = "dhs_2022", mine_data = mine_times_22, title = "2022 DHS"),
  list(svy = "mis_2017", mine_data = mine_times_17, title = "2017 MIS"),
  list(svy = "dhs_2015", mine_data = mine_times_15, title = "2015-16 DHS"),
  list(svy = "ais_2011", mine_data = mine_times_11, title = "2011-12 AIS"))

# Function to create prevalence map
create_prev_map <- function(cluster_data, mine_data, title) {
  ggplot() + geom_sf(data = bounds, fill = "grey85") + 
    geom_sf_text(data = bounds, aes(label = NAME_0), color = "grey35", size = 3, fontface = "italic") +
    geom_sf(data = Victoria, fill = "skyblue", color = "skyblue") +
    geom_sf(data = district_valid, fill = "grey90", color="grey70") +
    geom_point(data = cluster_data, aes(x = long, y = lat, color = prevalence), shape = 16, size = 4) +
    scale_color_viridis_c(option = "viridis", name = "Prevalence", direction = -1) +
    new_scale("color") + geom_sf(data = mine_data, aes(shape = size, color = size), size = 3.5, alpha = 0.6) + 
    scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
    scale_color_manual(values = c("artisanal" = "plum3", "industrial" = "tomato3"), name = "Mine type") +
    coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1.2)) + theme_void() + ggtitle(title)
}

# Generate all maps using lapply or purrr::map
prev_maps <- lapply(survey_specs, function(spec) {
  cluster_data <- cluster_prev %>% filter(svy == spec$svy)
  create_prev_map(cluster_data, spec$mine_data, spec$title)
})

# Arrange the maps
svys_prev <- ggarrange(plotlist = prev_maps, nrow = 2, ncol = 2)
svys_prev
