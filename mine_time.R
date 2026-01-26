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

table(mines_edit$cyanide)

bmines_edit <- big_mines[,c("lat", "long", "year_open", "year_close","mineral", "workers", "fem_workers",
                        "building", "num_pits","cyanide","mercury","site_type","facilities","closetores")]
bmines_edit$size <- "industrial"

mine_times <- rbind(mines_edit, bmines_edit)
remove(mines_edit, bmines_edit, geo_list)


## imputation for missing number of pits 
## using R MICE
set.seed(123)
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
#and average number of pits at mines within 21 km (model 3 rho estimate)
nw_rdt$npitxprox <- nw_rdt$nearest_num_pits*nw_rdt$nearest_dist
nw_rdt$rho_pitxprox <- nw_rdt$avg_pits_rho*nw_rdt$nearest_dist
nw_rdt$nmines_rho_x_avg_pits <- nw_rdt$avg_pits_rho*nw_rdt$num_mines_rho
nw_rdt$avg_pits_x_prox <- nw_rdt$avg_pits_rho*nw_rdt$nearest_dist


## create cluster offsets
clusters <- nw_rdt[,c("lat","long","urban","svy","HV001")]
cluster_coords <- clusters[,c("lat","long")]
clusters <- clusters %>% mutate(cluster_id = paste(HV001, svy, sep = "_"))
clusters_sf <- st_as_sf(clusters, coords = c("long", "lat"), crs = 4326)
clusters_sf <- st_transform(clusters_sf, 32736)  # adjust if outside Tanzania
clusters_sf <- clusters_sf %>% mutate(radius_m = ifelse(urban == 1, 5000, 10000))
cluster_buffers <- st_buffer(clusters_sf, dist = clusters_sf$radius_m)
cluster_buffers <- st_make_valid(cluster_buffers)

# safely sample points from within cluster buffers, skipping invalid/empty geometries
sample_points <- cluster_buffers %>% mutate(samples = map(
      geometry,
      ~ if (st_is_empty(.x) || st_area(.x) == 0) {
        st_sfc()  # empty geometry placeholder
      } else {
        st_sample(.x, size = 50)
      }
    )) %>% filter(lengths(samples) > 0) %>%  # keep only successful samples
  unnest(samples) %>% st_as_sf(crs = st_crs(cluster_buffers)) %>%
  mutate(cluster_id = rep(cluster_buffers$cluster_id, each = 50))




## part three : model structures using INLA------
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

# Assuming mine_times has long/lat columns
mine_times_sf <- st_as_sf(mine_times, coords = c("long", "lat"), crs = 4326)

# Compute nearest mine distance for prediction grid
nearest_distances <- st_distance(pred_grid_sf, mine_times_sf)
pred_grid_sf$nearest_dist <- apply(nearest_distances, 1, min) / 1000  # convert to km

pred_grid_sf$nearest_distc <- (pred_grid_sf$nearest_dist - mean(dat$nearest_dist, na.rm=TRUE)) /
  sd(dat$nearest_dist, na.rm=TRUE)

#centered variable for nearest distance
pred_nearest_distc <- pred_grid_sf$nearest_distc

##calculate (stratum-specific?) means for variables that are mean-centered
##using 0.5 for any binary indicators ig?
## means for: elevationb, agec, female, urban, nearest_industc, wealthc, 
## nearest_num_pits, npitxprox, num_mines_rho, nmines_rho_x_avg_pits, avg_pits_x_prox
mean_elev <- mean(nw_rdt$elevationb)
mean_fem <- mean(nw_rdt$female)
mean_urban <- mean(nw_rdt$urban)
mean_nnp <- mean(nw_rdt$nearest_num_pits)
mean_npxp <- mean(nw_rdt$npitxprox)
mean_nmr <- mean(nw_rdt$num_mines_rho)
mean_nmrxap <- mean(nw_rdt$nmines_rho_x_avg_pits)
avg_pits_x_prox = mean_apxp <- mean(nw_rdt$avg_pits_x_prox)


## run model within survey strata and overall
survey_strata <- list(ref_2022   = c(mis_2017 = 0, dhs_2015 = 0, ais_2011 = 0),
  mis_2017 = c(mis_2017 = 1, dhs_2015 = 0, ais_2011 = 0), 
  dhs_2015 = c(mis_2017 = 0, dhs_2015 = 1, ais_2011 = 0),
  ais_2011 = c(mis_2017 = 0, dhs_2015 = 0, ais_2011 = 1))

#function to run INLA models with different formulas within the same framework 
run_inla_strata <- function(dat, mine_times, formulas) { 

  # compute subset coords in same projection used to build mesh 
  lat0 <- mean(dat$lat, na.rm = TRUE) * pi/180 
  R <- 6371 
  x_km <- R * (dat$long * pi/180) * cos(lat0) 
  y_km <- R * (dat$lat * pi/180) 
  coords_sub <- cbind(x_km, y_km) 
# IMPORTANT: build A for only the subset locations (rows will match nrow(dat)) 
  A_obs_sub <- inla.spde.make.A(mesh, loc = coords_sub) 
  # spatially varying slope design matrix for nearest_dist 
  # (row-scale A by nearest_distc) 
  stopifnot(requireNamespace("Matrix", quietly = TRUE)) 
  A_obs_slope <- Matrix::Diagonal(n = nrow(A_obs_sub), x = dat$nearest_distc) %*% A_obs_sub 
  
## two separate SPDE indices (same mesh/spde, different names) 
  spde_idx0 <- inla.spde.make.index("spatial0", n.spde = spde$n.spde) 
  spde_idx1 <- inla.spde.make.index("spatial_slope", n.spde = spde$n.spde) 
  
# stack_obs: now has 3 A blocks: intercept/covariates + 2 spatial fields 
  stack_obs <- inla.stack(data = list(y = dat$rdt), 
                          A = list(A_obs_sub, A_obs_slope, 1), 
                          effects = list( spatial0 = spde_idx0, 
                                          spatial_slope = spde_idx1, data.frame(Intercept = 1, 
                          nearest_dist = dat$nearest_distc, # fixed (global) slope still included 
                          elevation = dat$elevationb, age = dat$agec, sex = dat$female, 
                          mis_2017 = dat$mis_2017, dhs_2015 = dat$dhs_2015, 
                          ais_2011 = dat$ais_2011, urban = dat$urban, 
                          indust = dat$nearest_industc, wealth = dat$wealthc, 
                          nearest_num_pits = dat$nearest_num_pits, 
                          npitxprox = dat$npitxprox, num_mines_rho = dat$num_mines_rho, 
                          nmines_rho_x_avg_pits = dat$nmines_rho_x_avg_pits, 
                          avg_pits_x_prox = dat$avg_pits_x_prox)), tag = "obs") 
## stack_pred: must mirror the SAME 3 A blocks, use the same centered/scaled nearest_dist variable as in training 
## Make sure you have centered/scaled nearest distance for the grid 
  pred_nearest_distc <- pred_grid_sf$nearest_distc  
  A_pred_slope <- Matrix::Diagonal(n = nrow(A_pred), x = pred_nearest_distc) %*% A_pred 
  
  ## need to change!!
  ## to wave-specific means for starting values
  base_pred_covars <- data.frame(Intercept = rep(1, n_pred), 
                                 nearest_dist = pred_nearest_distc, 
                                 elevation = mean_elev, age = 0, 
                                 sex = mean_fem, urban = mean_urban, 
                                 indust = 0, wealth = 0, 
                                 nearest_num_pits = mean_nnp, 
                                 npitxprox = mean_npxp, num_mines_rho = mean_nmr, 
                                 nmines_rho_x_avg_pits = mean_nmrxap, avg_pits_x_prox = mean_apxp) 

  # Build one prediction stack PER survey stratum 
  pred_stacks <- list() 
  for (sname in names(survey_strata)) {
    svals <- survey_strata[[sname]] 
  pred_df <- base_pred_covars 
  pred_df$mis_2017 <- svals["mis_2017"] 
  pred_df$dhs_2015 <- svals["dhs_2015"] 
  pred_df$ais_2011 <- svals["ais_2011"] 
  pred_stacks[[sname]] <- inla.stack( data = list(y = NA), A = list(A_pred, A_pred_slope, 1), 
                                      effects = list( spatial0 = 1:spde$n.spde, spatial_slope = 1:spde$n.spde, pred_df), 
                                      tag = paste0("pred_", sname)) } 
# Combine: obs + ALL pred stacks 
  stack_full <- stack_obs 
  for (sname in names(pred_stacks)) { stack_full <- inla.stack(stack_full, pred_stacks[[sname]]) } 

# run models and output the following
  results <- list() 
  for (i in seq_along(formulas)) { 
    fit <- inla(
    formulas[[i]], data = inla.stack.data(stack_full), family = "binomial", control.family = list(link = "logit"), 
    control.fixed = list(mean.intercept = 0, prec.intercept = 1e-4, mean = 0, prec = 1e-4), 
    control.predictor = list(A = inla.stack.A(stack_full), compute = TRUE), 
    control.compute = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE)) 
  
  for (sname in names(survey_strata)) { 
    idx_pred <- inla.stack.index(stack_full, paste0("pred_", sname))$data 
    pred_mean <- fit$summary.fitted.values[idx_pred, "mean"] 
    pred_sd <- fit$summary.fitted.values[idx_pred, "sd"] 
    # store on the grid with informative names 
    pred_grid_sf[[paste0("pred", i, "_", sname, "_eta")]] <- pred_mean 
    pred_grid_sf[[paste0("pred", i, "_", sname, "_sd")]] <- pred_sd 
    pred_grid_sf[[paste0("pred", i, "_", sname, "_prob")]] <- plogis(pred_mean) 
    pred_grid_sf[[paste0("pred", i, "_", sname, "_lower")]] <- plogis(pred_mean - 1.96 * pred_sd) 
    pred_grid_sf[[paste0("pred", i, "_", sname, "_upper")]] <- plogis(pred_mean + 1.96 * pred_sd) 
  } 
    results[[paste0("model_", i)]] <- fit } 
   
  return(list(models = results, predictions = pred_grid_sf)) 
  }


## overall results model function
## 1) A for observation stack 
A_obs_sub <- inla.spde.make.A(mesh, loc = coords) 

# separate As for observations and spatial slope?
A_obs_slope <- Matrix::Diagonal(n = nrow(A_obs_sub), x = dat$nearest_distc) %*% A_obs_sub 

## 2) two separate SPDE indices (same mesh/spde, different names) 
spde_idx0 <- inla.spde.make.index("spatial0", n.spde = spde$n.spde) 
spde_idx1 <- inla.spde.make.index("spatial_slope", n.spde = spde$n.spde) 

## 3) stack_obs: now has 3 A blocks: intercept/covariates + 2 spatial fields 
stack_obs <- inla.stack(data = list(y = dat$rdt), 
                        A = list(A_obs_sub, A_obs_slope, 1), 
                        effects = list( spatial0 = spde_idx0, 
                                        spatial_slope = spde_idx1, data.frame(Intercept = 1, 
                                        nearest_dist = dat$nearest_distc, # fixed (global) slope still included 
                                        elevation = dat$elevationb, age = dat$agec, sex = dat$female, 
                                        mis_2017 = dat$mis_2017, dhs_2015 = dat$dhs_2015, 
                                        ais_2011 = dat$ais_2011, urban = dat$urban, 
                                        indust = dat$nearest_industc, wealth = dat$wealthc, 
                                        nearest_num_pits = dat$nearest_num_pits, 
                                        npitxprox = dat$npitxprox, num_mines_rho = dat$num_mines_rho, 
                                        nmines_rho_x_avg_pits = dat$nmines_rho_x_avg_pits, 
                                        avg_pits_x_prox = dat$avg_pits_x_prox)), tag = "obs") 

## 4) stack_pred: must mirror the SAME 3 A blocks 
# IMPORTANT: use the same centered/scaled nearest_dist variable as in training 
# Make sure you have centered/scaled nearest distance for the grid 
  pred_nearest_distc <- pred_grid_sf$nearest_distc # <-- use centered version 
  A_pred_slope <- Matrix::Diagonal(n = nrow(A_pred), x = pred_nearest_distc) %*% A_pred 

# 5) Prediction grid is already made 
# Compute nearest mine distance & prediction coords, already done

# 6) prediction stack overall
  stack_pred <- inla.stack(data = list(y = NA), 
    A = list(A_pred, A_pred_slope, 1), effects = list(
      spatial0 = spde_idx0, spatial_slope = spde_idx1, 
      data.frame(Intercept = rep(1, n_pred),           # n_pred rows
        nearest_dist = pred_nearest_distc,    # prediction grid values
        elevation = rep(mean_elev, n_pred),   # use means for prediction
        age = rep(0, n_pred),                 # centered, so 0 = mean
        sex = rep(mean_fem, n_pred), 
        mis_2017 = rep(mean(dat$mis_2017), n_pred),  # or 0 if you want reference
        dhs_2015 = rep(mean(dat$dhs_2015), n_pred),
        ais_2011 = rep(mean(dat$ais_2011), n_pred),
        urban = rep(mean_urban, n_pred), 
        indust = rep(0, n_pred),              # centered, so 0 = mean
        wealth = rep(0, n_pred),              # centered, so 0 = mean
        nearest_num_pits = rep(mean_nnp, n_pred), 
        npitxprox = rep(mean_npxp, n_pred), 
        num_mines_rho = rep(mean_nmr, n_pred), 
        nmines_rho_x_avg_pits = rep(mean_nmrxap, n_pred), 
        avg_pits_x_prox = rep(mean_apxp, n_pred))), tag = "pred")

# Combine observed + prediction stacks
stack_full <- inla.stack(stack_obs, stack_pred)

#function to run INLA models with different formulas within the same framework
run_inla_overall <- function(dat, mine_times, formulas, grid_extent = c(29.4, 35, -5.5, -0.5)) {
  
  results <- list()
  for (i in seq_along(formulas)) {
    cat("Running model", i, "...\n")
    fit <- inla(formulas[[i]], 
                data = inla.stack.data(stack_full),
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
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde),
  formula3b <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde) +
    f(spatial_slope, model = spde))#,
                #formula4 <- y ~ 0 + Intercept + nearest_dist + indust + f(spatial, model = spde), 
                #formula5 <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +  
                #         mis_2017 + dhs_2015 + ais_2011 + urban + wealth + indust + 
                #         f(spatial, model = spde))

## run and attach stratified predictions to the grid shapefile
out_strata <- run_inla_strata(nw_rdt, mine_times, formulas)
pred_grid_strata <- out_strata$predictions

## run and export overall results (un-stratified)
out_all <- run_inla_overall(nw_rdt, mine_times, formulas)
## model predictions 
pred_grid_sf$model3a_prob <-   out_all$predictions$pred1_prob
pred_grid_sf$model3a_sd <-     out_all$predictions$pred1_sd
pred_grid_sf$model3a_lower <-  out_all$predictions$pred1_lower
pred_grid_sf$model3a_upper <-  out_all$predictions$pred1_upper

pred_grid_sf$model3b_prob <-  out_all$predictions$pred2_prob
pred_grid_sf$model3b_sd <-    out_all$predictions$pred2_sd
pred_grid_sf$model3b_lower <- out_all$predictions$pred2_lower
pred_grid_sf$model3b_upper <- out_all$predictions$pred2_upper


# Write separate GeoPackages (or shapefiles) per stratum of survey wave
tmp_list <- list()

for (sname in names(survey_strata)) {
  layer_cols <- c("geometry",
                  paste0("pred1_", sname, "_prob"),
                  paste0("pred1_", sname, "_lower"),
                  paste0("pred1_", sname, "_upper"))
  
  tmp_list[[sname]] <- pred_grid_strata[, layer_cols]
  
  st_write(tmp_list[[sname]],
           dsn = paste0("pred_surface_", sname, ".gpkg"),
           layer = "pred",
           delete_dsn = TRUE)
}

## model data at each cluster point ?


# stratified model 3 (less and more pits on average at mines within rho)
nw_lpits <- nw_rdt %>% filter(avg_pits_rho <= 1.5)
nw_mpits <- nw_rdt %>% filter(avg_pits_rho > 1.5)
#lpits_out <- run_inla_models(nw_lpits, mine_times, formulas)
#mpits_out <- run_inla_models(nw_mpits, mine_times, formulas)

#extract predicted probabilities, standard error and 95% credible intervals
#pred_grid_sf$lpits_prob <- lpits_out$predictions$pred1_prob
#pred_grid_sf$lpits_se <- lpits_out$predictions$pred1_sd
#pred_grid_sf$lpits_lower <- lpits_out$predictions$pred1_lower
#pred_grid_sf$lpits_upper <- lpits_out$predictions$pred1_upper

#pred_grid_sf$mpits_prob <- mpits_out$predictions$pred1_prob
#pred_grid_sf$mpits_se <- mpits_out$predictions$pred1_sd
#pred_grid_sf$mpits_lower <- mpits_out$predictions$pred1_lower
#pred_grid_sf$mpits_upper <- mpits_out$predictions$pred1_upper



## part 4: EMM by mine characteristics-----

## EMM assessed using models with interaction terms 
## formulas based on model 3 (a or b??) from above


## model 3 (a or b?) within strata of number of pits at nearest mine and average number of pits within rho 
# Function to run model within strata and extract beta1 + CI
extract_strata_results <- function(data, strat_var, strata_bounds, strata_labels, mine_times, formulas) {
  
  # make strata
  strata_data <- map2(strata_bounds, strata_labels, function(b, label) {
    if (length(b) == 1) { # exact zero case
      df <- data %>% filter(!!sym(strat_var) == b)
    } else {              # range case
      df <- data %>% filter(!!sym(strat_var) > b[1], !!sym(strat_var) <= b[2])
    }
    list(label = label, dat = df)
  })
  
  # run INLA + extract beta1
  results <- map_dfr(strata_data, function(s) {
    out <- run_inla_models(s$dat, mine_times, formulas)
    params <- exp(out$models$model_1$summary.fixed)
    tibble(stratum = s$label,
      var = rownames(params),
      beta = params["nearest_dist", "mean"],
      lcl = params["nearest_dist", "0.025quant"],
      ucl = params["nearest_dist", "0.975quant"])
  })
  return(results)
}

remove(rwa, bur, africa, uga, mines_missing, complete1, mine_times_imp1, mines_imp)


## part five: forest plots for model parameters----


## part six: map predicted probabilities over study area----
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


## first clip effects grid to TZ district boundaries, after fixing invalid geometries
district_valid <- st_make_valid(district)
# Now union them
tanzania_boundary <- st_union(district_valid)
# Re-project prediction grid to match boundary CRS (or vice versa)
pred_grid_sf <- st_transform(pred_grid_sf, st_crs(tanzania_boundary))
pred_strata_sf <- st_transform(pred_grid_strata, st_crs(tanzania_boundary))
# Clip the prediction grid to the Tanzania boundary
pred_grid_sf <- pred_grid_sf[st_within(st_centroid(pred_grid_sf), tanzania_boundary, sparse = FALSE), ]
pred_strata_sf <- pred_strata_sf[st_within(st_centroid(pred_strata_sf), tanzania_boundary, sparse = FALSE), ]

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
pred_strata_clipped <- st_intersection(pred_strata_sf, convex_poly_buff)


plot_pred_surface <- function(pred_grid, survey,
    model_id = 1, bounds, mines_sf, nw_cluster, title_suffix,
    model_label = NULL) #can replace with a title
  {pred_col <- paste0("pred", model_id, "_", survey, "_prob")
  
  if (!pred_col %in% names(pred_grid)) {
    stop("Column ", pred_col, " not found in pred_grid")
  }
  
  if (is.null(model_label)) {
    model_label <- paste("Model", model_id)
  }
  ggplot() + geom_sf(data = bounds, fill = "grey90") +
    geom_sf_text(data = bounds, aes(label = NAME_0),color = "grey35",size = 3, fontface = "italic") +
    # Prediction surface
    geom_sf(data = pred_grid, aes(color = .data[[pred_col]])) +
    scale_color_viridis_c(option = "viridis", name = "Predicted prevalence", direction = -1, alpha = 0.7) +
    ggnewscale::new_scale("color") +
    # Survey-specific mines
    geom_sf(data = mines_sf, aes(shape = size, color = size), size = 2, alpha = 0.7) +
    scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
    scale_color_manual(values = c("artisanal" = "plum1", "industrial" = "darkorchid4"), name = "Mine type") +
    # DHS clusters
    geom_point(data = nw_cluster, aes(x = long, y = lat), color = "grey95",shape = 21, size = 1.5) +
    coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) +
    annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7) +
    theme(legend.title = element_text(size = 12),
      legend.text  = element_text(size = 12), axis.text   = element_blank(), axis.ticks  = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(x = "", y = "") +
    ggtitle(paste0("Model 3b",
        "\nPredicted RDT positivity prevalence, ", title_suffix))
}


## overall/combined plot across all survey waves
pred_3a <- ggplot() + geom_sf(data = bounds, fill = "grey90") + geom_sf_text(data = bounds,
           aes(label = NAME_0),color = "grey35",size = 3, fontface = "italic") +
  geom_sf(data = pred_overall_clipped, aes(color = model3a_prob)) +
  scale_color_viridis_c(option = "viridis", name = "Predicted prevalence", direction = -1, alpha = 0.7) +
  ggnewscale::new_scale("color") +
  geom_sf(data = mine_times_sf, aes(shape = size, color = size), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
  scale_color_manual(values = c("artisanal" = "plum1", "industrial" = "darkorchid4"), name = "Mine type") +
  geom_point(data = nw_cluster, aes(x = long, y = lat), color = "grey95",shape = 21, size = 1.5) +
  coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1)) +
  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7) +
  theme(legend.title = element_text(size = 12), legend.text  = element_text(size = 12),
        axis.text   = element_blank(), axis.ticks  = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "", y = "") + ggtitle("Predicted RDT positivity prevalence: Model 3a")
#pred_3a
#pred_3b

## stratified predictions within individual survey waves
p3a_dhs22 <- plot_pred_surface(pred_strata_clipped, "ref_2022", 1, bounds, mine_times_22, nw_cluster, "DHS 2022")
p3a_mis17 <- plot_pred_surface(pred_strata_clipped, "mis_2017", 1, bounds, mine_times_17, nw_cluster, "MIS 2017")
p3a_dhs15 <- plot_pred_surface(pred_strata_clipped, "dhs_2015", 1, bounds, mine_times_15, nw_cluster, "DHS 2015-16")
p3a_ais11 <- plot_pred_surface(pred_strata_clipped, "ais_2011", 1, bounds, mine_times_11, nw_cluster, "AIS 2011-12")

p3b_dhs22 <- plot_pred_surface(pred_strata_clipped, "ref_2022", 2, bounds, mine_times_22, nw_cluster, "DHS 2022")
p3b_mis17 <- plot_pred_surface(pred_strata_clipped, "mis_2017", 2, bounds, mine_times_17, nw_cluster, "MIS 2017")
p3b_dhs15 <- plot_pred_surface(pred_strata_clipped, "dhs_2015", 2, bounds, mine_times_15, nw_cluster, "DHS 2015-15")
p3b_ais11 <- plot_pred_surface(pred_strata_clipped, "ais_2011", 2, bounds, mine_times_11, nw_cluster, "AIS 2011-12")

#model 3a (no spatial slope for proximity)
model3a_svys <- (p3a_dhs22 | p3a_mis17) / (p3a_dhs15 | p3a_ais11)
#model3a_svys

#model 3b (spatial slope for proximity)
model3b_svys <- (p3b_dhs22 | p3b_mis17) / (p3b_dhs15 | p3b_ais11)
#model3b_svys

#ggsave("C:/Users/cgait/OneDrive/Desktop/model3_allsvys.jpeg", width=20, height=15, units=c("cm"), model3_svys)
