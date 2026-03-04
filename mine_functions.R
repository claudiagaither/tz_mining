### functions for mine_time----

## part one: libraries?
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



## part two: calculate proximity

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



## part three: risk factor analysis

## rdt_svy: unadjusted glm for individual risk factor analysis
rdt_svy<- function(var) {m <- svyglm(as.formula(paste0('rdt ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}


## part four: spatial models using INLA
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


## part six: maps (predictions and observed)

# Function to create prevalence map (clusters only)
create_prev_map <- function(cluster_data, title) {
  ggplot() + geom_sf(data = bounds, fill = "grey85") + 
    geom_sf_text(data = bounds, aes(label = NAME_0), color = "grey35", size = 3, fontface = "italic") +
    geom_sf(data = Victoria, fill = "skyblue", color = "skyblue") +
    geom_sf(data = district_valid, fill = "grey90", color="grey70") +
    geom_point(data = cluster_data, aes(x = long, y = lat, color = prevalence), shape = 16, size = 4) +
    scale_color_viridis_c(option = "rocket", name = "Prevalence", direction = -1) +
    coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1.2)) + theme_void() + ggtitle(paste0(title, " analysis clusters"))
}


# Function to create mine locations map
create_mine_map <- function(mine_data, title) {
  ggplot() + geom_sf(data = bounds, fill = "grey85") + 
    geom_sf_text(data = bounds, aes(label = NAME_0), color = "grey35", size = 3, fontface = "italic") +
    geom_sf(data = Victoria, fill = "skyblue", color = "skyblue") +
    geom_sf(data = district_valid, fill = "grey90", color="grey70") +
    geom_sf(data = mine_data, aes(shape = size, color = size), size = 3.5, alpha = 0.6) + 
    scale_shape_manual(values = c("artisanal" = 17, "industrial" = 15), name = "Mine type") +
    scale_color_manual(values = c("artisanal" = "plum3", "industrial" = "tomato3"), name = "Mine type") +
    coord_sf(xlim = c(29.4, 34.5), ylim = c(-5.2, -1.2)) + theme_void() + ggtitle(paste0("Mines assumed active during ", title))
}


