## Functions for mine_time----
## using the same part headings as mine_time

### part two: calculate proximity----

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
  
  # Count mines within 28.05 km
  mines_within_rho <- apply(distances, 2, function(d) sum(d < 25540))
  big_mines_within_rho <- apply(big_distances, 2, function(d) sum(d < 25540))
  
  # Find nearest distances
  nearest_distances <- apply(distances, 2, min) / 1000 # km
  nearest_big_distances <- apply(big_distances, 2, min) / 1000 # km
  
  # Find nearest mine indices and get num_pits
  nearest_indices <- apply(distances, 2, which.min)

  # Add proximity calculations to cluster data
  cluster_data <- cluster_coords %>% mutate(
    !!paste0("num_mines_rho", year) := mines_within_rho,
    !!paste0("total_distances", year) := apply(distances, 2, sum) / 1000,
    !!paste0("mean_distances", year) := apply(distances, 2, mean) / 1000,
    !!paste0("num_bmines_rho", year) := big_mines_within_rho,
    !!paste0("nearest_dist", year) := nearest_distances,
    !!paste0("big_dist", year) := nearest_big_distances,
    !!paste0("close_mine", year) := if_else(nearest_distances <= 15, 1, 0),
    !!paste0("close_big", year) := if_else(nearest_big_distances <= 15, 1, 0))
  
  return(cluster_data)
}


## proximity measures for number of pits, needed because we are imputing num_pits
pits_proximity <- function(year, mine_data, cluster_coords) {
  # Filter active mines for the given year
  mines <- mine_data %>% filter(year_open <= year & year_close > year)
  
  mine_coords <- mines[, c("lat", "long")]
  cluster_coords <- cluster_coords %>% dplyr::select(HV001, lat, long)
  
  # Calculate distances
  distances <- apply(cluster_coords[, c("lat", "long")], 1, function(point) distHaversine(point, mine_coords))
  
  # Nearest mine's num_pits
  nearest_indices <- apply(distances, 2, which.min)
  nearest_num_pits <- mines$num_pits[nearest_indices]
  
  # Average num_pits within 21 km
  avg_pits_rho <- apply(distances, 2, function(d) {
    pits <- mines$num_pits[d < 25540]
    if (length(pits) > 0) mean(pits, na.rm = TRUE) else 0
  })
  
  cluster_data <- cluster_coords %>% mutate(
    !!paste0("nearest_num_pits", year) := nearest_num_pits,
    !!paste0("avg_pits_rho", year) := avg_pits_rho)
  
  return(cluster_data)
}



### part three: risk factor analysis----

## table one count functions
# counts for RDT positivity within northwest analysis population only
nw_counts <- function(vars, outcome = "rdt", design = nw_svy) {
  map(vars, ~ svyby(
    formula(paste0("~", outcome)),
    formula(paste0("~", .x)), design, svytotal,
    survey.lonely.psu = "adjust"))}

# counts for total participants within northwest analysis population only
nw_total <- function(vars, outcome = "select", design = nw_svy) {
  map(vars, ~ svyby(
    formula(paste0("~", outcome)),
    formula(paste0("~", .x)), design, svytotal,
    survey.lonely.psu = "adjust"))}

# counts for all survey participants
tz_counts <- function(vars, outcome = "rdt", design = tz_svy) {
  map(vars, ~ svyby(
    formula(paste0("~", outcome)),
    formula(paste0("~", .x)), design, svytotal,
    survey.lonely.psu = "adjust"))}

# counts for total participants within all surevy participants
tz_total <- function(vars, outcome = "select", design = tz_svy) {
  map(vars, ~ svyby(
    formula(paste0("~", outcome)),
    formula(paste0("~", .x)), design, svytotal,
    survey.lonely.psu = "adjust"))}


## rdt_svy: unadjusted glm for individual risk factor analysis
rdt_svy<- function(var) {m <- svyglm(as.formula(paste0('rdt ~', var)), DHS, family=quasibinomial("identity"))
cbind(tidy(m), confint(m))}


### part four: spatial models using INLA----
#function to run INLA models with different formulas within the same framework
run_inla_overall <- function(dat, mine_times, formulas, mesh, spde, 
                             pred_grid_sf, A_pred, n_pred,
                             grid_extent = c(29.4, 35, -5.5, -0.5)) {
  
  # rebuild observation stack from the SUBSETTED data
  # project lon/lat to km to match the mesh coordinate system
  R <- 6371
  lat0 <- mean(dat$lat, na.rm = TRUE) * pi / 180
  x_km <- R * (dat$long * pi / 180) * cos(lat0)
  y_km <- R * (dat$lat * pi / 180)
  coords <- cbind(x_km, y_km)
  A_obs_sub <- inla.spde.make.A(mesh, loc = coords)
  A_obs_slope <- Matrix::Diagonal(n = nrow(A_obs_sub), x = dat$nearest_distc) %*% A_obs_sub
  
  spde_idx0 <- inla.spde.make.index("spatial0", n.spde = spde$n.spde)
  spde_idx1 <- inla.spde.make.index("spatial1", n.spde = spde$n.spde)
  
  stack_obs <- inla.stack(
    data = list(y = dat$rdt),
    A = list(A_obs_sub, A_obs_slope, 1),
    effects = list(
      spatial0 = spde_idx0, spatial_slope = spde_idx1,
      data.frame(Intercept = 1, nearest_dist = dat$nearest_distc,
                 elevation = dat$elevationb, age = dat$agec, sex = dat$female,
                 mis_2017 = dat$mis_2017, dhs_2015 = dat$dhs_2015,
                 ais_2011 = dat$ais_2011, urban = dat$urban,
                 wealth = dat$wealthc, indust = dat$nearest_industc,
                 nearest_num_pits = dat$nearest_num_pitsc, avg_pits_rho = dat$avg_pits_rhoc,
                 npitxprox = dat$npitxprox, num_mines_rho = dat$num_mines_rhoc,
                 nmines_rho_x_prox = dat$nmines_rho_x_prox,
                 dhs_temp = dat$dhs_temp, rain = dat$rain, veg = dat$veg, 
                 avg_pits_x_prox = dat$avg_pits_x_prox, avgpits_2level = dat$avgpits_2level)),
    tag = "obs")
  
  # prediction stack (same grid for all strata)
  pred_nearest_dist <- pred_grid_sf$nearest_distc
  A_pred_slope <- Matrix::Diagonal(n = nrow(A_pred), x = pred_nearest_dist) %*% A_pred
  
  stack_pred <- inla.stack(
    data = list(y = NA),
    A = list(A_pred, A_pred_slope, 1),
    effects = list(
      spatial0 = spde_idx0, spatial_slope = spde_idx1,
      data.frame(Intercept = rep(1, n_pred), nearest_dist = pred_nearest_dist,
                 elevation = NA, age = NA, sex = NA, mis_2017 = NA, dhs_2015 = NA,
                 ais_2011 = NA, urban = NA, indust = NA, wealth = NA,
                 nearest_num_pits = NA, npitxprox = NA, num_mines_rho = NA, 
                 avg_pits_rho = NA, dhs_temp = NA, rain = NA, veg = NA, 
                 nmines_rho_x_avg_pits = NA, avg_pits_x_prox = NA, avgpits_2level = NA)),
    tag = "pred")
  
  stack_full <- inla.stack(stack_obs, stack_pred)
  
  # now fit models using the LOCAL stack_full
  results <- list()
  for (i in seq_along(formulas)) {
    cat("Running model", i, "...\n")
    fit <- inla(formulas[[i]], data = inla.stack.data(stack_full),
                family = "binomial", control.family = list(link = "logit"),
                control.fixed = list(mean.intercept = 0, prec.intercept = 1e-4,
                                     mean = 0, prec = 1e-4),
                control.predictor = list(A = inla.stack.A(stack_full), compute = TRUE),
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE))
    idx_pred <- inla.stack.index(stack_full, "pred")$data
    pred_mean <- fit$summary.fitted.values[idx_pred, "mean"]
    pred_grid_sf[[paste0("pred", i)]] <- pred_mean
    pred_grid_sf[[paste0("pred", i, "_prob")]] <- plogis(pred_mean)
    results[[paste0("model_", i)]] <- fit
  }
  
  return(list(models = results, predictions = pred_grid_sf))
}


### part six: sensitivity analysis for imputed number of pits----
## function to run full imputation + pooled svyglm pipeline for a given seed
run_imputed_glm <- function(seed, mines_missing, mine_times, nw_rdt, 
                            survey_years, survey_coords, svy_year_map) {
  
  set.seed(seed)
  mines_imp <- mice(mines_missing, m = 5)
  
  # build imputed datasets with pits proximity
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
    
    df <- nw_rdt_i %>% mutate(
      nnpits_2level = factor(case_when(nearest_num_pits <= 18 ~ 0, nearest_num_pits > 18 ~ 1)),
      avg_pits_rho = as.numeric(avg_pits_rho))
    return(df)
  })
  
  # survey design + pooled models
  imp_data <- imputationList(imp_list)
  DHS_imp <- svydesign(id = ~HV021, strata = ~HV023, weights = ~wt, 
                       data = imp_data, nest = TRUE)
  options(survey.lonely.psu = "adjust")
  
  fit_nnpits <- with(DHS_imp, svyglm(rdt ~ nnpits_2level, family = quasibinomial("identity")))

  pool_nnpits <- MIcombine(fit_nnpits)

  # extract results into a tidy dataframe
  extract_pooled <- function(pooled, var_name) {
    ci <- confint(pooled)
    # drop intercept row
    data.frame(
      term = var_name,
      estimate = pooled$coefficients[-1],
      std.error = sqrt(diag(pooled$variance))[-1],
      CIL_95 = ci[-1, 1],
      CIU_95 = ci[-1, 2],
      seed = seed
    )
  }
  
  bind_rows(
    extract_pooled(pool_nnpits, "nnpits_2level")
  )
}


### part seven: sensitivity analysis for range parameter ----

## Sensitivity analysis: vary range prior, hold variance prior fixed at (1, 0.01)
run_range_sensitivity <- function(range_val, range_prob, mesh, stack_full) {
  
  spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                              prior.range = c(range_val, range_prob),
                              prior.sigma = c(1, 0.01))
  
  # Build formula HERE so it captures the local spde
  formula_local <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde)
  
  tryCatch({
    mod <- inla(formula_local, family = "binomial",
                data = inla.stack.data(stack_full),
                control.predictor = list(A = inla.stack.A(stack_full), compute = TRUE),
                control.compute = list(waic = TRUE, dic = TRUE))
    
    data.frame(range_val = range_val, range_prob = range_prob,
               waic = mod$waic$waic, dic = mod$dic$dic,
               range_post_mean  = mod$summary.hyperpar["Range for spatial0", "mean"],
               range_post_lower = mod$summary.hyperpar["Range for spatial0", "0.025quant"],
               range_post_upper = mod$summary.hyperpar["Range for spatial0", "0.975quant"],
               sigma_post_mean  = mod$summary.hyperpar["Stdev for spatial0", "mean"],
               nd_mean  = mod$summary.fixed["nearest_dist", "mean"],
               nd_lower = mod$summary.fixed["nearest_dist", "0.025quant"],
               nd_upper = mod$summary.fixed["nearest_dist", "0.975quant"])
  }, error = function(e) {
    message("Failed: range=", range_val, " prob=", range_prob, " — ", e$message)
    data.frame(range_val = range_val, range_prob = range_prob,
               waic = NA, dic = NA, range_post_mean = NA, range_post_lower = NA,
               range_post_upper = NA, sigma_post_mean = NA,
               nd_mean = NA, nd_lower = NA, nd_upper = NA)
  })
}


run_var_sensitivity <- function(var_val, var_prob, formula, mesh, stack_full) {
  
  # Assign spde to this environment so the formula can find it
  spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                              prior.range = c(10, 0.01),
                              prior.sigma = c(var_val, var_prob))
  # Build formula HERE so it captures the local spde
  formula_local <- y ~ 0 + Intercept + nearest_dist + elevation + age + sex +
    mis_2017 + dhs_2015 + ais_2011 + urban + wealth + f(spatial0, model = spde)
  
  tryCatch({
    mod <- inla(formula_local, family = "binomial",
                data = inla.stack.data(stack_full),
                control.predictor = list(A = inla.stack.A(stack_full), compute = TRUE),
                control.compute = list(waic = TRUE, dic = TRUE))
    
    data.frame(var_val = var_val, var_prob = var_prob,
               waic = mod$waic$waic, dic = mod$dic$dic,
               range_post_mean = mod$summary.hyperpar["Range for spatial0", "mean"],
               var_post_mean  = mod$summary.hyperpar["Stdev for spatial0", "mean"]^2,
               var_post_lower = mod$summary.hyperpar["Stdev for spatial0", "0.025quant"]^2,
               var_post_upper = mod$summary.hyperpar["Stdev for spatial0", "0.975quant"]^2,
               sigma_post_mean  = mod$summary.hyperpar["Stdev for spatial0", "mean"],
               nd_mean  = mod$summary.fixed["nearest_dist", "mean"],
               nd_lower = mod$summary.fixed["nearest_dist", "0.025quant"],
               nd_upper = mod$summary.fixed["nearest_dist", "0.975quant"])
  }, error = function(e) {
    message("Failed: var=", var_val, " prob=", var_prob, " — ", e$message)
    data.frame(var_val = var_val, var_prob = var_prob,
               waic = NA, dic = NA, var_post_mean = NA, var_post_lower = NA,
               var_post_upper = NA, sigma_post_mean = NA,
               nd_mean = NA, nd_lower = NA, nd_upper = NA)
  })
}



### part eight: maps (predictions and observed)----

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


