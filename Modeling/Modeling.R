# load the required pkgs
pkgs <- c("sf", "sp", "tidyverse", "INLA", "spdep")
lapply(pkgs, library, character.only = T)

# import the main data
main_scaled_sf <- read_rds("Data/Spatial_data/main_sf.rds")
main_scaled_sp <- main_scaled_sf %>%
  st_zm() %>%
  as("Spatial")

# import neighborhood object
g <- inla.read.graph(filename = "Data/Sf_object/map.adj")


############################# Full model ####################################

full.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_crime_CNT + std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT +
  std_plant_rate + std_hospital + std_waterside_rate + std_edu_per_kid +
  std_first_inds_CNT + std_second_inds_CNT + std_third_inds_CNT +
  area:std_crime_CNT + area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_native_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)


######### Step 1 ##########

step1_tab <- INLA_model_selection(full.formula, main_scaled_sp)


######## Subsequent step #########

# .........
# .........
# .........

######## Final step ###########

# add two IDs for labeling phi and theta
main_scaled_sp@data <- main_scaled_sp@data %>%
  mutate("phi" = 1:nrow(main_scaled_sp@data)) %>%
  mutate("iid" = 1:nrow(main_scaled_sp@data))

# final formula 
final.formula <- (Ycount+1) ~ area +
  std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_born_CNT +
  std_edu_per_kid  +
  area:std_mid_edu_CNT + area:std_low_edu_CNT - 1 +
  f(phi, model = "besag", graph = g, scale.model = T) + 
  f(iid, model = "iid")

# Using INLA to estimate the parameters in Gassuian Latent Field
final.res <- 
  inla(final.formula,
       family = "poisson",
       data = main_scaled_sp@data,
       offset = log(population+1),
       control.compute = list(dic = TRUE, waic = TRUE),
       control.predictor = list(compute = TRUE)
  )

# random part
main_scaled_sf$model.bym.phi <- final.res$summary.random$phi$mean
main_scaled_sf$model.bym.iid <- final.res$summary.random$iid$mean

# residual part
vec.residual.model.bym <- (main_scaled_sf$Ycount+1) - 
  unlist(final.res$summary.fitted.values$mean, use.names = F)

main_scaled_sf$model.bym.res <- vec.residual.model.bym



# Vis it
ver1_template(main_scaled_sf, main_scaled_sf$model.bym.iid, "Theta")
ver1_template(main_scaled_sf, main_scaled_sf$model.bym.phi, "Phi")
ver1_template(main_scaled_sf, main_scaled_sf$model.bym.res, "Residuals")

# autocorrelation test for residuals and theta
nb <- poly2nb(main_scaled_sf)
W.list <- nb2listw(nb, style = "W", zero.policy = T)
moran.test(main_scaled_sf$model.bym.iid, W.list, alternative = "two.sided")  # 0.03095
moran.test(main_scaled_sf$model.bym.res, W.list, alternative = "two.sided")  # 0.01336


