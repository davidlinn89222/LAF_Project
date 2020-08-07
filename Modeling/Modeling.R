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

full_tab <- INLA_model_selection(full.formula, main_scaled_sp)
View(full_tab) 

# decide to kick area:std_native_CNT out of the model in this stage 

########################### Step 1 ##############################

step1.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_crime_CNT + std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT +
  std_plant_rate + std_hospital + std_waterside_rate + std_edu_per_kid +
  std_first_inds_CNT + std_second_inds_CNT + std_third_inds_CNT +
  area:std_crime_CNT + area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step1_tab <- INLA_model_selection(step1.formula, main_scaled_sp)
View(step1_tab)

# decide to kick std_first_inds_CNT out of the model in this stage 


########################### Step 2 ##############################

step2.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_crime_CNT + std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT +
  std_plant_rate + std_hospital + std_waterside_rate + std_edu_per_kid +
  std_second_inds_CNT + std_third_inds_CNT +
  area:std_crime_CNT + area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step2_tab <- INLA_model_selection(step2.formula, main_scaled_sp)
View(step2_tab)

# decide to kick std_third_inds_CNT out of the model in this stage

########################### Step 3 ##############################

step3.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_crime_CNT + std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT +
  std_plant_rate + std_hospital + std_waterside_rate + std_edu_per_kid +
  std_second_inds_CNT +
  area:std_crime_CNT + area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step3_tab <- INLA_model_selection(step3.formula, main_scaled_sp)
View(step3_tab)

# decide to kick std_waterside_rate out of the model in this stage


########################### Step 4 ##############################

step4.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_crime_CNT + std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT +
  std_plant_rate + std_hospital  + std_edu_per_kid +
  std_second_inds_CNT +
  area:std_crime_CNT + area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step4_tab <- INLA_model_selection(step4.formula, main_scaled_sp)
View(step4_tab)

# decide to kick std_hospital out of the model

########################### Step 5 ##############################

step5.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_crime_CNT + std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT +
  std_plant_rate  + std_edu_per_kid +
  std_second_inds_CNT +
  area:std_crime_CNT + area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step5_tab <- INLA_model_selection(step5.formula, main_scaled_sp)
View(step5_tab)

# decide to kick std_second_inds_CNT of the model in this stage 



########################### Step 6 ##############################

step6.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_crime_CNT + std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT +
  std_plant_rate  + std_edu_per_kid +
  area:std_crime_CNT + area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step6_tab <- INLA_model_selection(step6.formula, main_scaled_sp)
View(step6_tab)

# decide to kick std_plant_rate out of the model in this stage

########################### Step 7 ##############################

step7.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_crime_CNT + std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT + 
  std_edu_per_kid +
  area:std_crime_CNT + area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step7_tab <- INLA_model_selection(step7.formula, main_scaled_sp)
View(step7_tab)


########################### Step 8 ##############################

step8.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT + 
  std_edu_per_kid + std_crime_CNT +
  area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step8_tab <- INLA_model_selection(step8.formula, main_scaled_sp)
View(step8_tab)

########################### Step 9 ##############################

step9.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + std_native_CNT + 
  std_edu_per_kid +
  area:std_mid_edu_CNT + area:std_low_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step9_tab <- INLA_model_selection(step9.formula, main_scaled_sp)
View(step9_tab)

########################### Step 10 ##############################

step10.formula <- (Ycount+1) ~ area + std_born_CNT + std_divorce_CNT +
  std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + 
  std_edu_per_kid +
  area:std_mid_edu_CNT + area:std_low_edu_CNT + area:std_high_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step10_tab <- INLA_model_selection(step10.formula, main_scaled_sp)
View(step10_tab)

########################### Step 11 ##############################

step11.formula <- (Ycount+1) ~ area + std_born_CNT +
  std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + 
  std_edu_per_kid +
  area:std_mid_edu_CNT + area:std_low_edu_CNT + area:std_high_edu_CNT +
  area:std_disable_CNT + area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step11_tab <- INLA_model_selection(step11.formula, main_scaled_sp)
View(step11_tab)

########################### Step 12 ##############################

step12.formula <- (Ycount+1) ~ area + std_born_CNT +
  std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + 
  std_edu_per_kid +
  area:std_mid_edu_CNT + area:std_low_edu_CNT + area:std_high_edu_CNT +
  area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step12_tab <- INLA_model_selection(step12.formula, main_scaled_sp)
View(step12_tab)

########################### Step 13 ##############################

step13.formula <- (Ycount+1) ~ area + std_born_CNT +
  std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + 
  std_edu_per_kid +
  area:std_mid_edu_CNT + area:std_low_edu_CNT  +
  area:std_edu_per_kid - 1 +
  f(id, model = "bym", graph = g, scale.model = TRUE)

step13_tab <- INLA_model_selection(step13.formula, main_scaled_sp)
View(step13_tab)



######## Final step ###########

# add two IDs for labeling phi and theta
main_scaled_sp@data <- main_scaled_sp@data %>%
  mutate("phi" = 1:nrow(main_scaled_sp@data)) %>%
  mutate("iid" = 1:nrow(main_scaled_sp@data))

# final formula 
final.formula <- (Ycount+1) ~ area + std_born_CNT +
  std_high_edu_CNT + std_mid_edu_CNT + std_low_edu_CNT +
  std_cvs_CNT + std_disable_CNT + std_low_CNT + std_midlow_CNT + 
  std_edu_per_kid +
  area:std_mid_edu_CNT + area:std_low_edu_CNT  - 1 +
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


