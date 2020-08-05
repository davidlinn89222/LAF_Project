# load the required packages 
pkgs <- c("tidyverse", "sf", "sp", "ggmap", "ggspatial", "ggrepel")
lapply(pkgs, library, character.only = T)

# import main sf object containing geometry
main_sf <- read_rds("Data/Spatial data/main_sf.rds")

# transform sf to sp
main_sp <- st_zm(main_sf) %>%
  as("Spatial")

# read city border and transform it's CRS to 4326
city_border_sf <- st_read("Data/Geometry/city_border", "COUNTY_MOI_1081121")
city_border_sf <- st_transform(city_border_sf, 4326)

# read district border and transform it's CRS to 4326
dist_border_sf <- st_read("Data/Geometry/鄉鎮市區界線(TWD97經緯度)", "TOWN_MOI_1090324")
dist_border_sf <- st_transform(dist_border_sf, 4326)

# filter city border in TTK
ttk_border_sf <- city_border_sf %>%
  filter(COUNTYNAME %in% c("基隆市", "臺北市", "新北市"))

# filter dis border in TTK
ttk_dist_border_sf <- dist_border_sf %>%
  filter(COUNTYNAME %in% c("基隆市", "臺北市", "新北市"))

# change the data type of COUNTRYNAME
ttk_dist_border_sf$COUNTYNAME <- as.character(ttk_dist_border_sf$COUNTYNAME)
ttk_dist_border_sf$COUNTYNAME <- 
  factor(ttk_dist_border_sf$COUNTYNAME, 
         levels = c("基隆市", "新北市", "臺北市"), 
         labels = c("Keelung city", "New Taipei city", "Taipei city"))

# LAF locations
LAF_sites <- data.frame(
  branch = c("Keelung", "Taipei", "Shilin", "NewTaipei"),
  longitude = c(121.740179, 121.5259391, 121.5257161, 121.4887337),
  latitude = c(25.130924, 25.0278063, 25.0930044, 25.0576383))

LAF_sites_sf <- st_as_sf(LAF_sites, coords = c("longitude", "latitude"), remove = FALSE)
LAF_sites_sf <- st_set_crs(LAF_sites_sf, 4326)

# create a plot used for introducing locations of LAF and study area
intro_plt <- ggplot(data = st_union(city_border_sf)) + 
  geom_sf()+ 
  geom_sf(data = ttk_dist_border_sf, aes(fill = COUNTYNAME)) + 
  scale_fill_viridis_d(alpha = .3) + 
  geom_sf(data = LAF_sites_sf, col = "darkred") +
  geom_text_repel(data = LAF_sites_sf, aes(x = longitude, y = latitude, label = branch), 
                  fontface = "bold", 
                  nudge_x = c(0.2, -0.2, 0.2, -0.2), 
                  nudge_y = c(0.1, -0.25, 0.15, 0.18)) + 
  coord_sf(xlim = c(121.25, 122.05), ylim = c(24.67319, 25.35), expand = T) + 
  ggtitle("Study area: Taipei, New Taipei and Keelung city", 
          subtitle = "Points: Branches of LAF")+ 
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+ 
  xlab("") + ylab("")

# Plot it
intro_plt

ggsave("Plot/intro.png", width = 8, height = 6, dpi = 320)

# ====================================================================================================

# Visualization for response variable, caseRate and Ycount

# map the caseRate & Ycount
library(RColorBrewer)
pal <- brewer.pal(n = 9, name = "YlOrRd")

# standardize Ycount and caseRate
main_df <- st_set_geometry(main_sf, NULL) %>%
  dplyr::select("CODE2", "Ycount", "caseRate") %>%
  mutate("Ycount_std" = scale(Ycount), "caseRate_std" = scale(caseRate)) %>%
  dplyr::select("CODE2", "Ycount_std", "caseRate_std")

main_df_std <- tidyr::gather(data = main_df, key = type, value = value, c("Ycount_std","caseRate_std"))
main_df_std$type <- factor(x = main_df_std$type, levels = c("Ycount_std", "caseRate_std"))

index_region <- main_df_std %>%
  dplyr::filter(type == "Ycount_std") %>%
  dplyr::arrange(desc(value)) %>%
  dplyr::select(CODE2) %>% 
  unlist(use.names = F)

main_df_plt <- main_df_std %>%
  dplyr::filter(CODE2 %in% index_region[1:8])

main_df_plt$CODE2 <- factor(main_df_plt$CODE2, levels = index_region)

# bar plot for Ycount and caseRate
Ycount_caseRate_plt <- ggplot(data = main_df_plt, 
                              aes(x = CODE2, y = value, fill = type)) +
  geom_bar(stat="identity", position=position_dodge())+
  #scale_fill_manual(values=c('black','lightgray'))+
  theme_minimal()+
  ylab("Value") + xlab("CODE2") +
  labs(title = "Comparision of two measurements", 
       subtitle = "Total counts (std) v.s. Case rate (std)") +
  scale_fill_brewer(palette="Paired", name = "Type of variables", labels = c("Total cases", "Case rate (scaled)"))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45))+
  theme(legend.position="bottom", legend.direction="horizontal") +
  geom_segment(aes(x=6, xend=6, y=8, yend=6), 
               arrow = arrow(length = unit(2, "mm"), type = "closed"), col = "red")

# Plot it
Ycount_caseRate_plt

ggsave("Plot/Ycount_caseRate_plt.png", width = 8, height = 6, dpi = 320)

# ---------------------------------------------------------------------------------------------------

# Ycount
main_sf_un <- st_union(main_sf)

# build info outside the study area
temp <- dist_border_sf %>%
  filter(TOWNNAME %in% c("三芝區", "石門區", "金山區", "貢寮區", "雙溪區",
                         "坪林區", "烏來區,", "平溪區", "石碇區", "龜山區"))

temp$TOWNENG <- str_replace(temp$TOWNENG, " District", "")

temp <- st_transform(temp, 3826) %>%
  st_centroid() %>%
  st_transform(4326)

seal_coords <- do.call(rbind, st_geometry(temp)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

temp <- bind_cols(temp[, 5], seal_coords)

Ycount_ver1_plt <- ggplot(data = city_border_sf) +
  geom_sf()+
  geom_sf(data = temp, fill = NA)+
  geom_sf(data = main_sf_un, color = "black")+
  geom_sf(data = main_sf, aes(fill = Ycount),  color = "NA") +
  scale_fill_gradientn(colours = pal)+  
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  coord_sf(xlim = c(121.2826, 121.8081), ylim = c(24.8617, 25.24436), expand = T) +
  xlab("") + ylab("") +
  geom_label(data = temp, aes(lon, lat, label = TOWNENG), size = 3, fontface = "bold", 
             nudge_y = temp$nudge_y)+
  ggtitle("Choropleth map: Total cases", subtitle = "Year 2018")

# Plot it
Ycount_ver1_plt

ggsave("Ycount_ver1_plt.png", width = 8, height = 6, dpi = 320)

# ---------------------------------------------------------------------------------------------------

# CaseRate
caseRate_ver1_plt <- ggplot(data = city_border_sf) + 
  geom_sf()+
  geom_sf(data = temp, fill = NA)+ 
  geom_sf(data = main_sf_un, color = "black")+ 
  geom_sf(data = main_sf, aes(fill = caseRate),  color = "NA") + 
  scale_fill_gradientn(colours = pal)+  
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue")) + 
  coord_sf(xlim = c(121.2826, 121.8081), ylim = c(24.8617, 25.24436), expand = T) + 
  xlab("") + ylab("") +
  geom_label(data = temp, aes(lon, lat, label = TOWNENG), size = 3, fontface = "bold", 
             nudge_y = temp$nudge_y)+
  ggtitle("Choropleth map: Case rate", subtitle = "Year 2018") 

# Plot it
caseRate_ver1_plt

ggsave("caseRate_ver1_plt.png", width = 8, height = 6, dpi = 320)

# ------------------------------------------------------------------------------------------------------

# Location's plot (coordinates & heatmap)
loc_sf <- read_rds("Data/Location_LAF/real_lonlat_18.rds")

# filter model A, B
loc_AB_sf <- loc_sf %>%
  dplyr::filter(CODE2 %in% c(main_sf$CODE2))

# specify the map bounds
map_bounds <- c(left = 121.2826, bottom = 24.8617, right =  121.8081, top = 25.24436)

coords.map <- get_stamenmap(map_bounds, zoom = 14, maptype = "toner-lite")
coords.map <- ggmap(coords.map, extent = "device", legend = "none")

sample_index <- sample(1:14529, size = 2000, replace = F)

coords.map +
  geom_sf(data = temp, fill = NA)+
  stat_density2d(data = loc_AB_sf, 
                 aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),
                 geom = "polygon")+
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  geom_label(data = temp, aes(lon, lat, label = TOWNENG), size = 3, fontface = "bold", 
             nudge_y = temp$nudge_y) +
  geom_point(data = loc_AB_sf[sample_index, ], aes(x = lon, y = lat), 
             alpha = 0.4, fill = "red", shape = 23, size = 0.5) +
  theme(legend.position = "none")

ggsave("coords_map.png", width = 8, height = 6, dpi = 320)

coords.map +
  geom_sf(data = temp, fill = NA)+
  stat_density2d(data = loc_AB_sf, 
                 aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),
                 geom = "polygon")+
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  geom_label(data = temp, aes(lon, lat, label = TOWNENG), size = 3, fontface = "bold", 
             nudge_y = temp$nudge_y)+
  theme(legend.position = "none")

ggsave("coords_map2.png", width = 8, height = 6, dpi = 320)

# ----------------------------------------------------------------------------------------------------


### Standardized Morbidity Ratio (SMR)


# Calculate the overall case rate in ttk
r <- sum(main_sf$Ycount) / sum(main_sf$population)
r # 0.00215

# Calculate the expected number of cases per polygon
main_sf$Ycount_EXP <- main_sf$population * r

# Calculate the ratio of OBServed to EXPected (SMR)
main_sf$Y_SMR <- main_sf$Ycount / main_sf$Ycount_EXP


# SMR
SMR_ver1_plt <- ggplot(data = city_border_sf) +
  geom_sf()+
  geom_sf(data = temp, fill = NA)+
  geom_sf(data = main_sf_un, color = "black")+
  geom_sf(data = main_sf, aes(fill = Y_SMR),  color = "NA") +
  scale_fill_gradientn(colours = pal)+  
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  coord_sf(xlim = c(121.2826, 121.8081), ylim = c(24.8617, 25.24436), expand = T) +
  xlab("") + ylab("") +
  geom_label(data = temp, aes(lon, lat, label = TOWNENG), size = 3, fontface = "bold", 
             nudge_y = temp$nudge_y)+
  ggtitle("Choropleth map: Standardized mortality ratio (SMR)", 
          subtitle = "Year 2018")

# Plot it
SMR_ver1_plt

ggsave("SMR.png", width = 8, height = 6, dpi = 320)


# ---------------------------------------------------------------------------------------------------


# present a probability that a value is over a threshold
binom.exceed <- function(observed, population, expected, e){
  1 - pbinom(e * expected, population, prob = observed / population)
}

# Compute P(rate > 3)
main_sf$case_gt_3 <- binom.exceed(
  observed = main_sf$Ycount,
  population = main_sf$population,
  expected = main_sf$Ycount_EXP,
  e = 3
)

main_sf$case_gt_3[is.nan(main_sf$case_gt_3)] <- 0
main_sf$case_gt_3

# Use a 50-color palette that only starts changing at around 0.9
pal <- c(
  rep("#B0D0B0", 40),
  colorRampPalette(c("#B0D0B0", "orange"))(5), 
  colorRampPalette(c("orange", "red"))(5)
)

ggplot(data = city_border_sf) +
  geom_sf()+
  geom_sf(data = temp, fill = NA)+
  geom_sf(data = main_sf_un, color = "black")+
  geom_sf(data = main_sf, aes(fill = case_gt_3),  color = "NA") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  scale_fill_gradientn(colours = pal, name = "Relative risk") +
  coord_sf(xlim = c(121.2826, 121.8081), ylim = c(24.8617, 25.24436), expand = T) +
  xlab("") + ylab("") +
  geom_label(data = temp, aes(lon, lat, label = TOWNENG), size = 3, fontface = "bold", 
             nudge_y = temp$nudge_y)+
  ggtitle("Choropleth map: Exceedence probabilities", 
          subtitle = "relative risk exceeds 3")

ggsave("EP.png", width = 8, height = 6, dpi = 320)




