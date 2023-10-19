
# this code reproduces figure 1 (left), the study site FCR
library(dplyr)
library(rgdal) 
library(sf)
library(sp)
library(ggsn)
library(ggplot2)

bathy1 = st_read("Data/506_9.shp")
bathy2 = st_read("Data/504_9.shp")
bathy3 = st_read("Data/502_9.shp")
bathy4 = st_read("Data/500_9.shp")
bathy5 = st_read("Data/498_9.shp")

mycols = c("#eff3ff", "#bdd7e7","#6baed6","#3182bd","#08519c")

# Coordinates for sensor location. 
# Convert lat/long to a sf pt object for plotting
CRSnew = CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")
d <- data.frame(lon=-79.83726, lat=37.30325)
points_sf <- d %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326)
points_sf_trans = st_transform(points_sf, crs=CRSnew)

ggplot() +
  geom_sf(data = bathy1, aes(fill="A"), show.legend = "polygon") +
  geom_sf(data = bathy2, aes(fill="B"), show.legend = "polygon") +
  geom_sf(data = bathy3, aes(fill="C"), show.legend = "polygon") +
  geom_sf(data = bathy4, aes(fill="D"), show.legend = "polygon") +
  geom_sf(data = bathy5, aes(fill="E"), show.legend = "polygon") +
  geom_sf(data = points_sf_trans, aes(col = "samplept")) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust =.7, hjust=.75)) +
  scale_fill_manual(values = c("A" = mycols[1], "B" = mycols[2], "C"=mycols[3],
                               "D" = mycols[4], "E" = mycols[5]), 
                    labels = c("Surface", "3m", "5m", "7m", "9m"),
                    name = "Depth (m)") +
  scale_color_manual(values = c("samplept" = "red"), labels = "Sample site", name = "") +
  north(bathy1, scale=0.2, location = "topleft", symbol = 9) +
  scalebar(bathy1,dist = 100, dist_unit = "m", transform = FALSE,
           st.size = 3, location = "bottomleft", height = 0.015, box.fill = "white")
# ggsave("FCRSampleSiteMap.png", type = "cairo")