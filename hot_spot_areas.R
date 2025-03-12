library (readxl)
library(taxize)
library(magrittr)
library(taxadb)
library(tidyterra)
library(gridExtra)
library(ggspatial)
library(rgbif)
library(terra)
library(maps)
library (sf)
library(sp)
library (viridis)
library(rnaturalearth)
library(raster)
library(marmap)
library (tidyverse)

MAR_current<-read_xlsx("data/output_data/MAR_climatch_sp_list.xlsx")%>%
  filter(climate=="1981-2010")

future_combined<-read_xlsx("data/output_data/MAR_climatch_sp_list.xlsx")%>%
  filter(climate=="2011-2040")

#Now bring in the high risk species from the watchlist assessments
ISEIA<-read_excel("data/Species_screening_list_MAR_Gulf_for_pub.xlsx", sheet = "ISEIA Protocol")%>%
  filter (`Global environmental risk` %in% c(7,8,9,10,11,12))%>%
  as.data.frame()

#filter the current and future climate match results for the high-risk species 
MAR_current$species<-gsub("_", " ", MAR_current$species) #the species names in the climate match df have an _. Therefore, need to modify the name to match the names in ISEIA.

High_current<-MAR_current%>%
  filter(species %in% c(ISEIA$species_latin))

future_combined$species<-gsub("_", " ", future_combined$species)

High_future<-future_combined%>%
  filter(species %in% c(ISEIA$species_latin))

#set-up geographic area
proj <- "+proj=lcc +lon_0=-63.1 +lat_1=43.8 +lat_2=46.9 +lat_0=45.3 +datum=WGS84 +units=m +no_defs"
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
ll <- "+proj=longlat +datum=WGS84"
rl<-'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs'
sf::sf_use_s2(FALSE) # because of "Evaluation error: Found 1 feature with invalid spherical geometry." otherwise

#create the Nova Scotia polygon as the larger search area for filter species occurrrences
Maritimes <-  rnaturalearth::ne_states(country="Canada",returnclass = "sf") %>%
  filter(name %in% c ("Nova Scotia", "New Brunswick", "Prince Edward Island"))%>% 
  st_as_sfc()%>%
  st_transform(proj) %>% 
  st_transform(ll)

#create a raster layer of high risk NIS under current climate scenarios
test<-High_current%>%
  select("target_x", "target_y", "score", "species")

grid<-test%>%
  select("target_x", "target_y")%>%
  distinct()%>%
  tibble::rowid_to_column("ID")

test<-test%>%
  left_join(grid, by=c("target_x", "target_y"))

sum<-test%>%
  filter(score>0)%>%
  group_by(ID)%>%
  summarise(n_species=n_distinct(species))%>%
  ungroup()

grid_sum<-grid%>%
  left_join(sum, by="ID")%>%
  select(-"ID")

dat<-as.data.frame(grid_sum, xy=TRUE)

r<-terra::rast(dat,
               type='xyz',
               crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
r<-r%>%
  project('EPSG:4326')

r<-raster(r)

rast_temp <- raster(xmn=-69.3,xmx=-59.3,ymn=43.12,ymx=48.36, resolution=0.0833, crs=4326)

#resample to get raster with equal x and y resolutions (5 minutes) using template


r_5m <- raster::resample(r,rast_temp,method="bilinear")

#plot(r_5m)

#create the Nova Scotia polygon as the larger search area for filter species occurrrences
CAN <-  rnaturalearth::ne_states(country="Canada",returnclass = "sf") %>%
  filter(name %in% c("Nova Scotia", "New Brunswick", "Prince Edward Island"))%>%
  #st_as_sfc()%>%
  st_transform(proj) %>% 
  st_transform(ll)

r_masked<-mask(r_5m, CAN)

#plot(r_masked)

#write a raster tif file as a summary document
writeRaster(r_masked, "data/output_data/n_watchlist_sp_clim_match.tif", overwrite=TRUE)

#read back in the raster layer for mapping
current_n_sp<-read_stars("data/output_data/n_watchlist_sp_clim_match.tif")%>%
  st_as_sf()

#make CAN + US mapping layer for background map
us <- ne_countries(country = "united states of america", scale = "large", returnclass = "sf" ) %>%
  st_transform(proj) %>% 
  st_transform(ll)
can <- ne_countries(country = "canada", scale = "large", returnclass = "sf" ) %>%
  st_transform(proj) %>% 
  st_transform(ll)
n.amer <- rbind(us, can)


current_n_sp_map<-ggplot(n.amer)+
  geom_sf()+
  geom_sf(data = current_n_sp, aes(fill=n_watchlist_sp_clim_match.tif, col=n_watchlist_sp_clim_match.tif))+
  scale_fill_viridis(name="n species")+
  scale_color_viridis(guide="none")+
  geom_sf(data=CAN, fill="NA", colour="black")+
  coord_sf(xlim=c(-69.3, -59.3), ylim = c(43.12, 48.36), expand = FALSE)+
  theme_light()


anotated_current_n_sp_map<-current_n_sp_map+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.3, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
 
ggsave("data/output_data/cumulative_sp_clim_match.png", width = 10, height = 7, units = 'in') 

##Future Climate Scenario
#create a raster layer of high risk NIS under current climate scenarios
test_f<-High_future%>%
  select("target_x", "target_y", "score", "species")

grid_f<-test_f%>%
  select("target_x", "target_y")%>%
  distinct()%>%
  tibble::rowid_to_column("ID")

test_f<-test_f%>%
  left_join(grid_f, by=c("target_x", "target_y"))

sum_f<-test_f%>%
  filter(score>0)%>%
  group_by(ID)%>%
  summarise(n_species=n_distinct(species))%>%
  ungroup()

grid_sum_f<-grid_f%>%
  left_join(sum_f, by="ID")%>%
  select(-"ID")

dat_f<-as.data.frame(grid_sum_f, xy=TRUE)

r_f<-terra::rast(dat_f,
               type='xyz',
               crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
r_f<-r_f%>%
  project('EPSG:4326')

r_f<-raster(r_f)

#resample to get raster with equal x and y resolutions (5 minutes) using template
r_5m_f <- raster::resample(r_f,rast_temp,method="bilinear")

#plot(r_5m)

#mask the raster to focus in on the Maritimes
r_masked_f<-mask(r_5m_f, CAN)

#plot(r_masked)

#write a raster tif file as a summary document
writeRaster(r_masked_f, "data/output_data/n_watchlist_sp_clim_match_future.tif", overwrite=TRUE)

#read back in the raster layer for mapping
current_n_sp_f<-read_stars("data/output_data/n_watchlist_sp_clim_match_future.tif")%>%
  st_as_sf()

#make CAN + US mapping layer for background map

current_n_sp_map_f<-ggplot(n.amer)+
  geom_sf()+
  geom_sf(data = current_n_sp_f, aes(fill=n_watchlist_sp_clim_match_future.tif, col=n_watchlist_sp_clim_match_future.tif))+
  scale_fill_viridis(name="n species")+
  scale_color_viridis(guide="none")+
  geom_sf(data=CAN, fill="NA", colour="black")+
  coord_sf(xlim=c(-69.3, -59.3), ylim = c(43.12, 48.36), expand = FALSE)+
  theme_light()


anotated_current_n_sp_map_f<-current_n_sp_map_f+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.3, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

ggsave("data/output_data/cumulative_sp_clim_match_future.png", width = 10, height = 7, units = 'in') 
