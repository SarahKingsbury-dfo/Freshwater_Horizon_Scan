# install dev branch version of climatchR
# requires >= R 4.1.x
# remotes::install_gitlab('umesc/quant-ecology/climatchr@dev', host = 'code.usgs.gov')

library(climatchR)
library(tidyverse)
library(tidyterra)
library(gridExtra)
library(ggspatial)

states_use <-
  c("Wisconsin", "Minnesota", "Illinois", "Iowa", "Missouri",
    "Kentucky", "Tennessee", "Arkansas", "Mississippi", "Louisiana")

# 1. Read in climate data
# 1.1   Current climate
current_clim_dir <- './future_climate/chelsa_current_mw_standardized/'
current_clim <- climatchR::clim_stacker(path = current_clim_dir)

# 1.2.  Future climate
future_clim_dir <- './future_climate/chelsa_future_mw_standardized//'
future_clim <- climatchR::clim_stacker(path = future_clim_dir)

# 2. Create target reference using your own vector files
#    --often this is states/provinces/counties in a .shp, .sqlite or similar
#   or, use gpkg format 
# https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_USA.gpkg
vect_path <-  './gadm41_USA.gpkg'
vect_path_use <- 'gadm_1.sqlite'
v <- terra::vect(vect_path, layer = 'ADM_ADM_1')
terra::writeVector(v, vect_path_use,
                   filetype = 'SQLite')

target_clim_curr <- intersect_by_target(clim_dat = current_clim,
                                        vect_path = vect_path_use,
                                        col = 'name_1')
target_clim_curr <- target_clim_curr[target %in% states_use, ]


target_clim_future <- intersect_by_target(clim_dat = future_clim,
                                        vect_path = vect_path_use,
                                        col = 'name_1')
target_clim_future <- target_clim_future[target %in% states_use, ]

# 3. Read in extract climate data at occurrence locations
gbif_path <- 'hydrilla.csv'

# !!! Only need to do this for current climate data !!!
occ_dat <- read_occ_data(
  path = "gbif_results.csv",
  clim_dat = current_clim,
  coords = c('longitude', 'latitude'),
  name_col = 'searched_term',
  crs = "EPSG:4326"
)



# 4.    Run climatch
# 4.1.  Current
results_current <- calc_climatch(
  occ_dat = occ_dat,
  target = target_clim_curr,
  sensitivity = 1,
  progress = TRUE
)

# 4.2.  Future
results_future <- calc_climatch(
  occ_dat = occ_dat,
  target = target_clim_future,
  sensitivity = 1,
  progress = TRUE
)


# 5. Generate raster from results - right now this only takes the max result
# 5.1. Current
climatch_to_raster(
  results = results_current,
  template = './future_climate/chelsa_current_mw_standardized/bio1.tif',
  out_path = 'hydrilla_current.tif',
  overwrite = TRUE
)

# 5.1. Current
climatch_to_raster(
  results = results_future,
  template = './future_climate/chelsa_current_mw_standardized/bio1.tif',
  out_path = 'hydrilla_future.tif',
  overwrite = TRUE
)



### Extra: reprojecting, plotting, differencing

## Current plot
h_current <- 
  terra::rast('hydrilla_current.tif')|>
  terra::project('EPSG:4326')

v_plot <- 
  v |>
  filter(NAME_1 %in% states_use)

gg_current <-
  ggplot() +
  geom_spatraster(data = h_current) +
  scale_fill_gradient("Climatch\nscore",
                      low = 'white', high = 'blue', na.value=NA,
                      breaks = seq(0, 10, by = 2),
                      limits = c(0, 10)) +
  geom_spatvector(data = v_plot, fill = NA) +
  theme_bw() +
  coord_sf(xlim=c(-97,-82)) +
  ggtitle("Current") +
  annotation_scale()
gg_current

## Future plot
h_future <- 
  terra::rast('hydrilla_future.tif')|>
  terra::project('EPSG:4326')

gg_future <-
  ggplot() +
  geom_spatraster(data = h_future) +
  scale_fill_gradient("Climatch\nscore",
                      low = 'white', high = 'blue', na.value=NA,
                      breaks = seq(0, 10, by = 2),
                      limits = c(0, 10)) +
  geom_spatvector(data = v_plot, fill = NA) +
  theme_bw() +
  coord_sf(xlim=c(-97,-82)) +
  ggtitle("Future") +
  annotation_scale()
gg_future

## Diff plot
h_diff <- 
  (terra::rast('hydrilla_future.tif') - terra::rast('hydrilla_current.tif')) |> 
  terra::project('EPSG:4326')

gg_diff <-
  ggplot() +
  geom_spatraster(data = h_diff) +
  scale_fill_gradient("Climatch\ndiff.",
                      low = 'blue', high = 'red', na.value=NA,
                      breaks = seq(-5, 5, by = 2),
                      limits = c(-7, 7)) +
  geom_spatvector(data = v_plot, fill = NA) +
  theme_bw() +
  coord_sf(xlim=c(-97,-82)) +
  ggtitle(expression("Difference")) +
  annotation_scale()
gg_diff

## All three plots
margin = theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

plts <- list(gg_current, gg_future, gg_diff)
three_plot <-
  arrangeGrob(plts, ncol = 3,
               grobs = lapply(plts, "+", margin))
plot(three_plot)
ggsave("hydrilla_three.jpg", three_plot, width = 13, height = 6,
       dpi = 300)

## Change plot by state
h_diff |>
  as.data.frame() |>
  summary()

all_results <-
  results_current |>
  select(target, score, target_x, target_y) |>
  rename(score_current = score) |>
  full_join(
    results_future |>
    select(score, target_x, target_y) |>
    rename(score_future = score), by=c("target_x", "target_y")) |>
  mutate(diff = score_future - score_current)

state_change <-
  all_results |>
  ggplot(aes(x = target, y = diff)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Chnage in climate score") +
  xlab("State") +
  geom_hline(yintercept = 0, color = "red", linewidth = 2)
print(state_change)
ggsave("state_change.png", state_change, width = 8, height = 4)

change_plot <-
  all_results |>
  mutate(cell = paste(target_x, target_y)) |>
  select(-target_x, -target_y, -diff) |>
  pivot_longer(cols = starts_with("score"),
               names_to = "Data",
               values_to = "Score") |>
  mutate(Data = gsub("score_", "", Data)) |>
  ggplot(aes(x = Data, y = Score)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(aes(group = cell), alpha = 0.25) +
  facet_wrap(vars(target), nrow = 2) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  ylab("Climate data")
change_plot
ggsave("change_plot.jpg", change_plot, width = 8, height = 4,
       dpi = 400)
