# install dev branch version of climatchR
# requires >= R 4.1.x
remotes::install_gitlab('umesc/quant-ecology/climatchr@dev', host = 'code.usgs.gov')

library(climatchR)

# 1. Read in climate data
clim_dir <-
  '~/R-4.2.3/library/climatchR/extdat/GlobalClimate'
clim_dat <- climatchR::clim_stacker(path = clim_dir)



# 2. Create target reference using your own vector files
#    --often this is states/provinces/counties in a .shp, .sqlite or similar
vect_path <-  '~/poly.shp'

target_clim <- intersect_by_target(clim_dat = clim_dat,
                                   vect_path = vect_path,
                                   col = 'NAME_1')



# 3. Read in extract climate data at occurrence locations
gbif_path <-
  '~/R-4.2.3/library/climatchR/extdat/gbif_Bison_bison.csv'

occ_dat <- read_occ_data(
  path = gbif_path,
  clim_dat = clim_dat,
  coords = c('longitude', 'latitude'),
  name_col = 'searched_term',
  crs = "EPSG:4326"
)



# 4. Run climatch

# You now only need the data frame from the intersect_by_target() function,
# which can be subset if you'd like.
target_subset <- target_clim[target == 'Wisconsin',]

# sensitivity finds the maximum and second maximum score, higher numbers will 
# return more results but also increase file size proportionately.
results <- calc_climatch(
  occ_dat = occ_dat,
  target = target_subset,
  sensitivity = 2,  
  progress = F
)

# 5. Generate raster from results
climatch_to_raster(results = results,
                   template = '~/R-4.2.3/library/climatchR/extdat/GlobalClimate/CHELSA_bio10_01.tif',
                   out_path = '~/Desktop/test.tif',
                   overwrite = T)

# Reprojecting/plotting example
terra::rast('~/Desktop/test.tif') |> 
  terra::project('EPSG:4326') |> 
  terra::plot()
