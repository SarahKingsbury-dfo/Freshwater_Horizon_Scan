remotes::install_gitlab('umesc/quant-ecology/climatchr@dev', host = 'code.usgs.gov')

library(climatchR)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. Read in climate data
clim_dir <- 'input_data/GlobalClimate/'
clim_dat <- climatchR::clim_stacker(path = clim_dir)

# 2. Create target reference using your own vector files
#    --often this is states/provinces/counties in a .shp, .sqlite or similar
vect_path <- 'input_data/poly.shp'
target_clim <- intersect_by_target(clim_dat = clim_dat,
                                   vect_path = vect_path,
                                   col = 'NAME_1')

files <- list.files('species_occ_data/', full.names = T)

dir.create('output_data/', showWarnings = F, recursive = T)

for(f in files){
  
  cat("Working on:",f,'\n')
  
  occ_dat <- read_occ_data(
    path = f,
    clim_dat = clim_dat,
    coords = c('decimalLongitude', 'decimalLatitude'),
    name_col = 'species',
    crs = "EPSG:4326"
  )      
  
  results <- calc_climatch(
    occ_dat = occ_dat,
    target = target_clim,
    sensitivity = 2,  
    progress = T
  )
  
  write.csv(results, paste0('output_data/', unique(results$species),'.csv'), row.names = F)
  
  climatch_to_raster(results = results,
                     template = 'input_data/GlobalClimate/CHELSA_bio10_01.tif',
                     out_path = paste0('output_data/', unique(results$species),'.tif'),
                     overwrite = TRUE)
}