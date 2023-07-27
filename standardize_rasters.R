library(terra)


# grab CHELSA 1.2 files: https://chelsa-climate.org/
#need CHELSA bio layers bio1 and bio5-19

files <-
  list.files(
    path = 'C:/Users/kingsburys/Documents/GitHub/Freshwater_Horizon_Scan_NS/data/CHELSA/bio',
    pattern = "tif$",
    recursive = F,
    full.names = T
  )

rastClim <- rast(files)


# rescale the temp variables (metadata for 1.2 indicates they were multiplied by 10)
rastClim[[1:8]] <- rastClim[[1:8]] / 10

### OPTIONAL ###
# convert the CHELSA data to ~ 10 min resolution (~16.7 km^2) - takes about 3-5 min
rastClim <- aggregate(rastClim,
                      fun = 'mean',
                      na.rm = T,
                      fact = 20)

### OPTIONAL ###
# project to mollweide equal area (https://spatialreference.org/ref/esri/54009/)
rastClim <-
  terra::project(rastClim, 'ESRI:54009', method = 'bilinear')


# standardize values with mean = 0 and sd = 1
# z-score standardization
# value - mean(layer) / standard deviation
rast_means <- global(rastClim, mean, na.rm = T)
rast_sd <- global(rastClim, sd, na.rm = T)

out <- (rastClim - rast_means$mean) / rast_sd$sd

for (i in seq_along(names(out))) {
  writeRaster(
    out[[i]],
    paste0('./data/CHELSA/standardized_1.2/', names(out)[i], '.tif'),
    overwrite = T,
    gdal = c('COMPRESS=LZW', 'BIGTIFF=YES', 'TILED=YES')
  )
}
