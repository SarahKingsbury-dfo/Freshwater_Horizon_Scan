library(terra)

##### Historic Data ######
# grab CHELSA 2.1 files: https://chelsa-climate.org/
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

##### Future Climate Prediction Data ######
# use at least 3 climate model projections. Here we use IPSL-CM6A-LR, GFDL-ESM4, and MPI-ESM1-2-HR from: https://chelsa-climate.org/
#We are using all ssp 585 for projection layers 2011-2040
#need CHELSA bio layers bio1 and bio5-19

files <-
  list.files(
    path = 'C:/Users/kingsburys/Documents/GitHub/Freshwater_Horizon_Scan_NS/data/CHELSA/MPI-ESM1-2-HR/raw', #change file path depending on model folder
    pattern = "tif$",
    recursive = F,
    full.names = T
  )

rastClim <- rast(files) #create a raster stack of all the tif layers for each model


# rescale the temp variables (metadata for 2.1 indicates they were multiplied by 10)
rastClim[[1:8]] <- rastClim[[1:8]] / 10 #Now resolve the raster tif layers by dividing by 10

### OPTIONAL ###
# convert the CHELSA data to ~ 10 min resolution (~16.7 km^2) - takes about 3-5 min but is much easier to climat match with later on
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
    paste0('./data/CHELSA/MPI-ESM1-2-HR/Standardized/', names(out)[i], '.tif'),#write in file path to detsination folder
    overwrite = T,
    gdal = c('COMPRESS=LZW', 'BIGTIFF=YES', 'TILED=YES')
  )
}
