## load climatchR package
library(climatchR)
## add tidyverse package for formatting, plotting, and viewing data
library(tidyverse)
## load extra themes
library(ggthemes)
## load simple features
library(sf)
## 
library(usmap)

## spatial points data frame, species name and synonym species name
sp_df <- data.frame(valid_name = "Hydrilla verticillata",
                    syn_name = c(NA))

## species (output: numbers)
sp_list <- "hydrilla_list.csv"

## write csv (comma separated values) file
write.csv(sp_df, file = sp_list, row.names = FALSE)

## loops over species list, retrieves and cleans species occurrence data 
## from GBIF. Results written to external file sequentially to avoid
## IF file does not exists 
gbif_path <- "./gbif_results_current.csv"
if(!file.exists(gbif_path)){
  gbif_pull_clean(sp_list,
                  name_field = "valid_name",
                  syn_name_field = "syn_name",
                  outfile = gbif_path,
                  log = "./gbif_log.txt",
                  restart = FALSE)
}
print(read_csv(gbif_path))

## Load gadm data (part of climatchR package)
local_files <- system.file("extdat", package = "climatchR")
gdam_rds <-
  paste0(local_files, "/",
         c("gadm36_USA_1_sf.rds"))

## new code from Dr. E for current climate data
climate_folder <- "./Documents/99.0_GIT/future_climate/chelsa_current_mw_standardized"
state_list_path <- "states_climate_current.rds"

## IF .rds file DNE
if(!file.exists(state_list_path)){
  intersect_by_state(gdam_rds, climate_folder, out_path = state_list_path)
}

## Which States to Use %in% c("Wisconsin",
## "Minnesota",
## "Illinois")]
states_use <- c("Wisconsin", "Minnesota", "Illinois", "Iowa", "Missouri", "Kentucky", "Tennessee", "Arkansas", "Mississippi", "Louisiana")
state_list <- readRDS(state_list_path)
state_list_use <- state_list[names(state_list) %in% states_use]

## calculates climate scores
current_file <- "current_out.csv"

if(!file.exists(current_file)){
  example_out <-
    distance_from_gbif_file(clim_folder = climate_folder, 
                            gbif_path = gbif_path,
                            state_list = state_list_use,
                            clim_col_in = "^bio")
  ## Transform
  example_out_2 <-
    sf::st_as_sf(example_out, coords = c("x_state", "y_state"),
                 crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  example_out_3 <-
    sf::st_transform(example_out_2,
                     crs = "+proj=longlat +datum=WGS84 +no_defs")
  example_out <-
    example_out |>
    bind_cols(sf::st_coordinates(example_out_3))
  data.table::fwrite(example_out, current_file)
} else {
  example_data <- data.table::fread(current_file)
}

## Plot
ggplot(example_out, aes(x = x_state, y_state, fill = score)) +
  geom_tile()



## For some reason, tile no longer works
ggplot() +
  geom_point(data = example_out, aes(x = X, Y, color = score), size = 4.5) +
  theme_bw() +
  scale_color_continuous(low = "skyblue", high = "red") 
## Subtract two and create three images: Current score, future score, and difference

#
#plot_usmap(include = c("WI", "MN")), 
#           data = example_out, aes(x = X, Y, color = score))
