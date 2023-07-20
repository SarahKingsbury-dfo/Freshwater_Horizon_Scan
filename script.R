library (readxl)
library(taxize)
library(magrittr)
library(taxadb)
#remotes::install_gitlab('umesc/quant-ecology/climatchr@dev', host = 'code.usgs.gov')
library(climatchR)
library(tidyterra)
library(gridExtra)
library(ggspatial)
library(rgbif)
library(terra)
library(spocc)
library(scrubr)
library(maps)
library (sf)
library(sp)
library (tidyverse)


########Create the initial list from the CABI horizon scan tool pull
#Read in the list pulled from CABI horizon scan tool: https://www.cabi.org/HorizonScanningTool/Country/SearchResult
CABI<-read_excel("data/CABI_Horizon Scanning_pulled 27 Apr 2023.xlsx")
# names(CABI)
# unique(CABI$Class)

#These are the names of species class that are not applicable to this project
Undesired_classs<-c("Arachnida", "NA", "Insecta", "Amphibia", "Aves", "Hirudinoidea",
                    "Secernentea", "Ulvophyceae", "Reptilia", "Phaeophyceae", "Hydrozoa",
                    "Cestoda", "Ascidiacea" , "Trematoda", "Copepoda", "Adenophorea",
                    "Myxosporea Myxobolus cerebralis", "Diplopoda", "Ophiuroidea",
                    "Pinopsida", "Cephalopoda", "Chlorophyceae")

#filter out the undesired classes
CABI_classFilter<-CABI%>%
  dplyr::filter(!(Class %in% Undesired_classs))



#Look for species that are already listed as invasive somewhere
CABI_filtered_invasive<-CABI_classFilter%>%
  dplyr::filter(Invasive_Somewhere=="invasive")

Species_names<-CABI_filtered_invasive%>%
  dplyr::select(Preferred_scientific_name)%>%
  dplyr::filter(!is.na(Preferred_scientific_name))
  


#Use taxize to check the spelling of species names
src_pos<-gnr_datasources()

src <- c( "ITIS", "Invasive Species of Belgium", "Global Invasive Species Database",
          "OBIS", "Belgian Species List", "IUCN Red List of Threatened Species", "Database of Vascular Plants of Canada (VASCAN)",
          "Integrated Taxonomic Information SystemITIS")

subset(gnr_datasources(), title %in% src)

tax_fix_CABI<-CABI_filtered_invasive$Preferred_scientific_name %>%
  gnr_resolve(data_source_ids = c(3, 104, 125, 147, 149, 157, 163), 
              with_canonical_ranks=T)
  # mutate(name_correct = ifelse(user_clean %in% matched_name, T, F)) %>% #determine if the original name was in the list of names retrieved
  # mutate(species_clean = ifelse(name_correct ==T, user_clean, matched_name[(amatch(user_clean, matched_name, maxDist = Inf))]))

tax_fix.short <- tax_fix_CABI %>%
  select(submitted_name, matched_name2, score, data_source_title)%>%
  distinct()

tax_fix.itis<-tax_fix.short%>%
  filter(data_source_title=="Integrated Taxonomic Information SystemITIS")%>%
  filter(score >=0.8)

tax_fix.itis<- tax_fix.itis[match(unique(tax_fix.itis$matched_name2), tax_fix.itis$matched_name2),]%>%
  rename(ScientificName=matched_name2)


write.csv(tax_fix.short, "data/tax_fix.short.csv")

#read in list of completed species assessments
completed<-read_xlsx("data/Species_screening_list.xlsx", sheet=1)%>%
  rename(ScientificName=species_latin)

#Compare list of species with completed assessments to those without assessments and removed the completed species
Uncompleted_sp<-anti_join(tax_fix.itis, completed, by="ScientificName")

#Remove species already regulated by CFIA (see list of pests regulated: )
require(rvest)
library(data.table)

#First import list of CFIA pest species
page <- "https://inspection.canada.ca/plant-health/invasive-species/regulated-pests/eng/1363317115207/1363317187811" %>% 
  read_html()

CFIA_sp<-part2 <- (page %>% 
                     html_table(header = TRUE))
#convert list to dataframe
CFIA_df<-rbindlist(CFIA_sp)%>%
  select_all(~gsub("\\s+|\\.", "_", .))%>%
  mutate(ScientificName_CFIA=as.character(`Scientific_name_and_authority_Table_Note 1`),
         CommonName=as.character(`English_common_name_Table_Note 2`))%>%
  select(ScientificName_CFIA, CommonName)

#look for species from the CFIA list within our species list
library(fuzzyjoin)
library(stringr)

check_cfia<-fuzzyjoin::regex_left_join(Uncompleted_sp, CFIA_df, by=c("ScientificName"="ScientificName_CFIA"))%>%
  filter(!is.na(ScientificName_CFIA))

#remove species from our list that are included on the CFIA list
Uncompleted_sp<-Uncompleted_sp%>%
  filter (!ScientificName == (check_cfia$ScientificName))

#create a csv of the final list to be used below and also in the gbif_script.R
write.csv(Uncompleted_sp, "data/uncomplete_sp.csv")


###### import gbif species occurrence records (see gbif_script.R for details)

sp_records<-read.csv("data/clean_gbif_sp_data.csv")

#remove species that occur within Nova Scotia (note: this step can be modified to remove species from our list that occur within sink area)
devtools::install_github("ropensci/rnaturalearth")
library(rnaturalearth)
library(sf)

proj <- "+proj=lcc +lon_0=-63.1 +lat_1=43.8 +lat_2=46.9 +lat_0=45.3 +datum=WGS84 +units=m +no_defs"
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
ll <- "+proj=longlat +datum=WGS84"
sf::sf_use_s2(FALSE) # because of "Evaluation error: Found 1 feature with invalid spherical geometry." otherwise

#create the Nova Scotia polygon
nova_scotia <-  rnaturalearth::ne_states(country="Canada",returnclass = "sf") %>%
  filter(name=="Nova Scotia")

#convert the gbif species record dataframe to an sf object
sp_records_sf<-sp_records%>%
  st_as_sf(coords=c('lon', 'lat'), crs=st_crs(nova_scotia))

#check for the point data intersecting the Nova Scotia polygon
out<-sp_records_sf%>%
  st_filter(nova_scotia, join=st_intersects())

#plot to double check correctness
ggplot(nova_scotia)+
  geom_sf()+
  geom_sf(data=out, aes(col=ScientificName))

#remove the species that occur in Nova Scotia
sp_records<-sp_records%>%
  filter(!specieskey %in% (out$specieskey))%>%
  relocate(lat, .after = lon)

#write csv file for record tracking and to use later fir climate matching
write.csv(sp_records, "data/sp_occurrence_data.csv")

#Separate out species occurrence data into separate csv files because climatchr does not work well with the combined dataframe
dir.create('data/species_data/', showWarnings = F, recursive = T)

#first import sp records and filter the list of species to only include aquatic species
sp_to_filter_out<-read_xlsx("data/Freshwater_Terrestrial_Exclusion_List.xlsx")
sp_to_filter_out[sp_to_filter_out$F_T_freshwater_or_terrestrial=="F",]

sp_records<-read.csv("data/sp_occurrence_data.csv")%>%
  filter(ScientificName %in% sp_to_filter_out$ScientificName)

sp_records<-sp_records%>%
  select(ScientificName, lon, lat)

sp_records$ScientificName<- gsub(" ", "_", sp_records$ScientificName)

names(sp_records)[names(sp_records) == "ScientificName"] <- "species"

for(i in unique(sp_records$species)){
  ID2<-subset(sp_records, species==i)
  write.csv(ID2, file = paste0("data/species_data/", i, ".csv"))
}

#####Climate matching between Nova Scotia (assessment area) and everywhere else in the world :)
#remotes::install_gitlab('umesc/quant-ecology/climatchr@dev', host = 'code.usgs.gov')

library(climatchR)
library(dplyr)
#import climate data: Currently using the Chelsa data downloaded 4 March 2023 from https://chelsa-climate.org/

# 1. Read in climate data: bioclimate layers BIO1 and BIO5-19 as recommended here: https://code.usgs.gov/umesc/quant-ecology/climatchr
clim_dir <- 'data/CHELSA/standardized_1.2'
#raster stack of bioclimatic layer
clim_dat <- climatchR::clim_stacker(path = clim_dir)

# 2. Create target reference using your own vector files
#    --often this is states/provinces/counties in a .shp, .sqlite or similar

Canada<-raster::getData ("GADM", country="CAN", level=1)
NS<-Canada[Canada$NAME_1=="Nova Scotia"]

#vect_path <- 'Example Scripts from USGS/ns.sqlite'

target_clim <- intersect_by_target(clim_dat = clim_dat,
                                   vect_path = Canada,
                                   col = 'NAME_1')

# 3. Create output data folder
#dir.create('data/output_data/', showWarnings = F, recursive = T)

# 4. remove any species with already completed assessments before running the climatchR code
sp_list<- c(list.files('data/output_data/doc/'))
sp_no_code<-sp_records%>%
  select(-lon, -lat)%>%
  distinct()%>%
  mutate(filename =paste0(species, '.csv'))%>% #convert species list to match file names
  mutate(has_doc=ifelse(filename %in% sp_list==F, F, T))%>% #identify species already analyzed
  filter(has_doc==F) #filter out species already assessed

sp_records<-sp_records%>%
  filter(species %in% sp_no_code$species)%>% #filtering out species with completed assessments
  group_by(species)%>% #ClimatchR doesn't work on species with only 1 occurrence report. Group by species.
  mutate(freq=n())%>% #count how often that species occurs
  ungroup()%>% #ungroup dataframe
  filter(freq > 1)%>% #remove species with only 1 occurrence record
  select(-freq) #remova freq column from dataframe before generating new csv files

#Wraning: make sure you delete all individual species files from species_data folder before running this code
for(i in unique(sp_records$species)){
  ID2<-subset(sp_records, species==i)
  write.csv(ID2, file = paste0("data/species_data/", i, ".csv"))
}
# 5. Caluclate bioclimatic vairbales for species detections by species

files <- list.files('C:/Users/kingsburys/Documents/GitHub/Freshwater_Horizon_Scan_NS/data/species_data/', full.names = T)

# for(f in files){
# 
#     cat("Working on:",f,'\n')
#     
#     occ_dat <- read_occ_data(
#       path = f,
#       crs = "EPSG:4326",
#       x='lon',
#       y='lat',
#       name_col = 'species',
#       clim_dat = clim_dat
#     )
#     
#     results <- calc_climatch(
#       occ_dat = occ_dat,
#       target = target_clim,
#       sensitivity = 2,  
#       progress = T
#     )
#     
#     write.csv(results, paste0('data/output_data/doc/', unique(results$species),'.csv'), row.names = F)
#     
#     climatch_to_raster(results = results,
#                        template = 'Example Scripts from USGS/CHELSA_bio10_01.tif',
#                        out_path = paste0('data/output_data/plot/', unique(results$species),'.tif'))
#     
# }

#Compile all the individual species climatchR assessments into one dataframe
files_to_df <- list.files('data/output_data/doc/', full.names = T)
dat_list<- list()

for (i in files_to_df){
  dat<-read.csv(i)
  dat$i<- i #track which iteration produced the list
  dat_list[[i]]<-dat #add data to the list
  
}

species_dat_df<-do.call(rbind, dat_list)

# #reformat plots. Only do this if you find that the plots are backwards from expected. The older version of climatchR had issues with this.
# for (i in files_to_df){
#   dat <- read.csv(i)%>%
#     group_by(species, target_x, target_y)%>% 
#     summarise(score = max(score, na.rm = T))
#   
#   climatch_to_raster(results = dat,
#                      template = 'Example Scripts from USGS/bio1.tif',
#                      out_path = paste0('data/output_data/plot/', unique(dat$species),'.tif'),
#                      overwrite = T)
#   
# }
# 
# #test plot
# rast('data/output_data/plot/Abbottina_rivularis.tif')%>%
#   project('EPSG:4326')%>%
#   plot()

#Filter the combined dataframe for NS and species with scores greater than zero (i.e. unlikley to survive here)
ns_list<-species_dat_df%>%
  filter(target=="Nova Scotia")%>%
  filter(score>0)

distinct_ns_list<-ns_list%>%
  select(species)%>%
  distinct()

#save the NS list as a csv file. Will need to work through the screened list to remove the species that are native species or already established
write.csv(distinct_ns_list, 'data/NS_Screened_Species_List.csv')

#READ BACK-IN THE NS SCREENED SPECIES LIST 
ns_list_comp<-read.csv('data/NS_Screened_Species_List.csv')%>%
  filter(native=="N")%>% #filter to only include non-native species
  filter(established=="N")%>% #filter to retain species not yet established within assessment area
  filter(aquatic=="Y")%>% #filter to only include aquatic species
  select(species)

ns_list<-ns_list%>%
  filter(species %in% ns_list_comp$species)

#test ploting ns only

library(tmap)
library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()
library(terra)
library(sp)
library(stars)



image<-rast('data/output_data/plot/Abbottina_rivularis.tif')%>%
    project('EPSG:4326')
image_df<-terra::as.data.frame (image)
colnames(image_df)<-c("value", "x", "y") 
image_spdf<-as(image, "SpatialPixelsDataFrame")

test<-'data/output_data/plot/Abbottina_rivularis.tif'
test<-raster(test)
# new_test<-spTransform(test, CRS("+init=epsg:4238"))
# projection(raster)<-CRS("+init=epsg:4326")
test_spdf<-as(test, "SpatialPixelsDataFrame")
# proj4string(test_spdf)<-CRS('+init=epsg:4326 +proj=longlat
#                             +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0')
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")


tm_shape(image)+ 
  tm_raster(title='climate match score')+
  tm_layout(title='Abbottina_rivularis')




#plot all NS relevant species
file_plots<-ns_list_comp%>%
  mutate(filename =paste0('data/output_data/plot/',species, '.tif')) #convert species list to match file names

species_files <- unique(file_plots$filename)
species_plots <- list()

for(filename_ in species_files) {
  
  image<-rast(filename_)%>%
    project('EPSG:4326')
  
  species_plots[[filename_]] = tm_shape(image)+ 
    tm_raster(title='climate match score')+
    tm_layout(title=file_plots$species[filename_])
}

library(patchwork)

allspp_plot <-  tmap_arrange(species_plots, ncol=3) 
allspp_plot

