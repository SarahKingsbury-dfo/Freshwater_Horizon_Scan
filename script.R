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
library(spocc)
library(scrubr)
library(maps)
library (sf)
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

#####Climate matching between Nova Scotia (assessment area) and everywhere else in the world :)
#remotes::install_gitlab('umesc/quant-ecology/climatchr@dev', host = 'code.usgs.gov')

library(climatchR)
library(dplyr)
#import climate data: Currently using the Chelsa data downloaded 4 March 2023 from https://chelsa-climate.org/

# 1. Read in climate data: bioclimate layers BIO1 and BIO5-19 as recommended here: https://code.usgs.gov/umesc/quant-ecology/climatchr
clim_dir <- 'data/CHELSA/bio'
#raster stack of bioclimatic layer
clim_dat <- climatchR::clim_stacker(path = clim_dir)

# 2. Create target reference using your own vector files
#    --often this is states/provinces/counties in a .shp, .sqlite or similar
vect_path <- nova_scotia
target_clim <- intersect_by_target(clim_dat = clim_dat,
                                   vect_path = vect_path,
                                   col = 'name')
# target_clim<-target_clim%>%
#   relocate(target_x, .after = target_y)

# 3. Create output data folder
dir.create('data/output_data/', showWarnings = F, recursive = T)

# 4. Caluclate bioclimatic vairbales for species detections by species
# files<-'data/sp_occurrence_data.csv'
files<-'data/Test_data.csv'

occ_dat <-climatchR:: read_occ_data(
  path = files,
  clim_dat = clim_dat,
  #coords = c('lon', 'lat'),
  x='lat',
  y='lon',
  name_col = 'ScientificName',
  crs = "EPSG:4326"
)  

occ_dat<- occ_dat%>%
  drop_na()

species_names<-unique(occ_dat$species)
species_plots <- list()

for(species_ in species_names){
  
  cat("Working on:",species_,'\n')
  
  occ_dat<-occ_dat%>% filter(species == species_)%>%
  data.table:: as.data.table()
  
  results <- calc_climatch(
    occ_dat = occ_dat,
    target = target_clim,
    sensitivity = 2,  
    progress = T
  )
  
  write.csv(results, paste0('data/output_data/', species_,'.csv'), row.names = F)
  
  # climatch_to_raster(results = results,
  #                    template = 'data/CHELSA/bio/CHELSA_bio10_1981-2010_V.2.1.tif',
  #                    out_path = paste0('data/output_data/', ScientificName_ ,'.tif'),
  #                    overwrite = TRUE)
}