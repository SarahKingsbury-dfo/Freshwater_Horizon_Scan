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
library(readxl)
library (tidyverse)

#Step 1: CABI pull clean list for relevant species
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
  

#Step 2: Use taxize to clean up species names
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

#Step 3: read in any lists of species with completed assessments or determine irrelevant for this assessment and filter those out of master list
#read in list of completed species assessments
completed<-read_xlsx("data/Species_screening_list.xlsx", sheet=1)%>%
  rename(ScientificName=species_latin)

#Compare list of species with completed assessments to those without assessments and removed the completed species
Uncompleted_sp<-anti_join(tax_fix.itis, completed, by="ScientificName") #batch 2 species

batch1<-tax_fix.itis%>% #filter tax_fix.itis for batch 1 species so that climate matching for batch 1 species can b completed. 
  filter(ScientificName %in% completed$ScientificName)

write.csv(batch1, "data/batch1.csv") #write csv file of batch 1 species then go to gbif_script.R then return to step 5

#Remove species already regulated by CFIA (see list of pests regulated: )
require(rvest)
library(data.table)

#Step 4: remove CFIA regulated species
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

#Step 5: Now go gbif_script.R and gather occurrence reports for each species then read back in the detections

###### import gbif species occurrence records (see gbif_script.R for details)

sp_records<-read.csv("data/batch1_gbif_cleaned.csv") #batch 1
#("data/clean_gbif_sp_data.csv") #batch 2

#Step 6:fitler species established within assessment area (optional step!)
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

write.csv(sp_records, "data/sp_occurrence_data_batch1.csv") #batch 1

write.csv(sp_records, "data/sp_occurrence_data.csv") #batch 2


#Step 7: separate species occurrence reports into separate csv files per species
#Separate out species occurrence data into separate csv files because climatchr does not work well with the combined dataframe
dir.create('data/species_data/batch1/', showWarnings = F, recursive = T)

#first import sp records and filter the list of species to only include aquatic species
sp_to_filter_out<-read_xlsx("data/Freshwater_Terrestrial_Exclusion_List.xlsx") #only need this step for batch 2
sp_to_filter_out[sp_to_filter_out$F_T_freshwater_or_terrestrial=="F",]

sp_records<-read.csv("data/sp_occurrence_data_batch1.csv")%>%
  filter(ScientificName %in% sp_to_filter_out$ScientificName)  #Not necessary for batch 1

#only keep species name and geo corrinates
sp_records<-sp_records%>%
  select(ScientificName, lon, lat)

#need to remove space in species name and replace with _
sp_records$ScientificName<- gsub(" ", "_", sp_records$ScientificName)

#renamed column name from "ScientificName to species because that is what the USGS example code uses for column names
names(sp_records)[names(sp_records) == "ScientificName"] <- "species"

#for each species name within the dataframe (sp_records) write a csv and store in the new folder 
for(i in unique(sp_records$species)){
  ID2<-subset(sp_records, species==i)
  write.csv(ID2, file = paste0("data/species_data/batch1/", i, ".csv")) #change file output path depending on batch number
}

#Step 8: complete the climate matching analysis 
#####Climate matching between Nova Scotia (assessment area) and everywhere else in the world :)
#remotes::install_gitlab('umesc/quant-ecology/climatchr@dev', host = 'code.usgs.gov')

library(climatchR)
library(dplyr)
#import climate data: Currently using the Chelsa data downloaded 4 March 2023 from https://chelsa-climate.org/

# A. Read in climate data: bioclimate layers BIO1 and BIO5-19 as recommended here: https://code.usgs.gov/umesc/quant-ecology/climatchr
clim_dir <- 'data/CHELSA/standardized_1.2'
#raster stack of bioclimatic layer
clim_dat <- climatchR::clim_stacker(path = clim_dir)

# B. Create target reference using your own vector files
#    --often this is states/provinces/counties in a .shp, .sqlite or similar

Canada<-raster::getData ("GADM", country="CAN", level=1)
NS<-Canada[Canada$NAME_1=="Nova Scotia"]

#vect_path <- 'Example Scripts from USGS/ns.sqlite'

target_clim <- intersect_by_target(clim_dat = clim_dat,
                                   vect_path = Canada,
                                   col = 'NAME_1')

# C. Create output data folder
#dir.create('data/output_data/batch2/', showWarnings = F, recursive = T) #change batch number as needed

# D. Optional!! Only do (D) if you have already run climate matching on some species. Otherwise, skip to (E).
#remove any species with already completed assessments before running the climatchR code
sp_list<- c(list.files('data/output_data/batch1/doc/')) #change batch number as needed
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
  write.csv(ID2, file = paste0("data/species_data/batch1/", i, ".csv"))#change batch number as needed
}
# E. Caluclate bioclimatic vairbales for species detections by species

files <- list.files('C:/Users/kingsburys/Documents/GitHub/Freshwater_Horizon_Scan_NS/data/species_data/batch1/', full.names = T) #change file path for batch number

for(f in files){

    cat("Working on:",f,'\n')

    occ_dat <- read_occ_data(
      path = f,
      crs = "EPSG:4326",
      x='lon',
      y='lat',
      name_col = 'species',
      clim_dat = clim_dat
    )

    results <- calc_climatch(
      occ_dat = occ_dat,
      target = target_clim,
      sensitivity = 2,
      progress = T
    )

    write.csv(results, paste0('data/output_data/batch1/doc/', unique(results$species),'.csv'), row.names = F) #change batch number as needed

    climatch_to_raster(results = results,
                       template = 'Example Scripts from USGS/CHELSA_bio10_01.tif',
                       out_path = paste0('data/output_data/batch1/plot/', unique(results$species),'.tif')) #change batch number as needed

}

# F. Compile all the individual species climatchR assessments into one dataframe
files_to_df <- list.files('data/output_data/batch1/doc/', full.names = T) #change batch number as needed
dat_list<- list()

for (i in files_to_df){
  dat<-read.csv(i)
  dat$i<- i #track which iteration produced the list
  dat_list[[i]]<-dat #add data to the list
  
}

species_dat_df<-do.call(rbind, dat_list)

#save the combined species assessments as a single csv file
write.csv(species_dat_df, "data/combined_species_assessments_batch1.csv") #change batch number in file name as needed

# G. For records keeping: find distinct species names
distinct_sp<-species_dat_df%>%
  dplyr::select(species)%>%
  distinct()

#save a separate csv file with only assessed species names
write.csv(distinct_sp, "data/species_names_assessed_batch1.csv")  #change batch number in file name as needed

# H. Create a test plot, change batch number as needed Note: this is a random example to test if the lat and lon plot correctly.
library(raster)
library(terra)

rast('data/output_data/batch1/plot/Abramis_brama.tif')%>% #replace the path name with whatever species you want to test
  project('EPSG:4326')%>%
  plot()

# I. Only do this step if your tif plot bakcwards
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

#J. If plots are plotting correctly (i.e. the lat and lon aren't backwards) then load in all batch assessments and combine into one dataframe. 
#This will make it easier to communicate with other regions which species are of high concern based on climate match. 

batch1<-read.csv('data/combined_species_assessments_batch1.csv')%>%
  add_column(batch="Batch1")

batch2<-read.csv('data/combined_species_assessments.csv')%>%
  add_column(batch="Batch2")

batch_combined<-rbind(batch1, batch2)%>%
  dplyr::select(-X, -i)

write.csv(batch_combined, "data/combined_species_assessments_batch_combined.csv")

#Step 9: Filter climate matching assessment for applicable assessment region (e.g. Nova Scotia)
#Filter the combined dataframe for NS and species with scores greater than zero (i.e. unlikley to survive here)
ns_list<-species_dat_df%>%
  filter(target=="Nova Scotia")%>% #filter dataframe for Nova Scotia assessment
  filter(score>0) #only interested in species scoring higher than zero

distinct_ns_list<-ns_list%>% #look at distinct species names
  select(species)%>%
  distinct()

#save the NS list as a csv file. Will need to work through the screened list to remove the species that are native species or already established
write.csv(distinct_ns_list, 'data/NS_Screened_Species_List_batch1.csv') #change batch name as needed

#READ BACK-IN THE NS SCREENED SPECIES LIST 
ns_list_comp<-read.csv('data/NS_Screened_Species_List_batch1.csv')%>% #change batch name as needed
  filter(native=="N")%>% #filter to only include non-native species
  filter(established=="N")%>% #filter to retain species not yet established within assessment area
  filter(aquatic=="Y")%>% #filter to only include aquatic species
  select(species)

#filter the Nova Scotia list to only keep relevant species
ns_list<-ns_list%>%
  filter(species %in% ns_list_comp$species)

#step 10: Plotting Results as raster files-not necessary but nice to have

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



image<-rast('data/output_data/batch2/plot/Abbottina_rivularis.tif')%>% #read in test tif. Change batch number as needed.
    project('EPSG:4326') #set correct projection


Canada <- rnaturalearth::ne_states(country="Canada",returnclass = "sf")%>%
  dplyr::select(geometry, name)%>%
  #filter(name %in% c("Saskatchewan", "Manitoba", "Alberta"))%>% #can set the view of the plot to 'zoom in' on relevant areas
  st_set_crs(4326) #projection that is suitable for most of Canada

#tmaps is a package similar to ggplot for map making
tm_shape(image
         #bbox = st_bbox(Canada) #uncomment this line to zoom plots
         )+ 
  tm_raster(title='climate match score', breaks=c(1,2,3,4,5,6,7,8,9,10),
            palette='Spectral')+
  tm_layout(title='Abbottina_rivularis')+
  tm_shape(Canada)+
  tm_polygons(fill=NA, alpha=0.1)




#plot all NS relevant species
file_plots<-ns_list_comp%>% #list all plots
  mutate(filename =paste0('data/output_data/plot/',species, '.tif')) #convert species list to match file names

species_files <- unique(file_plots$filename) #make a maker for each individual species
species_plots <- list() #generate empty list for loop function to save to

for(filename_ in species_files) { #for every species, make a plot for that species and save to list
  
  image<-rast(filename_)%>%
    project('EPSG:4326')
  
  species_plots[[filename_]] = tm_shape(image)+ 
    tm_raster(title='climate match score')+
    tm_layout(title=file_plots$species[filename_])
}

library(patchwork)

allspp_plot <-  tmap_arrange(species_plots, ncol=3) #wrap all the plots together into one image
allspp_plot #print the image for viewing

#Step 11: convert all tif files into png files because png files are easier to share with non GIS or coder colleagues
library(raster)
library(terra)
library(fs)

files_plot <- list.files('data/output_data/batch1/plot', full.names = T) #Make list of all the files within the plot folder. change batch number as needed

for (f in files_plot){ #loop functions: for each plot with plot folder, change from tif to png and save to tif folder
  
  image<-rast(f)%>% #rasterize the tif
    project('EPSG:4326') #project to correct projection
  
  name<-path_file(f) #read file name. E.g. species_name.csv
  name<-path_ext_remove(name) #remove the ".csv" part of the name
  
  png(paste0('data/output_data/batch1/png/', name, '.png'),  width=718, height= 565, units="px") #save png files to png folder. change batch number as needed
  plot(image, maxpixels=ncell(image)) #also, plot the new png file to verify that the code is working correctly
  title(main=name) #add a title to each plot with the species name
  dev.off()
}

