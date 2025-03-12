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
  select(-freq) #remove freq column from dataframe before generating new csv files

#Warning: make sure you delete all individual species files from species_data folder before running this code
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

# I. Only do this step if your tif plot backwards. Otherwise skip to J
# #reformat plots. Only do this if you find that the plots are backwards from expected. The older version of climatchR had issues with this.
for (i in files_to_df){
  dat <- read.csv(i)%>%
    group_by(species, target_x, target_y)%>%
    summarise(score = max(score, na.rm = T))

  climatch_to_raster(results = dat,
                     template = 'Example Scripts from USGS/bio1.tif',
                     out_path = paste0('data/output_data/plot/', unique(dat$species),'.tif'),
                     overwrite = T)

}

#J. If plots are plotting correctly (i.e. the lat and lon aren't backwards) then load in all batch assessments and combine into one dataframe. 
#This will make it easier to communicate which species are of high concern based on climate match. 

batch1<-read.csv('data/combined_species_assessments_batch1.csv')%>%
  add_column(batch="Batch1")

batch2<-read.csv('data/combined_species_assessments.csv')%>%
  add_column(batch="Batch2")

batch3<-read.csv('data/combined_species_assessments_batchAntoine.csv')%>%
  add_column(batch="Batch3")

batch_combined<-rbind(batch1, batch2, batch3)%>%
  dplyr::select(-X, -i)

write.csv(batch_combined, "data/combined_species_assessments_batch_combined.csv")

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

############### Future Projections ###############

#Make folder of species data
batch_1_records<-read.csv("data/batch1_gbif_cleaned.csv")
batch_2_records<-read.csv("data/clean_gbif_sp_data.csv")
sp_records<- rbind(batch_1_records,batch_2_records)

#first import sp records and filter the list of species to only include aquatic species
sp_to_filter_out<-read_xlsx("data/Freshwater_Terrestrial_Exclusion_List.xlsx") 
sp_to_filter_out[sp_to_filter_out$F_T_freshwater_or_terrestrial=="F",]

sp_records<-sp_records%>%
  filter(ScientificName %in% sp_to_filter_out$ScientificName)  

#only keep species name and geo corrinates
sp_records<-sp_records%>%
  select(ScientificName, lon, lat)

#need to remove space in species name and replace with _
sp_records$ScientificName<- gsub(" ", "_", sp_records$ScientificName)

#renamed column name from "ScientificName to species because that is what the USGS example code uses for column names
names(sp_records)[names(sp_records) == "ScientificName"] <- "species"

#Filter out species with completed assessments
sp_list<- c(list.files('data/output_data/MPI-ESM1-2-HR/doc/')) #change batch number/climate model as needed

sp_no_code<-sp_records%>%
  select(-lon, -lat)%>%
  distinct()%>%
  mutate(filename =paste0(species, '.csv'))%>% #convert species list to match file names
  mutate(has_doc=ifelse(filename %in% sp_list==F, F, T))%>% #identify species already analyzed
  filter(has_doc==F) #filter out species already assessed

#remove species from the list that already have assessments completed and/or have one or few occurrence records

sp_records<-sp_records%>%
  #filter(species %in% sp_no_code$species)%>% #filtering out species with completed assessments
  group_by(species)%>% #ClimatchR doesn't work on species with only 1 occurrence report. Group by species.
  mutate(freq=n())%>% #count how often that species occurs
  ungroup()%>% #ungroup dataframe
  filter(freq > 1)%>% #remove species with only 1 occurrence record
  select(-freq) #remove freq column from dataframe before generating new csv files

#Warning: Make sure to delete all the csv files from the species_data/all_species/ folder before

dir.create('data/species_data/all_species/', showWarnings = F, recursive = T)

for(i in unique(sp_records$species)){
  ID2<-subset(sp_records, species==i)
  write.csv(ID2, file = paste0("data/species_data/all_species/", i, ".csv"))#change batch number as needed
}


library(climatchR)
library(dplyr)
#import climate data: Currently using the Chelsa data downloaded 4 March 2023 from https://chelsa-climate.org/

# A. Read in climate data: bioclimate layers BIO1 and BIO5-19 as recommended here: https://code.usgs.gov/umesc/quant-ecology/climatchr
clim_dir <- 'data/CHELSA/MPI-ESM1-2-HR/Standardized' #Change directory for each climate change model
#raster stack of bioclimatic layer
clim_dat <- climatchR::clim_stacker(path = clim_dir)

# B. Create target reference using your own vector files
#    --often this is states/provinces/counties in a .shp, .sqlite or similar

Canada<-raster::getData ("GADM", country="CAN", level=1)
NS<-Canada[Canada$NAME_1=="Nova Scotia"]

#vect_path <- 'Example Scripts from USGS/ns.sqlite'

target_clim <- intersect_by_target(clim_dat = clim_dat,
                                   vect_path = Canada,
                                   col = 'NAME_1'
                                  )

# #It's possible to further refine the target area to a specific province or territory
# NS_target<-target_clim[target_clim$target=="Nova Scotia"]

# C. Create output data folder
#dir.create('data/output_data/2011_2040_IPSL_CM6A_LR_ssp585/', showWarnings = F, recursive = T) #change climate model as needed

# D. Caluclate bioclimatic vairbales for species detections by species

files <- list.files('C:/Users/kingsburys/Documents/GitHub/Freshwater_Horizon_Scan_NS/data/species_data/all_species/', full.names = T) #change file path for batch number

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
  
  write.csv(results, paste0('data/output_data/MPI-ESM1-2-HR/doc/', unique(results$species),'.csv'), row.names = F) #change batch number as needed
  
  climatch_to_raster(results = results,
                     template = 'Example Scripts from USGS/CHELSA_bio10_01.tif',
                     out_path = paste0('data/output_data/MPI-ESM1-2-HR/plot/', 
                                       unique(results$species),'.tif'), #change batch number as needed
                     overwrite=TRUE) #this will overwrite any previous assessments and replace those with the latest assessment
  
}

# E. Compile all the individual species climatchR assessments into one dataframe
files_to_df <- list.files('data/output_data/2011_2040_IPSL_CM6A_LR_ssp585/doc/', full.names = T) #change climate model as needed
dat_list<- list()

for (i in files_to_df){
  dat<-read.csv(i)
  dat$i<- i #track which iteration produced the list
  dat_list[[i]]<-dat #add data to the list
  
}

species_dat_df<-do.call(rbind, dat_list)

#save the combined species assessments as a single csv file
write.csv(species_dat_df, "data/IPSL_CM6A_LR.csv") #change batch number in file name as needed

#F. If plots are plotting correctly (i.e. the lat and lon aren't backwards) then load in all batch assessments and combine into one dataframe. 
#This will make it easier to communicate with other regions which species are of high concern based on climate match. 

batch_CM6A<-read.csv('data/IPSL_CM6A_LR.csv')%>%
  filter (target=="Yukon" & score!= 0)  %>%
  add_column(batch="CM6A")

batch_GFDL<-read.csv('data/GFDL-ESM4.csv')%>%
  filter (target== "Yukon"  & score!= 0)  %>%
  add_column(batch="GFDL")

batch_ESM1<-read.csv('data/MPI-ESM1-2-HR.csv')%>%
  filter (target== "Yukon"  & score!= 0)  %>%
  add_column(batch="ESM1")

batch_combined<-rbind(batch_CM6A, batch_GFDL, batch_ESM1)%>%
  dplyr::select(-X, -i)

batch_combined<-batch_combined%>%
 group_by(species)%>% #group dtaaframe by species
  mutate(avg_score=mean(score), n=n())%>% #take the average value over the 3 models
  ungroup() #ungroup dataframe

#NOTE: Need to add average score to batch_combined



#Step 11: Filter combined list for specific area of interest
NS_list_2011_2040<-batch_combined%>%
  mutate(avg_score=as.numeric(avg_score))%>%
  filter(target== "Nova Scotia" & avg_score!= 0)

write.csv(NS_list_2011_2040, "data/NS_list_2011_2040.csv")

#filter the data to only retain the average climate match score for all of NS and species names
NS_2011_2040_sp.names<-NS_list_2011_2040%>%
  mutate(clim_match=case_when(
    avg_score<= 1~ "Low",
    avg_score<= 2~ "Medium",
    avg_score<=5~"High"
  ))%>%
  select(species, avg_score, clim_match)%>%
  distinct()%>%
  add_column(batch="2011_2040")

#compare to historic climate match results for NS 

historic_combined<-read.csv("data/combined_species_assessments_batch_combined.csv")

NS_historic<-historic_combined%>%
  filter (target== "Nova Scotia" & score!= 0)%>%
  group_by(species)%>% #group dtaaframe by species
  mutate(avg_score=mean(score), n=n())%>% #take the average value over the 3 models
  ungroup()%>% #ungroup dataframe
  mutate(clim_match=case_when(
    avg_score<= 1~ "Low",
    avg_score<= 2~ "Medium",
    avg_score<=5~"High"
  ))%>%
  select(species, avg_score, clim_match)%>%
  distinct()%>%
  add_column(batch="1981_2010")

#add the species lists together
NS_compare_batch<-rbind(NS_2011_2040_sp.names, NS_historic)

#check number of duplicates (i.e. how many species have predicted climate match in 1980-2011 and 2011-2040 assessments?)
sum(duplicated(NS_compare_batch$species))

#make a copy of the list for furhter exploration outside of R
write.csv(NS_compare_batch, "data/NS_Master_Species_List.csv")

#Now checking to see
ns_batch3<-batch3%>%
  filter(target== "Nova Scotia" & score!= 0)
n_distinct(ns_batch3$species)

#find the species that need assesing outside R for native species, establishment, and aqautic or not
NS_List_to_assess<-NS_compare_batch%>%
  filter(batch =="2011_2040")%>%
  anti_join (ns_list_comp, by="species")

write.csv(NS_List_to_assess, "data/NS_2011_2040_list_to_screen_corrected.csv")

#Now read back in the list of NS potential species that was assessed outside R
NS_list_2011_2040_screened<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(native=="N")%>% #keep only species that are non-native
  filter(established=="N")%>% #only keep species that have not alreayd been introduced to NS
  filter(aquatic=="Y")%>% #retain all potentially aquatic species
  filter(climate_mismatch=="N") #retain only species that have appropriate climate needs

#export the finalized 2011-2040 screened species list
write.csv(NS_list_2011_2040_screened, "data/NS_2011_2040_list_finalized.csv")

############# Compare climate match results from Nova Scotia, New Brunswick, PEI with all other Canadian Provinces.

#Gulf first


NB_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target %in% c("New Brunswick", "Prince Edward Island") & score!= 0)

NB_historic_list<-NB_historic%>%
  select (species)
NB_future_list<-batch_combined%>%
  select (species)

NB_batch<-rbind(NB_historic_list, NB_future_list)%>%
  distinct()

NS_list<-read.csv("data/NS_Master_Species_List.csv")%>%
  select (species)%>%
  distinct()

NS_to_NB<-NB_batch%>%
  anti_join (NS_list, by="species")

#Quebec

QC_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target=="Québec" & score!= 0)

QC_historic_list<-QC_historic%>%
  select (species)
QC_future_list<-batch_combined%>%
  select (species)

QC_batch<-rbind(QC_historic_list, QC_future_list)%>%
  distinct()

NS_to_QC<-QC_batch%>%
  anti_join (NS_list, by="species")

#export the Quebec list of species
write.csv(QC_batch, "data/QC_species_list.csv")

#Ontario

ON_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target=="Ontario" & score!= 0)

ON_historic_list<-ON_historic%>%
  select (species)
ON_future_list<-batch_combined%>%
  select (species)

ON_batch<-rbind(ON_historic_list, ON_future_list)%>%
  distinct()

NS_to_ON<-ON_batch%>%
  anti_join (NS_list, by="species")

#export the Ontario list of species
write.csv(ON_batch, "data/ON_species_list.csv")

#Manitoba

MB_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target=="Manitoba" & score!= 0)

MB_historic_list<-MB_historic%>%
  select (species)
MB_future_list<-batch_combined%>%
  select (species)

MB_batch<-rbind(MB_historic_list, MB_future_list)%>%
  distinct()

NS_to_MB<-MB_batch%>%
  anti_join (NS_list, by="species")

#export the Ontario list of species
write.csv(MB_batch, "data/MB_species_list.csv")

#Newfoundland and Labrador

NFL_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target=="Newfoundland and Labrador" & score!= 0)

NFL_historic_list<-NFL_historic%>%
  select (species)
NFL_future_list<-batch_combined%>%
  select (species)

NFL_batch<-rbind(NFL_historic_list, NFL_future_list)%>%
  distinct()

NS_to_NFL<-NFL_batch%>%
  anti_join (NS_list, by="species")

#export the Ontario list of species
write.csv(NFL_batch, "data/NFL_species_list.csv")

#Saskatchewan
SK_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target=="Saskatchewan" & score!= 0)

SK_historic_list<-SK_historic%>%
  select (species)
SK_future_list<-batch_combined%>%
  select (species)

SK_batch<-rbind(SK_historic_list, SK_future_list)%>%
  distinct()

NS_to_SK<-SK_batch%>%
  anti_join (NS_list, by="species")

#export the Ontario list of species
write.csv(SK_batch, "data/SK_species_list.csv")

#Alberta
AB_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target=="Alberta" & score!= 0)

AB_historic_list<-AB_historic%>%
  select (species)
AB_future_list<-batch_combined%>%
  select (species)

AB_batch<-rbind(AB_historic_list, AB_future_list)%>%
  distinct()

NS_to_AB<-AB_batch%>%
  anti_join (NS_list, by="species")

#export the Ontario list of species
write.csv(AB_batch, "data/AB_species_list.csv")

#British Columbia
BC_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target=="British Columbia" & score!= 0)

BC_historic_list<-BC_historic%>%
  select (species)
BC_future_list<-batch_combined%>%
  select (species)

BC_batch<-rbind(BC_historic_list, BC_future_list)%>%
  distinct()

#export the Ontario list of species
write.csv(BC_batch, "data/BC_species_list.csv")

#Yukon
YK_historic<-read.csv("data/combined_species_assessments_batch_combined.csv")%>%
  filter (target=="Yukon" & score!= 0)

YK_historic_list<-YK_historic%>%
  select (species)
YK_future_list<-batch_combined%>%
  select (species)

YK_batch<-rbind(YK_historic_list, YK_future_list)%>%
  distinct()

#export the Ontario list of species
write.csv(YK_batch, "data/YK_species_list.csv")
