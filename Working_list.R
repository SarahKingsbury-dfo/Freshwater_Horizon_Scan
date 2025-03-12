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
library(writexl)
library (tidyverse)

#Step 1: CABI pull clean list for relevant species
########Create the initial list from the CABI horizon scan tool pull
#Read in the list pulled from CABI horizon scan tool: https://www.cabi.org/HorizonScanningTool/Country/SearchResult
CABI<-read_excel("data/CABI_Horizon Scanning_pulled 27 Apr 2023.xlsx")
# names(CABI)
# unique(CABI$Class)

#Keep track of species names and why they were filtering out
list_name<-CABI%>%
  select(Preferred_scientific_name)

#These are the names of species class that are not applicable to this project
Undesired_classs<-c("Arachnida", "NA","Insecta", "Amphibia", "Aves", "Hirudinoidea",
                    "Secernentea", "Ulvophyceae", "Reptilia", "Phaeophyceae", "Hydrozoa",
                    "Cestoda", "Ascidiacea" , "Trematoda", "Copepoda", "Adenophorea",
                    "Myxosporea Myxobolus cerebralis", "Diplopoda", "Ophiuroidea",
                    "Pinopsida", "Cephalopoda", "Chlorophyceae")

#filter out the undesired classes
CABI_classFilter<-CABI%>%
  dplyr::filter(!(Class %in% Undesired_classs))


#Keep track of species names and why they were filtering out
undesired_CABI<-CABI%>% 
  dplyr::filter((Class %in% Undesired_classs)) #what were the desired species?

list_name<-list_name%>% #assign undesired species as mandate mismatch b/c they are not included in DFO's mandate
  mutate(category=case_when(Preferred_scientific_name %in% undesired_CABI$Preferred_scientific_name ~"Mandate Mismatch"))

#Look for species that are already listed as invasive somewhere
CABI_filtered_invasive<-CABI_classFilter%>%
  dplyr::filter(Invasive_Somewhere=="invasive")

Species_names<-CABI_filtered_invasive%>%
  dplyr::select(Preferred_scientific_name)%>%
  dplyr::filter(!is.na(Preferred_scientific_name))

#Keep track of species names and why they were filtering out
not_NIS<-CABI_classFilter%>%
  dplyr::filter(!(Preferred_scientific_name %in% CABI_filtered_invasive$Preferred_scientific_name))

list_name<-list_name%>% #assign undesired species as mandate mismatch b/c they are not included in DFO's mandate
  mutate(category=case_when(Preferred_scientific_name %in% undesired_CABI$Preferred_scientific_name ~"Mandate Mismatch",
                            Preferred_scientific_name %in% not_NIS$Preferred_scientific_name ~"No Invasion History"))

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

#Keep track of species names and why they were filtering out

list_name<-list_name%>% #assign undesired species as mandate mismatch b/c they are not included in DFO's mandate
  mutate(category=case_when(Preferred_scientific_name %in% undesired_CABI$Preferred_scientific_name ~"Mandate Mismatch",
                            Preferred_scientific_name %in% not_NIS$Preferred_scientific_name ~"No Invasion History",
                            !(Preferred_scientific_name %in% tax_fix.itis$ScientificName) ~"Naming Confusion"))


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

#Keep track of species names and why they were filtering out

list_name<-list_name%>% #assign undesired species as mandate mismatch b/c they are not included in DFO's mandate
  mutate(category=case_when(Preferred_scientific_name %in% c(undesired_CABI$Preferred_scientific_name, check_cfia$ScientificName) ~"Mandate Mismatch",
                            Preferred_scientific_name %in% not_NIS$Preferred_scientific_name ~"No Invasion History",
                            !(Preferred_scientific_name %in% tax_fix.itis$ScientificName) ~"Naming Confusion"))

#remove species from our list that are included on the CFIA list
Uncompleted_sp<-Uncompleted_sp%>%
  filter (!ScientificName == (check_cfia$ScientificName))

#create a csv of the final list to be used below and also in the gbif_script.R
write.csv(Uncompleted_sp, "data/uncomplete_sp.csv")

#Step 5: Now go gbif_script.R and gather occurrence reports for each species then read back in the detections

###### import gbif species occurrence records (see gbif_script.R for details)

sp_records<-read.csv("data/batch1_gbif_cleaned.csv") #batch 1
#("data/clean_gbif_sp_data.csv") #batch 2


#Step 6:filter species established within assessment area (optional step!)
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
sp_to_filter_out<-read_xlsx("data/Freshwater_Terrestrial_Exclusion_List.xlsx")

terrestrial<-sp_to_filter_out%>%
  filter(F_T_freshwater_or_terrestrial=="T")

#only need this step for batch 2
sp_to_filter_out[sp_to_filter_out$F_T_freshwater_or_terrestrial=="F",]

sp_records<-read.csv("data/sp_occurrence_data_batch1.csv")%>%`1`
filter(ScientificName %in% sp_to_filter_out$ScientificName)  #Not necessary for batch 1

#Keep track of species names and why they were filtering out

list_name<-list_name%>% #assign undesired species as mandate mismatch b/c they are not included in DFO's mandate
  mutate(category=case_when(Preferred_scientific_name %in% c(undesired_CABI$Preferred_scientific_name, check_cfia$ScientificName, terrestrial$scientificname) ~"Mandate Mismatch",
                            Preferred_scientific_name %in% not_NIS$Preferred_scientific_name ~"No Invasion History",
                            !(Preferred_scientific_name %in% tax_fix.itis$ScientificName) ~"Naming Confusion"))

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

#Step 8: Go to Climate Match Script

#read in the species list from the climate match: baseline

combined_current<-read.csv("data/combined_species_assessments_batch_combined.csv") #read in climate match results for 1981-2010

MAR_current<-combined_current %>% #filter to only retain climate match in desired areas
  dplyr::filter(target %in% c("Nova Scotia", "Prince Edward Island", "New Brunswick") & score!= 0)

#read in the species list from the climate match: future
#the future climate match predictions (2011-2040) are too large to combine and then filter. So we will add each model individually, filter, and then average.

GFDL_ESM4<-read.csv("data/GFDL-ESM4.csv")%>% #filter to only retain climate match in desired areas
  dplyr::filter(target %in% c("Nova Scotia", "Prince Edward Island", "New Brunswick") & score!= 0)%>%
  tibble::add_column(batch="GFDL")

IPSL_CM6A_LR<-read.csv("data/IPSL_CM6A_LR.csv")%>% #filter to only retain climate match in desired areas
  dplyr::filter(target %in% c("Nova Scotia", "Prince Edward Island", "New Brunswick") & score!= 0)%>%
  tibble::add_column(batch="CM6A")

MPI_ESM1_2_HR<-read.csv("data/MPI-ESM1-2-HR.csv")%>% #filter to only retain climate match in desired areas
  dplyr::filter(target %in% c("Nova Scotia", "Prince Edward Island", "New Brunswick") & score!= 0)%>%
  tibble::add_column(batch="ESM1")

future_combined<-rbind(GFDL_ESM4, IPSL_CM6A_LR, MPI_ESM1_2_HR)%>% #combine all future models together
  dplyr::select(-X, -i)

future_combined<-future_combined%>%
  dplyr::group_by(species)%>% #group dataframe by species
  dplyr::mutate(avg_score=mean(score))%>% #take the average value over the 3 models
  dplyr::ungroup() #ungroup dataframe

MAR_current_edited<-MAR_current%>%
  dplyr::select(-"batch", -"X")%>%
  mutate("climate"=as.factor("1981-2010"))%>%
  dplyr::select("species", "target", "climate")%>%
  distinct()

future_combined_edited<-future_combined%>%
  mutate("climate"=as.factor("2011-2040"))%>%
  dplyr::select("species", "target", "climate")%>%
  distinct()

MAR_prediction<-rbind(MAR_current_edited, future_combined_edited)%>%
  mutate(value=as.factor("Y"))%>%
  pivot_wider(names_from = target, values_from=(value))

write_xlsx(MAR_prediction, "data/output_data/MAR_climatch_sp_list.xlsx")
#Step 9: Filter climate matching assessment for applicable assessment region (e.g. Nova Scotia)
#Filter the combined dataframe for NS and species with scores greater than zero (i.e. unlikley to survive here)
#only interested in species scoring higher than zero

#make a list of distinct species names regardless of climate matching scenario
distinct_MAR_list<-MAR_prediction%>% #look at distinct species names
  select(species)%>%
  distinct()

#Keep track of species names and why they were filtering out
#not all the non-aquatic species that should be removed due to mandate mismatch
Not_aquatic_1<-read.csv('data/NS_Screened_Species_List.csv')%>% #change batch name as needed
  filter(aquatic %in% c("N"))%>% #filter to only include aquatic species
  dplyr::select(species)

Not_aquatic_2<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(aquatic %in% c("N"))%>% #non aquatic species
  dplyr::select(species)

Not_aquatic<-rbind(Not_aquatic_1, Not_aquatic_2)

#remove non-aquatic species from the list of species with climate match in the Maritimes
new_distinct_MAR_list<-distinct_MAR_list%>% 
  anti_join(Not_aquatic, by='species')

#determine the number of unique species with climate match in NS
NS_sp<-MAR_prediction%>%
  filter(`Nova Scotia`=="Y")%>%
  filter(species %in% c(new_distinct_MAR_list$species))%>%
  select(species)%>%
  distinct()
#determine the number of unique species with climate match in NB
NB_sp<-MAR_prediction%>%
  filter(`New Brunswick`=="Y")%>%
  filter(species %in% c(new_distinct_MAR_list$species))%>%
  select(species)%>%
  distinct()
#determine the number of unique species with climate match in PEI
PEI_sp<-MAR_prediction%>%
  filter(`Prince Edward Island`=="Y")%>%
  filter(species %in% c(new_distinct_MAR_list$species))%>%
  select(species)%>%
  distinct()
#determine the number of species with climate matching under baseline vs future vs both climate scenarios
both_climates<-MAR_prediction%>%
  select(species, climate)%>%
  group_by(species)%>%
  summarise(n_climates=n())%>%
  filter(n_climates==2)%>%
  anti_join(Not_aquatic, by='species')%>%
  distinct()

baseline_clim<-MAR_prediction%>%
  select(species, climate)%>%
  filter(climate == '1981-2010')%>%
  anti_join(both_climates, by='species')%>%
  anti_join(Not_aquatic, by='species')%>%
  distinct()

future_clim<-MAR_prediction%>%
  select(species, climate)%>%
  filter(climate == '2011-2040')%>%
  anti_join(both_climates, by='species')%>%
  anti_join(Not_aquatic, by='species')%>%
  distinct()

#Remove the underscore in the species names to make it easier to compare with other lists
Not_aquatic$species<-gsub("_", " ", Not_aquatic$species)%>%
  anti_join(Not_aquatic, by='species')

#setup stepwise process to see first which species were not aquatic, then which were aquatic but also indigenous, and final which were aquatic, not indigenous, but are alreayd established

indigenous_1<-read.csv('data/NS_Screened_Species_List.csv')%>%
  filter(native %in% c("Y", "DD"))%>%
  dplyr::select(species)

indigenous_2<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(native %in% c("Y", "DD"))%>% 
  dplyr::select(species)

indigenous<-rbind(indigenous_1, indigenous_2)%>%
  anti_join(Not_aquatic, by="species")
indigenous$species<-gsub("_", " ", indigenous$species)

present_1<-read.csv('data/NS_Screened_Species_List.csv')%>%
  filter(established %in% c("Y", "DD"))%>%
  dplyr::select(species)

present_2<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(established %in% c("Y", "DD"))%>% 
  dplyr::select(species)

present<-rbind(present_1, present_2)%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(indigenous, by="species")
present$species<-gsub("_", " ", present$species)

climate_mismatch<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(climate_mismatch %in% c("Y"))%>% 
  dplyr::select(species)%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(indigenous, by="species")%>%
  anti_join(present, by="species")
climate_mismatch$species<-gsub("_", " ", climate_mismatch$species)

ISEIA<-read_excel("data/Species_screening_list_MAR_Gulf_for_pub.xlsx", sheet = "ISEIA Protocol")%>%
  as.data.frame()

in_take<-read_excel("data/Species_screening_list_MAR_Gulf_for_pub.xlsx", sheet = "Initial In-Take")%>%
  as.data.frame()

No_vector<-anti_join(in_take, ISEIA, by="species_latin")

Not_from_CABI<-in_take%>%
  filter(!(species_latin %in% CABI$Preferred_scientific_name))%>%
  rename(c("Preferred_scientific_name"="species_latin"))%>%
  select("Preferred_scientific_name")

list_name<-full_join(CABI, Not_from_CABI, by="Preferred_scientific_name")%>%
  filter(Preferred_scientific_name != "NA")

itis_names<-read.csv("data/tax_fix.short.csv")%>%
  filter(score <=0.8)

gbif<- c(list.files('data/species_data/all_species/'))%>%
  as.data.frame()
names(gbif)[names(gbif)=="."]<-"species"
gbif$species<-gsub("_", " ", gbif$species)
gbif$species<-gsub(".csv", "", gbif$species)

list_name<-list_name%>% #assign undesired species as mandate mismatch b/c they are not included in DFO's mandate
  mutate(category=dplyr::case_when(Preferred_scientific_name %in% ISEIA$species_latin~"Assessed",
                                   Preferred_scientific_name %in% c(undesired_CABI$Preferred_scientific_name, terrestrial$scientificname, Not_aquatic$species) ~"Mandate Mismatch",
                                   Preferred_scientific_name %in% not_NIS$Preferred_scientific_name ~"No Invasion History",
                                   Preferred_scientific_name %in% itis_names$submitted_name~"Naming Confusion",
                                   !(Preferred_scientific_name %in% gbif$species)~ "Lack of Geospatial Data",
                                   Preferred_scientific_name %in% indigenous$species~"Indigenous to Area",
                                   Preferred_scientific_name %in% present$species~"Present in Area",
                                   Preferred_scientific_name %in% No_vector$species_latin~"No Vector",
                                   !(Preferred_scientific_name %in% MAR_prediction$species)~"Climate Mismatch",
                                   Preferred_scientific_name %in% climate_mismatch$species~"Climate Mismatch"
                                   
  )
  
  )%>%
  dplyr::select("Preferred_scientific_name", "Organism_type", "Kingdom", "Phylum", "Class", "Order", "Family", "Family", "category")

write_xlsx(list_name, "data/output_data/working_list_filtration.xlsx")

Table_filtered_list<-list_name%>%
  dplyr::select("Preferred_scientific_name", "Phylum", "category" )%>%
  group_by(Phylum, category)%>%
  summarise(n_species=dplyr:::n_distinct(Preferred_scientific_name))%>%
  tidyr::pivot_wider(names_from= Phylum, values_from=n_species)

write_xlsx(Table_filtered_list, "data/output_data/Table_filtered_list_by_phylum.xlsx")

#visualize the filteration steps
library(ggplot2)
library(viridis)
library(forcats)

filtered<-ggplot(list_name, aes(x=fct_rev(fct_infreq(category))))+
  ggplot2::geom_text(stat = 'count', aes(label=..count..), hjust = -0.05, nudge_y = -.5, col='black', fontface = "bold")+
  ggplot2::geom_bar(aes(fill=Phylum))+
  scale_fill_viridis_d(option = 'viridis', na.value='grey')+
  ggplot2::labs(y="Number of Species", x="Filters")+
  theme(
    panel.background = element_rect(fill='white'),
    panel.grid.major = element_line(colour = "grey"),
    axis.line = element_line(colour = "grey"),
    axis.text = element_text(size=22),
    axis.title = element_text(size=24))+
  scale_y_discrete(expand = expansion(mult = c(0, 0.05)))+
  coord_flip()

ggsave(filename="plots/sp_filtered_plot.png", plot=filtered, unit="cm",width=30,height=21,  dpi=600, device = png)


#compare primary assessment results with climate matching from other Ps&Ts
#Provide summary lists for other areas of Canada

AB<-read.csv("data/AB_species_list.csv")%>%
  mutate(species = str_replace(species, "_", " "))
BC<-read.csv("data/BC_species_list.csv")%>%
  mutate(species = str_replace(species, "_", " "))
SK<-read.csv("data/SK_species_list.csv")%>%
  mutate(species = str_replace(species, "_", " "))
MB<-read.csv("data/MB_species_list.csv")%>%
  mutate(species = str_replace(species, "_", " "))
ON<-read.csv("data/ON_species_list.csv")%>%
  mutate(species = str_replace(species, "_", " "))
QC<-read.csv("data/QC_species_list.csv")%>%
  mutate(species = str_replace(species, "_", " "))
NFL<-read.csv("data/NFL_species_list.csv")%>%
  mutate(species = str_replace(species, "_", " "))

#We filtered out a bunch of non-aquatic species when looking at the Maritimes (mostly plants)
Not_aquatic<-read.csv('data/NS_Screened_Species_List.csv')%>% #change batch name as needed
  filter(aquatic %in% c("N"))%>% #filter to only include aquatic species
  dplyr::select(species)%>%
  mutate(species = str_replace(species, "_", " "))

Not_aquatic_2<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(aquatic %in% c("N"))%>% #non aquatic species
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)

check_n<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(native %in% c("Y", "DD"))%>% #non aquatic species
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)
check_e<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(established %in% c("Y", "DD"))%>% #non aquatic species
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)
check_c<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(climate_mismatch %in% c("Y", "DD"))%>% #non aquatic species
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)

check_2<-read.csv('data/NS_Screened_Species_List.csv')%>%
  filter(aquatic %in% c("N"))%>%
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)

check_2_n<-read.csv('data/NS_Screened_Species_List.csv')%>%
  filter(established %in% c("Y", "DD"))%>%
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)

cehck_2_e<-read.csv('data/NS_Screened_Species_List.csv')%>%
  filter(established %in% c("Y", "DD"))%>%
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)

MAR<-read_xlsx("data/output_data/MAR_climatch_sp_list.xlsx")
MAR$species<-gsub("_", " ", MAR$species)

MAR<-MAR%>% 
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")

MAr_now<-MAR%>%
  filter(climate=="1981-2010")%>%
  dplyr::select(-"Nova Scotia", -"Prince Edward Island", -"New Brunswick")%>%
  distinct()

Mar_fut<-MAR%>%
  filter(climate=="2011-2040")%>%
  dplyr::select(-"Nova Scotia", -"Prince Edward Island", -"New Brunswick")%>%
  distinct()

MAR_both<-left_join(MAr_now, Mar_fut, by="species")%>%
  filter(climate.y != "NA")

MAR_NS<-MAR%>%
  dplyr::select(-climate)%>%
  filter(`Nova Scotia` == "Y")%>%
  distinct()

MAR_NB<-MAR%>%
  dplyr::select(-climate)%>%
  filter(`New Brunswick` == "Y")%>%
  distinct()

MAR_PEI<-MAR%>%
  dplyr::select(-climate)%>%
  filter(`Prince Edward Island` == "Y")%>%
  distinct()


AB_to_do<-AB%>%
  anti_join(in_take, by=c("species"="species_latin"))%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")%>%
  dplyr::select(-X)%>%
  mutate(prov=as.factor("Alberta"))

BC_to_do<-BC%>%
  anti_join(in_take, by=c("species"="species_latin"))%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")%>%
  dplyr::select(-X)%>%
  mutate(prov=as.factor("British Columbia"))

SK_to_do<-SK%>%
  anti_join(in_take, by=c("species"="species_latin"))%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")%>%
  dplyr::select(-X)%>%
  mutate(prov=as.factor("Saskatchewan"))

MB_to_do<-MB%>%
  anti_join(in_take, by=c("species"="species_latin"))%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")%>%
  dplyr::select(-X)%>%
  mutate(prov=as.factor("Manitoba"))

ON_to_do<-ON%>%
  anti_join(in_take, by=c("species"="species_latin"))%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")%>%
  dplyr::select(-X)%>%
  mutate(prov=as.factor("Ontario"))

QC_to_do<-QC%>%
  anti_join(in_take, by=c("species"="species_latin"))%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")%>%
  dplyr::select(-X)%>%
  mutate(prov=as.factor("Quebec"))

NFL_to_do<-NFL%>%
  anti_join(in_take, by=c("species"="species_latin"))%>%
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")%>%
  dplyr::select(-X)%>%
  mutate(prov=as.factor("Newfoundland & Labrador"))

df<-rbind(AB_to_do, BC_to_do, SK_to_do, MB_to_do, ON_to_do, QC_to_do, NFL_to_do)%>%
  mutate(value=as.numeric("1"))%>%
  pivot_wider(names_from = prov, values_from=value)

write_xlsx(df, "data/output_data/provincial_climatch_sp_list.xlsx")
