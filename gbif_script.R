
library(rgbif)
library(CoordinateCleaner)
library(tidyverse)

####GBIF----

# #edit r environ with gbif credentials (https://docs.ropensci.org/rgbif/articles/gbif_credentials.html)
# usethis::edit_r_environ()



#assign gbif credentials to objects
user <- "skingsbury" # your gbif.org username 
pwd <- "Benjamin231" # your gbif.org password
email <- "Sarah.Kingsbury@dfo-mpo.gc.ca" # your email 

#read in species list 
Uncompleted_sp<-read.csv("data/uncomplete_sp.csv") #batch 2

#read in batch 1 species
batch1<-read_xlsx("data/Species_screening_list.xlsx", sheet=1)%>%
  rename(ScientificName=species_latin)

#check for ScientificName to ensure there aren't any unexpected names
unique(batch1$ScientificName)

#clean the files to fix any odd names
batch1<-batch1%>%
 # filter(!ScientificName %in% c("Any species of the family Channidae", "All species of the genera Rhodeus and Tanakia"))%>%
  mutate(case_when(ScientificName=="Sander volgensis or Sander marinus" ~ "Sander volgensis",
                   ScientificName=="Morone mississippiensis and hybrids"~"Morone mississippiensis"))

batch1 <-batch1%>% #having issues with my comuter, so the filter function above didn't work. Had to manualy slice the applicable rows. 
  slice(-c(6,13))


##download occurrence data 

#extract from csv list of species (https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/)
gbif_taxon_keys_<-batch1%>% #switch this line dependent on if you run the assessment on batch 1 sp. or batch 2 sp. 
 # head(50)%>%
  pull("ScientificName")%>%
  name_backbone_checklist()  %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) # get the gbif taxonkeys
 
#download occurrence records
occ_download(pred_in("taxonKey",gbif_taxon_keys_),
                                     pred("hasGeospatialIssue", FALSE), #no geospatial issues
                                     pred("hasCoordinate", TRUE), #only observations with coordinates
                                     pred("occurrenceStatus","PRESENT"), #only presence records
                                     pred_gte("year", 1990),#only occurrence records since 1990
                                     pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN", "HUMAN_OBSERVATION", "PRESERVED SPECIMEN", "OBSERVATION"
                                                                        ))), #no specimens
                                     user = user,
                                     pwd = pwd,
                                     email = email,
                                     format = "SIMPLE_CSV")

#Check download status
#Note: The number in the '' below is what is provided by GBIF. It needs to be updated with each pull.
occ_download_wait('0132563-230530130749713')

#create object from downloaded data
#the key will need to be changed for each pull as each one is unique. 

sp_occ<- occ_download_get(key="0132563-230530130749713", overwrite=TRUE) %>% #Note: the key number changes with each pull from gbif. Therefore, need to change the number. 
 occ_download_import()

######clean_gbif_occ----
#clean data (https://data-blog.gbif.org/post/gbif-filtering-guide/)
clean_gbif_occ <- sp_occ%>%
  as.data.frame()%>%
  setNames(tolower(names(.))) %>% # set lowercase column names to work with CoordinateCleaner
  filter(coordinateprecision < 0.01 | is.na(coordinateprecision)) %>% #remove high-uncertainty observations if uncertainty value is present
  filter(coordinateuncertaintyinmeters < 10000 | is.na(coordinateuncertaintyinmeters)) %>% #remove high-uncertainty observations if uncertainty value is present
  filter(!coordinateuncertaintyinmeters %in% c(301,3036,999,9999))
clean_gbif_occ <-clean_gbif_occ %>%
  filter(!decimallatitude  == 0 | !decimallongitude == 0) %>% #remove observations with 0,0 coordinates
  distinct(decimallongitude,decimallatitude,specieskey,datasetkey, .keep_all = TRUE) %>% #remove duplicate records
  filter(species %in% (batch1$ScientificName))%>% #filter for only species in original species list (some taxon keys return higher-order taxa that bring in additional species)
  cc_sea(lon = "decimallongitude" ,lat = "decimallatitude" )%>%
  cc_inst(lon = "decimallongitude" ,lat = "decimallatitude", buffer=2000, value='clean', verbose = TRUE )%>%
  mutate(eventdate = as.Date(eventdate)) %>% #change date to simpler format (downloads as PosiXct)
  rename("ScientificName"=species, "lon" = decimallongitude, "lat" = decimallatitude, "date" = eventdate) %>% 
  mutate(obs_source = "GBIF")

#write to file
write_csv(clean_gbif_occ, file = "data/batch1_gbif_cleaned.csv") #change file name to reflect batch number


