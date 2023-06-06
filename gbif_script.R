
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

#prep buffsea object used to identify coordinates on land
data("buffland")

##download occurrence data 

#extract from csv list of species (https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/)
gbif_taxon_keys_<-Uncompleted_sp%>%
  head(50)%>%
  pull("ScientificName")%>%
  name_backbone_checklist()  %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) # get the gbif taxonkeys
 
# user <- "cpratt" # your gbif.org username 
# pwd <- "#XX4uJxKCwo9" # your gbif.org password
# email <- "c.pratt@dal.ca" # your email 
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
occ_download_wait('0012812-230530130749713')

#create object from downloaded data
# assign(paste0("gbif_obs_data_", listname), occ_download_get(get(paste0("obs_downloads_", listname)), overwrite = T) %>%
#   occ_download_import(),
#   envir=globalenv())

sp_occ<- occ_download_get(key="0012812-230530130749713", overwrite=TRUE) %>%
 occ_download_import()




######clean_gbif_occ----
#clean data (https://data-blog.gbif.org/post/gbif-filtering-guide/)
clean_gbif_occ <- sp_occ%>%
  as.data.frame()%>%
  setNames(tolower(names(.))) %>% # set lowercase column names to work with CoordinateCleaner
  filter(coordinateprecision < 0.01 | is.na(coordinateprecision)) %>% #remove high-uncertainty observations if uncertainty value is present
  filter(coordinateuncertaintyinmeters < 10000 | is.na(coordinateuncertaintyinmeters)) %>% #remove high-uncertainty observations if uncertainty value is present
  filter(!coordinateuncertaintyinmeters %in% c(301,3036,999,9999)) %>%
  filter(!decimallatitude == 0 | !decimallongitude == 0) %>% #remove observations with 0,0 coordinates
  distinct(decimallongitude,decimallatitude,specieskey,datasetkey, .keep_all = TRUE) %>% #remove duplicate records
  filter(species %in% (Uncompleted_sp$ScientificName))%>% #filter for only species in original species list (some taxon keys return higher-order taxa that bring in additional species)
  cc_sea(lon = "decimallongitude" ,lat = "decimallatitude" )%>%
  mutate(eventdate = as.Date(eventdate)) %>% #change date to simpler format (downloads as PosiXct)
  rename("ScientificName"=species, "lon" = decimallongitude, "lat" = decimallatitude, "date" = eventdate) %>% 
  mutate(obs_source = "GBIF")

#write to file
write_csv(clean_gbif_occ, file = "data/clean_gbif_sp_data.csv")


