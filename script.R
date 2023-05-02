library (readxl)
library(taxize)
library(magrittr)
library(taxadb)
library (tidyverse)

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

#Get species distribution data with the ClimatchR package


#Use Coordinate Cleaner to filter out marine species
# devtools::install_github("ropensci/CoordinateCleaner")
# library(CoordinateCleaner)

#Conduct climate match analysis with ClimatchR package



# #attempt to find symnoms with taxadb
# td_create("itis",overwrite=FALSE)
# 
# allnames<-read.csv(file="data/tax_fix.short.csv", header=TRUE)
# 
# 
# 
# #get  IDS for each scientific name
# syn1<-allnames %>%
#   select(matched_name2) %>%
#   distinct()%>%
#   mutate(ID=get_ids(matched_name2,"itis"))
# 
# 
# #Deal with NAs (one name corresponds to more than 1 ITIS code) (~10k names)
# 
# syn1_NA<-syn1%>%
#   filter(!is.na(ID))%>%
#   as.data.frame()
# 
# #Use Taxize to search for synonyms
# synlist1 <- synonyms(c(syn1_NA$matched_name2), db="itis", accepted=T)
# 
# synlist1<-synlist1%>%
#   as.matrix()
# 
# #   as.data.frame(syn1$matched_name2[is.na(syn1$ID)])
# # colnames(syn1_NA)<-c("name")
# # 
# # NA_IDS<-NULL
# # for(i in unique(syn1_NA$name)){
# #   tmp<-as.data.frame(filter_name(i, 'itis')[5])
# #   tmp$name<-paste0(i)
# #   NA_IDS<-rbind(NA_IDS,tmp)
# # }
# # 
# # #join with originial names
# # colnames(syn1)<-c("name","ID")
# # IDS<-left_join(syn1,NA_IDS,by="name") #I think its a left join double check this
# 
# 
# #extract just the unique IDs
# IDS<-syn1_NA%>%
#   distinct(ID)
# # IDS<-data.frame(ID=c(IDS[,"ID"],IDS[,"acceptedNameUsageID"]))
# # IDS<-as.data.frame(unique(IDS$ID))
# # IDS<-as.data.frame(IDS[-is.na(IDS)])
# # colnames(IDS)<-"ID"
# 
# #extract all names with synonyms in ITIS that are at the species level [literally all of them]
# #set query
# ITIS<-taxa_tbl("itis") %>%
#   select(scientificName,taxonRank,acceptedNameUsageID,taxonomicStatus) %>%
#   filter(taxonRank == "species")
# 
# #see query
# ITIS %>% show_query()
# #retrieve results
# ITIS_names<-ITIS %>% collect()
# 
# #filter to only those that match ITIS codes for all my species
# ITIS_names<-ITIS_names %>%
#   filter(acceptedNameUsageID %in% IDS$ID)