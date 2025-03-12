library (readxl)
library(taxize)
library(magrittr)
library(taxadb)
library(writexl)
library(wesanderson)
library (tidyverse)

#######First Comparison: our study (Kingsbury et al.) working list comparison with three other species lists
#First our study list names
CABI<-read_excel("data/CABI_Horizon Scanning_pulled 27 Apr 2023.xlsx")

in_take<-read_excel("data/Species_screening_list_MAR_Gulf_for_pub.xlsx", sheet = "Initial In-Take")%>%
  as.data.frame()

Not_from_CABI<-in_take%>%
  filter(!(species_latin %in% CABI$Preferred_scientific_name))%>%
  rename(c("Preferred_scientific_name"="species_latin"))%>%
  select("Preferred_scientific_name")

list_name<-full_join(CABI, Not_from_CABI, by="Preferred_scientific_name")%>%
  filter(Preferred_scientific_name != "NA")%>%
  select("Preferred_scientific_name")%>%
  mutate(list=as.factor("Kingsbury et al."))

#Now read in species list from GLANSIS
GLANSIS<-read.csv("COmparison_data/GLANSIS_SpeciesList_21May2024.csv")%>%
  filter(Group %in% c("Coelenterates-Hydrozoans", "Crustaceans-Amphipods", "Crustaceans-Cladocerans", "Crustaceans-Crayfish",
                      "Fishes", "Mollusks-Bivalves", "Mollusks-Gastropods", "Plants"))%>% #focus list on groups that fall within the scope of this study
  select("Scientific.Name")%>%
  mutate(list=as.factor("Glansis"))
#Now read in and filter GRIIS list

GRIIS<-read.csv("COmparison_data/GRIIS_Canada_21May2024.csv")%>%
  filter(accepted_name.habitat %in% c("[\"freshwater\"]", "[\"freshwater\",\"brackish\",\"marine\"]", "[\"terrestrial\",\"freshwater\"]"))%>% #focus on freshwater species
  filter(is_invasive=="invasive")%>% #look for species with known invasion history
  filter(!accepted_name.class %in% c("Arachnida", "NA","Insecta", "Amphibia", "Aves", "Hirudinoidea", #remove any species who's class is outside the scope of work
                                     "Secernentea", "Ulvophyceae", "Reptilia", "Phaeophyceae", "Hydrozoa",
                                     "Cestoda", "Ascidiacea" , "Trematoda", "Copepoda", "Adenophorea",
                                     "Myxosporea Myxobolus cerebralis", "Diplopoda", "Ophiuroidea",
                                     "Pinopsida", "Cephalopoda", "Chlorophyceae"))%>%
  select("accepted_name.species")%>%
  mutate(list=as.factor("GRIIS"))

#Now read in and filter list from Briski et al. (2023)
Briski<-read_xlsx("Comparison_data/Briski_et_al._2023_raw_sp_list.xlsx")%>%
  filter(!Class %in% c("Arachnida", "NA","Insecta", "Amphibia", "Aves", "Hirudinoidea", #remove any species who's class is outside the scope of work
                                     "Secernentea", "Ulvophyceae", "Reptilia", "Phaeophyceae", "Hydrozoa",
                                     "Cestoda", "Ascidiacea" , "Trematoda", "Copepoda", "Adenophorea",
                                     "Myxosporea Myxobolus cerebralis", "Diplopoda", "Ophiuroidea",
                                     "Pinopsida", "Cephalopoda", "Chlorophyceae"))%>%
  filter(Habitat %in% c("FRESHWATER", "FRESHWATER|MARINE", "FRESHWATER|TERRESTRIAL", "FRESHWATER|TERRESTRIAL|HOST"))%>% #focus on freshwater species
  select("ScientificName")%>%
  mutate(list=as.factor("Briski et al.")) 


#check that the list of species names used in our study (Kingsbury et al. 2025) adaquately reflects other species lists
check<-list_name%>%
  anti_join(GLANSIS, by=c("Preferred_scientific_name"="Scientific.Name"))%>%
  anti_join(GRIIS, by=c("Preferred_scientific_name"="accepted_name.species"))%>%
  anti_join(Briski, by=c("Preferred_scientific_name"="ScientificName"))

#now look to see how many lists appeared in check. If everything says "Kingsbury et al." then all the other species lists were contianed within our study
unique(check$list)

#Result was:
# [1] Kingsbury et al.
# Levels: Kingsbury et al.

######## Second Compression: Our study's inter-assessor variability

inter_assess<-read_xlsx("Comparison_data/SS5-Interassessor Variability.xlsx", sheet="Comparison Overview")
  #pivot_longer( cols=starts_with("Assessor"), names_to = "Assessor", names_prefix = "Assessor", values_to = "rank")%>%
 # rename("score"="rank")

assessor_plot<- ggplot(inter_assess)+
  geom_point(aes(x=`Assessor 1`, y=`Assessor 2`), alpha=0.2)+
  geom_smooth(aes(x=`Assessor 1`, y=`Assessor 2`, colour="A.1-A.2"), method = lm, size=0.75, se=FALSE)+
  geom_point(aes(x=`Assessor 1`, y=`Assessor 3`), alpha=0.2)+
  geom_smooth(aes(x=`Assessor 1`, y=`Assessor 3`, colour="A.1-A.3"), method = lm, size=0.75, se=FALSE)+
  geom_point(aes(x=`Assessor 1`, y=`Assessor 4`), alpha=0.2)+
  geom_smooth(aes(x=`Assessor 1`, y=`Assessor 4`, colour="A.1-A.4"), method = lm, size=0.75, se=FALSE)+
  geom_point(aes(x=`Assessor 2`, y=`Assessor 3`), alpha=0.2)+
  geom_smooth(aes(x=`Assessor 2`, y=`Assessor 3`, colour="A.2-A.3"), method = lm, size=0.75, se=FALSE)+
  geom_point(aes(x=`Assessor 2`, y=`Assessor 4`), alpha=0.2)+
  geom_smooth(aes(x=`Assessor 2`, y=`Assessor 4`, colour="A.2-A.4"), method = lm, size=0.75, se=FALSE)+
  geom_point(aes(x=`Assessor 3`, y=`Assessor 4`), alpha=0.2)+
  geom_smooth(aes(x=`Assessor 3`, y=`Assessor 4`, colour="A.3-A.4"), method = lm, size=0.75, se=FALSE)+
  geom_abline(intercept = 0,slope = 1, size=1)+
  ylab("") +
  xlab("") +
  labs(colour = "Assessor combination") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.4, "cm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5, size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", fill = "white"));assessor_plot

ggsave("plots/Inter-Assessor_Variability.png")


######## Third Comparison: ISEIA scores of species that were assessed but did not have climate match in the Maritimes
MAR_clim<-read_xlsx("data/output_data/MAR_climatch_sp_list.xlsx")
MAR_clim$species<-gsub("_", " ", MAR_clim$species) #the species names in the climate match df have an _. Therefore, need to modify the name to match the names in ISEIA.

#read in manual removal of species that are non-aquatic
Not_aquatic<-read.csv('data/NS_Screened_Species_List.csv')%>% #change batch name as needed
  filter(aquatic %in% c("N", "DD"))%>% #filter to only include aquatic species
  dplyr::select(species)%>%
  mutate(species = str_replace(species, "_", " "))

Not_aquatic_2<-read.csv("data/NS_2011_2040_list_to_screen_corrected.csv")%>%
  filter(aquatic %in% c("N", "DD"))%>% #non aquatic species
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)

#also remove native species and species with occurrence reocrds in the Maritimes

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

check_2_n<-read.csv('data/NS_Screened_Species_List.csv')%>%
  filter(established %in% c("Y", "DD"))%>%
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)

check_2_e<-read.csv('data/NS_Screened_Species_List.csv')%>%
  filter(established %in% c("Y", "DD"))%>%
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)

MAR_clim_corrected<-MAR_clim%>% 
  anti_join(Not_aquatic, by="species")%>%
  anti_join(Not_aquatic_2, by="species")%>%
  anti_join(check_n, by="species")%>%
  anti_join(check_e, by="species")%>%
  anti_join(check_2_n, by="species")%>%
  anti_join(check_2_e, by="species")%>%
  anti_join(check_c, by="species")

#Now bring in the high risk species from the watchlist assessments
ISEIA<-read_excel("data/Species_screening_list_MAR_Gulf_for_pub.xlsx", sheet = "ISEIA Protocol")%>%
  as.data.frame()

#first compare names from the initial in-take (i.e. working list) to those from the climate match
working_l_unmatched<-in_take%>%
  anti_join(MAR_clim_corrected, by=c("species_latin"="species"))
#Result: 129 species with no climate match were included in working list

#now compare the list of species full assessed (i.e. RLRA, which was ISEIA) to those with climate match
assessed_l_unmatched<-ISEIA%>%
  anti_join(MAR_clim_corrected, by=c("species_latin"="species"))
#Result: 70 species with no predicted climate match were assessed via RLRA

#now look at the vice-versa fr comparing the climate matched species and the 'in-take' tab. This is to ensure that no species were found with climate match and then missed in follow-up assessments.
clim_match_not_assessed<-MAR_clim_corrected%>% 
  anti_join(in_take, by=c("species"="species_latin")) 
#result was clim_match_not_assessed= 0 obs.

#lets plot the scores of each species for Qu.1 of RLRA which deals with pathways of introduction and potential further spread of NIS
#also consider plotting Qu.2 which addressed potential of NIS to establish in high value habitat

QU.1<-assessed_l_unmatched%>%
  select("species_latin", "Qu.1")%>%
  rename("score"="Qu.1")%>%
  mutate(question=as.factor("Qu.1"))

QU.2<-assessed_l_unmatched%>%
  select("species_latin", "Qu.2")%>%
  rename("score"="Qu.2")%>%
  mutate(question=as.factor("Qu.2"))

df<-rbind(QU.1, QU.2)

ggplot(df, aes(x=score, fill=question))+ 
  geom_density(alpha = 0.2)+
  theme_light()

ggsave("plots/RLRA_assessed_no_climatch.png")

#Now compare to see is the species that did not have climate match still scored high via RLRA
ggplot(assessed_l_unmatched, aes(x=`Global environmental risk`))+
  geom_histogram(fill='lightblue', binwidth=1, col='darkgrey')+
  geom_vline(xintercept=c(3, 7), linetype=2, col='black')+
  scale_y_continuous(limits=c(0, 10.5), breaks=c(0,2,4,6,8,10))+
  scale_x_continuous(limits=c(0, 12), breaks=c(0,2,4,6,8,10,12))+
  labs(y="Number of species", x="Global environmental risk score")+
  theme_light()

ggsave("plots/RLRA_global_scores_no_climatch.png")


########fourth comparison: comapre list from this study (Kingsbury et al.) to A. Moreau-Johnson (unpub.)

Moreau_Johnson<-read.csv("data/combined_species_assessments_batchAntoine.csv")%>% # 21 species were identified as Moreau-Johnson (unpublished) but we will filter to only retain climate match in desired areas
  dplyr::filter(target %in% c("Nova Scotia", "Prince Edward Island", "New Brunswick") & score!= 0)%>%
  mutate(species = str_replace(species, "_", " "))%>%
  dplyr::select(species)%>%
  distinct()

#Result: single species (Andropogon glomeratus) was identified by Moreau-Johnson (unpub.). 
#This species would have been filtered out from Kingsbury et al. watchlist because it is native to much of continental America (https://www.missouribotanicalgarden.org/PlantFinder/PlantFinderDetails.aspx?taxonid=285172)
#and is widespread throughout Canada, especially southern Canada, including all Maritime provinces (https://www.inaturalist.org/observations?place_id=6712&subview=map)

