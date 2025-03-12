library (readxl)
library(gridExtra)
library(ggspatial)
library(scales)
library(ggpattern)
library(ggrepel)
require(rvest)
library(data.table)
library(janitor)
library(rnaturalearth)
library(sf)
library(patchwork)
library(ggthemes)
library(writexl)
library(ggstance)
library (tidyverse)

#read in species RLRA excel doc

ISEIA<-read_excel("data/Species_screening_list_MAR_Gulf_for_pub.xlsx", sheet = "ISEIA Protocol")%>%
  as.data.frame()

in_take<-read_excel("data/Species_screening_list_MAR_Gulf_for_pub.xlsx", sheet = "Initial In-Take")%>%
  as.data.frame()

#combine the in-take list with the ISEIA species assessments
combined_list<-left_join(ISEIA, in_take, by="species_latin" )%>%
  filter(`Global environmental risk` != "NA")

write.csv(combined_list, "data/RLRA_Assessed_List.csv")
#box and whisker plot
overview_plot<-ggplot(combined_list, aes(Family, `Global environmental risk`))+
  geom_boxplot()+
  geom_jitter(width=0.2)+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme_bw()+
  coord_flip()

overview_plot_sliced<-overview_plot + facet_wrap(~taxa, scales = "free_y", ncol = 1)

ggsave(filename="plots/all_sp_by_taxa_n_family.png", plot=overview_plot_sliced, unit="cm",width=18.2,height=23.7,  dpi=600, device = png)

overview_by_taxa<-ggplot(combined_list, aes(taxa, `Global environmental risk`))+
  geom_boxplot(aes(fill=taxa), lwd=0.6)+
  #geom_point(alpha=0.1, size=5)+
  stat_boxplot(geom="errorbar", width=0.2, size=1)+
  scale_fill_brewer(palette = 'RdYlBu', guide="none")+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))+
  #geom_jitter(width=0.2)+ 
  theme(
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        axis.text = element_text(size=22),
        legend.text=element_text(size=22),
        axis.title = element_text(size=24),
        legend.title = element_text(size=24)
        #axis.title.y = element_blank()
        )+
  coord_flip()

ggsave(filename="plots/all_sp_by_taxa_boxPlot.png", plot=overview_by_taxa, unit="cm",width=22,height=24,  dpi=600, device = png)

# overview_bar<-ggplot(combined_list, aes(`Global environmental risk`, fill=taxa, pattern=taxa))+
#   geom_bar_pattern(colour="black", 
#                    pattern_fill="black")+
#   scale_fill_brewer(palette = 'RdYlBu')+
#   scale_pattern_manual()
#   theme_bw()

#high risk species with global score 7-12
  high_risk<-combined_list %>%
    filter (`Global environmental risk` %in% c(7,8,9,10,11,12))%>%
    as.data.frame()

  detach(package:plyr)    
  library(dplyr)
  
sum_high_risk<-high_risk%>%
  dplyr::select("species_latin", "Global environmental risk")
  
write_xlsx(sum_high_risk,"data/output_data/summary_of_scores_by_family.xlsx")

#box and whisker plot for high risk species (scores 7-12)

high_risk_taxa<-unique(high_risk$taxa)
high_risk_sp_list<-list()
  
for (taxa_ in high_risk_taxa){
  high_risk_sp_list[[taxa_]] = ggplot(high_risk%>%filter(taxa==taxa_), aes(Family, `Global environmental risk`))+
    geom_boxplot(fill='lightgrey')+
    ggtitle(paste0(taxa_))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.text.x=element_text(angle=90))
   
  print(high_risk_sp_list[[taxa_]])
}

#count of species active in each vector
vectors<-combined_list%>%
  dplyr::select("species_latin","taxa", "pet_aquarium", "Aquaculture", "rec_fish", "biofouler", "Bilge Water", "water_garden" )%>%
  mutate(pet_aquarium=as.integer(as.character(pet_aquarium)=="Yes"),
         Aquaculture=as.integer(as.character(Aquaculture)=="Yes"),
         rec_fish=as.integer(as.character(rec_fish)=="Yes"),
         biofouler=as.integer(as.character(biofouler)=="Yes"),
         `Bilge Water`=as.integer(as.character(`Bilge Water`)=="Yes"),
         water_garden=as.integer(as.character(water_garden)=="Yes"))%>%
  group_by(taxa)%>%
  summarise(n_aquaria=sum(pet_aquarium),
            n_aquaculture=sum(Aquaculture),
            n_recFish=sum(rec_fish),
            n_boifouling=sum(biofouler),
            n_bilge=sum(`Bilge Water`),
            n_waterGarden=sum(water_garden))%>%
  pivot_longer(!taxa, names_to = "vector", values_to = "count")%>%
  as.data.frame()

High_risk_vectors<-high_risk%>%
  dplyr::select("species_latin","taxa", "pet_aquarium", "Aquaculture", "rec_fish", "biofouler", "Bilge Water", "water_garden" )%>%
  mutate(pet_aquarium=as.integer(as.character(pet_aquarium)=="Yes"),
         Aquaculture=as.integer(as.character(Aquaculture)=="Yes"),
         rec_fish=as.integer(as.character(rec_fish)=="Yes"),
         biofouler=as.integer(as.character(biofouler)=="Yes"),
         `Bilge Water`=as.integer(as.character(`Bilge Water`)=="Yes"),
         water_garden=as.integer(as.character(water_garden)=="Yes"))%>%
  group_by(taxa)%>%
  summarise(n_aquaria=sum(pet_aquarium),
            n_aquaculture=sum(Aquaculture),
            n_recFish=sum(rec_fish),
            n_boifouling=sum(biofouler),
            n_bilge=sum(`Bilge Water`),
            n_waterGarden=sum(water_garden))%>%
  pivot_longer(!taxa, names_to = "vector", values_to = "count")%>%
  as.data.frame()

vectors_bar<-ggplot(vectors, aes(x=vector, y=count, fill=taxa))+
  geom_bar(stat='identity')+
  scale_fill_brewer(palette = 'RdYlBu')+
  scale_x_discrete(labels=c("aquaculture", " aquarium", "bilge\n water", "boifouling", " recreational\n fishing", "water\n garden"))+
  #ggtitle("Summary of Number of Species per vector")+
  labs(y="Count", x="Vectors")+
  geom_text(data=subset(vectors, count !=0), aes(label= count), position=position_stack(vjust=0.5), size=5)+
  theme(
    #axis.text.y=element_blank(),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        axis.text = element_text(size=22),
        legend.text=element_text(size=22),
        axis.title = element_text(size=24),
        legend.title = element_text(size=24),
        plot.title = element_text(size=24),
        axis.text.x = element_text(angle=45, vjust=0.75))

high_risk_vectors_bar<-ggplot(High_risk_vectors, aes(x=vector, y=count, fill=taxa))+
  geom_bar(stat='identity')+
  scale_fill_brewer(palette = 'RdYlBu')+
  scale_x_discrete(labels=c("aquaculture", " aquarium", "bilge\n water", "boifouling", " recreational\n fishing", "water\n garden"))+
  #ggtitle("Summary of Number of Species per vector")+
  labs(y="Count", x="Vectors")+
  geom_text(data=subset(High_risk_vectors, count !=0), aes(label= count), position=position_stack(vjust=0.5))+
  theme(
    #axis.text.y=element_blank(),
    panel.background = element_rect(fill='white'),
    panel.grid.major = element_line(colour = "grey"),
    axis.line = element_line(colour = "grey"),
    axis.text = element_text(size=22),
    legend.text=element_text(size=22),
    axis.title = element_text(size=24),
    legend.title = element_text(size=24),
    plot.title = element_text(size=24),
    axis.text.x = element_text(angle=45, vjust=0.75))

ggsave(filename="plots/vectors_bar.png", plot=vectors_bar, unit="cm",width=30,height=25,  dpi=600, device = png)
ggsave(filename="plots/vectors_of_high_risk_sp.png", plot=high_risk_vectors_bar, unit="cm",width=30,height=25,  dpi=600, device = png)

#AISR Lists

response<-read_html("https://laws-lois.justice.gc.ca/eng/regulations/sor-2015-121/FullText.html")
tables<-response %>% html_table(header = TRUE)

table_one<-tables[[1]]%>%
  row_to_names(row_number=1)%>%
  dplyr::select("Scientific Name")

table_two<-tables[[2]]%>%
  row_to_names(row_number=1)%>%
  dplyr::select("Scientific Name")

AISR_listed<-rbind(table_one,table_two)

#compare AISR list with high risk species list
high_risk_listed<-high_risk%>%
  filter(species_latin %in% AISR_listed$`Scientific Name`)%>%
  mutate(Already_listed="1")%>%
  dplyr::select("species_latin", "Already_listed")

high_risk<-left_join(high_risk, high_risk_listed, by="species_latin")
high_risk$Already_listed[is.na(high_risk$Already_listed)]<-0

listed_plot<-ggplot(high_risk, aes(Already_listed, fill=taxa))+
  geom_bar()+
  scale_fill_brewer(palette = 'RdYlBu')+
  #scale_fill_manual(values=c("#D73027", "#FDAE61","#FEE090"))+
  scale_x_discrete(labels=c("No", "Yes"))+
  #ggtitle("Number of High-Risk Species Already Listed in \nCanada's Aquatic Invasive Species Regulations")+
  geom_text(stat = 'count', aes(label=..count..), position=position_stack(vjust=0.5))+
  labs(y="Count", x="Listing Status")+
  theme(
    #axis.text.y=element_blank(),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        axis.text = element_text(size=22),
        legend.text=element_text(size=22),
        axis.title = element_text(size=24),
        legend.title = element_text(size=24),
        plot.title = element_text(size=24))

ggsave(filename="plots/listed_plot.png", plot=listed_plot, unit="cm",width=25,height=21,  dpi=600, device = png)

#make maps of assessment area
#devtools::install_github("ropensci/rnaturalearth")


proj <- "+proj=lcc +lon_0=-63.1 +lat_1=43.8 +lat_2=46.9 +lat_0=45.3 +datum=WGS84 +units=m +no_defs"
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
ll <- "+proj=longlat +datum=WGS84"
sf::sf_use_s2(FALSE) # because of "Evaluation error: Found 1 feature with invalid spherical geometry." otherwise

#create the Maritimes polygon
Maritimes <-  rnaturalearth::ne_states(country="Canada",returnclass = "sf") %>%
  filter(name %in% c("Nova Scotia", "New Brunswick", "Prince Edward Island"))%>%
  st_transform(proj)
Canada<-rnaturalearth::ne_states(country="Canada",returnclass = "sf")%>%
  st_transform(proj)
world<-rnaturalearth::ne_countries(type = "countries", scale = "medium" ,returnclass = "sf")%>%
  select(geometry)
# lakes
lakes110 <- ne_download(scale = 110, type = "lakes", category = "physical" ,returnclass = "sf")%>%
  select(geometry)


plain <- theme(
   axis.text = element_blank(),
  # axis.line = element_blank(),
   axis.ticks = element_blank(),
   panel.border = element_blank(),
  # panel.grid = element_blank(),
  # axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

#create bounding box center in Canada
bb<-st_sfc(
  st_polygon(list(cbind(
    c(-143, -55, -55, -143, -143),
    c(32, 32, 81, 81, 32)
  ))),
  crs=ll
)

#now reproject bounding box to new coordinate system (crs_string)
bb_trans<-st_transform(bb, crs=crs_string)

#the extent of the bounding box in new projection
b<-st_bbox(bb_trans)
b

map_Canada<-ggplot(world)+
  geom_sf(fill='antiquewhite1')+
  geom_sf(data=lakes110, fill='white')+
  geom_sf(data=Canada, fill='burlywood2', col='black')+
  geom_sf(data=Maritimes, fill='red', col='black')+
  theme_bw()+
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c('burlywood3', "white"),
    text_family = "ArcherPro Book"
  )+
  coord_sf(crs=crs_string, xlim=c(-4796723  , 3599174), ylim=c(5675105, 10449173), expand=FALSE )+
  plain

ggsave(filename="plots/assessment_area.png", plot=map_Canada, unit="cm",width=18,height=10, dpi=600, device = png)

#now find the highest high risk species
highest_high_risk<-high_risk%>%
  filter(`Global environmental risk`==12)%>%
  dplyr::select("species_common.x", "species_latin", "Qu.1",  "RationaleQu1", "Qu.2" ,"RationaleQu.2" , "Qu.3" , "RationaleQu.3" ,           
"Qu.4" ,"RationaleQu.4" , "Global environmental risk", "AISR_Listing")%>%
  as.data.table()

write.csv(highest_high_risk, "plots/highest_high_risk.csv")

#write csv of high risk species with scores
df_high_risk<-high_risk%>%
  dplyr::select(species_common.x, "species_latin", "Global environmental risk", "AISR_Listing")%>%
  as.data.table()

write.csv(df_high_risk, "plots/df_high_risk.csv")

#data deficient species
d.d.sp<-combined_list%>%
  filter(Qu.1==0 | Qu.2==0 | Qu.3==0 | Qu.4==0)

d.d.sp.list<-d.d.sp%>%
  dplyr::select("species_latin")

write_xlsx(d.d.sp.list, "data/output_data/df_data_deficient.xlsx")

for_plot<-d.d.sp%>%
  dplyr::select("species_latin", "taxa", "Qu.1", "Qu.2", "Qu.3", "Qu.4" )%>%
  pivot_longer(cols = starts_with("Qu"),
               names_to="question",
               values_to="score")%>%
  group_by(question, taxa, score)%>%
  summarise(n=n_distinct(species_latin))


uncertaint_qu<-ggplot(for_plot, aes(question, score, fill=n))+
  geom_tile()+
  scale_fill_viridis_c(name="Number of Species")+
  facet_wrap(~taxa)+
  theme_bw()

ggsave(filename="plots/data_gaps_by_qu.png", plot=uncertaint_qu, unit="cm",width=22,height=24,  dpi=600, device = png)

d.d._plot<-ggplot(d.d.sp, aes(taxa, `Global environmental risk`))+
  geom_boxplot(aes(fill=taxa))+
  scale_fill_brewer(palette = 'RdYlBu', guide="none")+
  #geom_jitter(width=0.2)+ 
  theme(
    #axis.text.y=element_blank(),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        axis.text = element_text(size=22),
        legend.text=element_text(size=22),
        axis.title = element_text(size=24),
        legend.title = element_text(size=24)
        #axis.title.y = element_blank()
        )+
  coord_flip()

ggsave(filename="plots/data_deficient_scores.png", plot=d.d._plot, unit="cm",width=22,height=24,  dpi=600, device = png)

#secondary assessment species
second_look<-combined_list%>%
  filter(Secondary_Assessment %in% c("Yes", "yes", "uncertain", "Maybe", "maybe"))

second_look_list<-second_look%>%
  dplyr::select("species_latin")

write_xlsx(second_look_list, "data/output_data/second_look_list.xlsx")

second_look_plot<-ggplot(second_look, aes(taxa, `Global environmental risk`))+
  geom_boxplot(aes(fill=taxa))+
  scale_fill_brewer(palette = 'RdYlBu', guide="none")+
  geom_jitter(width=0.2)+ 
  theme(
    #axis.text.y=element_blank(),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        axis.text = element_text(size=22),
        legend.text=element_text(size=22),
        axis.title = element_text(size=24),
        legend.title = element_text(size=24)
        #axis.title.y = element_blank()
        )+
  coord_flip()

ggsave(filename="plots/secondary_assessment_plot.png", plot=second_look_plot, unit="cm",width=22,height=24,  dpi=600, device = png)

#moderate-risk species, species not noted on any other list
mod_risk<-combined_list%>%
  anti_join(d.d.sp, by="species_latin")%>%
  anti_join(second_look, by="species_latin")%>%
  anti_join(high_risk, by="species_latin")

mod_risk_list<-mod_risk%>%
  dplyr::select("species_latin")

write_xlsx(mod_risk_list, "data/output_data/moderate_risk_list.xlsx")


mod_risk_plot<-ggplot(mod_risk, aes(taxa, `Global environmental risk`))+
  geom_boxplot(aes(fill=taxa))+
  scale_fill_brewer(palette = 'RdYlBu', guide="none")+
  #geom_jitter(width=0.2)+ 
  theme(
    #axis.text.y=element_blank(),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        axis.text = element_text(size=22),
        legend.text=element_text(size=22),
        axis.title = element_text(size=24),
        legend.title = element_text(size=24)
        #axis.title.y = element_blank()
        )+
  coord_flip()+
  scale_y_continuous(limits=c(2,8),breaks=seq(0, 12, 1))

ggsave(filename="plots/mod_risk_plot.png", plot=mod_risk_plot, unit="cm",width=22,height=24,  dpi=600, device = png)

#scoring histogram
hist<-ggplot(combined_list, aes(x=`Global environmental risk`))+
  geom_histogram(binwidth=1, fill='lightgrey', col='black')+ 
  theme(panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        axis.text = element_text(size=22),
        legend.text=element_text(size=22),
        axis.title = element_text(size=24),
        legend.title = element_text(size=24))

ggsave(filename="plots/histogram_of_all_scores.png", plot=hist, dpi=600, device = png)

