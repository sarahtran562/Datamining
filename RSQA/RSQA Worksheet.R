#RSQA Worksheet
#Start Date: 6/14/21

#Working Directory
setwd("~/FRI/R Folder/RSQA")

#Packages Libraries and Databases
library(readr)
#library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)
library(ggpubr)
library(tidyr)
library(pheatmap)
library(plyr)
library(dplyr)
library(ggbiplot)

#Loading Data Into R
mydata <- read.csv(file = 'Results.csv')
expcount <- mydata%>%
  group_by(PARM_NM)%>%
  summarize(count=n())
expcount
ggplot(data=mydata, aes(x=PARM_NM,color=PARM_NM))+
  geom_histogram(binwidth=100,stat="count")+
  labs(title="Frequency of Different Measured Parameters")+
  theme_classic()+
  theme(axis.text.x = element_blank())
expcount <- expcount[expcount$count > 50, ]
expcount <- expcount[order(expcount$count, decreasing = T), ]
head(expcount)

#Plotting One Parameter
SpecimenLength_data<- mydata[mydata$PARM_NM == "Specimen length, average", ]
ggplot(data=SpecimenLength_data, aes(x=PARM_NM, y=RESULT_VA))+ 
  geom_jitter()+ 
  labs(title="Average Specimen Length", x="Specimen", 
       y="Specimen Length Average")+
  theme_classic()

#Plotting Multiple Parameters
top_5_data <- mydata[mydata$PARM_NM %in% expcount$PARM_NM[1:5], ]
top_5_data
ggplot(data=top_5_data, aes(x=PARM_NM, y=RESULT_VA))+ 
  geom_jitter()+ 
  labs(title="Measurements for Different Parameters", x="Parameter", 
       y="Count")+
  theme_classic()+
  theme(axis.text.x = element_blank())+
  facet_wrap(PARM_NM ~ .,scales="free")

#Plotting Data On Maps
sites <- read.csv("Sites.csv")
head(sites)
head(top_5_data)
left_joined_df <- top_5_data %>%
  left_join(sites)
world <- ne_countries(scale="medium", returnclass = "sf")
world_map <-ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Map of NESQA biological sampling sites", 
       x="Longitude", y="Latitude",
       subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))),
                       " sites"))+
  geom_point(data=left_joined_df,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=STATE_NM),
             size=1,shape=19)+
  coord_sf(xlim = c((min(sites$DEC_LONG_VA)-1) , (max(sites$DEC_LONG_VA)+1)),
           ylim = c((min(sites$DEC_LAT_VA)-1) , (max(sites$DEC_LAT_VA)+1)))+
  annotation_scale(location = "bl", width_hint = 0.3)+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)
world_map

#Replicating Plots
results_problemset <- read.csv("Results Problemset.csv")
sites_problemset <- read.csv("Sites Problemset.csv")
pyrene_data<- results_problemset[results_problemset$PARM_NM == 
                                   "Pyrene, solids", ]
#figure 1
ggplot(data=pyrene_data, aes(x=PARM_NM,y=RESULT_VA))+ 
  geom_jitter(aes(color=COUNTY_NM), position = position_jitterdodge(jitter.width=0.05,
      dodge.width=1.2),size=1.5)+ 
  labs(title="Max Pyrene Concentration for Each Site by County",
       x=element_blank(),
       y="Max Concentration of Pyrene per site (ug/kg)",
       color="California County")+
  theme_bw()+
  theme(axis.text.x = element_blank())
#figure 2
sites <- read.csv("Sites Problemset.csv")
head(sites)
head(pyrene_data)
problemset_join <- pyrene_data %>%
  left_join(sites)
world <- ne_countries(scale="medium", returnclass = "sf")
world_map <-ggplot(data=world,color="gray") +
  geom_sf() + theme_bw() +
  theme(panel.background = element_rect(fill = "gray92"),
        panel.grid.major = element_line(size = 0.7, linetype = 'solid',
                                        colour = "white"))+
  labs(title="Map of California Sampling Sites", 
       x="Longitude", y="Latitude",
       subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))),
                       " sites"),
       color="California County",size="Max Pyrene concentration (ug/kg)")+
  geom_point(data=problemset_join,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=COUNTY_NM,
             size=RESULT_VA,shape=19), alpha=0.5)+
  coord_sf(xlim = c((min(sites$DEC_LONG_VA)-1) , (max(sites$DEC_LONG_VA)+1)),
           ylim = c((min(sites$DEC_LAT_VA)-1) , (max(sites$DEC_LAT_VA)+1)))+
  scale_shape_identity()+
  theme(legend.key = element_rect(fill = "gray90"))
world_map
#figure 1- new chemical
imida_data<- results_problemset[results_problemset$PARM_NM == 
                                   "Imidacloprid, wf", ]
ggplot(data=imida_data, aes(x=PARM_NM,y=RESULT_VA))+ 
  geom_jitter(aes(color=COUNTY_NM), position = position_jitterdodge(jitter.width=0.05,
       dodge.width=1.2),size=1.5)+ 
  labs(title="Max Imidacloprid Concentration for Each Site by County",
       x=element_blank(),
       y="Max Concentration of Imidacloprid per site (ug/kg)",
       color="California County")+
  theme_bw()+
  theme(axis.text.x = element_blank())
#figure 2- new chemical
sites <- read.csv("Sites Problemset.csv")
head(sites)
head(imida_data)
problemset_join_ <- imida_data %>%
  left_join(sites)
world <- ne_countries(scale="medium", returnclass = "sf")
world_map <-ggplot(data=world,color="gray") +
  geom_sf() + theme_bw() +
  theme(panel.background = element_rect(fill = "gray92"),
        panel.grid.major = element_line(size = 0.7, linetype = 'solid',
                                        colour = "white"))+
  labs(title="Map of California Sampling Sites", 
       x="Longitude", y="Latitude",
       subtitle=paste0("A total of 487 sites"),
       color="California County",size="Max Imidacloprid concentration (ug/kg)")+
  geom_point(data=problemset_join_,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=COUNTY_NM,
                                      size=RESULT_VA,shape=19), alpha=0.3)+
  coord_sf(xlim = c((min(sites$DEC_LONG_VA)-1) , (max(sites$DEC_LONG_VA)+1)),
           ylim = c((min(sites$DEC_LAT_VA)-1) , (max(sites$DEC_LAT_VA)+1)))+
  scale_shape_identity()+
  theme(legend.key = element_rect(fill = "gray90"))
world_map
#figure 1- NESQA dataset
mydata <- read.csv(file = 'Results.csv')
sites <- read.csv("Sites.csv")
length_data<- mydata[mydata$PARM_NM == 
                                   "Specimen length, average", ]
ggplot(data=length_data, aes(x=PARM_NM,y=RESULT_VA))+ 
  geom_jitter(aes(color=STATE_NM), position = position_jitterdodge(jitter.width=0.05,
       dodge.width=1.2),size=1.5)+ 
  labs(title="Average Specimen Length for Each Site by State",
       x=element_blank(),
       y="Average Length of Specimen per site",
       color="State")+
  theme_bw()+
  theme(axis.text.x = element_blank())
#figure 2- NESQA dataset
sites <- read.csv("Sites.csv")
head(sites)
head(length_data)
dataset_join <- length_data %>%
  left_join(sites)
world <- ne_countries(scale="medium", returnclass = "sf")
world_map <-ggplot(data=world,color="gray") +
  geom_sf() + theme_bw() +
  theme(panel.background = element_rect(fill = "gray92"),
        panel.grid.major = element_line(size = 0.7, linetype = 'solid',
                                        colour = "white"))+
  labs(title="Map of NESQA Sampling Sites", 
       x="Longitude", y="Latitude",
       subtitle=paste0("A total of 174 sites"),
       color="State",size="Average Length of Specimen")+
  geom_point(data=dataset_join,aes(x=DEC_LONG_VA,y=DEC_LAT_VA,color=STATE_NM,
                                       size=RESULT_VA,shape=19), alpha=0.3)+
  coord_sf(xlim = c((min(sites$DEC_LONG_VA)-1) , (max(sites$DEC_LONG_VA)+1)),
           ylim = c((min(sites$DEC_LAT_VA)-1) , (max(sites$DEC_LAT_VA)+1)))+
  scale_shape_identity()+
  theme(legend.key = element_rect(fill = "gray90"))
world_map

#Running Statistical Tests
kruskal <- compare_means(RESULT_VA ~ COUNTY_NM, pyrene_data, method="kruskal.test", 
              paired = FALSE , p.adjust.method="fdr")
kruskal_2 <- compare_means(RESULT_VA ~ COUNTY_NM, imida_data, method="kruskal.test", 
                         paired = FALSE , p.adjust.method="fdr")
kruskal_3 <- compare_means(RESULT_VA ~ COUNTY_NM, length_data, method="kruskal.test", 
                           paired = FALSE , p.adjust.method="fdr")
posthoc_results_pyrene <- compare_means(RESULT_VA ~ COUNTY_NM , pyrene_data, 
              method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
posthoc_results_i <- compare_means(RESULT_VA ~ COUNTY_NM , imida_data, 
             method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")
posthoc_results_nesqa <- compare_means(RESULT_VA ~ STATE_NM , length_data, 
             method="wilcox.test",paired=FALSE, p.adjust.method = "fdr")

#Adding Statistics to your Figures
psignificant <- posthoc_results_pyrene[posthoc_results_pyrene$p < 0.05, ]
psignificant2 <- posthoc_results_i[posthoc_results_i$p < 0.05, ]
psignificant3 <- posthoc_results_nesqa[posthoc_results_nesqa$p < 0.05, ]
#pyrene data
ggplot(data=pyrene_data, aes(x=COUNTY_NM, y=RESULT_VA,fill=COUNTY_NM))+
  geom_boxplot()+
  labs(title="Pyrene Concentration for Each Site by County",
       subtitle=paste0("A total of 95 sites"),
       x=" ",
       y="Concentration of Pyrene per site (ug/kg)",
       fill="California County")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
       axis.ticks.x=element_blank())+
  stat_pvalue_manual(inherit.aes=FALSE, data=psignificant, 
                       label = "p", y.position=220)
#imidacloprid data
ggplot(data=imida_data, aes(x=COUNTY_NM, y=RESULT_VA,color=COUNTY_NM))+
  geom_boxplot()+
  labs(title="Imidacloprid Concentration for Each Site by County",
       subtitle=paste0("A total of 487 sites"),
       x=" ",
       y="Concentration of Imidacloprid per site (ug/kg)",
       color="California County")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  stat_pvalue_manual(inherit.aes=FALSE, data=psignificant2, 
                     label = "p.signif", y.position=2100,step.increase=0.1)
#nesqa data
ggplot(data=mydata, aes(x=STATE_NM, y=RESULT_VA,color=STATE_NM))+
  geom_boxplot()+
  labs(title="Average Specimen Length for Each Site by State",
       subtitle=paste0("A total of 174 sites"),
       x=" ",
       y="Average Length of Specimen per site",
       color="State")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  stat_pvalue_manual(inherit.aes=FALSE, data=psignificant3, 
                     label = "p.signif", y.position=7200,step.increase=0.1)

#Multivariable Analysis: Preparing Your Data
results <- read.csv("Results Inorganic.csv")
sites <- read.csv("Sites Inorganic.csv")
inorganic_median <- results%>%
  group_by(SITE_NO, PARM_NM)%>%
  dplyr::summarize(RESULT_VA = median(RESULT_VA))%>%
  ungroup()
inorganic_median <- inorganic_median[-c(5745), ] 
short_form <- pivot_wider(inorganic_median,
    id_cols = SITE_NO,
    names_from = PARM_NM,
    values_from = RESULT_VA
    )
colSums(is.na(short_form))
Interestingcolumns_data<-as.data.frame(short_form[,c(2,7,8,9,11,12,16,17)])
rownames(Interestingcolumns_data)<-short_form$SITE_NO
finaldata <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]
metadata<-sites[match(rownames(finaldata),sites$SITE_NO),]

#Multicomponent Analysis: Heatmaps and Hierarchical Clustering
data_cor<-cor(finaldata)
pheatmap(data_cor, annotations=rownames(data_cor),
         color=colorRampPalette(c("blue", "white", "red"))(50), 
         show_rownames=T, show_colnames=T)
data_cor<-cor(t(finaldata))
pheatmap(data_cor,show_rownames=F, show_colnames=F)

#Multivariable Analysis: Principle Component Analysis
install_github("vqv/ggbiplot")
prcompData<-prcomp(finaldata,center=T, scale=T)
summary(prcompData)
ggscreeplot(prcompData, type="pev")
ggscreeplot(prcompData, type="cev")
ggbiplot(prcompData, choices = c(1,2), var.axes=F, 
         groups=paste(metadata$RSQA_STUDY), ellipse=T)
ggbiplot(prcompData, choices = c(1,2), var.axes=T, 
         groups=paste(metadata$RSQA_STUDY))+theme_classic()
ggbiplot(prcompData, choices = c(1,2), var.axes=F, 
         groups=paste(metadata$STATE_ABBREV), ellipse=T)

#Dealing With Non-Detects
newlength_data <- SpecimenLength_data %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD == "<", 0, RESULT_VA))
ggplot(data=newlength_data, aes(x=PARM_NM, y=RESULT_CORR))+ 
  geom_jitter()+ 
  labs(title="Average Specimen Length", x="Specimen", 
       y="Specimen Length Average")+
  theme_classic()
newmydata_data <- mydata %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD == "<", 0, RESULT_VA))
detectfreq <- newmydata_data %>%
  group_by(PARM_NM, RESULT_CORR)%>%
  summarize(count=n())
top_5detect_data <- newmydata_data[
  newmydata_data$RESULT_CORR %in% detectfreq$RESULT_CORR[1:5], ]
ggplot(data=top_5detect_data, aes(x=PARM_NM, y=RESULT_CORR))+ 
  geom_jitter()+ 
  labs(title="Top 5 Chemicals Detected", x="Parameter", 
       y="Count")+
  theme_classic()+
  theme(axis.text.x = element_blank())+
  facet_wrap(PARM_NM ~ .,scales="free")
#Multivariable Analysis: Preparing Your Data PART 2
results <- read.csv("Results Inorganic.csv")
newresults_data <- results %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD == "<", 0, RESULT_VA))
sites <- read.csv("Sites Inorganic.csv")
inorganic_median <- newresults_data%>%
  group_by(SITE_NO, PARM_NM)%>%
  dplyr::summarize(RESULT_VA = median(RESULT_VA))%>%
  ungroup()
inorganic_median <- inorganic_median[-c(5745), ] 
short_form <- pivot_wider(inorganic_median,
                          id_cols = SITE_NO,
                          names_from = PARM_NM,
                          values_from = RESULT_VA
)
colSums(is.na(short_form))
Interestingcolumns_data<-as.data.frame(short_form[,c(2,7,8,9,11,12,16,17)])
rownames(Interestingcolumns_data)<-short_form$SITE_NO
finaldata <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]
metadata<-sites[match(rownames(finaldata),sites$SITE_NO),]
#Multicomponent Analysis: Heatmaps and Hierarchical Clustering PART 2
data_cor<-cor(finaldata)
pheatmap(data_cor, annotations=rownames(data_cor),
         color=colorRampPalette(c("blue", "white", "red"))(50), 
         show_rownames=T, show_colnames=T)
data_cor<-cor(t(finaldata))
pheatmap(data_cor,show_rownames=F, show_colnames=F)
#Multivariable Analysis: Principle Component Analysis PART 2
install_github("vqv/ggbiplot")
prcompData<-prcomp(finaldata,center=T, scale=T)
summary(prcompData)
ggscreeplot(prcompData, type="pev")
ggscreeplot(prcompData, type="cev")
ggbiplot(prcompData, choices = c(1,2), var.axes=F, 
         groups=paste(metadata$RSQA_STUDY), ellipse=T)
ggbiplot(prcompData, choices = c(1,2), var.axes=T, 
         groups=paste(metadata$RSQA_STUDY))+theme_classic()
ggbiplot(prcompData, choices = c(1,2), var.axes=F, 
         groups=paste(metadata$STATE_ABBREV), ellipse=T)

#hi :) :)

