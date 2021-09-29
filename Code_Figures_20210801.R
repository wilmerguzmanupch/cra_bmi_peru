library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)     # for fortifying shapefiles
library(gridExtra) # to combine plots (if mulitple maps are made and want them in one plot)
library(maptools)
library(RColorBrewer)
library(dplyr)
library(tmaptools)
library(ggpubr)
library(reshape2)
library(ggrepel)
library(scales)
library(purrr)
library(tidyr)
library(tidyverse)
library(gplots)


setwd("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables")
data<-read.csv("Proportion_and_deaths_high_BMI_by_region_2021-08-12.csv")

### FIGURE 1: DM and CVDs PAFs attributable to high BMI in all 25 regions by sex, 2018  ###
########################

a<-select(data, 1, 3 , 4)
a$region[a$region == "MADRE DE DIOS"] <- "Madre de Dios"
a$region[a$region == "PASCO"] <- "Pasco"
a$region[a$region =="AYACUCHO"] <- "Ayacucho"
a$region[a$region == "AMAZONAS"] <- "Amazonas"
a$region[a$region == "HUANCAVELICA"] <- "Huancavelica"
a$region[a$region == "APURIMAC"] <- "Apurimac"
a$region[a$region =="MOQUEGUA"] <- "Moquegua"
a$region[a$region =="TUMBES"] <- "Tumbes"
a$region[a$region =="UCAYALI"] <- "Ucayali"
a$region[a$region =="TACNA"] <- "Tacna"
a$region[a$region =="HUANUCO"] <- "Huanuco"
a$region[a$region =="LORETO"] <- "Loreto"
a$region[a$region =="PUNO"] <- "Puno"
a$region[a$region == "CUSCO"] <- "Cusco"
a$region[a$region =="SAN MARTIN"] <- "San Martin"
a$region[a$region =="JUNIN"] <- "Junin"
a$region[a$region == "CAJAMARCA"] <- "Cajamarca"
a$region[a$region =="ANCASH"] <- "Ancash"
a$region[a$region =="AREQUIPA"] <- "Arequipa"
a$region[a$region == "CALLAO"] <- "Callao"
a$region[a$region =="ICA"] <- "Ica"
a$region[a$region =="PIURA"] <- "Piura"
a$region[a$region =="LAMBAYEQUE"] <- "Lambayeque"
a$region[a$region =="LA LIBERTAD"] <- "La Libertad"
a$region[a$region =="LIMA"] <- "Lima"
a <- a %>% mutate("Natural region" = ifelse(region == "Moquegua" | region =="Tacna"|  region =="Ancash" |
                                              region =="Tumbes"| region =="Arequipa"|region =="Callao"| 
                                              region =="Ica"|region =="Piura"|region =="Lambayeque"|region =="La Libertad"|
                                              region =="Lima","Coast", ifelse(region == "Pasco" | region =="Ayacucho"|
                                                                                region =="Huancavelica"| region =="Apurimac"| region =="Huanuco"|region =="Puno"|  region =="Cusco"|region =="Junin"|region =="Cajamarca", "Highlands", "Amazon")))


#######################
a_men<-filter(a, sex == "Men")
a_women<-filter(a, sex == "Women")

colours<-c( "#5E4FA2", "#3288BD","#66C2A5" , "#ABDDA4","#E6F598", "#FEE08B", "#FDAE61","#F46D43", "#D53E4F","#9E0142")

### MAP

# PREPARE DATASETS
setwd("~/Desktop/Artículos/accepted:published/Nacimientos/gadm36_per_3")
shapefile <- readOGR(dsn = ".", "gadm36_per_3")  
shapefile_df<-fortify(shapefile, region = "name_1")

shapefile_df$id<-toupper(shapefile_df$id)
shapefile_df$id <- ifelse(shapefile_df$id == "LIMA PROVINCE", "LIMA", shapefile_df$id)
shapefile_df$id <- iconv(shapefile_df$id, to = 'ASCII//TRANSLIT')
shapefile_df$id <- ifelse(shapefile_df$id == "APUR'IMAC", "APURIMAC", shapefile_df$id)
shapefile_df$id <- ifelse(shapefile_df$id == "HU'ANUCO", "HUANUCO", shapefile_df$id)
shapefile_df$id <- ifelse(shapefile_df$id == "JUN'IN", "JUNIN", shapefile_df$id)
shapefile_df$id <- ifelse(shapefile_df$id == "SAN MART'IN", "SAN MARTIN", shapefile_df$id)

# MERGE MAP DATASET AND INFORMATION DATASET (use region as unique identifier)
colnames(shapefile_df)<-c("long", "lat", "order", "hole", "piece", "DEPARTAMENTO", "group")
pafbmi.dt<-a

colnames(pafbmi.dt)[1]<-"Region"

pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Amazonas", "AMAZONAS", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Ancash", "ANCASH", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Apurimac", "APURIMAC", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Arequipa", "AREQUIPA", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Ayacucho", "AYACUCHO", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Cajamarca", "CAJAMARCA", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Callao", "CALLAO", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Cusco", "CUSCO", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Huancavelica", "HUANCAVELICA", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Huanuco", "HUANUCO", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Ica", "ICA", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Junin", "JUNIN", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "La Libertad", "LA LIBERTAD", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Lambayeque", "LAMBAYEQUE", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Lima", "LIMA", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Loreto", "LORETO", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Madre de Dios", "MADRE DE DIOS", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Moquegua", "MOQUEGUA", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Pasco", "PASCO", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Piura", "PIURA", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Puno", "PUNO", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "San Martin", "SAN MARTIN", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Tacna", "TACNA", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Tumbes", "TUMBES", pafbmi.dt$Region)
pafbmi.dt$Region<-ifelse(pafbmi.dt$Region == "Ucayali", "UCAYALI", pafbmi.dt$Region)

colnames(pafbmi.dt)[1]<-"DEPARTAMENTO"

pafbmi.dt_men<-filter(pafbmi.dt, sex == "Men")
pafbmi.dt_women<-filter(pafbmi.dt, sex == "Women")

shapefile_df2_men <- merge(shapefile_df, pafbmi.dt_men, by = "DEPARTAMENTO", all = TRUE)
shapefile_df2_men<- shapefile_df2_men[order(shapefile_df2_men$order),]

shapefile_df2_women <- merge(shapefile_df, pafbmi.dt_women, by = "DEPARTAMENTO", all = TRUE)
shapefile_df2_women<- shapefile_df2_women[order(shapefile_df2_women$order),]



colours<-c("#5E4FA2", "#3288BD","#66C2A5" , "#ABDDA4","#E6F598", "#FEE08B", "#FDAE61","#F46D43", "#D53E4F","#9E0142")

paf_men <-ggplot() +
  # county polygons
  geom_polygon(data =shapefile_df2_men, aes(fill = paf.median,
                                        x = long,
                                        y = lat,
                                        group = group), colour  = "black",  size =  0.5)  +
  # county outline
  coord_equal() +
  scale_fill_gradientn(colours = colours, breaks = c(0.1, 0.3, 0.5),
                       limits = c(0.04, 0.60),labels =c("10%", "30%", "50%"), na.value = "gray",
                       name = "Percentage of CMD deaths in 2018 attributable\n                  to high BMI in 2016") +
  theme_bw() + labs (caption = "Men") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(), 
        plot.caption = element_text(hjust = 0.5, size = 30)) +
  theme(legend.text=element_text(size=30), legend.title = element_text(size=30), legend.key.width = unit(3, "cm"), 
        legend.key.height = unit(0.7,"cm"))  


paf_women <-ggplot() +
  # county polygons
  geom_polygon(data =shapefile_df2_women, aes(fill = paf.median,
                                            x = long,
                                            y = lat,
                                            group = group), colour  = "black",  size =  0.5)  +
  # county outline
  coord_equal() +
  scale_fill_gradientn(colours = colours, breaks = c(0.1, 0.3, 0.5),
                       limits = c(0.04, 0.60),labels =c("10%", "30%", "50%"), na.value = "gray",
                       name = "Percentage of CMD deaths in 2018 attributable\n                 to high BMI in 2016") +
  theme_bw()+ labs (caption = "Women") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(), 
        plot.caption = element_text(hjust = 0.5, size = 30))+
  theme(legend.text=element_text(size=30), legend.title = element_text(size=30), legend.key.width = unit(3, "cm"), 
        legend.key.height = unit(0.7,"cm"))  


maps <-ggarrange(paf_men, paf_women, common.legend = TRUE, legend = "bottom")

# SAVE FIGURES 
date <- Sys.Date()
pdf(paste0("~/Desktop/Artículos/CRA High BMI Peru/Figures/Fig1_Proportion_CMD_by_region_sex_", date, ".pdf"), height = 17, width = 26)
plot(maps)
dev.off()

#

#########################################################################################################

### FIGURE 2: Absolute number of attributable deaths in all 25 regions by sex, 2018  ###
########################
setwd("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables")
a<-read.csv("Proportion_and_deaths_high_BMI_by_region_2021-08-11.csv")
a<-filter(a, year == 2018)

a$region[a$region == "MADRE DE DIOS"] <- "Madre de Dios"
a$region[a$region == "PASCO"] <- "Pasco"
a$region[a$region =="AYACUCHO"] <- "Ayacucho"
a$region[a$region == "AMAZONAS"] <- "Amazonas"
a$region[a$region == "HUANCAVELICA"] <- "Huancavelica"
a$region[a$region == "APURIMAC"] <- "Apurimac"
a$region[a$region =="MOQUEGUA"] <- "Moquegua"
a$region[a$region =="TUMBES"] <- "Tumbes"
a$region[a$region =="UCAYALI"] <- "Ucayali"
a$region[a$region =="TACNA"] <- "Tacna"
a$region[a$region =="HUANUCO"] <- "Huanuco"
a$region[a$region =="LORETO"] <- "Loreto"
a$region[a$region =="PUNO"] <- "Puno"
a$region[a$region == "CUSCO"] <- "Cusco"
a$region[a$region =="SAN MARTIN"] <- "San Martin"
a$region[a$region =="JUNIN"] <- "Junin"
a$region[a$region == "CAJAMARCA"] <- "Cajamarca"
a$region[a$region =="ANCASH"] <- "Ancash"
a$region[a$region =="AREQUIPA"] <- "Arequipa"
a$region[a$region == "CALLAO"] <- "Callao"
a$region[a$region =="ICA"] <- "Ica"
a$region[a$region =="PIURA"] <- "Piura"
a$region[a$region =="LAMBAYEQUE"] <- "Lambayeque"
a$region[a$region =="LA LIBERTAD"] <- "La Libertad"
a$region[a$region =="LIMA"] <- "Lima"

a<-data.table(a)

b<-a[, ord := sprintf("%02i", frank(a, sex, -Attributable.deaths, ties.method = "first"))]


## Plot
pdf(paste0("~/Desktop/Artículos/CRA High BMI Peru/Figures/Fig2_Absolute_number_attributable_deaths_2018_by_region_", Sys.Date(), ".pdf"),
    width = 20, height =15)
ggplot(b, aes(x= ord, y = Attributable.deaths, fill = sex)) +
  geom_col(size = 0.6, width = .7) + 
  coord_flip() +
  theme(panel.grid.major.x  = element_line(colour = "grey30"), panel.grid.minor.y  = element_line(colour = "black"), 
        panel.grid.minor.x = element_line(colour = "white"), panel.grid.major.y  = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"), 
        legend.position = "none", legend.justification = c("right", "top"), legend.box.just = "right",
        legend.box.margin = margin(0, 0, 0, 0, "cm"), legend.text = element_text(size = 15),
        legend.direction = "horizontal", axis.text.x =element_text(size = 15), 
        axis.text.y =element_text(size = 20),  axis.title.x = element_text(size= 20), 
        strip.text.y = element_text( size = 20), strip.text.x = element_text( size = 20))  +
  guides(fill=guide_legend(""))+
  facet_wrap(~sex, ncol = 1, scales = "free_y", drop = TRUE)  +
  scale_x_discrete(labels = b[, setNames(as.character(region), ord)]) + 
  ylab("Absolute number of CMD deaths in 2018 attributable to high BMI in 2016") +  
  ggtitle("")  + xlab ("") +  geom_text(aes(label = Attributable.deaths), size = 5, hjust = -0.5, vjust = 0.5) 
dev.off()  





### FIGURE 3: Proportion of CMD deaths attributable to specific BMI categories in all 25 regions by sex, 2018  ###
########################
setwd("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables")
a<-read.csv("Proportion_and_deaths_high_BMI_by_region_bmi_cat_2021-08-12.csv")
#a<-read.csv("PAF_Deaths_by_BMI_cat_region_sex_2018_2021-08-12.csv")
a<-filter(a, year == 2018)
names(a)[1]<-"region"

a$region[a$region == "MADRE DE DIOS"] <- "Madre de Dios"
a$region[a$region == "PASCO"] <- "Pasco"
a$region[a$region =="AYACUCHO"] <- "Ayacucho"
a$region[a$region == "AMAZONAS"] <- "Amazonas"
a$region[a$region == "HUANCAVELICA"] <- "Huancavelica"
a$region[a$region == "APURIMAC"] <- "Apurimac"
a$region[a$region =="MOQUEGUA"] <- "Moquegua"
a$region[a$region =="TUMBES"] <- "Tumbes"
a$region[a$region =="UCAYALI"] <- "Ucayali"
a$region[a$region =="TACNA"] <- "Tacna"
a$region[a$region =="HUANUCO"] <- "Huanuco"
a$region[a$region =="LORETO"] <- "Loreto"
a$region[a$region =="PUNO"] <- "Puno"
a$region[a$region == "CUSCO"] <- "Cusco"
a$region[a$region =="SAN MARTIN"] <- "San Martin"
a$region[a$region =="JUNIN"] <- "Junin"
a$region[a$region == "CAJAMARCA"] <- "Cajamarca"
a$region[a$region =="ANCASH"] <- "Ancash"
a$region[a$region =="AREQUIPA"] <- "Arequipa"
a$region[a$region == "CALLAO"] <- "Callao"
a$region[a$region =="ICA"] <- "Ica"
a$region[a$region =="PIURA"] <- "Piura"
a$region[a$region =="LAMBAYEQUE"] <- "Lambayeque"
a$region[a$region =="LA LIBERTAD"] <- "La Libertad"
a$region[a$region =="LIMA"] <- "Lima"

a<-data.table(a)
#a <- a %>% mutate("NCD" = ifelse(outcome == "Atrial fibrillation and flutter" | outcome =="Hypertensive heart disease"|  outcome =="Ischaemic heart disease" |
#                                   outcome =="Ischaemic stroke"| outcome =="Subaracnoid hemorrhage","Cardiovascular diseases", ifelse(outcome == "Type 2 diabetes mellitus" , "Type 2 diabetes", ifelse(outcome == "Asthma" , "Asthma","Malignant neoplasms"))))

a$BMI_cat <- ifelse(a$BMI_cat == "Overweight", 1, 
                            ifelse(a$BMI_cat == "Obesity 1", 2, 
                                   ifelse(a$BMI_cat == "Obesity 2", 3, 
                                          ifelse(a$BMI_cat == "Obesity 3", 4,
                                          NA))))
a$BMI_cat <- factor(a$BMI_cat, levels = c(1,2,3, 4), labels = c("BMI 25 to 29.9", "BMI 30 to 34.9", "BMI 35 to 39.9", "BMI 40+"))

b_men<-filter(a, sex == "Men")
b_women<-filter(a, sex == "Women")

pdf(paste0("~/Desktop/Artículos/CRA High BMI Peru/Figures/Fig3A_Proportion_attributable_deaths_2018_by_BMI_region_men_", Sys.Date(), ".pdf"),
    width = 25, height =20)
ggplot(b_men, aes(x= BMI_cat, y = paf.median*100, fill = BMI_cat)) +
  geom_bar(stat = "identity", size = 0.6, width = .7) + 
  coord_flip() + 
  theme(panel.grid.major.x  = element_line(colour = "grey30"), panel.grid.minor.y  = element_line(colour = "black"), 
        panel.grid.minor.x = element_line(colour = "white"), panel.grid.major.y  = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"), 
        legend.position = c("top"), legend.justification = c("right", "top"), legend.box.just = "right",
        legend.box.margin = margin(0, 0, 0, 0, "cm"), legend.text = element_text(size = 20),
        legend.direction = "horizontal", axis.text.x =element_text(size = 20), 
        axis.text.y =element_text(size = 20),  axis.title.x = element_text(size= 20), 
        strip.text.y = element_text( size = 20), strip.text.x = element_text( size = 20))  +
  guides(fill=guide_legend(""))+
  facet_wrap(~region) +
  xlab("") +  scale_y_continuous(name="Proportion of CMD deaths in 2018 attributable to each high BMI category in men in 2016", labels = comma, limits = c(0,50)) +
  ggtitle("")   +  geom_text(aes(label = paste0(round(paf.median*100, 0), "%")), size = 7, hjust = -0.5, vjust = 0.5) 
dev.off()  

pdf(paste0("~/Desktop/Artículos/CRA High BMI Peru/Figures/Fig3B_Proportion_attributable_deaths_2018_by_BMI_region_women_", Sys.Date(), ".pdf"),
    width = 25, height =20)
ggplot(b_women, aes(x= BMI_cat, y = paf.median*100, fill = BMI_cat)) +
  geom_bar(stat = "identity", size = 0.6, width = .7) + 
  coord_flip() + 
  theme(panel.grid.major.x  = element_line(colour = "grey30"), panel.grid.minor.y  = element_line(colour = "black"), 
        panel.grid.minor.x = element_line(colour = "white"), panel.grid.major.y  = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"), 
        legend.position = c("top"), legend.justification = c("right", "top"), legend.box.just = "right",
        legend.box.margin = margin(0, 0, 0, 0, "cm"), legend.text = element_text(size = 20),
        legend.direction = "horizontal", axis.text.x =element_text(size = 20), 
        axis.text.y =element_text(size = 20),  axis.title.x = element_text(size= 20), 
        strip.text.y = element_text( size = 20), strip.text.x = element_text( size = 20))  +
  guides(fill=guide_legend(""))+
  facet_wrap(~region) +
  xlab("") +  scale_y_continuous(name="Proportion of CMD deaths in 2018 attributable to each high BMI category in women in 2016", labels = comma, limits = c(0,50)) +
  ggtitle("") + geom_text(aes(label = paste0(round(paf.median*100, 0), "%")),  size = 7, hjust = -0.5, vjust = 0.5) 
dev.off()  






###TABLE 1: Results at the national level
setwd("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables")
national_NCD<-read.csv("Proportion_and_deaths_high_BMI_national_by_NCD_category_2021-08-12.csv")
national_CMD<-read.csv("Proportion_and_deaths_high_BMI_national_2021-08-12.csv")
national_CMD_bmi_cat<-read.csv("Proportion_and_deaths_high_BMI_national_bmi_cat_2021-08-12.csv")
national_NCD_bmi_cat<-read.csv("Proportion_and_deaths_high_BMI_national_by_NCD_bmi_cat_2021-08-12.csv")

national_NCD$deaths<-NULL
national_NCD$exposure<-"5-unit increase in BMI"
names(national_NCD)[3]<-"outcome"

national_CMD$deaths<-NULL
national_CMD$exposure<-"5-unit increase in BMI"
national_CMD$outcome<-"Cardiometabolic diseases"

national_CMD_bmi_cat$deaths<-NULL
names(national_CMD_bmi_cat)[3]<-"exposure"
national_CMD_bmi_cat$outcome<-"Cardiometabolic diseases"

national_NCD_bmi_cat$deaths<-NULL
names(national_NCD_bmi_cat)[3:4]<-c("outcome", "exposure")

table<-rbind(national_NCD,national_CMD,national_CMD_bmi_cat, national_NCD_bmi_cat)
table$year<-NULL
table$paf.median<-paste0(round(table$paf.median*100, 1), " (", round(table$paf.lcl*100, 1), "-", round(table$paf.ucl*100, 1), ")")
table$Attributable.deaths.val<-paste0(table$Attributable.deaths.val, " (", table$Attributable.deaths.lower, "-", table$Attributable.deaths.upper, ")")
table$Attributable.deaths.lower<-NULL
table$Attributable.deaths.upper<-NULL
table$paf.lcl<-NULL
table$paf.ucl<-NULL

table<-select(table, exposure, outcome, sex, paf.median, Attributable.deaths.val)


table$exposure <- ifelse(table$exposure == "5-unit increase in BMI", 0,
                            ifelse(table$exposure== "Overweight", 1, 
                                   ifelse(table$exposure== "Obesity 1", 2, 
                                          ifelse(table$exposure== "Obesity 2", 3, 
                                                 ifelse(table$exposure== "Obesity 3", 4, NA)))))

table$exposure <- factor(table$exposure,
                            levels = c(0,1, 2, 3, 4 ),
                            labels = c("5-unit increase in BMI", "BMI 25-29.9", "BMI 30-34.9", "BMI 35-39.9", "BMI 40+"))


table<-arrange(table, exposure, outcome, sex)
table_m<-filter(table, sex == "Men")
table_f<-filter(table, sex == "Women")
table<-left_join(table_m, table_f, by= c("exposure", "outcome"))
table$sex.x<-NULL
table$sex.y<-NULL
names(table)<-c("High BMI exposure", "Outcome", "Proportion of deaths \nattributable to high BMI \nin 2016 in men (%)","Absolute number of deaths in 2018 \nattributable to high\n BMI in 2016 in men","Proportion of deaths \nattributable to high BMI \nin 2016 in women (%)","Absolute number of deaths in 2018 \nattributable to high\n BMI in 2016 in women")
write.csv(table, paste0("~/Desktop/Artículos/CRA High BMI Peru/Figures/Tab_Proportions_deaths_national_", Sys.Date(), ".csv"), row.names = FALSE)


####Supplementary tables:
#RRs
setwd("~/Desktop/Artículos/CRA High BMI Peru/Data")

rr<-read.csv("RRs_men.csv")
rr<-rr %>% mutate(outcome = ifelse(outcome == "Intracerebral hemorrhage", "Haemorrhagic stroke", ifelse(outcome == "Subarachnoid hemorrhage", "Haemorrhagic stroke",  outcome)))
rr<-unique(rr)
rr$sex<-"Men"
rr_men<-rr

##Women
rr<-read.csv("RRs_women.csv")
rr<-rr %>% mutate(outcome = ifelse(outcome == "Intracerebral hemorrhage", "Haemorrhagic stroke", ifelse(outcome == "Subarachnoid hemorrhage", "Haemorrhagic stroke",  outcome)))
rr<-unique(rr)
rr$sex<-"Women"
rr_women<-rr

rr<-rbind(rr_men, rr_women)
rm(rr_men, rr_women)
rr<-data.table(rr)

rr <- rr[which(!rr$age == "90 to 94"),]
rr <- rr[which(!rr$age == "95 plus"),]

rr<-filter(rr, outcome == "Ischaemic heart disease" | outcome == "Ischaemic stroke" | 
             outcome == "Haemorrhagic stroke" | outcome == "Hypertensive heart disease" | 
             outcome == "Atrial fibrillation and flutter" | outcome == "Diabetes mellitus type 2" )

rr$age[rr$age == "85 to 89"]<-"85 plus"
rr<-arrange(rr, sex, outcome, age)
rr<-select(rr, sex, outcome, age, RR, RR.lower, RR.upper)
names(rr)<-c("Sex", "Outcome", "Age group", "RR", "RR lower limit", "RR upper limit")
write.csv(rr, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Sup1_RR_by_outcome_sex_age_",Sys.Date(),".csv"), row.names = F)

#Sup tab 2: 
results_standardised_regional<-read.csv("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_by_region_2021-08-12.csv")
sup2<-results_standardised_regional
sup2$paf.median<-round(sup2$paf.median, 2)
sup2$paf.lcl<-round(sup2$paf.lcl, 2)
sup2$paf.ucl<-round(sup2$paf.ucl, 2)

names(sup2)<-c("Region","Year", "Sex","Proportion of CMD deaths", "Proportion of CMD\n deaths lower limit (95% CI)", "Proportion of CMD\n deaths upper limit (95% CI)", "Attributable deaths", "Attributable deaths\n lower limit (95% CI)", "Attributable deaths\n upper limit (95% CI)", "CMD deaths registered \nin the national\n death registry")
write.csv(sup2, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Sup2_Proportion_and_deaths_high_BMI_by_region_", Sys.Date(), ".csv"),  row.names = F)

rm(sup2)

#Sup 3
results_standardised_regional_bmi_cat<-read.csv("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_by_region_bmi_cat_2021-08-12.csv")
sup2<-results_standardised_regional_bmi_cat[,1:10]
sup2$paf.median<-round(sup2$paf.median, 2)
sup2$paf.lcl<-round(sup2$paf.lcl, 2)
sup2$paf.ucl<-round(sup2$paf.ucl, 2)

names(sup2)<-c("Region","Year","Sex","BMI category", "Proportion of CMD deaths", "Proportion of CMD\n deaths lower limit (95% CI)", "Proportion of CMD\n deaths upper limit (95% CI)", "Attributable deaths", "Attributable deaths\n lower limit (95% CI)", "Attributable deaths\n upper limit (95% CI)")
write.csv(sup2, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Sup3_Proportion_and_deaths_by_BMI_cat_region_sex_2018_", Sys.Date(),".csv"), row.names = FALSE)





##### SUMMARY FOR RESULTS
setwd("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables")
a<-read.csv("PAF_Deaths_by_region_sex_2015_2020_2021-07-09.csv")
a<-filter(a, year == 2018)
sum(a$Attributable.deaths)


deaths<-read.csv("~/Desktop/Artículos/CRA High BMI Peru/Data/Deaths_NCD_Peru_2021-07-09.csv")
deaths<-filter(deaths, year == 2018)
sum(deaths$deaths)


