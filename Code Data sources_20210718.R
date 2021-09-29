
options(scipen = 999)
library(foreign)
library(readxl)
library(haven)
library(dplyr)
library(survey)
library(meta)
library(metafor)
library(purrr)

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = TRUE)

### READ ALL INDIVIDUAL DATASETS ###
####################################
setwd("~/Desktop/DHS/Peru")


### ENDES 2014 ###
################## 

endes2014 <- read.csv("ENDES2014.csv")


attach(endes2014)
endes2014 <- data.frame(id                   = HHID,
                        psu                  = CONGLOMERADO,
                        stratum              = ESTRATO,
                        sample_weight_15more = PONDERACION,
                        sex                  = HV104,
                        age                  = HV105,
                        region               = HV024,
                        bmi                  = bmi,
                        height               = height,
                        weight               = weight,
                        waist                = NA,
                        year                 = 2014)
detach(endes2014)
endes2014 <- endes2014[which(endes2014$age >= 15),]

endes2014 <- endes2014[which(!is.na(endes2014$bmi)),]

endes2014 <- endes2014[which(endes2014$bmi <= 80),]
endes2014 <- endes2014[which(endes2014$bmi >= 10),]


endes2014$bmi_cat <- ifelse(endes2014$bmi <18.5, 0,
                            ifelse(endes2014$bmi>=18.5 & endes2014$bmi < 25, 1, 
                                   ifelse(endes2014$bmi >= 25 & endes2014$bmi < 30, 2, 
                                          ifelse(endes2014$bmi >= 30 & endes2014$bmi < 35, 3, 
                                                 ifelse(endes2014$bmi >= 35 & endes2014$bmi < 40, 4, 
                                                        ifelse(endes2014$bmi >= 40 , 5 , NA))))))


endes2014$bmi_cat <- factor(endes2014$bmi_cat,
                            levels = c(0,1, 2, 3, 4, 5),
                            labels = c("Under", "Normal", "Ov", "Ob1", "Ob2", "Ob3"))


table(endes2014$bmi_cat, useNA = c("always"))



endes2014$central_ob <- NA


####################################################################################################




### ENDES 2015 ###
################## 

endes2015 <- read.csv("ENDES2015.csv")


attach(endes2015)
endes2015 <- data.frame(id                   = HHID,
                        psu                  = CONGLOMERADO,
                        stratum              = ESTRATO,
                        sample_weight_15more = PONDERACION,
                        sex                  = HV104,
                        age                  = HV105,
                        region               = HV024.x,
                        bmi                  = bmi,
                        height               = height,
                        weight               = weight,
                        waist                = NA,
                        year                 = 2015)
detach(endes2015)
endes2015 <- endes2015[which(endes2015$age >= 15),]

endes2015 <- endes2015[which(!is.na(endes2015$bmi)),]

endes2015 <- endes2015[which(endes2015$bmi <= 80),]
endes2015 <- endes2015[which(endes2015$bmi >= 10),]


endes2015$bmi_cat <- ifelse(endes2015$bmi <18.5, 0,
                            ifelse(endes2015$bmi>=18.5 & endes2015$bmi < 25, 1, 
                                   ifelse(endes2015$bmi >= 25 & endes2015$bmi < 30, 2, 
                                          ifelse(endes2015$bmi >= 30 & endes2015$bmi < 35, 3, 
                                                 ifelse(endes2015$bmi >= 35 & endes2015$bmi < 40, 4, 
                                                        ifelse(endes2015$bmi >= 40 , 5 , NA))))))


endes2015$bmi_cat <- factor(endes2015$bmi_cat,
                            levels = c(0,1, 2, 3, 4, 5),
                            labels = c("Under", "Normal", "Ov", "Ob1", "Ob2", "Ob3"))


table(endes2015$bmi_cat, useNA = c("always"))



endes2015$central_ob <- NA


####################################################################################################


### ENDES 2016 ###
##################
endes2016 <- read.csv("ENDES2016.csv")


attach(endes2016)
endes2016 <- data.frame(id                   = HHID,
                        psu                  = CONGLOMERADO,
                        stratum              = ESTRATO,
                        sample_weight_15more = PONDERACION,
                        sex                  = HV104,
                        age                  = HV105,
                        region               = HV024,
                        bmi                  = bmi,
                        height               = height,
                        weight               = weight,
                        waist                = NA,
                        year                 = 2016)
detach(endes2016)
endes2016 <- endes2016[which(endes2016$age >= 15),]

endes2016 <- endes2016[which(!is.na(endes2016$bmi)),]

endes2016 <- endes2016[which(endes2016$bmi <= 80),]
endes2016 <- endes2016[which(endes2016$bmi >= 10),]


endes2016$bmi_cat <- ifelse(endes2016$bmi <18.5, 0,
                            ifelse(endes2016$bmi>=18.5 & endes2016$bmi < 25, 1, 
                                   ifelse(endes2016$bmi >= 25 & endes2016$bmi < 30, 2, 
                                          ifelse(endes2016$bmi >= 30 & endes2016$bmi < 35, 3, 
                                                 ifelse(endes2016$bmi >= 35 & endes2016$bmi < 40, 4, 
                                                        ifelse(endes2016$bmi >= 40 , 5 , NA))))))


endes2016$bmi_cat <- factor(endes2016$bmi_cat,
                            levels = c(0,1, 2, 3, 4, 5),
                            labels = c("Under", "Normal", "Ov", "Ob1", "Ob2", "Ob3"))


table(endes2016$bmi_cat, useNA = c("always"))



endes2016$central_ob <- NA

####################################################################################################



### ENDES 2017 ###
##################
endes2017 <- read.csv("ENDES2017.csv")


attach(endes2017)
endes2017 <- data.frame(id                   = HHID,
                        psu                  = CONGLOMERADO,
                        stratum              = ESTRATO,
                        sample_weight_15more = PONDERACION,
                        sex                  = HV104,
                        age                  = HV105,
                        region               = HV024,
                        bmi                  = bmi,
                        height               = height,
                        weight               = weight,
                        waist                = NA,
                        year                 = 2017)
detach(endes2017)
endes2017 <- endes2017[which(endes2017$age >= 15),]

endes2017 <- endes2017[which(!is.na(endes2017$bmi)),]

endes2017 <- endes2017[which(endes2017$bmi <= 80),]
endes2017 <- endes2017[which(endes2017$bmi >= 10),]


endes2017$bmi_cat <- ifelse(endes2017$bmi <18.5, 0,
                            ifelse(endes2017$bmi>=18.5 & endes2017$bmi < 25, 1, 
                                   ifelse(endes2017$bmi >= 25 & endes2017$bmi < 30, 2, 
                                          ifelse(endes2017$bmi >= 30 & endes2017$bmi < 35, 3, 
                                                 ifelse(endes2017$bmi >= 35 & endes2017$bmi < 40, 4, 
                                                        ifelse(endes2017$bmi >= 40 , 5 , NA))))))


endes2017$bmi_cat <- factor(endes2017$bmi_cat,
                            levels = c(0,1, 2, 3, 4, 5),
                            labels = c("Under", "Normal", "Ov", "Ob1", "Ob2", "Ob3"))


table(endes2017$bmi_cat, useNA = c("always"))



endes2017$central_ob <- NA


####################################################################################################




### ENDES 2018 ###
##################
endes2018 <- read.csv("ENDES2018.csv")


attach(endes2018)
endes2018 <- data.frame(id                   = HHID,
                        psu                  = CONGLOMERADO,
                        stratum              = ESTRATO,
                        sample_weight_15more = PONDERACION,
                        sex                  = HV104,
                        age                  = HV105,
                        region               = HV024,
                        bmi                  = bmi,
                        height               = height,
                        weight               = weight,
                        waist                = QS907,
                        year                 = 2018)
detach(endes2018)
endes2018 <- endes2018[which(endes2018$age >= 15),]

endes2018 <- endes2018[which(!is.na(endes2018$bmi)),]

endes2018 <- endes2018[which(endes2018$bmi <= 80),]
endes2018 <- endes2018[which(endes2018$bmi >= 10),]


endes2018$bmi_cat <- ifelse(endes2018$bmi <18.5, 0,
                            ifelse(endes2018$bmi>=18.5 & endes2018$bmi < 25, 1, 
                                   ifelse(endes2018$bmi >= 25 & endes2018$bmi < 30, 2, 
                                          ifelse(endes2018$bmi >= 30 & endes2018$bmi < 35, 3, 
                                                 ifelse(endes2018$bmi >= 35 & endes2018$bmi < 40, 4, 
                                                        ifelse(endes2018$bmi >= 40 , 5 , NA))))))


endes2018$bmi_cat <- factor(endes2018$bmi_cat,
                            levels = c(0,1, 2, 3, 4, 5),
                            labels = c("Under", "Normal", "Ov", "Ob1", "Ob2", "Ob3"))


table(endes2018$bmi_cat, useNA = c("always"))



endes2018$central_ob <- ifelse(endes2018$waist >= 90 & endes2018$sex == 1, 1,
                               ifelse(endes2018$waist >= 80 & endes2018$sex == 2, 1, 
                                      ifelse(endes2018$waist < 90 & endes2018$sex == 1, 0,
                                             ifelse(endes2018$waist < 80 & endes2018$sex == 2, 0, NA))))
endes2018$central_ob <- factor(endes2018$central_ob, levels = c(0,1), labels = c("No", "Yes"))
table(endes2018$central_ob, useNA = c("always"))


####################################################################################################

endes_all<-rbind(endes2014,endes2015, endes2016, endes2017, endes2018)


## not possible with 5-year age groups, grouping 10-year age  groups, except for 15-29 and 80 plus
endes_all$agecat <- ifelse(endes_all$age >=15 & endes_all$age <=29, 1, 
                           ifelse(endes_all$age >= 30  & endes_all$age <=39, 2, 
                                  ifelse(endes_all$age >= 40  & endes_all$age <=49, 3,
                                         ifelse(endes_all$age >= 50  & endes_all$age <=59,4,
                                                ifelse(endes_all$age >= 60  & endes_all$age <=69, 5,
                                                       ifelse(endes_all$age >= 70  & endes_all$age <=79, 6,
                                                              ifelse(endes_all$age >= 80, 7, NA)))))))
endes_all$agecat <- factor(endes_all$agecat, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))

table(endes_all$agecat, endes_all$bmi_cat)

## Compute proportions for each BMI range at the region, by sex and both sex combined

d.s                     <- svydesign(id = ~psu, strata = ~stratum, weights = ~sample_weight_15more, data = endes_all, nest = TRUE)   

prevalence_under <- svyby(~bmi_cat=="Under", by = ~region+sex+agecat, design = d.s, FUN = svyciprop, vartype = c('se'), na.rm.all = T)
prevalence_normal     <- svyby(~bmi_cat=="Normal", by = ~region+sex+agecat, design = d.s, FUN = svyciprop, vartype = c('se'), na.rm.all = T)
prevalence_ov     <- svyby(~bmi_cat=="Ov", by = ~region+sex+agecat, design = d.s, FUN = svyciprop, vartype = c('se'), na.rm.all = T)
prevalence_ob1     <- svyby(~bmi_cat=="Ob1", by = ~region+sex+agecat, design = d.s, FUN = svyciprop, vartype = c('se'), na.rm.all = T)
prevalence_ob2     <- svyby(~bmi_cat=="Ob2", by = ~region+sex+agecat, design = d.s, FUN = svyciprop, vartype = c('se'), na.rm.all = T)
prevalence_ob3     <- svyby(~bmi_cat=="Ob3", by = ~region+sex+agecat, design = d.s, FUN = svyciprop, vartype = c('se'), na.rm.all = T)


names(prevalence_under) <- c("region", "sex", "age_cat", "bmi.20", "se.bmi20")
names(prevalence_normal) <- c("region", "sex","age_cat", "bmi.20.25", "se.bmi20.25")
names(prevalence_ov)     <- c("region", "sex", "age_cat", "bmi.25.30", "se.bmi25.30")
names(prevalence_ob1)     <- c("region", "sex", "age_cat", "bmi.30.35", "se.bmi30.35")
names(prevalence_ob2)     <- c( "region", "sex","age_cat",  "bmi.35.40", "se.bmi35.40")
names(prevalence_ob3)     <- c("region", "sex","age_cat",  "bmi.40", "se.bmi40")


prevalence <- merge(prevalence_under,
                    prevalence_normal, 
                    by = c("region", "sex", "age_cat"))

prevalence <- merge(prevalence,
                    prevalence_ov, 
                    by = c("region", "sex", "age_cat"))

prevalence <- merge(prevalence,
                    prevalence_ob1, 
                    by = c("region", "sex", "age_cat"))

prevalence <- merge(prevalence,
                    prevalence_ob2, 
                    by = c("region", "sex", "age_cat"))

prevalence <- merge(prevalence,
                    prevalence_ob3, 
                    by = c("region", "sex", "age_cat"))

prevalence$year <- "2015-2018"


## Save dataset
write.csv(prevalence, paste0("~/Desktop/Artículos/CRA High BMI Peru/Data/DHS/endes_2014_2018_bmi_proportions_", Sys.Date(), ".csv"), row.names = FALSE)
#
rm(list = ls())

###### Simulate prevalence estimates
prev<-read.csv("~/Desktop/Artículos/CRA High BMI Peru/Data/DHS/endes_2014_2018_bmi_proportions_2021-08-11.csv")
## Separating into  5-year age groups
age.table <- read.table(header = TRUE, text = "
                        age_cat age 
                        20-29 20-24 
                        20-29 25-29 
                        30-39 30-34 
                        30-39 35-39 
                        40-49 40-44 
                        40-49 45-49 
                        50-59 50-55 
                        50-59 55-59 
                        60-69 60-64 
                        60-69 65-69 
                        70-79 70-74 
                        70-79 75-79 
                        80+ 80-84 
                        80+ 85plus 
                        ")

prev<-left_join(prev, age.table)

prev$Number<-1000

under<-select(prev, year, region, sex, age, bmi.20, se.bmi20, Number)
normal<-select(prev, year,region, sex, age, bmi.20.25, se.bmi20.25, Number)
ov<-select(prev, year,region, sex, age, bmi.25.30, se.bmi25.30, Number)
ob1<-select(prev, year,region, sex, age, bmi.30.35, se.bmi30.35, Number)
ob2<-select(prev, year,region, sex, age, bmi.35.40, se.bmi35.40, Number)
ob3<-select(prev, year,region, sex, age, bmi.40, se.bmi40, Number)

names(under)[5:6]<-c("val", "se")
names(normal)[5:6]<-c("val", "se")
names(ov)[5:6]<-c("val", "se")
names(ob1)[5:6]<-c("val", "se")
names(ob2)[5:6]<-c("val", "se")
names(ob3)[5:6]<-c("val", "se")

#Prev_20
set.seed(123)
under<- under %>% 
  mutate(rnorms = pmap(list(Number, val, se^2), function(n, mu, sd) rnorm(n, mu, sd)),
         rnorms = map_chr(rnorms, ~ paste(., collapse = " "))) %>% 
  bind_cols(., read.table(text = .$rnorms, sep = " ")) %>%
  select(-rnorms)


under<-under[,-(5:7)]


#Prev_20.25
set.seed(123)
normal<- normal %>% 
  mutate(rnorms = pmap(list(Number, val, se^2), function(n, mu, sd) rnorm(n, mu, sd)),
         rnorms = map_chr(rnorms, ~ paste(., collapse = " "))) %>% 
  bind_cols(., read.table(text = .$rnorms, sep = " ")) %>%
  select(-rnorms)
normal<-normal[,-(5:7)]

#Prev_25.30
set.seed(123)
ov<- ov %>% 
  mutate(rnorms = pmap(list(Number, val, se^2), function(n, mu, sd) rnorm(n, mu, sd)),
         rnorms = map_chr(rnorms, ~ paste(., collapse = " "))) %>% 
  bind_cols(., read.table(text = .$rnorms, sep = " ")) %>%
  select(-rnorms)
ov<-ov[,-(5:7)]

#Prev_30.35
set.seed(123)
ob1<- ob1 %>% 
  mutate(rnorms = pmap(list(Number, val, se^2), function(n, mu, sd) rnorm(n, mu, sd)),
         rnorms = map_chr(rnorms, ~ paste(., collapse = " "))) %>% 
  bind_cols(., read.table(text = .$rnorms, sep = " ")) %>%
  select(-rnorms)
ob1<-ob1[,-(5:7)]

#Prev35.40
set.seed(123)
ob2<- ob2 %>% 
  mutate(rnorms = pmap(list(Number, val, se^2), function(n, mu, sd) rnorm(n, mu, sd)),
         rnorms = map_chr(rnorms, ~ paste(., collapse = " "))) %>% 
  bind_cols(., read.table(text = .$rnorms, sep = " ")) %>%
  select(-rnorms)
ob2<-ob2[,-(5:7)]

#Prev40
set.seed(123)
ob3<- ob3 %>% 
  mutate(rnorms = pmap(list(Number, val, se^2), function(n, mu, sd) rnorm(n, mu, sd)),
         rnorms = map_chr(rnorms, ~ paste(., collapse = " "))) %>% 
  bind_cols(., read.table(text = .$rnorms, sep = " ")) %>%
  select(-rnorms)
ob3<-ob3[,-(5:7)]


### Now combine all BMI categories

bmi <- bind_rows(
  mutate(under, category = "TEEN"),
  mutate(normal, category = "TWEN"),
  mutate(ov, category = "TFIVE"),
  mutate(ob1, category = "THIRT"),
  mutate(ob2, category = "THFIVE"),
  mutate(ob3, category = "FORT")
)

write.csv(bmi, paste0("~/Desktop/Artículos/CRA High BMI Peru/Data/BMI_prevalence_by_region_sex_age_1000sims",Sys.Date(),".csv"), row.names = FALSE)
####################################################################################################

###

##### RELATIVE RISKS ###### 
rm(list = ls())


###SIMULATE AGE-SPECIFIC RRs
setwd("~/Desktop/Artículos/CRA High BMI Peru/Data")

##Men
rr<-read.csv("RRs_men.csv")
rr<-rr %>% mutate(outcome = ifelse(outcome == "Intracerebral hemorrhage", "Haemorrhagic stroke", ifelse(outcome == "Subarachnoid hemorrhage", "Haemorrhagic stroke",  outcome)))
rr<-unique(rr)

rr<-data.table(rr)
rr[, Number:= rep(1000, times = 480)]
rr[, RR:= log(RR)]
rr[, RR.upper:= log(RR.upper)]
rr[, RR.lower:= log(RR.lower)]
rr[, se:= (RR.upper-RR.lower)/3.92] #Cochrane formula for se: https://training.cochrane.org/node/1152


set.seed(123)
rr<- rr%>% 
  mutate(rnorms = pmap(list(Number, RR, se^2), function(n, mu, sd) rnorm(n, mu, sd)),
         rnorms = map_chr(rnorms, ~ paste(., collapse = " "))) %>% 
  bind_cols(., read.table(text = .$rnorms, sep = " ")) %>%
  select(-rnorms)
rr<-rr[,-(3:7)]

names(rr)<-c("outcome", "age", paste0("V.",c(1:1000)))
rr<-data.table(rr)
rr<-melt(rr, id.vars = c("outcome", "age"), 
         variable.name = "V", value.name = "rr")
rr<-data.table(rr)
rr[, draw:=as.numeric(substring(V, first = 3))]
rr[,rr:= exp(rr)]
rr<-select(rr, outcome, age,rr, draw)
rr$sex<-1

rr_men<-rr
rm(rr)

setwd("~/Desktop/Artículos/CRA High BMI Peru/Data")

##Women
rr<-read.csv("RRs_women.csv")
rr<-rr %>% mutate(outcome = ifelse(outcome == "Intracerebral hemorrhage", "Haemorrhagic stroke", ifelse(outcome == "Subarachnoid hemorrhage", "Haemorrhagic stroke",  outcome)))
rr<-unique(rr)

rr<-data.table(rr)
rr[, Number:= rep(1000, times = 528)]
rr[, RR:= log(RR)]
rr[, RR.upper:= log(RR.upper)]
rr[, RR.lower:= log(RR.lower)]
rr[, se:= (RR.upper-RR.lower)/3.92]
set.seed(123)
rr<- rr%>% 
  mutate(rnorms = pmap(list(Number, RR, se^2), function(n, mu, sd) rnorm(n, mu, sd)),
         rnorms = map_chr(rnorms, ~ paste(., collapse = " "))) %>% 
  bind_cols(., read.table(text = .$rnorms, sep = " ")) %>%
  select(-rnorms)
rr<-rr[,-(3:7)]

names(rr)<-c("outcome", "age", paste0("V.",c(1:1000)))
rr<-data.table(rr)
rr<-melt(rr, id.vars = c("outcome", "age"), 
         variable.name = "V", value.name = "rr")
rr<-data.table(rr)
rr[, draw:=as.numeric(substring(V, first = 3))]
rr[,rr:= exp(rr)]
rr<-select(rr, outcome, age,rr, draw)
rr$sex<-2
rr_women<-rr

rr<-rbind(rr_men, rr_women)

write.csv(rr, paste0("~/Desktop/Artículos/CRA High BMI Peru/Data/RRs_metaanalisis_1000sims_by_sex_age",".csv"), row.names = FALSE)


####################################################################################################
rm(list=ls())



