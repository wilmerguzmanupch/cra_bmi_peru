library(data.table)
library(dplyr)
library(purrr)
library(tidyr)
options(scipen=999)

setwd("~/Desktop/Artículos/CRA High BMI Peru/Data")


# Manually define table to join dm to deaths age groups
age.table <- read.table(header = TRUE, text = "
                        dm.age deaths.age 
                        20 20 
                        20 25 
                        30 30 
                        30 35 
                        40 40 
                        40 45 
                        50 50 
                        50 55 
                        60 60 
                        60 65 
                        70 70 
                        70 75 
                        80 80 
                        80 85 
                        ")

#Load BMI prevalences (1000 simulations)

bmi<-read.csv("BMI_prevalence_by_region_sex_age_1000sims2021-08-11.csv")
bmi<-data.table(bmi)

bmi$age[bmi$age == "20-24"] <- 20
bmi$age[bmi$age == "25-29"] <- 25
bmi$age[bmi$age == "30-34"] <- 30
bmi$age[bmi$age == "35-39"] <- 35
bmi$age[bmi$age == "40-44"] <- 40
bmi$age[bmi$age == "45-49"] <- 45
bmi$age[bmi$age == "50-55"] <- 50
bmi$age[bmi$age == "55-59"] <- 55
bmi$age[bmi$age == "60-64"] <- 60
bmi$age[bmi$age == "65-69"] <- 65
bmi$age[bmi$age == "70-74"] <- 70
bmi$age[bmi$age == "75-79"] <- 75
bmi$age[bmi$age == "80-84"] <- 80
bmi$age[bmi$age == "85plus"] <- 85

names(bmi)<-c("year","region", "sex", "dm.age", paste0("V.",c(1:1000)), "category")
dt<-melt(bmi, id.vars = c("year","region", "sex","dm.age", "category"), 
         variable.name = "V", value.name = "prev")
dt<-data.table(dt)

dt[,draw:=as.numeric(substring(V, first = 3))]
dt<-dt[,-(6)]
## wide format
dt <- spread(dt, category, prev)
dt$dm.age<-as.numeric(dt$dm.age)
dt <- dt %>%
  left_join(age.table, by = "dm.age")  %>%
  dplyr::select(-dm.age)

### Load RRs  (1000 simulations)
rr<-read.csv("RRs_metaanalisis_1000sims_by_sex_age.csv")
rr <- spread(rr, outcome, rr)
rr<-data.table(rr)

rr <- rr[which(!rr$age == "90 to 94"),]
rr <- rr[which(!rr$age == "95 plus"),]

rr$age[rr$age == "20 to 24"] <- 20
rr$age[rr$age == "25 to 29"] <- 25
rr$age[rr$age == "30 to 34"] <- 30
rr$age[rr$age == "35 to 39"] <- 35
rr$age[rr$age == "40 to 44"] <- 40
rr$age[rr$age == "45 to 49"] <- 45
rr$age[rr$age == "50 to 54"] <- 50
rr$age[rr$age == "55 to 59"] <- 55
rr$age[rr$age == "60 to 64"] <- 60
rr$age[rr$age == "65 to 69"] <- 65
rr$age[rr$age == "70 to 74"] <- 70
rr$age[rr$age == "75 to 79"] <- 75
rr$age[rr$age == "80 to 84"] <- 80
rr$age[rr$age == "85 to 89"] <- 85

colnames(rr)[1]<-"deaths.age"
rr$deaths.age<-as.numeric(rr$deaths.age)

#set keys for both dataset
setkey(dt, sex, deaths.age, draw)
setkey(rr, sex, deaths.age, draw)

# create new dataset as result of joining both tables
pafbmi.dt<-dt[rr] 

#Now we have a dataset with all BMI prevalence and RRs simulations

#######################################################################################################################

##CALCULATE PAF FOR EACH OUTCOME
#Atrial Fibrillation and flutter
pafbmi.dt[, num:=((TEEN * 1) + (TWEN * 1) + (TFIVE *`Atrial fibrillation and flutter`) + (THIRT *`Atrial fibrillation and flutter`^2) + 
                    (THFIVE *`Atrial fibrillation and flutter`^3) + (FORT *`Atrial fibrillation and flutter`^4) -1)]

pafbmi.dt[, denom:=(TEEN * 1) + (TWEN * 1) + (TFIVE *`Atrial fibrillation and flutter`) + (THIRT * `Atrial fibrillation and flutter`^2) + 
            (THFIVE * `Atrial fibrillation and flutter`^3) + (FORT * `Atrial fibrillation and flutter`^4)]

pafbmi.dt[, paf_af:=num/denom] 


#Diabetes mellitus type 2 

pafbmi.dt[, num:=((TEEN * 1) + (TWEN * 1) + (TFIVE *`Diabetes mellitus type 2`) + (THIRT *`Diabetes mellitus type 2`^2) + 
                    (THFIVE *`Diabetes mellitus type 2`^3) + (FORT *`Diabetes mellitus type 2`^4) -1)]

pafbmi.dt[, denom:=(TEEN * 1) + (TWEN * 1) + (TFIVE *`Diabetes mellitus type 2`) + (THIRT * `Diabetes mellitus type 2`^2) + 
            (THFIVE * `Diabetes mellitus type 2`^3) + (FORT * `Diabetes mellitus type 2`^4)]

pafbmi.dt[, paf_DM:=num/denom] 


#Hypertensive heart disease

pafbmi.dt[, num:=((TEEN * 1) + (TWEN * 1) + (TFIVE *`Hypertensive heart disease`) + (THIRT *`Hypertensive heart disease`^2) + 
                    (THFIVE *`Hypertensive heart disease`^3) + (FORT *`Hypertensive heart disease`^4) -1)]

pafbmi.dt[, denom:=(TEEN * 1) + (TWEN * 1) + (TFIVE *`Hypertensive heart disease`) + (THIRT * `Hypertensive heart disease`^2) + 
            (THFIVE * `Hypertensive heart disease`^3) + (FORT * `Hypertensive heart disease`^4)]

pafbmi.dt[, paf_HHD:=num/denom] 


#Haemorrhagic stroke

pafbmi.dt[, num:=((TEEN * 1) + (TWEN * 1) + (TFIVE *`Haemorrhagic stroke`) + (THIRT *`Haemorrhagic stroke`^2) + 
                    (THFIVE *`Haemorrhagic stroke`^3) + (FORT *`Haemorrhagic stroke`^4) -1)]

pafbmi.dt[, denom:=(TEEN * 1) + (TWEN * 1) + (TFIVE *`Haemorrhagic stroke`) + (THIRT * `Haemorrhagic stroke`^2) + 
            (THFIVE * `Haemorrhagic stroke`^3) + (FORT * `Haemorrhagic stroke`^4)]

pafbmi.dt[, paf_h_stroke:=num/denom] 

#Ischaemic stroke

pafbmi.dt[, num:=((TEEN * 1) + (TWEN * 1) + (TFIVE *`Ischaemic stroke`) + (THIRT *`Ischaemic stroke`^2) + 
                    (THFIVE *`Ischaemic stroke`^3) + (FORT *`Ischaemic stroke`^4) -1)]

pafbmi.dt[, denom:=(TEEN * 1) + (TWEN * 1) + (TFIVE *`Ischaemic stroke`) + (THIRT * `Ischaemic stroke`^2) + 
            (THFIVE * `Ischaemic stroke`^3) + (FORT * `Ischaemic stroke`^4)]

pafbmi.dt[, paf_isch_stroke:=num/denom] 


#Ischemic heart disease

pafbmi.dt[, num:=((TEEN * 1) + (TWEN * 1) + (TFIVE *`Ischaemic heart disease`) + (THIRT *`Ischaemic heart disease`^2) + 
                    (THFIVE *`Ischaemic heart disease`^3) + (FORT *`Ischaemic heart disease`^4) -1)]

pafbmi.dt[, denom:=(TEEN * 1) + (TWEN * 1) + (TFIVE *`Ischaemic heart disease`) + (THIRT * `Ischaemic heart disease`^2) + 
            (THFIVE * `Ischaemic heart disease`^3) + (FORT * `Ischaemic heart disease`^4)]

pafbmi.dt[, paf_IHD:=num/denom] 


############################################################################################################################################

## summary stats  
paf_results<-pafbmi.dt[,list(paf.median_af=median(paf_af), paf.lcl_af=quantile(paf_af, 0.025), paf.ucl_af=quantile(paf_af, 0.975),
                             paf.median_DM=median(paf_DM), paf.lcl_DM=quantile(paf_DM, 0.025), paf.ucl_DM=quantile(paf_DM, 0.975),
                             paf.median_HHD=median(paf_HHD), paf.lcl_HHD=quantile(paf_HHD, 0.025), paf.ucl_HHD=quantile(paf_HHD, 0.975),
                             paf.median_h_stroke=median(paf_h_stroke), paf.lcl_h_stroke=quantile(paf_h_stroke, 0.025), paf.ucl_h_stroke=quantile(paf_h_stroke, 0.975),
                             paf.median_isch_stroke=median(paf_isch_stroke), paf.lcl_isch_stroke=quantile(paf_isch_stroke, 0.025), paf.ucl_isch_stroke=quantile(paf_isch_stroke, 0.975),
                             paf.median_IHD=median(paf_IHD), paf.lcl_IHD=quantile(paf_IHD, 0.025), paf.ucl_IHD=quantile(paf_IHD, 0.975)
), 
by=list(year, region, sex, deaths.age)]   

#convert negative PAF to 0
paf_results$paf.lcl_af[paf_results$paf.lcl_af<0] <- 0
paf_results$paf.lcl_DM[paf_results$paf.lcl_DM<0] <- 0
paf_results$paf.lcl_HHD[paf_results$paf.lcl_HHD<0] <- 0
paf_results$paf.lcl_h_stroke[paf_results$paf.lcl_h_stroke<0] <- 0
paf_results$paf.lcl_isch_stroke[paf_results$paf.lcl_isch_stroke<0] <- 0
paf_results$paf.lcl_IHD[paf_results$paf.lcl_IHD<0] <- 0

#Load NCD deaths

deaths<-read.csv("Deaths_NCD_Peru_2021-08-11.csv")
deaths<-spread(deaths, outcome, deaths)
deaths$sex<-ifelse(deaths$sex == "MASCULINO", 1, 2)
deaths<-data.table(deaths)

deaths$agecat[deaths$agecat == "20-24"] <- 20
deaths$agecat[deaths$agecat == "25-29"] <- 25
deaths$agecat[deaths$agecat == "30-34"] <- 30
deaths$agecat[deaths$agecat == "35-39"] <- 35
deaths$agecat[deaths$agecat == "40-44"] <- 40
deaths$agecat[deaths$agecat == "45-49"] <- 45
deaths$agecat[deaths$agecat == "50-54"] <- 50
deaths$agecat[deaths$agecat == "55-59"] <- 55
deaths$agecat[deaths$agecat == "60-64"] <- 60
deaths$agecat[deaths$agecat == "65-69"] <- 65
deaths$agecat[deaths$agecat == "70-74"] <- 70
deaths$agecat[deaths$agecat == "75-79"] <- 75
deaths$agecat[deaths$agecat == "80-84"] <- 80
deaths$agecat[deaths$agecat == "85+"] <- 85


colnames(deaths)[4]<-"deaths.age"
deaths$deaths.age<-as.numeric(deaths$deaths.age)


#change regions
paf_results$region<-ifelse(paf_results$region == 1, "AMAZONAS", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 2, "ANCASH", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 3, "APURIMAC", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 4, "AREQUIPA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 5, "AYACUCHO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 6, "CAJAMARCA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 7, "CALLAO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 8, "CUSCO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 9, "HUANCAVELICA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 10, "HUANUCO",paf_results$region)
paf_results$region<-ifelse(paf_results$region == 11, "ICA",paf_results$region)
paf_results$region<-ifelse(paf_results$region == 12, "JUNIN", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 13, "LA LIBERTAD", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 14, "LAMBAYEQUE", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 15, "LIMA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 16, "LORETO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 17, "MADRE DE DIOS", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 18, "MOQUEGUA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 19, "PASCO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 20, "PIURA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 21, "PUNO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 22, "SAN MARTIN", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 23, "TACNA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 24, "TUMBES", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 25, "UCAYALI", paf_results$region)


paf_results$year<-2018
paf_results$year<-as.numeric(paf_results$year)

#set keys for both dataset
setkey(paf_results, year, region, sex, deaths.age)
setkey(deaths, year, region, sex, deaths.age)

# create new dataset as result of joining both tables
results<-left_join(paf_results, deaths)

# calculate attributable deaths (AD) for each outcome
results$AD_AF<-round(results$paf.median_af*results$AF)
results$AD_AF_l<-round(results$paf.lcl_af*results$AF)
results$AD_AF_u<-round(results$paf.ucl_af*results$AF)

results$AD_DM<-round(results$paf.median_DM*results$DM)
results$AD_DM_l<-round(results$paf.lcl_DM*results$DM)
results$AD_DM_u<-round(results$paf.ucl_DM*results$DM)

results$AD_HHD<-round(results$paf.median_HHD*results$HHD)
results$AD_HHD_l<-round(results$paf.lcl_HHD*results$HHD)
results$AD_HHD_u<-round(results$paf.ucl_HHD*results$HHD)

results$AD_h_stroke<-round(results$paf.median_h_stroke*results$`Haemorrhagic stroke`)
results$AD_h_stroke_l<-round(results$paf.lcl_h_stroke*results$`Haemorrhagic stroke`)
results$AD_h_stroke_u<-round(results$paf.ucl_h_stroke*results$`Haemorrhagic stroke`)

results$AD_isch_stroke<-round(results$paf.median_isch_stroke*results$`Ischemic stroke`)
results$AD_isch_stroke_l<-round(results$paf.lcl_isch_stroke*results$`Ischemic stroke`)
results$AD_isch_stroke_u<-round(results$paf.ucl_isch_stroke*results$`Ischemic stroke`)

results$AD_IHD<-round(results$paf.median_IHD*results$IHD)
results$AD_IHD_l<-round(results$paf.lcl_IHD*results$IHD)
results$AD_IHD_u<-round(results$paf.ucl_IHD*results$IHD)



############################################################################################################
#all in one column
results_AF<-select(results, 1:4, paf.median_af,paf.lcl_af, paf.ucl_af, AD_AF, AD_AF_l, AD_AF_u)
results_DM<-select(results, 1:4, paf.median_DM,paf.lcl_DM, paf.ucl_DM, AD_DM, AD_DM_l, AD_DM_u)
results_HHD<-select(results, 1:4, paf.median_HHD,paf.lcl_HHD, paf.ucl_HHD, AD_HHD, AD_HHD_l, AD_HHD_u)
results_isch_stroke<-select(results, 1:4, paf.median_isch_stroke,paf.lcl_isch_stroke, paf.ucl_isch_stroke, AD_isch_stroke, AD_isch_stroke_l, AD_isch_stroke_u)
results_IHD<-select(results, 1:4, paf.median_IHD,paf.lcl_IHD, paf.ucl_IHD, AD_IHD, AD_IHD_l, AD_IHD_u)
results_h_stroke<-select(results, 1:4, paf.median_h_stroke,paf.lcl_h_stroke, paf.ucl_h_stroke, AD_h_stroke, AD_h_stroke_l, AD_h_stroke_u)

results_AF$outcome<- "Atrial fibrillation and flutter"
results_DM$outcome<- "Type 2 diabetes mellitus"
results_HHD$outcome<- "Hypertensive heart disease"
results_isch_stroke$outcome<- "Ischaemic stroke"
results_IHD$outcome<- "Ischaemic heart disease"
results_h_stroke$outcome<- "Haemorrhagic stroke"

names(results_AF)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_DM)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_HHD)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_isch_stroke)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_IHD)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_h_stroke)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")


rm(dt, paf_results, pafbmi.dt, deaths, bmi, rr, results, age.table)

results = sapply(.GlobalEnv, is.data.frame) 
results<-do.call(rbind, mget(names(results)[results]))

rm(list=setdiff(ls(), c("results")))

#results
results<-arrange(results, year, region, sex, deaths.age, outcome)
results$sex <- factor(results$sex,
                      levels = c(1,2),
                      labels = c("Men", "Women"))

##Join deaths to standardize paf
deaths<-read.csv("Deaths_NCD_Peru_2021-08-11.csv")
deaths<-filter(deaths, year == 2018)
deaths<-data.table(deaths)

deaths$agecat[deaths$agecat == "20-24"] <- 20
deaths$agecat[deaths$agecat == "25-29"] <- 25
deaths$agecat[deaths$agecat == "30-34"] <- 30
deaths$agecat[deaths$agecat == "35-39"] <- 35
deaths$agecat[deaths$agecat == "40-44"] <- 40
deaths$agecat[deaths$agecat == "45-49"] <- 45
deaths$agecat[deaths$agecat == "50-54"] <- 50
deaths$agecat[deaths$agecat == "55-59"] <- 55
deaths$agecat[deaths$agecat == "60-64"] <- 60
deaths$agecat[deaths$agecat == "65-69"] <- 65
deaths$agecat[deaths$agecat == "70-74"] <- 70
deaths$agecat[deaths$agecat == "75-79"] <- 75
deaths$agecat[deaths$agecat == "80-84"] <- 80
deaths$agecat[deaths$agecat == "85+"] <- 85

names(deaths)[4]<-"deaths.age"
deaths$deaths.age<-as.numeric(deaths$deaths.age)

deaths$sex<-ifelse(deaths$sex == "MASCULINO", 1, 2)
deaths$sex <- factor(deaths$sex,
                     levels = c(1,2),
                     labels = c("Men", "Women"))


deaths$outcome[deaths$outcome == "AF"] <- "Atrial fibrillation and flutter"
deaths$outcome[deaths$outcome == "DM"] <- "Type 2 diabetes mellitus"
deaths$outcome[deaths$outcome == "HHD"] <- "Hypertensive heart disease"
deaths$outcome[deaths$outcome == "IHD"] <- "Ischaemic heart disease"
deaths$outcome[deaths$outcome == "Ischemic stroke"] <- "Ischaemic stroke"
deaths$outcome[deaths$outcome == "Haemorrhagic stroke"] <- "Haemorrhagic stroke"

#set keys for both dataset
setkey(results, year, region, sex, deaths.age, outcome)
setkey(deaths, year, region, sex, deaths.age, outcome)

# create new dataset as result of joining both tables
results<-left_join(results, deaths)

## joining a column of CVD and T2DM 
results <- results %>% mutate("NCD" = ifelse(outcome =="Hypertensive heart disease"| outcome =="Atrial fibrillation and flutter"| outcome =="Ischaemic heart disease" | 
                                   outcome =="Ischaemic stroke"| outcome =="Haemorrhagic stroke","Cardiovascular diseases", ifelse(outcome == "Type 2 diabetes mellitus" , "Type 2 diabetes",NA)))

#individual outcomes by region, sex
results_standardised_regional_outcome<-results[, list(paf.median=sum(AD)/sum(deaths), 
                                                      paf.lcl=sum(AD.lower)/sum(deaths), 
                                                      paf.ucl=sum(AD.upper)/sum(deaths),
                                                      Attributable.deaths.val=sum(AD), 
                                     Attributable.deaths.lower=sum(AD.lower),
                                     Attributable.deaths.upper=sum(AD.upper), deaths = sum(deaths),  NCD = first(NCD)),
                              by=list(region, year, sex, outcome)]  

#paf.median=weighted.mean(paf.median, w= deaths), 
#paf.lcl=weighted.mean(paf.lower, w= deaths),
#paf.ucl=weighted.mean(paf.upper, w= deaths),

#CMD results by region, sex
results_standardised_regional<-results_standardised_regional_outcome[, list(paf.median=sum(Attributable.deaths.val)/sum(deaths), 
                                                               paf.lcl=sum(Attributable.deaths.lower)/sum(deaths), 
                                                               paf.ucl=sum(Attributable.deaths.upper)/sum(deaths),
                                                               Attributable.deaths.val=sum(Attributable.deaths.val), 
                                                               Attributable.deaths.lower=sum(Attributable.deaths.lower),
                                                               Attributable.deaths.upper=sum(Attributable.deaths.upper), deaths = sum(deaths)),
                                                        by=list(region, year, sex)]  

#Individual outcomes at the national level by sex
results_standardised_national_outcome<-results_standardised_regional_outcome[, list(paf.median=sum(Attributable.deaths.val)/sum(deaths), 
                                              paf.lcl=sum(Attributable.deaths.lower)/sum(deaths), 
                                              paf.ucl=sum(Attributable.deaths.upper)/sum(deaths),
                                              Attributable.deaths.val=sum(Attributable.deaths.val), 
                                              Attributable.deaths.lower=sum(Attributable.deaths.lower),
                                              Attributable.deaths.upper=sum(Attributable.deaths.upper), deaths = sum(deaths)),
                                       by=list(year, sex, outcome)] 

#NCD outcomes at the national level by sex
results_standardised_national_NCD<-results_standardised_regional_outcome[, list(paf.median=sum(Attributable.deaths.val)/sum(deaths), 
                                                                                paf.lcl=sum(Attributable.deaths.lower)/sum(deaths), 
                                                                                paf.ucl=sum(Attributable.deaths.upper)/sum(deaths),
                                                                                Attributable.deaths.val=sum(Attributable.deaths.val), 
                                                                                Attributable.deaths.lower=sum(Attributable.deaths.lower),
                                                                                Attributable.deaths.upper=sum(Attributable.deaths.upper), deaths = sum(deaths)),
                                                                         by=list(year, sex, NCD)]  

#CMD at the national level by sex
results_standardised_national<-results_standardised_regional_outcome[, list(paf.median=sum(Attributable.deaths.val)/sum(deaths), 
                                               paf.lcl=sum(Attributable.deaths.lower)/sum(deaths), 
                                               paf.ucl=sum(Attributable.deaths.upper)/sum(deaths),
                                              Attributable.deaths.val=sum(Attributable.deaths.val), 
                                              Attributable.deaths.lower=sum(Attributable.deaths.lower),
                                              Attributable.deaths.upper=sum(Attributable.deaths.upper), deaths = sum(deaths)),
                                       by=list(year, sex)]  


##Write csv of results
write.csv(results_standardised_regional_outcome, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_by_region_outcome_", Sys.Date(), ".csv"),  row.names = F)
write.csv(results_standardised_regional, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_by_region_", Sys.Date(), ".csv"),  row.names = F)
write.csv(results_standardised_national_outcome, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_national_by_outcome_", Sys.Date(), ".csv"),  row.names = F)
write.csv(results_standardised_national_NCD, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_national_by_NCD_category_", Sys.Date(), ".csv"),  row.names = F)
write.csv(results_standardised_national, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_national_", Sys.Date(), ".csv"),  row.names = F)
####




####################################
rm(list=ls())

#### ANALYSIS BY EACH BMI CATEGORY

setwd("~/Desktop/Artículos/CRA High BMI Peru/Data")

# Manually define table to join dm to deaths age groups
age.table <- read.table(header = TRUE, text = "
                        dm.age deaths.age 
                        20 20 
                        20 25 
                        30 30 
                        30 35 
                        40 40 
                        40 45 
                        50 50 
                        50 55 
                        60 60 
                        60 65 
                        70 70 
                        70 75 
                        80 80 
                        80 85 
                        ")

#Load BMI prevalences (1000 simulations)

bmi<-read.csv("BMI_prevalence_by_region_sex_age_1000sims2021-08-11.csv")
bmi<-data.table(bmi)

bmi$age[bmi$age == "20-24"] <- 20
bmi$age[bmi$age == "25-29"] <- 25
bmi$age[bmi$age == "30-34"] <- 30
bmi$age[bmi$age == "35-39"] <- 35
bmi$age[bmi$age == "40-44"] <- 40
bmi$age[bmi$age == "45-49"] <- 45
bmi$age[bmi$age == "50-55"] <- 50
bmi$age[bmi$age == "55-59"] <- 55
bmi$age[bmi$age == "60-64"] <- 60
bmi$age[bmi$age == "65-69"] <- 65
bmi$age[bmi$age == "70-74"] <- 70
bmi$age[bmi$age == "75-79"] <- 75
bmi$age[bmi$age == "80-84"] <- 80
bmi$age[bmi$age == "85plus"] <- 85

names(bmi)<-c("year","region", "sex","dm.age", paste0("V.",c(1:1000)), "category")
dt<-melt(bmi, id.vars = c("year","region", "sex","dm.age", "category"), 
         variable.name = "V", value.name = "prev")
dt<-data.table(dt)
dt[,draw:=as.numeric(substring(V, first = 3))]
dt<-dt[,-(6)]
## wide format
dt <- spread(dt, category, prev)

dt$dm.age<-as.numeric(dt$dm.age)
dt <- dt %>%
  left_join(age.table, by = "dm.age")  %>%
  dplyr::select(-dm.age)

### Load RRs  (1000 simulations)
rr<-read.csv("RRs_metaanalisis_1000sims_by_sex_age.csv")
rr <- spread(rr, outcome, rr)
rr<-data.table(rr)


rr <- rr[which(!rr$age == "90 to 94"),]
rr <- rr[which(!rr$age == "95 plus"),]

rr$age[rr$age == "20 to 24"] <- 20
rr$age[rr$age == "25 to 29"] <- 25
rr$age[rr$age == "30 to 34"] <- 30
rr$age[rr$age == "35 to 39"] <- 35
rr$age[rr$age == "40 to 44"] <- 40
rr$age[rr$age == "45 to 49"] <- 45
rr$age[rr$age == "50 to 54"] <- 50
rr$age[rr$age == "55 to 59"] <- 55
rr$age[rr$age == "60 to 64"] <- 60
rr$age[rr$age == "65 to 69"] <- 65
rr$age[rr$age == "70 to 74"] <- 70
rr$age[rr$age == "75 to 79"] <- 75
rr$age[rr$age == "80 to 84"] <- 80
rr$age[rr$age == "85 to 89"] <- 85

colnames(rr)[1]<-"deaths.age"
rr$deaths.age<-as.numeric(rr$deaths.age)


#set keys for both dataset
setkey(dt, sex, deaths.age, draw)
setkey(rr, sex, deaths.age, draw)

# create new dataset as result of joining both tables
pafbmi.dt<-dt[rr] 

#Now we have a dataset with all BMI prevalence and RRs simulations
#######################################################################################################################

##CALCULATE PAF FOR EACH BMI CATEGORY IN EACH NCD
#Atrial Fibrillation and flutter
pafbmi.dt[, paf_AF_25_30:=TFIVE * (`Atrial fibrillation and flutter`-1)/(TFIVE*(`Atrial fibrillation and flutter`-1)+1)]  #Overweight
pafbmi.dt[, paf_AF_30_35:=THIRT * ((`Atrial fibrillation and flutter`^2)-1)/(THIRT*((`Atrial fibrillation and flutter`^2)-1)+1)]  #Obesity 1
pafbmi.dt[, paf_AF_35_40:=THFIVE* ((`Atrial fibrillation and flutter`^3)-1)/(THFIVE*((`Atrial fibrillation and flutter`^3)-1)+1)]  #Obesity 2
pafbmi.dt[, paf_AF_40:=FORT * ((`Atrial fibrillation and flutter`^4)-1)/(FORT*((`Atrial fibrillation and flutter`^4)-1)+1)]  #Obesity 3

#Diabetes mellitus type 2 
pafbmi.dt[, paf_DM_25_30:=TFIVE * (`Diabetes mellitus type 2`-1)/(TFIVE*(`Diabetes mellitus type 2`-1)+1)]  #Overweight
pafbmi.dt[, paf_DM_30_35:=THIRT * ((`Diabetes mellitus type 2`^2)-1)/(THIRT*((`Diabetes mellitus type 2`^2)-1)+1)]  #Obesity 1
pafbmi.dt[, paf_DM_35_40:=THFIVE* ((`Diabetes mellitus type 2`^3)-1)/(THFIVE*((`Diabetes mellitus type 2`^3)-1)+1)]  #Obesity 2
pafbmi.dt[, paf_DM_40:=FORT * ((`Diabetes mellitus type 2`^4)-1)/(FORT*((`Diabetes mellitus type 2`^4)-1)+1)]  #Obesity 3

#Hypertensive heart disease
pafbmi.dt[, paf_HHD_25_30:=TFIVE * (`Hypertensive heart disease`-1)/(TFIVE*(`Hypertensive heart disease`-1)+1)]  #Overweight
pafbmi.dt[, paf_HHD_30_35:=THIRT * ((`Hypertensive heart disease`^2)-1)/(THIRT*((`Hypertensive heart disease`^2)-1)+1)]  #Obesity 1
pafbmi.dt[, paf_HHD_35_40:=THFIVE* ((`Hypertensive heart disease`^3)-1)/(THFIVE*((`Hypertensive heart disease`^3)-1)+1)]  #Obesity 2
pafbmi.dt[, paf_HHD_40:=FORT * ((`Hypertensive heart disease`^4)-1)/(FORT*((`Hypertensive heart disease`^4)-1)+1)]  #Obesity 3

#Haemorrhagic stroke
pafbmi.dt[, paf_h_stroke_25_30:=TFIVE * (`Haemorrhagic stroke`-1)/(TFIVE*(`Haemorrhagic stroke`-1)+1)]  #Overweight
pafbmi.dt[, paf_h_stroke_30_35:=THIRT * ((`Haemorrhagic stroke`^2)-1)/(THIRT*((`Haemorrhagic stroke`^2)-1)+1)]  #Obesity 1
pafbmi.dt[, paf_h_stroke_35_40:=THFIVE* ((`Haemorrhagic stroke`^3)-1)/(THFIVE*((`Haemorrhagic stroke`^3)-1)+1)]  #Obesity 2
pafbmi.dt[, paf_h_stroke_40:=FORT * ((`Haemorrhagic stroke`^4)-1)/(FORT*((`Haemorrhagic stroke`^4)-1)+1)]  #Obesity 3

#Ischaemic stroke
pafbmi.dt[, paf_isch_stroke_25_30:=TFIVE * (`Ischaemic stroke`-1)/(TFIVE*(`Ischaemic stroke`-1)+1)]  #Overweight
pafbmi.dt[, paf_isch_stroke_30_35:=THIRT * ((`Ischaemic stroke`^2)-1)/(THIRT*((`Ischaemic stroke`^2)-1)+1)]  #Obesity 1
pafbmi.dt[, paf_isch_stroke_35_40:=THFIVE* ((`Ischaemic stroke`^3)-1)/(THFIVE*((`Ischaemic stroke`^3)-1)+1)]  #Obesity 2
pafbmi.dt[, paf_isch_stroke_40:=FORT * ((`Ischaemic stroke`^4)-1)/(FORT*((`Ischaemic stroke`^4)-1)+1)]  #Obesity 3

#Ischemic heart disease
pafbmi.dt[, paf_IHD_25_30:=TFIVE * (`Ischaemic heart disease`-1)/(TFIVE*(`Ischaemic heart disease`-1)+1)]  #Overweight
pafbmi.dt[, paf_IHD_30_35:=THIRT * ((`Ischaemic heart disease`^2)-1)/(THIRT*((`Ischaemic heart disease`^2)-1)+1)]  #Obesity 1
pafbmi.dt[, paf_IHD_35_40:=THFIVE* ((`Ischaemic heart disease`^3)-1)/(THFIVE*((`Ischaemic heart disease`^3)-1)+1)]  #Obesity 2
pafbmi.dt[, paf_IHD_40:=FORT * ((`Ischaemic heart disease`^4)-1)/(FORT*((`Ischaemic heart disease`^4)-1)+1)]  #Obesity 3

############################################################################################################################################

## summary stats  
paf_results<-pafbmi.dt[,list(paf.median_af_25_30=median(paf_AF_25_30), paf.lcl_af_25_30=quantile(paf_AF_25_30, 0.025), paf.ucl_af_25_30=quantile(paf_AF_25_30, 0.975),
                             paf.median_af_30_35=median(paf_AF_30_35), paf.lcl_af_30_35=quantile(paf_AF_30_35, 0.025), paf.ucl_af_30_35=quantile(paf_AF_30_35, 0.975),
                             paf.median_af_35_40=median(paf_AF_35_40), paf.lcl_af_35_40=quantile(paf_AF_35_40, 0.025), paf.ucl_af_35_40=quantile(paf_AF_35_40, 0.975),
                             paf.median_af_40=median(paf_AF_40), paf.lcl_af_40=quantile(paf_AF_40, 0.025), paf.ucl_af_40=quantile(paf_AF_40, 0.975),
                             paf.median_DM_25_30=median(paf_DM_25_30), paf.lcl_DM_25_30=quantile(paf_DM_25_30, 0.025), paf.ucl_DM_25_30=quantile(paf_DM_25_30, 0.975),
                             paf.median_DM_30_35=median(paf_DM_30_35), paf.lcl_DM_30_35=quantile(paf_DM_30_35, 0.025), paf.ucl_DM_30_35=quantile(paf_DM_30_35, 0.975),
                             paf.median_DM_35_40=median(paf_DM_35_40), paf.lcl_DM_35_40=quantile(paf_DM_35_40, 0.025), paf.ucl_DM_35_40=quantile(paf_DM_35_40, 0.975),
                             paf.median_DM_40=median(paf_DM_40), paf.lcl_DM_40=quantile(paf_DM_40, 0.025), paf.ucl_DM_40=quantile(paf_DM_40, 0.975),
                             paf.median_HHD_25_30=median(paf_HHD_25_30), paf.lcl_HHD_25_30=quantile(paf_HHD_25_30, 0.025), paf.ucl_HHD_25_30=quantile(paf_HHD_25_30, 0.975),
                             paf.median_HHD_30_35=median(paf_HHD_30_35), paf.lcl_HHD_30_35=quantile(paf_HHD_30_35, 0.025), paf.ucl_HHD_30_35=quantile(paf_HHD_30_35, 0.975),
                             paf.median_HHD_35_40=median(paf_HHD_35_40), paf.lcl_HHD_35_40=quantile(paf_HHD_35_40, 0.025), paf.ucl_HHD_35_40=quantile(paf_HHD_35_40, 0.975),
                             paf.median_HHD_40=median(paf_HHD_40), paf.lcl_HHD_40=quantile(paf_HHD_40, 0.025), paf.ucl_HHD_40=quantile(paf_HHD_40, 0.975),
                             paf.median_h_stroke_25_30=median(paf_h_stroke_25_30), paf.lcl_h_stroke_25_30=quantile(paf_h_stroke_25_30, 0.025), paf.ucl_h_stroke_25_30=quantile(paf_h_stroke_25_30, 0.975),
                             paf.median_h_stroke_30_35=median(paf_h_stroke_30_35), paf.lcl_h_stroke_30_35=quantile(paf_h_stroke_30_35, 0.025), paf.ucl_h_stroke_30_35=quantile(paf_h_stroke_30_35, 0.975),
                             paf.median_h_stroke_35_40=median(paf_h_stroke_35_40), paf.lcl_h_stroke_35_40=quantile(paf_h_stroke_35_40, 0.025), paf.ucl_h_stroke_35_40=quantile(paf_h_stroke_35_40, 0.975),
                             paf.median_h_stroke_40=median(paf_h_stroke_40), paf.lcl_h_stroke_40=quantile(paf_h_stroke_40, 0.025), paf.ucl_h_stroke_40=quantile(paf_h_stroke_40, 0.975),
                             paf.median_isch_stroke_25_30=median(paf_isch_stroke_25_30), paf.lcl_isch_stroke_25_30=quantile(paf_isch_stroke_25_30, 0.025), paf.ucl_isch_stroke_25_30=quantile(paf_isch_stroke_25_30, 0.975),
                             paf.median_isch_stroke_30_35=median(paf_isch_stroke_30_35), paf.lcl_isch_stroke_30_35=quantile(paf_isch_stroke_30_35, 0.025), paf.ucl_isch_stroke_30_35=quantile(paf_isch_stroke_30_35, 0.975),
                             paf.median_isch_stroke_35_40=median(paf_isch_stroke_35_40), paf.lcl_isch_stroke_35_40=quantile(paf_isch_stroke_35_40, 0.025), paf.ucl_isch_stroke_35_40=quantile(paf_isch_stroke_35_40, 0.975),
                             paf.median_isch_stroke_40=median(paf_isch_stroke_40), paf.lcl_isch_stroke_40=quantile(paf_isch_stroke_40, 0.025), paf.ucl_isch_stroke_40=quantile(paf_isch_stroke_40, 0.975),
                             paf.median_IHD_25_30=median(paf_IHD_25_30), paf.lcl_IHD_25_30=quantile(paf_IHD_25_30, 0.025), paf.ucl_IHD_25_30=quantile(paf_IHD_25_30, 0.975),
                             paf.median_IHD_30_35=median(paf_IHD_30_35), paf.lcl_IHD_30_35=quantile(paf_IHD_30_35, 0.025), paf.ucl_IHD_30_35=quantile(paf_IHD_30_35, 0.975),
                             paf.median_IHD_35_40=median(paf_IHD_35_40), paf.lcl_IHD_35_40=quantile(paf_IHD_35_40, 0.025), paf.ucl_IHD_35_40=quantile(paf_IHD_35_40, 0.975),
                             paf.median_IHD_40=median(paf_IHD_40), paf.lcl_IHD_40=quantile(paf_IHD_40, 0.025), paf.ucl_IHD_40=quantile(paf_IHD_40, 0.975)
                             ), 
by=list(year, region, sex, deaths.age)]   

#convert negative PAF to 0
paf_results$paf.lcl_af_25_30[paf_results$paf.lcl_af_25_30<0] <- 0
paf_results$paf.lcl_af_30_35[paf_results$paf.lcl_af_30_35<0] <- 0
paf_results$paf.lcl_af_35_40[paf_results$paf.lcl_af_35_40<0] <- 0
paf_results$paf.lcl_af_40[paf_results$paf.lcl_af_40<0] <- 0
paf_results$paf.lcl_DM_25_30[paf_results$paf.lcl_DM_25_30<0] <- 0
paf_results$paf.lcl_DM_30_35[paf_results$paf.lcl_DM_30_35<0] <- 0
paf_results$paf.lcl_DM_35_40[paf_results$paf.lcl_DM_35_40<0] <- 0
paf_results$paf.lcl_DM_40[paf_results$paf.lcl_DM_40<0] <- 0
paf_results$paf.lcl_HHD_25_30[paf_results$paf.lcl_HHD_25_30<0] <- 0
paf_results$paf.lcl_HHD_30_35[paf_results$paf.lcl_HHD_30_35<0] <- 0
paf_results$paf.lcl_HHD_35_40[paf_results$paf.lcl_HHD_35_40<0] <- 0
paf_results$paf.lcl_HHD_40[paf_results$paf.lcl_HHD_40<0] <- 0
paf_results$paf.lcl_h_stroke_25_30[paf_results$paf.lcl_h_stroke_25_30<0] <- 0
paf_results$paf.lcl_h_stroke_30_35[paf_results$paf.lcl_h_stroke_30_35<0] <- 0
paf_results$paf.lcl_h_stroke_35_40[paf_results$paf.lcl_h_stroke_35_40<0] <- 0
paf_results$paf.lcl_h_stroke_40[paf_results$paf.lcl_h_stroke_40<0] <- 0
paf_results$paf.lcl_isch_stroke_25_30[paf_results$paf.lcl_isch_stroke_25_30<0] <- 0
paf_results$paf.lcl_isch_stroke_30_35[paf_results$paf.lcl_isch_stroke_30_35<0] <- 0
paf_results$paf.lcl_isch_stroke_35_40[paf_results$paf.lcl_isch_stroke_35_40<0] <- 0
paf_results$paf.lcl_isch_stroke_40[paf_results$paf.lcl_isch_stroke_40<0] <- 0
paf_results$paf.lcl_IHD_25_30[paf_results$paf.lcl_IHD_25_30<0] <- 0
paf_results$paf.lcl_IHD_30_35[paf_results$paf.lcl_IHD_30_35<0] <- 0
paf_results$paf.lcl_IHD_35_40[paf_results$paf.lcl_IHD_35_40<0] <- 0
paf_results$paf.lcl_IHD_40[paf_results$paf.lcl_IHD_40<0] <- 0

#Load NCD deaths
deaths<-read.csv("Deaths_NCD_Peru_2021-08-11.csv")
deaths<-spread(deaths, outcome, deaths)
deaths$sex<-ifelse(deaths$sex == "MASCULINO", 1, 2)
deaths<-data.table(deaths)

deaths$agecat[deaths$agecat == "20-24"] <- 20
deaths$agecat[deaths$agecat == "25-29"] <- 25
deaths$agecat[deaths$agecat == "30-34"] <- 30
deaths$agecat[deaths$agecat == "35-39"] <- 35
deaths$agecat[deaths$agecat == "40-44"] <- 40
deaths$agecat[deaths$agecat == "45-49"] <- 45
deaths$agecat[deaths$agecat == "50-54"] <- 50
deaths$agecat[deaths$agecat == "55-59"] <- 55
deaths$agecat[deaths$agecat == "60-64"] <- 60
deaths$agecat[deaths$agecat == "65-69"] <- 65
deaths$agecat[deaths$agecat == "70-74"] <- 70
deaths$agecat[deaths$agecat == "75-79"] <- 75
deaths$agecat[deaths$agecat == "80-84"] <- 80
deaths$agecat[deaths$agecat == "85+"] <- 85


colnames(deaths)[4]<-"deaths.age"
deaths$deaths.age<-as.numeric(deaths$deaths.age)


#change regions
paf_results$region<-ifelse(paf_results$region == 1, "AMAZONAS", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 2, "ANCASH", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 3, "APURIMAC", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 4, "AREQUIPA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 5, "AYACUCHO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 6, "CAJAMARCA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 7, "CALLAO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 8, "CUSCO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 9, "HUANCAVELICA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 10, "HUANUCO",paf_results$region)
paf_results$region<-ifelse(paf_results$region == 11, "ICA",paf_results$region)
paf_results$region<-ifelse(paf_results$region == 12, "JUNIN", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 13, "LA LIBERTAD", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 14, "LAMBAYEQUE", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 15, "LIMA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 16, "LORETO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 17, "MADRE DE DIOS", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 18, "MOQUEGUA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 19, "PASCO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 20, "PIURA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 21, "PUNO", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 22, "SAN MARTIN", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 23, "TACNA", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 24, "TUMBES", paf_results$region)
paf_results$region<-ifelse(paf_results$region == 25, "UCAYALI", paf_results$region)

paf_results$year<-2018
paf_results$year<-as.numeric(paf_results$year)

#set keys for both dataset
setkey(paf_results, year, region, sex, deaths.age)
setkey(deaths, year, region, sex, deaths.age)

# create new dataset as result of joining both tables
results<-left_join(paf_results, deaths)

# calculate attributable deaths (AD) for each NCD in each BMI category
results$AD_af_25_30<-round(results$paf.median_af_25_30*results$AF)
results$AD_af_l_25_30<-round(results$paf.lcl_af_25_30*results$AF)
results$AD_af_u_25_30<-round(results$paf.ucl_af_25_30*results$AF)

results$AD_af_30_35<-round(results$paf.median_af_30_35*results$AF)
results$AD_af_l_30_35<-round(results$paf.lcl_af_30_35*results$AF)
results$AD_af_u_30_35<-round(results$paf.ucl_af_30_35*results$AF)

results$AD_af_35_40<-round(results$paf.median_af_35_40*results$AF)
results$AD_af_l_35_40<-round(results$paf.lcl_af_35_40*results$AF)
results$AD_af_u_35_40<-round(results$paf.ucl_af_35_40*results$AF)

results$AD_af_40<-round(results$paf.median_af_40*results$AF)
results$AD_af_l_40<-round(results$paf.lcl_af_40*results$AF)
results$AD_af_u_40<-round(results$paf.ucl_af_40*results$AF)


results$AD_DM_25_30<-round(results$paf.median_DM_25_30*results$DM)
results$AD_DM_l_25_30<-round(results$paf.lcl_DM_25_30*results$DM)
results$AD_DM_u_25_30<-round(results$paf.ucl_DM_25_30*results$DM)

results$AD_DM_30_35<-round(results$paf.median_DM_30_35*results$DM)
results$AD_DM_l_30_35<-round(results$paf.lcl_DM_30_35*results$DM)
results$AD_DM_u_30_35<-round(results$paf.ucl_DM_30_35*results$DM)

results$AD_DM_35_40<-round(results$paf.median_DM_35_40*results$DM)
results$AD_DM_l_35_40<-round(results$paf.lcl_DM_35_40*results$DM)
results$AD_DM_u_35_40<-round(results$paf.ucl_DM_35_40*results$DM)

results$AD_DM_40<-round(results$paf.median_DM_40*results$DM)
results$AD_DM_l_40<-round(results$paf.lcl_DM_40*results$DM)
results$AD_DM_u_40<-round(results$paf.ucl_DM_40*results$DM)


results$AD_HHD_25_30<-round(results$paf.median_HHD_25_30*results$HHD)
results$AD_HHD_l_25_30<-round(results$paf.lcl_HHD_25_30*results$HHD)
results$AD_HHD_u_25_30<-round(results$paf.ucl_HHD_25_30*results$HHD)

results$AD_HHD_30_35<-round(results$paf.median_HHD_30_35*results$HHD)
results$AD_HHD_l_30_35<-round(results$paf.lcl_HHD_30_35*results$HHD)
results$AD_HHD_u_30_35<-round(results$paf.ucl_HHD_30_35*results$HHD)

results$AD_HHD_35_40<-round(results$paf.median_HHD_35_40*results$HHD)
results$AD_HHD_l_35_40<-round(results$paf.lcl_HHD_35_40*results$HHD)
results$AD_HHD_u_35_40<-round(results$paf.ucl_HHD_35_40*results$HHD)

results$AD_HHD_40<-round(results$paf.median_HHD_40*results$HHD)
results$AD_HHD_l_40<-round(results$paf.lcl_HHD_40*results$HHD)
results$AD_HHD_u_40<-round(results$paf.ucl_HHD_40*results$HHD)


results$AD_h_stroke_25_30<-round(results$paf.median_h_stroke_25_30*results$`Haemorrhagic stroke`)
results$AD_h_stroke_l_25_30<-round(results$paf.lcl_h_stroke_25_30*results$`Haemorrhagic stroke`)
results$AD_h_stroke_u_25_30<-round(results$paf.ucl_h_stroke_25_30*results$`Haemorrhagic stroke`)

results$AD_h_stroke_30_35<-round(results$paf.median_h_stroke_30_35*results$`Haemorrhagic stroke`)
results$AD_h_stroke_l_30_35<-round(results$paf.lcl_h_stroke_30_35*results$`Haemorrhagic stroke`)
results$AD_h_stroke_u_30_35<-round(results$paf.ucl_h_stroke_30_35*results$`Haemorrhagic stroke`)

results$AD_h_stroke_35_40<-round(results$paf.median_h_stroke_35_40*results$`Haemorrhagic stroke`)
results$AD_h_stroke_l_35_40<-round(results$paf.lcl_h_stroke_35_40*results$`Haemorrhagic stroke`)
results$AD_h_stroke_u_35_40<-round(results$paf.ucl_h_stroke_35_40*results$`Haemorrhagic stroke`)

results$AD_h_stroke_40<-round(results$paf.median_h_stroke_40*results$`Haemorrhagic stroke`)
results$AD_h_stroke_l_40<-round(results$paf.lcl_h_stroke_40*results$`Haemorrhagic stroke`)
results$AD_h_stroke_u_40<-round(results$paf.ucl_h_stroke_40*results$`Haemorrhagic stroke`)


results$AD_isch_stroke_25_30<-round(results$paf.median_isch_stroke_25_30*results$`Ischemic stroke`)
results$AD_isch_stroke_l_25_30<-round(results$paf.lcl_isch_stroke_25_30*results$`Ischemic stroke`)
results$AD_isch_stroke_u_25_30<-round(results$paf.ucl_isch_stroke_25_30*results$`Ischemic stroke`)

results$AD_isch_stroke_30_35<-round(results$paf.median_isch_stroke_30_35*results$`Ischemic stroke`)
results$AD_isch_stroke_l_30_35<-round(results$paf.lcl_isch_stroke_30_35*results$`Ischemic stroke`)
results$AD_isch_stroke_u_30_35<-round(results$paf.ucl_isch_stroke_30_35*results$`Ischemic stroke`)

results$AD_isch_stroke_35_40<-round(results$paf.median_isch_stroke_35_40*results$`Ischemic stroke`)
results$AD_isch_stroke_l_35_40<-round(results$paf.lcl_isch_stroke_35_40*results$`Ischemic stroke`)
results$AD_isch_stroke_u_35_40<-round(results$paf.ucl_isch_stroke_35_40*results$`Ischemic stroke`)

results$AD_isch_stroke_40<-round(results$paf.median_isch_stroke_40*results$`Ischemic stroke`)
results$AD_isch_stroke_l_40<-round(results$paf.lcl_isch_stroke_40*results$`Ischemic stroke`)
results$AD_isch_stroke_u_40<-round(results$paf.ucl_isch_stroke_40*results$`Ischemic stroke`)


results$AD_IHD_25_30<-round(results$paf.median_IHD_25_30*results$IHD)
results$AD_IHD_l_25_30<-round(results$paf.lcl_IHD_25_30*results$IHD)
results$AD_IHD_u_25_30<-round(results$paf.ucl_IHD_25_30*results$IHD)

results$AD_IHD_30_35<-round(results$paf.median_IHD_30_35*results$IHD)
results$AD_IHD_l_30_35<-round(results$paf.lcl_IHD_30_35*results$IHD)
results$AD_IHD_u_30_35<-round(results$paf.ucl_IHD_30_35*results$IHD)

results$AD_IHD_35_40<-round(results$paf.median_IHD_35_40*results$IHD)
results$AD_IHD_l_35_40<-round(results$paf.lcl_IHD_35_40*results$IHD)
results$AD_IHD_u_35_40<-round(results$paf.ucl_IHD_35_40*results$IHD)

results$AD_IHD_40<-round(results$paf.median_IHD_40*results$IHD)
results$AD_IHD_l_40<-round(results$paf.lcl_IHD_40*results$IHD)
results$AD_IHD_u_40<-round(results$paf.ucl_IHD_40*results$IHD)


############################################################################################################
#all in one column

results_af_25_30<-select(results, 1:4, paf.median_af_25_30,paf.lcl_af_25_30, paf.ucl_af_25_30, AD_af_25_30, AD_af_l_25_30, AD_af_u_25_30)
results_af_30_35<-select(results, 1:4, paf.median_af_30_35,paf.lcl_af_30_35, paf.ucl_af_30_35, AD_af_30_35, AD_af_l_30_35, AD_af_u_30_35)
results_af_35_40<-select(results, 1:4, paf.median_af_35_40,paf.lcl_af_35_40, paf.ucl_af_35_40, AD_af_35_40, AD_af_l_35_40, AD_af_u_35_40)
results_af_40<-select(results, 1:4, paf.median_af_40,paf.lcl_af_40, paf.ucl_af_40, AD_af_40, AD_af_l_40, AD_af_u_40)

results_DM_25_30<-select(results, 1:4, paf.median_DM_25_30,paf.lcl_DM_25_30, paf.ucl_DM_25_30, AD_DM_25_30, AD_DM_l_25_30, AD_DM_u_25_30)
results_DM_30_35<-select(results, 1:4, paf.median_DM_30_35,paf.lcl_DM_30_35, paf.ucl_DM_30_35, AD_DM_30_35, AD_DM_l_30_35, AD_DM_u_30_35)
results_DM_35_40<-select(results, 1:4, paf.median_DM_35_40,paf.lcl_DM_35_40, paf.ucl_DM_35_40, AD_DM_35_40, AD_DM_l_35_40, AD_DM_u_35_40)
results_DM_40<-select(results, 1:4, paf.median_DM_40,paf.lcl_DM_40, paf.ucl_DM_40, AD_DM_40, AD_DM_l_40, AD_DM_u_40)

results_HHD_25_30<-select(results, 1:4, paf.median_HHD_25_30,paf.lcl_HHD_25_30, paf.ucl_HHD_25_30, AD_HHD_25_30, AD_HHD_l_25_30, AD_HHD_u_25_30)
results_HHD_30_35<-select(results, 1:4, paf.median_HHD_30_35,paf.lcl_HHD_30_35, paf.ucl_HHD_30_35, AD_HHD_30_35, AD_HHD_l_30_35, AD_HHD_u_30_35)
results_HHD_35_40<-select(results, 1:4, paf.median_HHD_35_40,paf.lcl_HHD_35_40, paf.ucl_HHD_35_40, AD_HHD_35_40, AD_HHD_l_35_40, AD_HHD_u_35_40)
results_HHD_40<-select(results, 1:4, paf.median_HHD_40,paf.lcl_HHD_40, paf.ucl_HHD_40, AD_HHD_40, AD_HHD_l_40, AD_HHD_u_40)

results_isch_stroke_25_30<-select(results, 1:4, paf.median_isch_stroke_25_30,paf.lcl_isch_stroke_25_30, paf.ucl_isch_stroke_25_30, AD_isch_stroke_25_30, AD_isch_stroke_l_25_30, AD_isch_stroke_u_25_30)
results_isch_stroke_30_35<-select(results, 1:4, paf.median_isch_stroke_30_35,paf.lcl_isch_stroke_30_35, paf.ucl_isch_stroke_30_35, AD_isch_stroke_30_35, AD_isch_stroke_l_30_35, AD_isch_stroke_u_30_35)
results_isch_stroke_35_40<-select(results, 1:4, paf.median_isch_stroke_35_40,paf.lcl_isch_stroke_35_40, paf.ucl_isch_stroke_35_40, AD_isch_stroke_35_40, AD_isch_stroke_l_35_40, AD_isch_stroke_u_35_40)
results_isch_stroke_40<-select(results, 1:4, paf.median_isch_stroke_40,paf.lcl_isch_stroke_40, paf.ucl_isch_stroke_40, AD_isch_stroke_40, AD_isch_stroke_l_40, AD_isch_stroke_u_40)

results_IHD_25_30<-select(results, 1:4, paf.median_IHD_25_30,paf.lcl_IHD_25_30, paf.ucl_IHD_25_30, AD_IHD_25_30, AD_IHD_l_25_30, AD_IHD_u_25_30)
results_IHD_30_35<-select(results, 1:4, paf.median_IHD_30_35,paf.lcl_IHD_30_35, paf.ucl_IHD_30_35, AD_IHD_30_35, AD_IHD_l_30_35, AD_IHD_u_30_35)
results_IHD_35_40<-select(results, 1:4, paf.median_IHD_35_40,paf.lcl_IHD_35_40, paf.ucl_IHD_35_40, AD_IHD_35_40, AD_IHD_l_35_40, AD_IHD_u_35_40)
results_IHD_40<-select(results, 1:4, paf.median_IHD_40,paf.lcl_IHD_40, paf.ucl_IHD_40, AD_IHD_40, AD_IHD_l_40, AD_IHD_u_40)


results_h_stroke_25_30<-select(results, 1:4, paf.median_h_stroke_25_30,paf.lcl_h_stroke_25_30, paf.ucl_h_stroke_25_30, AD_h_stroke_25_30, AD_h_stroke_l_25_30, AD_h_stroke_u_25_30)
results_h_stroke_30_35<-select(results, 1:4, paf.median_h_stroke_30_35,paf.lcl_h_stroke_30_35, paf.ucl_h_stroke_30_35, AD_h_stroke_30_35, AD_h_stroke_l_30_35, AD_h_stroke_u_30_35)
results_h_stroke_35_40<-select(results, 1:4, paf.median_h_stroke_35_40,paf.lcl_h_stroke_35_40, paf.ucl_h_stroke_35_40, AD_h_stroke_35_40, AD_h_stroke_l_35_40, AD_h_stroke_u_35_40)
results_h_stroke_40<-select(results, 1:4, paf.median_h_stroke_40,paf.lcl_h_stroke_40, paf.ucl_h_stroke_40, AD_h_stroke_40, AD_h_stroke_l_40, AD_h_stroke_u_40)


results_af_25_30$outcome<- "Atrial fibrillation and flutter"
results_af_30_35$outcome<- "Atrial fibrillation and flutter"
results_af_35_40$outcome<- "Atrial fibrillation and flutter"
results_af_40$outcome<- "Atrial fibrillation and flutter"
results_af_25_30$BMI_cat<-"Overweight"
results_af_30_35$BMI_cat<-"Obesity 1"
results_af_35_40$BMI_cat<-"Obesity 2"
results_af_40$BMI_cat<-"Obesity 3"


results_DM_25_30$outcome<- "Type 2 diabetes mellitus"
results_DM_30_35$outcome<- "Type 2 diabetes mellitus"
results_DM_35_40$outcome<- "Type 2 diabetes mellitus"
results_DM_40$outcome<- "Type 2 diabetes mellitus"
results_DM_25_30$BMI_cat<-"Overweight"
results_DM_30_35$BMI_cat<-"Obesity 1"
results_DM_35_40$BMI_cat<-"Obesity 2"
results_DM_40$BMI_cat<-"Obesity 3"

results_HHD_25_30$outcome<- "Hypertensive heart disease"
results_HHD_30_35$outcome<- "Hypertensive heart disease"
results_HHD_35_40$outcome<- "Hypertensive heart disease"
results_HHD_40$outcome<- "Hypertensive heart disease"
results_HHD_25_30$BMI_cat<-"Overweight"
results_HHD_30_35$BMI_cat<-"Obesity 1"
results_HHD_35_40$BMI_cat<-"Obesity 2"
results_HHD_40$BMI_cat<-"Obesity 3"

results_isch_stroke_25_30$outcome<- "Ischaemic stroke"
results_isch_stroke_30_35$outcome<- "Ischaemic stroke"
results_isch_stroke_35_40$outcome<- "Ischaemic stroke"
results_isch_stroke_40$outcome<- "Ischaemic stroke"
results_isch_stroke_25_30$BMI_cat<-"Overweight"
results_isch_stroke_30_35$BMI_cat<-"Obesity 1"
results_isch_stroke_35_40$BMI_cat<-"Obesity 2"
results_isch_stroke_40$BMI_cat<-"Obesity 3"

results_IHD_25_30$outcome<- "Ischaemic heart disease"
results_IHD_30_35$outcome<- "Ischaemic heart disease"
results_IHD_35_40$outcome<- "Ischaemic heart disease"
results_IHD_40$outcome<- "Ischaemic heart disease"
results_IHD_25_30$BMI_cat<-"Overweight"
results_IHD_30_35$BMI_cat<-"Obesity 1"
results_IHD_35_40$BMI_cat<-"Obesity 2"
results_IHD_40$BMI_cat<-"Obesity 3"


results_h_stroke_25_30$outcome<- "Haemorrhagic stroke"
results_h_stroke_30_35$outcome<- "Haemorrhagic stroke"
results_h_stroke_35_40$outcome<- "Haemorrhagic stroke"
results_h_stroke_40$outcome<- "Haemorrhagic stroke"
results_h_stroke_25_30$BMI_cat<-"Overweight"
results_h_stroke_30_35$BMI_cat<-"Obesity 1"
results_h_stroke_35_40$BMI_cat<-"Obesity 2"
results_h_stroke_40$BMI_cat<-"Obesity 3"


names(results_af_25_30)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_af_30_35)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_af_35_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_af_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")

names(results_DM_25_30)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_DM_30_35)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_DM_35_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_DM_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")

names(results_HHD_25_30)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_HHD_30_35)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_HHD_35_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_HHD_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")

names(results_isch_stroke_25_30)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_isch_stroke_30_35)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_isch_stroke_35_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_isch_stroke_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")

names(results_IHD_25_30)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_IHD_30_35)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_IHD_35_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_IHD_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")

names(results_h_stroke_25_30)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_h_stroke_30_35)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_h_stroke_35_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")
names(results_h_stroke_40)[5:10]<-c("paf.median", "paf.lower", "paf.upper", "AD", "AD.lower", "AD.upper")


rm(dt, paf_results, pafbmi.dt, deaths, bmi, rr, results,age.table)

results = sapply(.GlobalEnv, is.data.frame) 
results<-do.call(rbind, mget(names(results)[results]))

rm(list=setdiff(ls(), c("results")))

#results
results<-arrange(results, year, region, sex, deaths.age,outcome)
results$sex <- factor(results$sex,
                      levels = c(1,2),
                      labels = c("Men", "Women"))


##Join deaths to standardize paf
deaths<-read.csv("Deaths_NCD_Peru_2021-08-11.csv")
deaths<-filter(deaths, year == 2018)
deaths<-data.table(deaths)

deaths$agecat[deaths$agecat == "20-24"] <- 20
deaths$agecat[deaths$agecat == "25-29"] <- 25
deaths$agecat[deaths$agecat == "30-34"] <- 30
deaths$agecat[deaths$agecat == "35-39"] <- 35
deaths$agecat[deaths$agecat == "40-44"] <- 40
deaths$agecat[deaths$agecat == "45-49"] <- 45
deaths$agecat[deaths$agecat == "50-54"] <- 50
deaths$agecat[deaths$agecat == "55-59"] <- 55
deaths$agecat[deaths$agecat == "60-64"] <- 60
deaths$agecat[deaths$agecat == "65-69"] <- 65
deaths$agecat[deaths$agecat == "70-74"] <- 70
deaths$agecat[deaths$agecat == "75-79"] <- 75
deaths$agecat[deaths$agecat == "80-84"] <- 80
deaths$agecat[deaths$agecat == "85+"] <- 85

names(deaths)[4]<-"deaths.age"
deaths$deaths.age<-as.numeric(deaths$deaths.age)

deaths$sex<-ifelse(deaths$sex == "MASCULINO", 1, 2)
deaths$sex <- factor(deaths$sex,
                     levels = c(1,2),
                     labels = c("Men", "Women"))


deaths$outcome[deaths$outcome == "AF"] <- "Atrial fibrillation and flutter"
deaths$outcome[deaths$outcome == "DM"] <- "Type 2 diabetes mellitus"
deaths$outcome[deaths$outcome == "HHD"] <- "Hypertensive heart disease"
deaths$outcome[deaths$outcome == "IHD"] <- "Ischaemic heart disease"
deaths$outcome[deaths$outcome == "Ischemic stroke"] <- "Ischaemic stroke"
deaths$outcome[deaths$outcome == "Haemorrhagic stroke"] <- "Haemorrhagic stroke"

#set keys for both dataset
setkey(results, year, region, sex, deaths.age, outcome)
setkey(deaths, year, region, sex, deaths.age, outcome)

# create new dataset as result of joining both tables
results<-left_join(results, deaths)

## joining a column of CVD and T2DM and neoplasms
results <- results %>% mutate(NCD = ifelse(outcome =="Hypertensive heart disease"|  outcome =="Ischaemic heart disease" | outcome == "Haemorrhagic stroke" |
                                               outcome =="Ischaemic stroke"| outcome =="Atrial fibrillation and flutter","Cardiovascular diseases", ifelse(outcome == "Type 2 diabetes mellitus" , "Type 2 diabetes",NA)))

 #group all age categories in one result, by individual outcomes
results_standardised_regional_outcome_bmi_cat<-results[, list(paf.median=sum(AD)/sum(deaths), 
                                                      paf.lcl=sum(AD.lower)/sum(deaths), 
                                                      paf.ucl=sum(AD.upper)/sum(deaths),
                                                      Attributable.deaths.val=sum(AD), 
                                                      Attributable.deaths.lower=sum(AD.lower),
                                                      Attributable.deaths.upper=sum(AD.upper), deaths = sum(deaths),  NCD = first(NCD)),
                                               by=list(region, year, sex, outcome, BMI_cat)]  

#group by CMD by region
results_standardised_regional_bmi_cat<-results_standardised_regional_outcome_bmi_cat[, list(paf.median=sum(Attributable.deaths.val)/sum(deaths), 
                                                                            paf.lcl=sum(Attributable.deaths.lower)/sum(deaths), 
                                                                            paf.ucl=sum(Attributable.deaths.upper)/sum(deaths),
                                                                            Attributable.deaths.val=sum(Attributable.deaths.val), 
                                                                            Attributable.deaths.lower=sum(Attributable.deaths.lower),
                                                                            Attributable.deaths.upper=sum(Attributable.deaths.upper), deaths = sum(deaths)),
                                                                     by=list(region, year, sex, BMI_cat)]  


 # national, by individual outcomes
results_standardised_national_outcome_bmi_cat<-results_standardised_regional_outcome_bmi_cat[, list(paf.median=sum(Attributable.deaths.val)/sum(deaths), 
                                                                                    paf.lcl=sum(Attributable.deaths.lower)/sum(deaths), 
                                                                                    paf.ucl=sum(Attributable.deaths.upper)/sum(deaths),
                                                                                    Attributable.deaths.val=sum(Attributable.deaths.val), 
                                                                                    Attributable.deaths.lower=sum(Attributable.deaths.lower),
                                                                                    Attributable.deaths.upper=sum(Attributable.deaths.upper), deaths = sum(deaths)),
                                                                             by=list(year, sex, outcome, BMI_cat)]  
# national, by NCD
results_standardised_national_NCD_bmi_cat<-results_standardised_regional_outcome_bmi_cat[, list(paf.median=sum(Attributable.deaths.val)/sum(deaths), 
                                                                                                    paf.lcl=sum(Attributable.deaths.lower)/sum(deaths), 
                                                                                                    paf.ucl=sum(Attributable.deaths.upper)/sum(deaths),
                                                                                                    Attributable.deaths.val=sum(Attributable.deaths.val), 
                                                                                                    Attributable.deaths.lower=sum(Attributable.deaths.lower),
                                                                                                    Attributable.deaths.upper=sum(Attributable.deaths.upper), deaths = sum(deaths)),
                                                                                             by=list(year, sex, NCD, BMI_cat)]  


results_standardised_national_bmi_cat<-results_standardised_regional_outcome_bmi_cat[, list(paf.median=sum(Attributable.deaths.val)/sum(deaths), 
                                                                            paf.lcl=sum(Attributable.deaths.lower)/sum(deaths), 
                                                                            paf.ucl=sum(Attributable.deaths.upper)/sum(deaths),
                                                                            Attributable.deaths.val=sum(Attributable.deaths.val), 
                                                                            Attributable.deaths.lower=sum(Attributable.deaths.lower),
                                                                            Attributable.deaths.upper=sum(Attributable.deaths.upper), deaths = sum(deaths)),
                                                                     by=list(year, sex, BMI_cat)]  



##Write csv of results
write.csv(results_standardised_regional_outcome_bmi_cat, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_by_region_outcome_bmi_cat_", Sys.Date(), ".csv"),  row.names = F)
write.csv(results_standardised_regional_bmi_cat, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_by_region_bmi_cat_", Sys.Date(), ".csv"),  row.names = F)
write.csv(results_standardised_national_outcome_bmi_cat, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_national_by_outcome_bmi_cat_", Sys.Date(), ".csv"),  row.names = F)
write.csv(results_standardised_national_bmi_cat, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_national_bmi_cat_", Sys.Date(), ".csv"),  row.names = F)
write.csv(results_standardised_national_NCD_bmi_cat, paste0("~/Desktop/Artículos/CRA High BMI Peru/Supplementary materials/Tables/Proportion_and_deaths_high_BMI_national_by_NCD_bmi_cat_", Sys.Date(), ".csv"),  row.names = F)




