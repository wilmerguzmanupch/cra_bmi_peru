setwd("~/Desktop/MINSA-Defunciones 2003-2018/MINSA-Defunciones 2013")

library(dplyr)
library(readxl)
library(data.table)





##Extracting cause-specific NCDs deaths 
data2013<-read_excel("~/Desktop/MINSA-Defunciones 2003-2018/MINSA-Defunciones 2013/Causa de Muerte 2013 (Trabajada).xlsx")
data2013<-select(data2013, departamento, ano, sex, edad, causa_basica)
colnames(data2013) <- c("region", "year", "sex", "age", "cause.death")
table(data2013$age, useNA = c("always"))
data2013<-filter(data2013, age >=20)  ##Esta es la base de datos del 2013


data2014<-read_excel("~/Desktop/MINSA-Defunciones 2003-2018/MINSA-Defunciones 2014/Causa de Muerte 2014 (Trabajada).xlsx")
data2014<-select(data2014, departamento, ano, sexo, edad, causa_basica)
colnames(data2014) <- c("region", "year", "sex", "age", "cause.death")
table(data2014$age, useNA = c("always"))
data2014<-filter(data2014, age >=20)  ##Esta es la base de datos del 2014

data2015<-read_excel("~/Desktop/MINSA-Defunciones 2003-2018/MINSA-Defunciones 2015/_old/Causa de Muerte 2015 (Trabajada).xlsx")
data2015<-select(data2015, departamento, ano, sexo, edad, causa_basica)
colnames(data2015) <- c("region", "year", "sex", "age", "cause.death")
table(data2015$age, useNA = c("always"))
data2015<-filter(data2015, age >=20)  ##Esta es la base de datos del 2015
data2015$sex<-ifelse(data2015$sex == 1, "MASCULINO", ifelse(data2015$sex == 2, "FEMENINO", NA))

data2016<-read_excel("~/Desktop/MINSA-Defunciones 2003-2018/MINSA-Defunciones 2016/Causa de Muerte 2016 (Trabajada).xlsx")
data2016$year<- 2016
data2016<-select(data2016, departamento, year, sexo, edad, causa_basica)
colnames(data2016) <- c("region", "year", "sex", "age", "cause.death")
table(data2016$age, useNA = c("always"))
data2016<-filter(data2016, age >=20)  ##Esta es la base de datos del 2016
data2016$sex<-ifelse(data2016$sex == 1, "MASCULINO", ifelse(data2016$sex == 2, "FEMENINO", NA))
data2016$region<-as.numeric(data2016$region)
data2016$region <- ifelse(data2016$region == 1, "AMAZONAS",
                                         ifelse(data2016$region == 2, "ANCASH",
                                                ifelse(data2016$region == 3, "APURIMAC",
                                                       ifelse(data2016$region == 4, "AREQUIPA",
                                                              ifelse(data2016$region == 5, "AYACUCHO",
                                                                     ifelse(data2016$region == 6, "CAJAMARCA",
                                                                            ifelse(data2016$region == 7, "CALLAO",
                                                                                   ifelse(data2016$region == 8, "CUSCO",
                                                                                          ifelse(data2016$region == 9, "HUANCAVELICA",
                                                                                                 ifelse(data2016$region == 10, "HUANUCO",
                                                                                                        ifelse(data2016$region == 11, "ICA",
                                                                                                               ifelse(data2016$region == 12, "JUNIN",
                                                                                                                      ifelse(data2016$region == 13, "LA LIBERTAD",
                                                                                                                             ifelse(data2016$region == 14, "LAMBAYEQUE",
                                                                                                                                    ifelse(data2016$region == 15, "LIMA", 
                                                                                                                                           ifelse(data2016$region == 16, "LORETO",
                                                                                                                                                  ifelse(data2016$region == 17, "MADRE DE DIOS",
                                                                                                                                                         ifelse(data2016$region == 18, "MOQUEGUA",
                                                                                                                                                                ifelse(data2016$region == 19, "PASCO",
                                                                                                                                                                       ifelse(data2016$region == 20, "PIURA",
                                                                                                                                                                              ifelse(data2016$region == 21, "PUNO",
                                                                                                                                                                                     ifelse(data2016$region == 22, "SAN MARTIN",
                                                                                                                                                                                            ifelse(data2016$region == 23, "TACNA",
                                                                                                                                                                                                   ifelse(data2016$region == 24, "TUMBES",
                                                                                                                                                                                                          ifelse(data2016$region == 25, "UCAYALI", NA)))))))))))))))))))))))))





data2017<-read_excel("~/Desktop/MINSA-Defunciones 2003-2018/MINSA-Defunciones 2017/Año 2017.xlsx")
data2017<-select(data2017, Departamento_lugar, Año, Sexo, Edad, cd63caubas)
colnames(data2017) <- c("region", "year", "sex", "age", "cause.death")
table(data2017$age, useNA = c("always"))
data2017<-filter(data2017, age >=20)  ##Esta es la base de datos del 2017
data2017$sex<-ifelse(data2017$sex == "Masculino", "MASCULINO", ifelse(data2017$sex == "Femenino", "FEMENINO", NA))


data2018<-read_excel("~/Desktop/MINSA-Defunciones 2003-2018/MINSA-Defunciones 2018/Año 2018.xlsx")
data2018<-select(data2018, rh42dpto, Año, sexo, edad, causa_basic)
colnames(data2018) <- c("region", "year", "sex", "age", "cause.death")
table(data2018$age, useNA = c("always"))
data2018<-filter(data2018, age >=20)  ##Esta es la base de datos del 2018

data2018$region[data2018$region == "ÁNCASH"]<-"ANCASH"
data2018$region[data2018$region == "APURÍMAC"]<-"APURIMAC"
data2018$region[data2018$region == "HUÁNUCO"]<-"HUANUCO"
data2018$region[data2018$region == "JUNÍN"]<-"JUNIN"
data2018$region[data2018$region == "SAN MARTÍN"]<-"SAN MARTIN"




##############################################################################



#####2013

AF2013<-filter(data2013, cause.death == "I48X" | cause.death == "I48" |
                      cause.death == "I480" | cause.death == "I481"
                    | cause.death == "I482" | cause.death == "I483" | cause.death == "I484" | cause.death == "I489")

asthma2013<-filter(data2013, cause.death == "J45" | cause.death == "J450" | cause.death == "J451"
                 | cause.death == "J458" | cause.death == "J459" | cause.death == "J45X")


colon2013<-filter(data2013, cause.death == "C180" | cause.death == "C181" | cause.death == "C182" | cause.death == "C183" |
                      cause.death == "C184" | cause.death == "C185" | cause.death == "C186" | cause.death == "C187" |
                      cause.death == "C188" | cause.death == "C189" | cause.death == "C18X" |
                      
                      cause.death == "C190" | cause.death == "C191" | cause.death == "C192" | cause.death == "C193" |
                      cause.death == "C194" | cause.death == "C195" | cause.death == "C196" | cause.death == "C197" |
                      cause.death == "C198" | cause.death == "C199" | cause.death == "C19X" |
                      
                      cause.death == "C200" | cause.death == "C201" | cause.death == "C202" | cause.death == "C203" |
                      cause.death == "C204" | cause.death == "C205" | cause.death == "C206" | cause.death == "C207" |
                      cause.death == "C208" | cause.death == "C209" | cause.death == "C20X")

HHD2013<-filter(data2013, cause.death == "I11" | cause.death == "I110" | cause.death == "I11X"
                    |cause.death == "I119")


IHD2013<-filter(data2013, cause.death == "I200" | cause.death == "I201" | cause.death == "I202" | cause.death == "I203" |
                      cause.death == "I204" | cause.death == "I205" | cause.death == "I206" | cause.death == "I207" |
                      cause.death == "I208" | cause.death == "I209" |
                      
                      cause.death == "I210" | cause.death == "I211" | cause.death == "I212" | cause.death == "I213" |
                      cause.death == "I214" | cause.death == "I215" | cause.death == "I216" | cause.death == "I217" |
                      cause.death == "I218" | cause.death == "I219" |
                      
                      cause.death == "I220" | cause.death == "I221" | cause.death == "I222" | cause.death == "I223" |
                      cause.death == "I224" | cause.death == "I225" | cause.death == "I226" | cause.death == "I227" |
                      cause.death == "I228" | cause.death == "I229" |
                      
                      cause.death == "I230" | cause.death == "I231" | cause.death == "I232" | cause.death == "I233" |
                      cause.death == "I234" | cause.death == "I235" | cause.death == "I236" | cause.death == "I237" |
                      cause.death == "I238" | cause.death == "I239" |
                      
                      cause.death == "I240" | cause.death == "I241" | cause.death == "I242" | cause.death == "I243" |
                      cause.death == "I244" | cause.death == "I245" | cause.death == "I246" | cause.death == "I247" |
                      cause.death == "I248" | cause.death == "I249" |
                      
                      cause.death == "I250" | cause.death == "I251" | cause.death == "I252" | cause.death == "I253" |
                      cause.death == "I254" | cause.death == "I255" | cause.death == "I256" | cause.death == "I257" |
                      cause.death == "I258" | cause.death == "I259")


Isch_stroke2013<-filter(data2013, cause.death == "I630" | cause.death == "I631" | cause.death == "I632" | cause.death == "I633" |
                      cause.death == "I634" | cause.death == "I635" | cause.death == "I636" | cause.death == "I637" |
                      cause.death == "I638" | cause.death == "I639" | cause.death == "I63X" |
                      
                      cause.death == "I650" | cause.death == "I651" | cause.death == "I652" | cause.death == "I653" |
                      cause.death == "I654" | cause.death == "I655" | cause.death == "I656" | cause.death == "I657" |
                      cause.death == "I658" | cause.death == "I659" | cause.death == "I65X" |
                      
                      cause.death == "I660" | cause.death == "I661" | cause.death == "I662" | cause.death == "I663" |
                      cause.death == "I664" | cause.death == "I665" | cause.death == "I666" | cause.death == "I667" |
                      cause.death == "I668" | cause.death == "I669" | cause.death == "I66X" |
                      
                      cause.death == "I670" | cause.death == "I671" | cause.death == "I672" | cause.death == "I673" |
                      cause.death == "I674" | cause.death == "I675" | cause.death == "I676" | cause.death == "I677" |
                      cause.death == "I678" | cause.death == "I679" | cause.death == "I67X" |
                      cause.death == "I69.3")

kidney2013<-filter(data2013, cause.death == "C64X" | cause.death == "C65X")

oesphagus2013<-filter(data2013, cause.death == "C150" | cause.death == "C151" | cause.death == "C152" | cause.death == "C153" |
                      cause.death == "C154" | cause.death == "C155" | cause.death == "C156" | cause.death == "C157" |
                      cause.death == "C158" | cause.death == "C159" | cause.death == "C15X")

pancreas2013<-filter(data2013, 
                    cause.death == "C250" | cause.death == "C251" | cause.death == "C252" | cause.death == "C253" |
                      cause.death == "C254" | cause.death == "C255" | cause.death == "C256" | cause.death == "C257" |
                      cause.death == "C258" | cause.death == "C259" | cause.death == "C25X" )

subaracnoid2013<-filter(data2013, cause.death == "I600" | cause.death == "I601" | cause.death == "I602" | cause.death == "I603" |
                      cause.death == "I604" | cause.death == "I605" | cause.death == "I606" | cause.death == "I607" |
                      cause.death == "I608" | cause.death == "I609" | cause.death == "I60X" |cause.death == "I620" | cause.death == "I621" | cause.death == "I622" | cause.death == "I623" |
                      cause.death == "I624" | cause.death == "I625" | cause.death == "I626" | cause.death == "I627" |
                      cause.death == "I628" | cause.death == "I629" | cause.death == "I62X" | cause.death == "I69.0" )

thyroid2013<-filter(data2013, cause.death == "C73X" | cause.death == "C73" | cause.death == "C730")

DM2013<-filter(data2013, cause.death == "E100" | cause.death == "E101" | cause.death == "E102" | cause.death == "E103" |
                           cause.death == "E104" | cause.death == "E105" | cause.death == "E106" | cause.death == "E107" |
                           cause.death == "E108" | cause.death == "E109" |
                           
                           cause.death == "E110" | cause.death == "E111" | cause.death == "E112" | cause.death == "E113" |
                           cause.death == "E114" | cause.death == "E115" | cause.death == "E116" | cause.death == "E117" |
                           cause.death == "E118" | cause.death == "E119" |
                           
                           cause.death == "E120" | cause.death == "E121" | cause.death == "E122" | cause.death == "E123" |
                           cause.death == "E124" | cause.death == "E125" | cause.death == "E126" | cause.death == "E127" |
                           cause.death == "E128" | cause.death == "E129" |
                           
                           cause.death == "E130" | cause.death == "E131" | cause.death == "E132" | cause.death == "E133" |
                           cause.death == "E134" | cause.death == "E135" | cause.death == "E136" | cause.death == "E137" |
                           cause.death == "E138" | cause.death == "E139" |
                           
                           cause.death == "E140" | cause.death == "E141" | cause.death == "E142" | cause.death == "E143" |
                           cause.death == "E144" | cause.death == "E145" | cause.death == "E146" | cause.death == "E147" |
                           cause.death == "E148" | cause.death == "E149")

Uterine2013<-filter(data2013, cause.death == "C53" | cause.death == "C53X" | cause.death == "C530" | cause.death == "C531"
                    | cause.death == "C538" | cause.death == "C539" | cause.death == "C54" | cause.death == "C54X"
                    | cause.death == "C540" | cause.death == "C541" | cause.death == "C542" | cause.death == "C543" 
                    | cause.death == "C548" | cause.death == "C549" | cause.death == "C55" | cause.death == "C55X"
                    | cause.death == "C550")
breast2013<-filter(data2013, 
                    cause.death == "C500" | cause.death == "C501" | cause.death == "C502" | cause.death == "C503" |
                       cause.death == "C504" | cause.death == "C505" | cause.death == "C506" | cause.death == "C507" |
                       cause.death == "C508" | cause.death == "C509" | cause.death == "C50X" )

#####2014

AF2014<-filter(data2014, cause.death == "I48X" | cause.death == "I48" |
                 cause.death == "I480" | cause.death == "I481"
               | cause.death == "I482" | cause.death == "I483" | cause.death == "I484" | cause.death == "I489")

asthma2014<-filter(data2014, cause.death == "J45" | cause.death == "J450" | cause.death == "J451"
                   | cause.death == "J458" | cause.death == "J459" | cause.death == "J45X")


colon2014<-filter(data2014, cause.death == "C180" | cause.death == "C181" | cause.death == "C182" | cause.death == "C183" |
                    cause.death == "C184" | cause.death == "C185" | cause.death == "C186" | cause.death == "C187" |
                    cause.death == "C188" | cause.death == "C189" | cause.death == "C18X" |
                    
                    cause.death == "C190" | cause.death == "C191" | cause.death == "C192" | cause.death == "C193" |
                    cause.death == "C194" | cause.death == "C195" | cause.death == "C196" | cause.death == "C197" |
                    cause.death == "C198" | cause.death == "C199" | cause.death == "C19X" |
                    
                    cause.death == "C200" | cause.death == "C201" | cause.death == "C202" | cause.death == "C203" |
                    cause.death == "C204" | cause.death == "C205" | cause.death == "C206" | cause.death == "C207" |
                    cause.death == "C208" | cause.death == "C209" | cause.death == "C20X")

HHD2014<-filter(data2014, cause.death == "I11" | cause.death == "I110" | cause.death == "I11X"
                |cause.death == "I119")


IHD2014<-filter(data2014, cause.death == "I200" | cause.death == "I201" | cause.death == "I202" | cause.death == "I203" |
                  cause.death == "I204" | cause.death == "I205" | cause.death == "I206" | cause.death == "I207" |
                  cause.death == "I208" | cause.death == "I209" |
                  
                  cause.death == "I210" | cause.death == "I211" | cause.death == "I212" | cause.death == "I213" |
                  cause.death == "I214" | cause.death == "I215" | cause.death == "I216" | cause.death == "I217" |
                  cause.death == "I218" | cause.death == "I219" |
                  
                  cause.death == "I220" | cause.death == "I221" | cause.death == "I222" | cause.death == "I223" |
                  cause.death == "I224" | cause.death == "I225" | cause.death == "I226" | cause.death == "I227" |
                  cause.death == "I228" | cause.death == "I229" |
                  
                  cause.death == "I230" | cause.death == "I231" | cause.death == "I232" | cause.death == "I233" |
                  cause.death == "I234" | cause.death == "I235" | cause.death == "I236" | cause.death == "I237" |
                  cause.death == "I238" | cause.death == "I239" |
                  
                  cause.death == "I240" | cause.death == "I241" | cause.death == "I242" | cause.death == "I243" |
                  cause.death == "I244" | cause.death == "I245" | cause.death == "I246" | cause.death == "I247" |
                  cause.death == "I248" | cause.death == "I249" |
                  
                  cause.death == "I250" | cause.death == "I251" | cause.death == "I252" | cause.death == "I253" |
                  cause.death == "I254" | cause.death == "I255" | cause.death == "I256" | cause.death == "I257" |
                  cause.death == "I258" | cause.death == "I259")


Isch_stroke2014<-filter(data2014, cause.death == "I630" | cause.death == "I631" | cause.death == "I632" | cause.death == "I633" |
                          cause.death == "I634" | cause.death == "I635" | cause.death == "I636" | cause.death == "I637" |
                          cause.death == "I638" | cause.death == "I639" | cause.death == "I63X" |
                          
                          cause.death == "I650" | cause.death == "I651" | cause.death == "I652" | cause.death == "I653" |
                          cause.death == "I654" | cause.death == "I655" | cause.death == "I656" | cause.death == "I657" |
                          cause.death == "I658" | cause.death == "I659" | cause.death == "I65X" |
                          
                          cause.death == "I660" | cause.death == "I661" | cause.death == "I662" | cause.death == "I663" |
                          cause.death == "I664" | cause.death == "I665" | cause.death == "I666" | cause.death == "I667" |
                          cause.death == "I668" | cause.death == "I669" | cause.death == "I66X" |
                          
                          cause.death == "I670" | cause.death == "I671" | cause.death == "I672" | cause.death == "I673" |
                          cause.death == "I674" | cause.death == "I675" | cause.death == "I676" | cause.death == "I677" |
                          cause.death == "I678" | cause.death == "I679" | cause.death == "I67X" |
                          cause.death == "I69.3")

kidney2014<-filter(data2014, cause.death == "C64X" | cause.death == "C65X")

oesphagus2014<-filter(data2014, cause.death == "C150" | cause.death == "C151" | cause.death == "C152" | cause.death == "C153" |
                        cause.death == "C154" | cause.death == "C155" | cause.death == "C156" | cause.death == "C157" |
                        cause.death == "C158" | cause.death == "C159" | cause.death == "C15X")

pancreas2014<-filter(data2014, 
                     cause.death == "C250" | cause.death == "C251" | cause.death == "C252" | cause.death == "C253" |
                       cause.death == "C254" | cause.death == "C255" | cause.death == "C256" | cause.death == "C257" |
                       cause.death == "C258" | cause.death == "C259" | cause.death == "C25X" )

subaracnoid2014<-filter(data2014, cause.death == "I600" | cause.death == "I601" | cause.death == "I602" | cause.death == "I603" |
                          cause.death == "I604" | cause.death == "I605" | cause.death == "I606" | cause.death == "I607" |
                          cause.death == "I608" | cause.death == "I609" | cause.death == "I60X" |cause.death == "I620" | cause.death == "I621" | cause.death == "I622" | cause.death == "I623" |
                          cause.death == "I624" | cause.death == "I625" | cause.death == "I626" | cause.death == "I627" |
                          cause.death == "I628" | cause.death == "I629" | cause.death == "I62X" | cause.death == "I69.0" )

thyroid2014<-filter(data2014, cause.death == "C73X" | cause.death == "C73" | cause.death == "C730")

DM2014<-filter(data2014, cause.death == "E100" | cause.death == "E101" | cause.death == "E102" | cause.death == "E103" |
                 cause.death == "E104" | cause.death == "E105" | cause.death == "E106" | cause.death == "E107" |
                 cause.death == "E108" | cause.death == "E109" |
                 
                 cause.death == "E110" | cause.death == "E111" | cause.death == "E112" | cause.death == "E113" |
                 cause.death == "E114" | cause.death == "E115" | cause.death == "E116" | cause.death == "E117" |
                 cause.death == "E118" | cause.death == "E119" |
                 
                 cause.death == "E120" | cause.death == "E121" | cause.death == "E122" | cause.death == "E123" |
                 cause.death == "E124" | cause.death == "E125" | cause.death == "E126" | cause.death == "E127" |
                 cause.death == "E128" | cause.death == "E129" |
                 
                 cause.death == "E130" | cause.death == "E131" | cause.death == "E132" | cause.death == "E133" |
                 cause.death == "E134" | cause.death == "E135" | cause.death == "E136" | cause.death == "E137" |
                 cause.death == "E138" | cause.death == "E139" |
                 
                 cause.death == "E140" | cause.death == "E141" | cause.death == "E142" | cause.death == "E143" |
                 cause.death == "E144" | cause.death == "E145" | cause.death == "E146" | cause.death == "E147" |
                 cause.death == "E148" | cause.death == "E149")


Uterine2014<-filter(data2014, cause.death == "C53" | cause.death == "C53X" | cause.death == "C530" | cause.death == "C531"
                    | cause.death == "C538" | cause.death == "C539" | cause.death == "C54" | cause.death == "C54X"
                    | cause.death == "C540" | cause.death == "C541" | cause.death == "C542" | cause.death == "C543" 
                    | cause.death == "C548" | cause.death == "C549" | cause.death == "C55" | cause.death == "C55X"
                    | cause.death == "C550")

breast2014<-filter(data2014, 
                   cause.death == "C500" | cause.death == "C501" | cause.death == "C502" | cause.death == "C503" |
                      cause.death == "C504" | cause.death == "C505" | cause.death == "C506" | cause.death == "C507" |
                      cause.death == "C508" | cause.death == "C509" | cause.death == "C50X" )

#####2015

AF2015<-filter(data2015, cause.death == "I48X" | cause.death == "I48" |
                 cause.death == "I480" | cause.death == "I481"
               | cause.death == "I482" | cause.death == "I483" | cause.death == "I484" | cause.death == "I489")

asthma2015<-filter(data2015, cause.death == "J45" | cause.death == "J450" | cause.death == "J451"
                   | cause.death == "J458" | cause.death == "J459" | cause.death == "J45X")


colon2015<-filter(data2015, cause.death == "C180" | cause.death == "C181" | cause.death == "C182" | cause.death == "C183" |
                    cause.death == "C184" | cause.death == "C185" | cause.death == "C186" | cause.death == "C187" |
                    cause.death == "C188" | cause.death == "C189" | cause.death == "C18X" |
                    
                    cause.death == "C190" | cause.death == "C191" | cause.death == "C192" | cause.death == "C193" |
                    cause.death == "C194" | cause.death == "C195" | cause.death == "C196" | cause.death == "C197" |
                    cause.death == "C198" | cause.death == "C199" | cause.death == "C19X" |
                    
                    cause.death == "C200" | cause.death == "C201" | cause.death == "C202" | cause.death == "C203" |
                    cause.death == "C204" | cause.death == "C205" | cause.death == "C206" | cause.death == "C207" |
                    cause.death == "C208" | cause.death == "C209" | cause.death == "C20X")

HHD2015<-filter(data2015, cause.death == "I11" | cause.death == "I110" | cause.death == "I11X"
                |cause.death == "I119")


IHD2015<-filter(data2015, cause.death == "I200" | cause.death == "I201" | cause.death == "I202" | cause.death == "I203" |
                  cause.death == "I204" | cause.death == "I205" | cause.death == "I206" | cause.death == "I207" |
                  cause.death == "I208" | cause.death == "I209" |
                  
                  cause.death == "I210" | cause.death == "I211" | cause.death == "I212" | cause.death == "I213" |
                  cause.death == "I214" | cause.death == "I215" | cause.death == "I216" | cause.death == "I217" |
                  cause.death == "I218" | cause.death == "I219" |
                  
                  cause.death == "I220" | cause.death == "I221" | cause.death == "I222" | cause.death == "I223" |
                  cause.death == "I224" | cause.death == "I225" | cause.death == "I226" | cause.death == "I227" |
                  cause.death == "I228" | cause.death == "I229" |
                  
                  cause.death == "I230" | cause.death == "I231" | cause.death == "I232" | cause.death == "I233" |
                  cause.death == "I234" | cause.death == "I235" | cause.death == "I236" | cause.death == "I237" |
                  cause.death == "I238" | cause.death == "I239" |
                  
                  cause.death == "I240" | cause.death == "I241" | cause.death == "I242" | cause.death == "I243" |
                  cause.death == "I244" | cause.death == "I245" | cause.death == "I246" | cause.death == "I247" |
                  cause.death == "I248" | cause.death == "I249" |
                  
                  cause.death == "I250" | cause.death == "I251" | cause.death == "I252" | cause.death == "I253" |
                  cause.death == "I254" | cause.death == "I255" | cause.death == "I256" | cause.death == "I257" |
                  cause.death == "I258" | cause.death == "I259")


Isch_stroke2015<-filter(data2015, cause.death == "I630" | cause.death == "I631" | cause.death == "I632" | cause.death == "I633" |
                          cause.death == "I634" | cause.death == "I635" | cause.death == "I636" | cause.death == "I637" |
                          cause.death == "I638" | cause.death == "I639" | cause.death == "I63X" |
                          
                          cause.death == "I650" | cause.death == "I651" | cause.death == "I652" | cause.death == "I653" |
                          cause.death == "I654" | cause.death == "I655" | cause.death == "I656" | cause.death == "I657" |
                          cause.death == "I658" | cause.death == "I659" | cause.death == "I65X" |
                          
                          cause.death == "I660" | cause.death == "I661" | cause.death == "I662" | cause.death == "I663" |
                          cause.death == "I664" | cause.death == "I665" | cause.death == "I666" | cause.death == "I667" |
                          cause.death == "I668" | cause.death == "I669" | cause.death == "I66X" |
                          
                          cause.death == "I670" | cause.death == "I671" | cause.death == "I672" | cause.death == "I673" |
                          cause.death == "I674" | cause.death == "I675" | cause.death == "I676" | cause.death == "I677" |
                          cause.death == "I678" | cause.death == "I679" | cause.death == "I67X" |
                          cause.death == "I69.3")

kidney2015<-filter(data2015, cause.death == "C64X" | cause.death == "C65X")

oesphagus2015<-filter(data2015, cause.death == "C150" | cause.death == "C151" | cause.death == "C152" | cause.death == "C153" |
                        cause.death == "C154" | cause.death == "C155" | cause.death == "C156" | cause.death == "C157" |
                        cause.death == "C158" | cause.death == "C159" | cause.death == "C15X")

pancreas2015<-filter(data2015, 
                     cause.death == "C250" | cause.death == "C251" | cause.death == "C252" | cause.death == "C253" |
                       cause.death == "C254" | cause.death == "C255" | cause.death == "C256" | cause.death == "C257" |
                       cause.death == "C258" | cause.death == "C259" | cause.death == "C25X" )

subaracnoid2015<-filter(data2015, cause.death == "I600" | cause.death == "I601" | cause.death == "I602" | cause.death == "I603" |
                          cause.death == "I604" | cause.death == "I605" | cause.death == "I606" | cause.death == "I607" |
                          cause.death == "I608" | cause.death == "I609" | cause.death == "I60X" |cause.death == "I620" | cause.death == "I621" | cause.death == "I622" | cause.death == "I623" |
                          cause.death == "I624" | cause.death == "I625" | cause.death == "I626" | cause.death == "I627" |
                          cause.death == "I628" | cause.death == "I629" | cause.death == "I62X" | cause.death == "I69.0" )

thyroid2015<-filter(data2015, cause.death == "C73X" | cause.death == "C73" | cause.death == "C730")

DM2015<-filter(data2015, cause.death == "E100" | cause.death == "E101" | cause.death == "E102" | cause.death == "E103" |
                 cause.death == "E104" | cause.death == "E105" | cause.death == "E106" | cause.death == "E107" |
                 cause.death == "E108" | cause.death == "E109" |
                 
                 cause.death == "E110" | cause.death == "E111" | cause.death == "E112" | cause.death == "E113" |
                 cause.death == "E114" | cause.death == "E115" | cause.death == "E116" | cause.death == "E117" |
                 cause.death == "E118" | cause.death == "E119" |
                 
                 cause.death == "E120" | cause.death == "E121" | cause.death == "E122" | cause.death == "E123" |
                 cause.death == "E124" | cause.death == "E125" | cause.death == "E126" | cause.death == "E127" |
                 cause.death == "E128" | cause.death == "E129" |
                 
                 cause.death == "E130" | cause.death == "E131" | cause.death == "E132" | cause.death == "E133" |
                 cause.death == "E134" | cause.death == "E135" | cause.death == "E136" | cause.death == "E137" |
                 cause.death == "E138" | cause.death == "E139" |
                 
                 cause.death == "E140" | cause.death == "E141" | cause.death == "E142" | cause.death == "E143" |
                 cause.death == "E144" | cause.death == "E145" | cause.death == "E146" | cause.death == "E147" |
                 cause.death == "E148" | cause.death == "E149")

Uterine2015<-filter(data2015, cause.death == "C53" | cause.death == "C53X" | cause.death == "C530" | cause.death == "C531"
                    | cause.death == "C538" | cause.death == "C539" | cause.death == "C54" | cause.death == "C54X"
                    | cause.death == "C540" | cause.death == "C541" | cause.death == "C542" | cause.death == "C543" 
                    | cause.death == "C548" | cause.death == "C549" | cause.death == "C55" | cause.death == "C55X"
                    | cause.death == "C550")

breast2015<-filter(data2015, 
                   cause.death == "C500" | cause.death == "C501" | cause.death == "C502" | cause.death == "C503" |
                      cause.death == "C504" | cause.death == "C505" | cause.death == "C506" | cause.death == "C507" |
                      cause.death == "C508" | cause.death == "C509" | cause.death == "C50X" )

#####2016

AF2016<-filter(data2016, cause.death == "I48X" | cause.death == "I48" |
                 cause.death == "I480" | cause.death == "I481"
               | cause.death == "I482" | cause.death == "I483" | cause.death == "I484" | cause.death == "I489")

asthma2016<-filter(data2016, cause.death == "J45" | cause.death == "J450" | cause.death == "J451"
                   | cause.death == "J458" | cause.death == "J459" | cause.death == "J45X")


colon2016<-filter(data2016, cause.death == "C180" | cause.death == "C181" | cause.death == "C182" | cause.death == "C183" |
                    cause.death == "C184" | cause.death == "C185" | cause.death == "C186" | cause.death == "C187" |
                    cause.death == "C188" | cause.death == "C189" | cause.death == "C18X" |
                    
                    cause.death == "C190" | cause.death == "C191" | cause.death == "C192" | cause.death == "C193" |
                    cause.death == "C194" | cause.death == "C195" | cause.death == "C196" | cause.death == "C197" |
                    cause.death == "C198" | cause.death == "C199" | cause.death == "C19X" |
                    
                    cause.death == "C200" | cause.death == "C201" | cause.death == "C202" | cause.death == "C203" |
                    cause.death == "C204" | cause.death == "C205" | cause.death == "C206" | cause.death == "C207" |
                    cause.death == "C208" | cause.death == "C209" | cause.death == "C20X")

HHD2016<-filter(data2016, cause.death == "I11" | cause.death == "I110" | cause.death == "I11X"
                |cause.death == "I119")


IHD2016<-filter(data2016, cause.death == "I200" | cause.death == "I201" | cause.death == "I202" | cause.death == "I203" |
                  cause.death == "I204" | cause.death == "I205" | cause.death == "I206" | cause.death == "I207" |
                  cause.death == "I208" | cause.death == "I209" |
                  
                  cause.death == "I210" | cause.death == "I211" | cause.death == "I212" | cause.death == "I213" |
                  cause.death == "I214" | cause.death == "I215" | cause.death == "I216" | cause.death == "I217" |
                  cause.death == "I218" | cause.death == "I219" |
                  
                  cause.death == "I220" | cause.death == "I221" | cause.death == "I222" | cause.death == "I223" |
                  cause.death == "I224" | cause.death == "I225" | cause.death == "I226" | cause.death == "I227" |
                  cause.death == "I228" | cause.death == "I229" |
                  
                  cause.death == "I230" | cause.death == "I231" | cause.death == "I232" | cause.death == "I233" |
                  cause.death == "I234" | cause.death == "I235" | cause.death == "I236" | cause.death == "I237" |
                  cause.death == "I238" | cause.death == "I239" |
                  
                  cause.death == "I240" | cause.death == "I241" | cause.death == "I242" | cause.death == "I243" |
                  cause.death == "I244" | cause.death == "I245" | cause.death == "I246" | cause.death == "I247" |
                  cause.death == "I248" | cause.death == "I249" |
                  
                  cause.death == "I250" | cause.death == "I251" | cause.death == "I252" | cause.death == "I253" |
                  cause.death == "I254" | cause.death == "I255" | cause.death == "I256" | cause.death == "I257" |
                  cause.death == "I258" | cause.death == "I259")


Isch_stroke2016<-filter(data2016, cause.death == "I630" | cause.death == "I631" | cause.death == "I632" | cause.death == "I633" |
                          cause.death == "I634" | cause.death == "I635" | cause.death == "I636" | cause.death == "I637" |
                          cause.death == "I638" | cause.death == "I639" | cause.death == "I63X" |
                          
                          cause.death == "I650" | cause.death == "I651" | cause.death == "I652" | cause.death == "I653" |
                          cause.death == "I654" | cause.death == "I655" | cause.death == "I656" | cause.death == "I657" |
                          cause.death == "I658" | cause.death == "I659" | cause.death == "I65X" |
                          
                          cause.death == "I660" | cause.death == "I661" | cause.death == "I662" | cause.death == "I663" |
                          cause.death == "I664" | cause.death == "I665" | cause.death == "I666" | cause.death == "I667" |
                          cause.death == "I668" | cause.death == "I669" | cause.death == "I66X" |
                          
                          cause.death == "I670" | cause.death == "I671" | cause.death == "I672" | cause.death == "I673" |
                          cause.death == "I674" | cause.death == "I675" | cause.death == "I676" | cause.death == "I677" |
                          cause.death == "I678" | cause.death == "I679" | cause.death == "I67X" |
                          cause.death == "I69.3")

kidney2016<-filter(data2016, cause.death == "C64X" | cause.death == "C65X")

oesphagus2016<-filter(data2016, cause.death == "C150" | cause.death == "C151" | cause.death == "C152" | cause.death == "C153" |
                        cause.death == "C154" | cause.death == "C155" | cause.death == "C156" | cause.death == "C157" |
                        cause.death == "C158" | cause.death == "C159" | cause.death == "C15X")

pancreas2016<-filter(data2016, 
                     cause.death == "C250" | cause.death == "C251" | cause.death == "C252" | cause.death == "C253" |
                       cause.death == "C254" | cause.death == "C255" | cause.death == "C256" | cause.death == "C257" |
                       cause.death == "C258" | cause.death == "C259" | cause.death == "C25X" )

subaracnoid2016<-filter(data2016, cause.death == "I600" | cause.death == "I601" | cause.death == "I602" | cause.death == "I603" |
                          cause.death == "I604" | cause.death == "I605" | cause.death == "I606" | cause.death == "I607" |
                          cause.death == "I608" | cause.death == "I609" | cause.death == "I60X" |cause.death == "I620" | cause.death == "I621" | cause.death == "I622" | cause.death == "I623" |
                          cause.death == "I624" | cause.death == "I625" | cause.death == "I626" | cause.death == "I627" |
                          cause.death == "I628" | cause.death == "I629" | cause.death == "I62X" | cause.death == "I69.0" )

thyroid2016<-filter(data2016, cause.death == "C73X" | cause.death == "C73" | cause.death == "C730")

DM2016<-filter(data2016, cause.death == "E100" | cause.death == "E101" | cause.death == "E102" | cause.death == "E103" |
                 cause.death == "E104" | cause.death == "E105" | cause.death == "E106" | cause.death == "E107" |
                 cause.death == "E108" | cause.death == "E109" |
                 
                 cause.death == "E110" | cause.death == "E111" | cause.death == "E112" | cause.death == "E113" |
                 cause.death == "E114" | cause.death == "E115" | cause.death == "E116" | cause.death == "E117" |
                 cause.death == "E118" | cause.death == "E119" |
                 
                 cause.death == "E120" | cause.death == "E121" | cause.death == "E122" | cause.death == "E123" |
                 cause.death == "E124" | cause.death == "E125" | cause.death == "E126" | cause.death == "E127" |
                 cause.death == "E128" | cause.death == "E129" |
                 
                 cause.death == "E130" | cause.death == "E131" | cause.death == "E132" | cause.death == "E133" |
                 cause.death == "E134" | cause.death == "E135" | cause.death == "E136" | cause.death == "E137" |
                 cause.death == "E138" | cause.death == "E139" |
                 
                 cause.death == "E140" | cause.death == "E141" | cause.death == "E142" | cause.death == "E143" |
                 cause.death == "E144" | cause.death == "E145" | cause.death == "E146" | cause.death == "E147" |
                 cause.death == "E148" | cause.death == "E149")

Uterine2016<-filter(data2016, cause.death == "C53" | cause.death == "C53X" | cause.death == "C530" | cause.death == "C531"
                    | cause.death == "C538" | cause.death == "C539" | cause.death == "C54" | cause.death == "C54X"
                    | cause.death == "C540" | cause.death == "C541" | cause.death == "C542" | cause.death == "C543" 
                    | cause.death == "C548" | cause.death == "C549" | cause.death == "C55" | cause.death == "C55X"
                    | cause.death == "C550")

breast2016<-filter(data2016, 
                   cause.death == "C500" | cause.death == "C501" | cause.death == "C502" | cause.death == "C503" |
                      cause.death == "C504" | cause.death == "C505" | cause.death == "C506" | cause.death == "C507" |
                      cause.death == "C508" | cause.death == "C509" | cause.death == "C50X" )

#####2017

AF2017<-filter(data2017, cause.death == "I48X" | cause.death == "I48" |
                 cause.death == "I480" | cause.death == "I481"
               | cause.death == "I482" | cause.death == "I483" | cause.death == "I484" | cause.death == "I489")

asthma2017<-filter(data2017, cause.death == "J45" | cause.death == "J450" | cause.death == "J451"
                   | cause.death == "J458" | cause.death == "J459" | cause.death == "J45X")


colon2017<-filter(data2017, cause.death == "C180" | cause.death == "C181" | cause.death == "C182" | cause.death == "C183" |
                    cause.death == "C184" | cause.death == "C185" | cause.death == "C186" | cause.death == "C187" |
                    cause.death == "C188" | cause.death == "C189" | cause.death == "C18X" |
                    
                    cause.death == "C190" | cause.death == "C191" | cause.death == "C192" | cause.death == "C193" |
                    cause.death == "C194" | cause.death == "C195" | cause.death == "C196" | cause.death == "C197" |
                    cause.death == "C198" | cause.death == "C199" | cause.death == "C19X" |
                    
                    cause.death == "C200" | cause.death == "C201" | cause.death == "C202" | cause.death == "C203" |
                    cause.death == "C204" | cause.death == "C205" | cause.death == "C206" | cause.death == "C207" |
                    cause.death == "C208" | cause.death == "C209" | cause.death == "C20X")

HHD2017<-filter(data2017, cause.death == "I11" | cause.death == "I110" | cause.death == "I11X"
                |cause.death == "I119")


IHD2017<-filter(data2017, cause.death == "I200" | cause.death == "I201" | cause.death == "I202" | cause.death == "I203" |
                  cause.death == "I204" | cause.death == "I205" | cause.death == "I206" | cause.death == "I207" |
                  cause.death == "I208" | cause.death == "I209" |
                  
                  cause.death == "I210" | cause.death == "I211" | cause.death == "I212" | cause.death == "I213" |
                  cause.death == "I214" | cause.death == "I215" | cause.death == "I216" | cause.death == "I217" |
                  cause.death == "I218" | cause.death == "I219" |
                  
                  cause.death == "I220" | cause.death == "I221" | cause.death == "I222" | cause.death == "I223" |
                  cause.death == "I224" | cause.death == "I225" | cause.death == "I226" | cause.death == "I227" |
                  cause.death == "I228" | cause.death == "I229" |
                  
                  cause.death == "I230" | cause.death == "I231" | cause.death == "I232" | cause.death == "I233" |
                  cause.death == "I234" | cause.death == "I235" | cause.death == "I236" | cause.death == "I237" |
                  cause.death == "I238" | cause.death == "I239" |
                  
                  cause.death == "I240" | cause.death == "I241" | cause.death == "I242" | cause.death == "I243" |
                  cause.death == "I244" | cause.death == "I245" | cause.death == "I246" | cause.death == "I247" |
                  cause.death == "I248" | cause.death == "I249" |
                  
                  cause.death == "I250" | cause.death == "I251" | cause.death == "I252" | cause.death == "I253" |
                  cause.death == "I254" | cause.death == "I255" | cause.death == "I256" | cause.death == "I257" |
                  cause.death == "I258" | cause.death == "I259")


Isch_stroke2017<-filter(data2017, cause.death == "I630" | cause.death == "I631" | cause.death == "I632" | cause.death == "I633" |
                          cause.death == "I634" | cause.death == "I635" | cause.death == "I636" | cause.death == "I637" |
                          cause.death == "I638" | cause.death == "I639" | cause.death == "I63X" |
                          
                          cause.death == "I650" | cause.death == "I651" | cause.death == "I652" | cause.death == "I653" |
                          cause.death == "I654" | cause.death == "I655" | cause.death == "I656" | cause.death == "I657" |
                          cause.death == "I658" | cause.death == "I659" | cause.death == "I65X" |
                          
                          cause.death == "I660" | cause.death == "I661" | cause.death == "I662" | cause.death == "I663" |
                          cause.death == "I664" | cause.death == "I665" | cause.death == "I666" | cause.death == "I667" |
                          cause.death == "I668" | cause.death == "I669" | cause.death == "I66X" |
                          
                          cause.death == "I670" | cause.death == "I671" | cause.death == "I672" | cause.death == "I673" |
                          cause.death == "I674" | cause.death == "I675" | cause.death == "I676" | cause.death == "I677" |
                          cause.death == "I678" | cause.death == "I679" | cause.death == "I67X" |
                          cause.death == "I69.3")

kidney2017<-filter(data2017, cause.death == "C64X" | cause.death == "C65X")

oesphagus2017<-filter(data2017, cause.death == "C150" | cause.death == "C151" | cause.death == "C152" | cause.death == "C153" |
                        cause.death == "C154" | cause.death == "C155" | cause.death == "C156" | cause.death == "C157" |
                        cause.death == "C158" | cause.death == "C159" | cause.death == "C15X")

pancreas2017<-filter(data2017, 
                     cause.death == "C250" | cause.death == "C251" | cause.death == "C252" | cause.death == "C253" |
                       cause.death == "C254" | cause.death == "C255" | cause.death == "C256" | cause.death == "C257" |
                       cause.death == "C258" | cause.death == "C259" | cause.death == "C25X" )

subaracnoid2017<-filter(data2017, cause.death == "I600" | cause.death == "I601" | cause.death == "I602" | cause.death == "I603" |
                          cause.death == "I604" | cause.death == "I605" | cause.death == "I606" | cause.death == "I607" |
                          cause.death == "I608" | cause.death == "I609" | cause.death == "I60X" |cause.death == "I620" | cause.death == "I621" | cause.death == "I622" | cause.death == "I623" |
                          cause.death == "I624" | cause.death == "I625" | cause.death == "I626" | cause.death == "I627" |
                          cause.death == "I628" | cause.death == "I629" | cause.death == "I62X" | cause.death == "I69.0" )

thyroid2017<-filter(data2017, cause.death == "C73X" | cause.death == "C73" | cause.death == "C730")

DM2017<-filter(data2017, cause.death == "E100" | cause.death == "E101" | cause.death == "E102" | cause.death == "E103" |
                 cause.death == "E104" | cause.death == "E105" | cause.death == "E106" | cause.death == "E107" |
                 cause.death == "E108" | cause.death == "E109" |
                 
                 cause.death == "E110" | cause.death == "E111" | cause.death == "E112" | cause.death == "E113" |
                 cause.death == "E114" | cause.death == "E115" | cause.death == "E116" | cause.death == "E117" |
                 cause.death == "E118" | cause.death == "E119" |
                 
                 cause.death == "E120" | cause.death == "E121" | cause.death == "E122" | cause.death == "E123" |
                 cause.death == "E124" | cause.death == "E125" | cause.death == "E126" | cause.death == "E127" |
                 cause.death == "E128" | cause.death == "E129" |
                 
                 cause.death == "E130" | cause.death == "E131" | cause.death == "E132" | cause.death == "E133" |
                 cause.death == "E134" | cause.death == "E135" | cause.death == "E136" | cause.death == "E137" |
                 cause.death == "E138" | cause.death == "E139" |
                 
                 cause.death == "E140" | cause.death == "E141" | cause.death == "E142" | cause.death == "E143" |
                 cause.death == "E144" | cause.death == "E145" | cause.death == "E146" | cause.death == "E147" |
                 cause.death == "E148" | cause.death == "E149")

Uterine2017<-filter(data2017, cause.death == "C53" | cause.death == "C53X" | cause.death == "C530" | cause.death == "C531"
                    | cause.death == "C538" | cause.death == "C539" | cause.death == "C54" | cause.death == "C54X"
                    | cause.death == "C540" | cause.death == "C541" | cause.death == "C542" | cause.death == "C543" 
                    | cause.death == "C548" | cause.death == "C549" | cause.death == "C55" | cause.death == "C55X"
                    | cause.death == "C550")
breast2017<-filter(data2017, 
                   cause.death == "C500" | cause.death == "C501" | cause.death == "C502" | cause.death == "C503" |
                      cause.death == "C504" | cause.death == "C505" | cause.death == "C506" | cause.death == "C507" |
                      cause.death == "C508" | cause.death == "C509" | cause.death == "C50X" )

#####2018

AF2018<-filter(data2018, cause.death == "I48X" | cause.death == "I48" |
                 cause.death == "I480" | cause.death == "I481"
               | cause.death == "I482" | cause.death == "I483" | cause.death == "I484" | cause.death == "I489")

asthma2018<-filter(data2018, cause.death == "J45" | cause.death == "J450" | cause.death == "J451"
                   | cause.death == "J458" | cause.death == "J459" | cause.death == "J45X")


colon2018<-filter(data2018, cause.death == "C180" | cause.death == "C181" | cause.death == "C182" | cause.death == "C183" |
                    cause.death == "C184" | cause.death == "C185" | cause.death == "C186" | cause.death == "C187" |
                    cause.death == "C188" | cause.death == "C189" | cause.death == "C18X" |
                    
                    cause.death == "C190" | cause.death == "C191" | cause.death == "C192" | cause.death == "C193" |
                    cause.death == "C194" | cause.death == "C195" | cause.death == "C196" | cause.death == "C197" |
                    cause.death == "C198" | cause.death == "C199" | cause.death == "C19X" |
                    
                    cause.death == "C200" | cause.death == "C201" | cause.death == "C202" | cause.death == "C203" |
                    cause.death == "C204" | cause.death == "C205" | cause.death == "C206" | cause.death == "C207" |
                    cause.death == "C208" | cause.death == "C209" | cause.death == "C20X")

HHD2018<-filter(data2018, cause.death == "I11" | cause.death == "I110" | cause.death == "I11X"
                |cause.death == "I119")

haemo_stroke_2018<-filter(data2018, cause.death == "I600" | cause.death == "I601" | cause.death == "I602" | cause.death == "I603" |
                        cause.death == "I604" | cause.death == "I605" | cause.death == "I606" | cause.death == "I607" |
                        cause.death == "I608" | cause.death == "I609" | cause.death == "I60X" |cause.death == "I61" |cause.death == "I610" | cause.death == "I611" | cause.death == "I612" | cause.death == "I613" |
                       cause.death == "I614" | cause.death == "I615" | cause.death == "I616" | cause.death == "I617" |
                       cause.death == "I618" | cause.death == "I619" | cause.death == "I61X"| cause.death == "I620" | cause.death == "I621" | cause.death == "I622" | cause.death == "I623" |
                           cause.death == "I624" | cause.death == "I625" | cause.death == "I626" | cause.death == "I627" |
                           cause.death == "I628" | cause.death == "I629" | cause.death == "I62X" | cause.death == "I690" | cause.death == "I691"| cause.death == "I692" )

IHD2018<-filter(data2018, cause.death == "I200" | cause.death == "I201" | cause.death == "I202" | cause.death == "I203" |
                  cause.death == "I204" | cause.death == "I205" | cause.death == "I206" | cause.death == "I207" |
                  cause.death == "I208" | cause.death == "I209" |
                  
                  cause.death == "I210" | cause.death == "I211" | cause.death == "I212" | cause.death == "I213" |
                  cause.death == "I214" | cause.death == "I215" | cause.death == "I216" | cause.death == "I217" |
                  cause.death == "I218" | cause.death == "I219" | cause.death == "I21" |
                  
                  cause.death == "I220" | cause.death == "I221" | cause.death == "I222" | cause.death == "I223" |
                  cause.death == "I224" | cause.death == "I225" | cause.death == "I226" | cause.death == "I227" |
                  cause.death == "I228" | cause.death == "I229" | cause.death == "I22" |
                  
                  cause.death == "I230" | cause.death == "I231" | cause.death == "I232" | cause.death == "I233" |
                  cause.death == "I234" | cause.death == "I235" | cause.death == "I236" | cause.death == "I237" |
                  cause.death == "I238" | cause.death == "I239" | cause.death == "I23" |
                  
                  cause.death == "I240" | cause.death == "I241" | cause.death == "I242" | cause.death == "I243" |
                  cause.death == "I244" | cause.death == "I245" | cause.death == "I246" | cause.death == "I247" |
                  cause.death == "I248" | cause.death == "I249" |
                  
                  cause.death == "I250" | cause.death == "I251" | cause.death == "I252" | cause.death == "I253" |
                  cause.death == "I254" | cause.death == "I255" | cause.death == "I256" | cause.death == "I257" |
                  cause.death == "I258" | cause.death == "I259")


Isch_stroke2018<-filter(data2018, cause.death == "I630" | cause.death == "I631" | cause.death == "I632" | cause.death == "I633" |
                          cause.death == "I634" | cause.death == "I635" | cause.death == "I636" | cause.death == "I637" |
                          cause.death == "I638" | cause.death == "I639" | cause.death == "I63X" | cause.death == "I63" |
                          
                          cause.death == "I650" | cause.death == "I651" | cause.death == "I652" | cause.death == "I653" |
                          cause.death == "I654" | cause.death == "I655" | cause.death == "I656" | cause.death == "I657" |
                          cause.death == "I658" | cause.death == "I659" | cause.death == "I65X" |
                          
                          cause.death == "I660" | cause.death == "I661" | cause.death == "I662" | cause.death == "I663" |
                          cause.death == "I664" | cause.death == "I665" | cause.death == "I666" | cause.death == "I667" |
                          cause.death == "I668" | cause.death == "I669" | cause.death == "I66X" |
                          
                          cause.death == "I670" | cause.death == "I671" | cause.death == "I672" | cause.death == "I673" |
                          cause.death == "I674" | cause.death == "I675" | cause.death == "I676" | cause.death == "I677" |
                          cause.death == "I678" | cause.death == "I679" | cause.death == "I67X" | cause.death == "I67" |
                          cause.death == "I693" | cause.death == "I69.3")

kidney2018<-filter(data2018, cause.death == "C64X" | cause.death == "C65X" |cause.death == "C64" | cause.death == "C65" )

oesphagus2018<-filter(data2018, cause.death == "C150" | cause.death == "C151" | cause.death == "C152" | cause.death == "C153" |
                        cause.death == "C154" | cause.death == "C155" | cause.death == "C156" | cause.death == "C157" |
                        cause.death == "C158" | cause.death == "C159" | cause.death == "C15X")

pancreas2018<-filter(data2018, 
                     cause.death == "C250" | cause.death == "C251" | cause.death == "C252" | cause.death == "C253" |
                       cause.death == "C254" | cause.death == "C255" | cause.death == "C256" | cause.death == "C257" |
                       cause.death == "C258" | cause.death == "C259" | cause.death == "C25X" )


thyroid2018<-filter(data2018, cause.death == "C73X" | cause.death == "C73" | cause.death == "C730")

DM2018<-filter(data2018, cause.death == "E100" | cause.death == "E101" | cause.death == "E102" | cause.death == "E103" |
                 cause.death == "E104" | cause.death == "E105" | cause.death == "E106" | cause.death == "E107" |
                 cause.death == "E108" | cause.death == "E109" |  cause.death == "E10" | 
                 
                 cause.death == "E110" | cause.death == "E111" | cause.death == "E112" | cause.death == "E113" |
                 cause.death == "E114" | cause.death == "E115" | cause.death == "E116" | cause.death == "E117" |
                 cause.death == "E118" | cause.death == "E119" | cause.death == "E11" | 
                 
                 cause.death == "E120" | cause.death == "E121" | cause.death == "E122" | cause.death == "E123" |
                 cause.death == "E124" | cause.death == "E125" | cause.death == "E126" | cause.death == "E127" |
                 cause.death == "E128" | cause.death == "E129" |  cause.death == "E12" | 
                 
                 cause.death == "E130" | cause.death == "E131" | cause.death == "E132" | cause.death == "E133" |
                 cause.death == "E134" | cause.death == "E135" | cause.death == "E136" | cause.death == "E137" |
                 cause.death == "E138" | cause.death == "E139" |  cause.death == "E13" | 
                 
                 cause.death == "E140" | cause.death == "E141" | cause.death == "E142" | cause.death == "E143" |
                 cause.death == "E144" | cause.death == "E145" | cause.death == "E146" | cause.death == "E147" |
                 cause.death == "E148" | cause.death == "E149" |  cause.death == "E14" )


Uterine2018<-filter(data2018, cause.death == "C53" | cause.death == "C53X" | cause.death == "C530" | cause.death == "C531"
                    | cause.death == "C538" | cause.death == "C539" | cause.death == "C54" | cause.death == "C54X"
                    | cause.death == "C540" | cause.death == "C541" | cause.death == "C542" | cause.death == "C543" 
                    | cause.death == "C548" | cause.death == "C549" | cause.death == "C55" | cause.death == "C55X"
                    | cause.death == "C550")

breast2018<-filter(data2018, 
                   cause.death == "C500" | cause.death == "C501" | cause.death == "C502" | cause.death == "C503" |
                      cause.death == "C504" | cause.death == "C505" | cause.death == "C506" | cause.death == "C507" |
                      cause.death == "C508" | cause.death == "C509" | cause.death == "C50X"  | cause.death == "C50")



AF<-rbind(AF2013, AF2014, AF2015, AF2016, AF2017, AF2018)
AF$outcome<- "AF"

asthma<-rbind(asthma2013, asthma2014, asthma2015, asthma2016, asthma2017, asthma2018)
asthma$outcome<- "Asthma"

colon<-rbind(colon2013, colon2014, colon2015, colon2016, colon2017, colon2018)
colon$outcome<- "Colorrectal"

DM<-rbind(DM2013,DM2014, DM2015, DM2016, DM2017, DM2018)
DM$outcome<- "DM"

HHD<-rbind(HHD2013, HHD2014, HHD2015, HHD2016, HHD2017, HHD2018)
HHD$outcome<- "HHD"

IHD<-rbind(IHD2013, IHD2014, IHD2015, IHD2016, IHD2017, IHD2018)
IHD$outcome<- "IHD"

Isch_stroke<-rbind(Isch_stroke2013, Isch_stroke2014, Isch_stroke2015, Isch_stroke2016, Isch_stroke2017, Isch_stroke2018)
Isch_stroke$outcome<- "Ischemic stroke"

haemo_stroke<-haemo_stroke_2018 #
haemo_stroke$outcome<- "Haemorrhagic stroke"

kidney<-rbind(kidney2013, kidney2014, kidney2015, kidney2016, kidney2017, kidney2018)
kidney$outcome<- "Kidney"

oesphagus<-rbind(oesphagus2013, oesphagus2014, oesphagus2015, oesphagus2016, oesphagus2017, oesphagus2018)
oesphagus$outcome<- "Oesophageal"

pancreas<-rbind(pancreas2013, pancreas2014, pancreas2015, pancreas2016, pancreas2017, pancreas2018)
pancreas$outcome<- "Pancreatic"

#subaracnoid<-rbind(subaracnoid2013, subaracnoid2014, subaracnoid2015, subaracnoid2016, subaracnoid2017, subaracnoid2018)
#subaracnoid$outcome<- "SHA"

thyroid<-rbind(thyroid2013, thyroid2014, thyroid2015, thyroid2016, thyroid2017, thyroid2018)
thyroid$outcome<- "Thyroid"

uterine<-rbind(Uterine2013, Uterine2014, Uterine2015, Uterine2016, Uterine2017, Uterine2018)
uterine$outcome<- "Uterine"
   
breast<-rbind(breast2013, breast2014, breast2015, breast2016, breast2017, breast2018)
breast$outcome<- "Breast"




rm(AF2013, AF2014, AF2015, AF2016, AF2017, AF2018, asthma2013, asthma2014, asthma2015, asthma2016, asthma2017, asthma2018, 
   colon2013, colon2014, colon2015, colon2016, colon2017, colon2018, DM2013,DM2014, DM2015, DM2016, DM2017, DM2018, 
   HHD2013, HHD2014, HHD2015, HHD2016, HHD2017, HHD2018, IHD2013, IHD2014, IHD2015, IHD2016, IHD2017, IHD2018, 
   Isch_stroke2013, Isch_stroke2014, Isch_stroke2015, Isch_stroke2016, Isch_stroke2017, Isch_stroke2018, kidney2013, kidney2014, kidney2015, kidney2016, kidney2017, kidney2018, 
   oesphagus2013, oesphagus2014, oesphagus2015, oesphagus2016, oesphagus2017, oesphagus2018,pancreas2013, pancreas2014, pancreas2015, pancreas2016, pancreas2017, pancreas2018, 
   subaracnoid2013, subaracnoid2014, subaracnoid2015, subaracnoid2016, subaracnoid2017, subaracnoid2018,  thyroid2013, thyroid2014, thyroid2015, thyroid2016, thyroid2017, thyroid2018,
   Uterine2013, Uterine2014, Uterine2015, Uterine2016, Uterine2017, Uterine2018,  
   breast2013, breast2014, breast2015, breast2016, breast2017, breast2018, Intra2018)


deaths<-rbind(AF, asthma,colon,DM, HHD, IHD, haemo_stroke, Isch_stroke, kidney, oesphagus, pancreas,  thyroid, uterine, breast)

rm(AF, asthma,colon,DM, HHD, IHD, haemo_stroke, Isch_stroke, kidney, oesphagus, pancreas,  thyroid, uterine, breast)

##Removing observations with missing data in sex and region, also removing foreigners
deaths<-deaths[which(!is.na(deaths$sex)),]

deaths<-deaths[which(!is.na(deaths$region)),]
deaths<-deaths[which(!deaths$region == "NULL"),]

deaths<-deaths[which(!deaths$region == "EXTRANJEROS"),]

## Group in 14 age groups
## 
deaths$agecat <- ifelse(deaths$age >=20 & deaths$age <=24, 1, 
                           ifelse(deaths$age >= 25  & deaths$age <=29, 2,
                                  ifelse(deaths$age >= 30  & deaths$age <=34, 3, 
                                         ifelse(deaths$age >= 35  & deaths$age <=39, 4, 
                                                ifelse(deaths$age >= 40  & deaths$age <=44, 5, 
                                                       ifelse(deaths$age >= 45  & deaths$age <=49, 6, 
                                  ifelse(deaths$age >= 50  & deaths$age <=54, 7,
                                         ifelse(deaths$age >= 55  & deaths$age <=59,8,
                                                ifelse(deaths$age >= 60  & deaths$age <=64, 9,
                                                       ifelse(deaths$age >= 65  & deaths$age <=69, 10,
                                                              ifelse(deaths$age >= 70  & deaths$age <=74, 11,
                                                       ifelse(deaths$age >= 75  & deaths$age <=79, 12,
                                                              ifelse(deaths$age >= 80  & deaths$age <= 84, 13,
                                                              ifelse(deaths$age >= 85, 14, NA))))))))))))))
deaths$agecat <- factor(deaths$agecat, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), labels = c("20-24", "25-29","30-34","35-39","40-44","45-49","50-54", "55-59", "60-64", "65-69","70-74","75-79", "80-84","85+"))


#Sum deaths by specific NCDs outcome
deaths <- data.frame(with(deaths, table(year, region, sex, agecat, outcome)))
names(deaths)[6]<-"deaths"

write.csv(deaths,
          paste0("~/Desktop/Artículos/CRA High BMI Peru/Data/Deaths_NCD_Peru_", Sys.Date(), ".csv"),
          row.names = FALSE)


