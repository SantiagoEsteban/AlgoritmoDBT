library(dplyr)
library(tidyr)
library(readr)
library(xlsx)
library(stringr)

#Evoluciones
############
evol <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/pma0719336_evol_full_tst_DBT.xlsx", 1)
evol <- select(evol, -ESTADO, -DIAGNOSTICO, -FULLYSPECIFIEDNAME)
evol$TEXTO <- paste(evol$FECHA, evol$TEXTO, sep=' - ')
evol <- group_by(evol, ID_PACIENTE) %>% mutate(evol=rank(FECHA, ties.method='random')) %>% arrange(evol)
evol <- evol[!duplicated(select(evol, -FECHA)),] %>% select(-FECHA) 
evol <- evol[!duplicated(evol),] %>% spread(evol, TEXTO)
evol_short <- evol[is.na(evol$`10`),] %>% select(-EDAD20150101, -(12:101))
evol_long <- evol[!is.na(evol$`10`),] %>% select(-EDAD20150101)

#Problemas
##########
prob <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/pma0719336_prob_full_tst_DBT.xlsx", 1)
prob <- select(prob, -EDAD20150101, -PROBLEMA)
prob_count <- group_by(prob, ID_PACIENTE) %>% mutate(RANK=rank(CONCEPTO, ties.method='random'), PROBLEMAS=max(RANK))
prob_count <- prob_count %>% select(-CONCEPTO, -RANK)
prob_count <- prob_count[!duplicated(prob_count),]

#Medicacion
###########
meds <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/PMA0719336_TST_antidbT.xlsx", 1)
meds <- meds %>% group_by(ID_PACIENTE) %>% summarise(CANT_CONSUMOS_TOTAL=sum(CANT_CONSUMOS))

#Lista
######
Valid_DBT_2015 <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/PMA0719336_TST_DBT_800.xlsx", 1)
Valid_DBT_2015 <- select(Valid_DBT_2015, -FECHAING, -FECHABAJA)

#GLU_120
########
glu_120 <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/PMA0719336_TST_Glu120.xlsx", 1)
glu_120 <- glu_120[!is.na(glu_120$GLU120),]
glu_120$glu_120_FR <- glu_120$GLU120>=200
glu_120$glu_120_R <- glu_120$GLU120<200
glu_120 <- glu_120 %>% group_by(ID_PACIENTE) %>% summarise(GLU_120M_FR=sum(glu_120_FR), GLU_120M_R=sum(glu_120_R))

#Glucemia ayunas
################
glu_ayunas <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/PMA0719336_TST_GluOut.xlsx", 1)
glu_ayunas <- glu_ayunas[!is.na(glu_ayunas$GLU_OUTP),]
glu_ayunas$GLU_OUTP_FR <- glu_ayunas$GLU_OUTP>=126
glu_ayunas$GLU_OUTP_R <- glu_ayunas$GLU_OUTP<126
glu_ayunas <- glu_ayunas %>% group_by(ID_PACIENTE) %>% summarise(count_GLU_OUTP_FR=sum(GLU_OUTP_FR), count_GLU_OUTP_R=sum(GLU_OUTP_R))

#HbA1c
######
hb1ac <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/PMA0719336_TST_HbGli.xlsx", 1)
hb1ac <- hb1ac[!is.na(hb1ac$HB_GLI),]
hb1ac$HB_GLI_FR <- hb1ac$HB_GLI>=6.5
hb1ac$HB_GLI_R <- hb1ac$HB_GLI<6.5
hb1ac <- hb1ac %>% group_by(ID_PACIENTE) %>% summarise(count_HB_GLI_FR=sum(HB_GLI_FR), count_HB_GLI_R=sum(HB_GLI_R))


#Putting it all together
########################

Valid_DBT_2015_short <- Valid_DBT_2015 %>% arrange(ID_PACIENTE)
Valid_DBT_2015_short <- left_join(Valid_DBT_2015_short, evol_short, by='ID_PACIENTE') %>% left_join(prob_count, by='ID_PACIENTE') %>%
                  left_join(meds, by='ID_PACIENTE') %>% left_join(glu_120, by='ID_PACIENTE') %>% left_join(glu_ayunas, by="ID_PACIENTE") %>%
                  left_join(hb1ac, by='ID_PACIENTE')
Valid_DBT_2015_short <- replace_na(Valid_DBT_2015_short, list(PROBLEMAS=0, CANT_CONSUMOS_TOTAL=0, GLU_120M_FR=0, GLU_120M_R=0,
                                                            count_GLU_OUTP_FR=0, count_GLU_OUTP_R=0, count_HB_GLI_FR=0, count_HB_GLI_R=0))
Valid_DBT_2015_short <- filter(Valid_DBT_2015_short, !(ID_PACIENTE %in% evol_long$ID_PACIENTE))

Valid_DBT_2015_long <- Valid_DBT_2015 %>% arrange(ID_PACIENTE)
Valid_DBT_2015_long <- filter(Valid_DBT_2015_long, !(ID_PACIENTE %in% Valid_DBT_2015_short$ID_PACIENTE))
Valid_DBT_2015_long <- left_join(Valid_DBT_2015_long, evol_long, by='ID_PACIENTE')%>% left_join(glu_ayunas, by="ID_PACIENTE") %>%
    left_join(hb1ac, by='ID_PACIENTE') %>% left_join(prob_count, by='ID_PACIENTE') %>% left_join(meds, by='ID_PACIENTE') %>% left_join(glu_120, by='ID_PACIENTE') 
Valid_DBT_2015_long <- replace_na(Valid_DBT_2015_long, list(PROBLEMAS=0, CANT_CONSUMOS_TOTAL=0, GLU_120M_FR=0, GLU_120M_R=0,
                                                              count_GLU_OUTP_FR=0, count_GLU_OUTP_R=0, count_HB_GLI_FR=0, count_HB_GLI_R=0))



#Extraer valores de las evol
############################
evol2 <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/pma0719336_evol_full_tst_DBT.xlsx", 1)
evol2 <- select(evol2, -ESTADO, -DIAGNOSTICO, -FULLYSPECIFIEDNAME, -EDAD20150101)
evol2$glu1 <- as.numeric(str_extract_all(evol2$TEXTO, "(?<=(?i)glu)[0-9]+"))
evol2$glu2 <- as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glu )[0-9]+"))
evol2$glu3 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glu  )[0-9]+"))
evol2$glu4 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glu: )[0-9]+"))
evol2$glu5 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glu:  )[0-9]+"))
evol2$glu6 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glucemia)[0-9]+"))
evol2$glu7 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glucemia )[0-9]+"))
evol2$glu8 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glucemia  )[0-9]+"))
evol2$glu9 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glucemia: )[0-9]+"))
evol2$glu10 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)glucemia:  )[0-9]+"))
evol2$glu11 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)gluc)[0-9]+"))
evol2$glu12 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)gluc )[0-9]+"))
evol2$glu13 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)gluc  )[0-9]+"))
evol2$glu14 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)gluc: )[0-9]+"))
evol2$glu15 <-as.numeric(str_extract(evol2$TEXTO, "(?<=(?i)gluc:  )[0-9]+"))
evol2$glu_normal <- str_detect(evol2$TEXTO, "(?i)glucemia normal|(?i)glucemia sp|(?i)glucemia s\\/p|glu normal|(?i)glu sp|(?i)glu s\\/p")
evol2$glu_normal <- ifelse(evol2$glu_normal==TRUE,1,0)
evol2$glu_total <- rowMeans(select(evol2, 4:18), na.rm=T)
evol2 <- select(evol2, ID_PACIENTE, FECHA, glu_total, glu_normal)
evol2$glu_total[evol2$glu_total=='NaN']<- NA
evol2$glu_total_FR <- ifelse(evol2$glu_total>=126, 1, 0)
evol2$glu_total_R <- ifelse(evol2$glu_total<126, 1, 0)


#Looking up the patients who have 0 for all covariates

evol3 <- group_by(ID_PACIENTE)





#Exporting
##########

write.xlsx(Valid_DBT_2015_long, "Valid_DBT_2015_long.xlsx", showNA=F)
write.xlsx(Valid_DBT_2015_short, "Valid_DBT_2015_short.xlsx", showNA=F)
