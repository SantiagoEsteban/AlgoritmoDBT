library(dplyr)
library(tidyr)
library(readr)
library(xlsx)

#Evoluciones
############
evol <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/pma0719336_evol_full_tst_DBT.xlsx", 1)
evol <- select(evol, -ESTADO, -DIAGNOSTICO, -FULLYSPECIFIEDNAME)
evol$TEXTO <- paste(evol$FECHA, evol$TEXTO, sep=' - ')
evol <- group_by(evol, ID_PACIENTE) %>% mutate(evol=rank(FECHA, ties.method='random')) %>% arrange(evol)
evol <- evol[!duplicated(select(evol, -FECHA)),] %>% select(-FECHA) 
evol <- evol[!duplicated(evol),] %>% spread(evol, TEXTO)
evol_short <- evol[is.na(evol$`10`),]
evol_long <- evol[!is.na(evol$`10`),]

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

