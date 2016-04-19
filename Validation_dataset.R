library(dplyr)
library(tidyr)
library(readr)
library(xlsx)

evol <- read.xlsx("D:/Google Drive/Medicina/Investigacion/Investigaciones En curso/Data mining/Validacion algortimo DBT/Validation dataset/pma0719336_evol_full_tst_DBT.xlsx", 1)
evol <- select(evol, -EDAD20150101, -ESTADO, -DIAGNOSTICO, -FULLYSPECIFIEDNAME)
evol$TEXTO <- paste(evol$FECHA, evol$TEXTO, sep=' - ')
evol <- group_by(evol, ID_PACIENTE) %>% mutate(evol=rank(FECHA, ties.method='random')) %>% arrange(evol)
evol <- evol[!duplicated(select(evol, -FECHA)),] %>% select(-FECHA) 
evol <- evol[!duplicated(evol),] %>% spread(evol, TEXTO)
evol_short <- evol[is.na(evol$`10`),]
evol_long <- evol[!is.na(evol$`10`),]

