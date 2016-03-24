library(dplyr)
library(caret)
library(tidyr)
library(broom)
library(readxl)

DBT <- read_excel('PMA0719336_ScoreRiesgoCV_Corr_20150715 - FINAL CON OUTCOMES -Missing 2.xlsx')
DBT <- select(DBT, -Comentarios, -Total_con_guardia, -Total_sin_guardia, -CANT_GLU_GUARDIA_FR, -CANT_GLU_GUARDIA_R)
