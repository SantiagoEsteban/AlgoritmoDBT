library(dplyr)
library(caret)
library(tidyr)
library(readxl)
library(gbm)
library(doSNOW)
library(stringr)


#Read in data
DBT <- read_excel('PMA0719336_ScoreRiesgoCV_Corr_20150715 - FINAL CON OUTCOMES -Missing 2.xlsx')
DBT <- select(DBT, -Comentarios, -Total_con_guardia, -Total_sin_guardia, -CANT_GLU_GUARDIA_FR, -CANT_GLU_GUARDIA_R)
DBT$DBT_Manual <- make.names(DBT$DBT_Manual)
DBT <- DBT[-1664,]

#Seleccionar solo aquellos que no tienen datos
DBT$total <- select(DBT, 4:12) %>% rowSums()

#Seleccionar solo aquellas evoluciones de quienes no tienen datos
missing <- filter(DBT, total==0) %>% select(ID_PACIENTE)
evol_missing <- read_excel('pma0719336_dbt_01_05_evoluc.xls')
evol_missing <- select(evol_missing, -ESTADO)
evol_missing <- filter(evol_missing, FECHA < '2005-01-01')
evol_missing2 <- inner_join(evol_missing, missing, by='ID_PACIENTE')
#nmissing <- evol_missing2 %>% group_by(ID_PACIENTE) %>% summarise(n=n())


#HASTA ACA
evol_missing2$glu1 <- as.numeric(str_extract_all(evol_missing2$TEXTO, regex("(?<=(?i)glu)[0-9]+", ignore.case=TRUE)))
evol_missing2$glu2 <- as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glu )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu3 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glu  )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu4 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glu: )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu5 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glu:  )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu6 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glucemia)[0-9]+", ignore.case=TRUE)))
evol_missing2$glu7 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glucemia )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu8 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glucemia  )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu9 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glucemia: )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu10 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glucemia:  )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu11 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gluc)[0-9]+", ignore.case=TRUE)))
evol_missing2$glu12 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gluc )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu13 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gluc. )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu14 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gluc:  )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu15 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gl)[0-9]+", ignore.case=TRUE)))
evol_missing2$glu16 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gl )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu17 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gl\\. )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu18 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gl:  )[0-9]+", ignore.case=TRUE)))
evol_missing2$glu19 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glu:)[0-9]+", ignore.case=TRUE)))
evol_missing2$glu20 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gluc:)[0-9]+", ignore.case=TRUE)))
evol_missing2$glu21 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)glucemia:)[0-9]+", ignore.case=TRUE)))
evol_missing2$glu22 <-as.numeric(str_extract(evol_missing2$TEXTO, regex("(?<=(?i)gl:)[0-9]+", ignore.case=TRUE)))
evol_missing2$glu_normal <- str_detect(evol_missing2$TEXTO, regex("glucemia normal|glucemia es normal|glucemia sp|glucemia s\\/p|glu normal|gl sp|glu sp|glu s\\/p", 
                                                                        ignore.case=TRUE))
evol_missing2$glu_normal <- ifelse(evol_missing2$glu_normal==TRUE,1,0)
evol_missing2$glu_total <- rowMeans(select(evol_missing2, 5:26), na.rm=T)
evol_missing2 <- select(evol_missing2, ID_PACIENTE, FECHA, glu_total, glu_normal)
evol_missing2$glu_total[evol_missing2$glu_total=='NaN']<- NA
evol_missing2$glu_total_FR_r <- ifelse(evol_missing2$glu_total>=126, 1, 0)
evol_missing2$glu_total_R_r <- ifelse(evol_missing2$glu_total<126, 1, 0)
evol_missing2$glu_total_R_r <- rowSums(select(evol_missing2, glu_total_R_r, glu_normal), na.rm=T)
evol_missing2 <- group_by(select(evol_missing2, ID_PACIENTE, glu_total_FR_r, glu_total_R_r), ID_PACIENTE) %>% 
    summarise(glu_total_FR=sum(glu_total_FR_r, na.rm=T), glu_total_R=sum(glu_total_R_r, na.rm=T))


#Joining
DBT <- left_join(DBT, evol_missing2, by='ID_PACIENTE') %>% replace_na(list(glu_total_FR=0, glu_total_R=0))
DBT$CANT_GLU_AMB_FR <- rowSums(select(DBT, CANT_GLU_AMB_FR, glu_total_FR))
DBT$CANT_GLU_AMB_R <- rowSums(select(DBT, CANT_GLU_AMB_R, glu_total_R))
DBT <- select(DBT, -total, -glu_total_FR, -glu_total_R, -Total_sin_guardia)
#VOLAR ULTIMAS DOS COLUMNAS

#Describe
hist(DBT$CANT_GLU_AMB_R)
hist(DBT$CANT_GLU_AMB_FR)
hist(DBT$CANT_CONSUMOS)
hist(DBT$CANT_HGLI_FR)
summary(DBT)

#Near zero var
nearZeroVar(DBT)

#Correlations
library(corrplot)
DBT_cor <- cor(select(DBT, -ID_PACIENTE, -DBT_Manual), method='spearman')
corrplot(DBT_cor, order = "hclust", tl.cex=0.7)
#Finding highly correlated variables
findCorrelation(DBT_cor, cutoff = .75)
DBT <- select(DBT, -CANT_FECHAS_EVOL)

library(NMF)
DBT.scaled <- t(select(DBT, -ID_PACIENTE, -DBT_Manual, -pcolor, -color, -EDAD20050101))
hc.DBT <- hclust(dist(DBT.scaled), method ="complete")
plot(hc.DBT,main='Hierarchical Clustering', xlab='', sub='', cex=1)
heatmap(as.matrix(dist(DBT.scaled)))

PCA2 <- preProcess(select(DBT, -ID_PACIENTE, -pcolor, -color), method = c("pca"))
DBT.pca <- predict(PCA2, DBT)
DBT.pca <- cbind(DBT.pca, DBT$DBT_Manual)

DBT.pca %>%  ggplot(aes(PC1, PC2, color=as.factor(`DBT$DBT_Manual`))) + geom_point() +
    scale_color_discrete(name='Status', labels=c('No-DBT', 'DBT', 'Indeterm'))


library(rgl)
#3d MDS plot
DBT$pcolor[DBT$DBT_Manual==0] <- "red"
DBT$pcolor[DBT$DBT_Manual==1] <- "blue"
DBT$pcolor[DBT$DBT_Manual==9] <- "darkgreen"
fit <- cmdscale(dist(select(DBT, -ID_PACIENTE)),eig=TRUE, k=3) # k is the number of dim
# plot solution 
x1 <- fit$points[,1]
y1 <- fit$points[,2]
z1 <- fit$points[,3]
plot3d(x1, y1, z1, col=DBT$pcolor, type="p", box=F)


#Split dataset
traintestpartition <- createDataPartition(DBT$DBT_Manual,
                                 p=.7,
                                 list=F,
                                 times=1)
DBT_train <- DBT[traintestpartition,]
DBT_test <- DBT[-traintestpartition,]

#Describe using boosted trees
cl<-makeCluster(2) #change the 4 to your number of CPU cores
registerDoSNOW(cl)

ctrl <- trainControl(method='cv', 
                     number=10,
                     repeats=3, 
                     verboseIter = T, 
                     classProbs = T, 
                     allowParallel = F,
                     summaryFunction = multiClassSummary)

grid <- expand.grid(n.trees=c(50, 100,500),
                    interaction.depth=c(4:8),
                    shrinkage=c(0.01, 0.001, 0.1),
                    n.minobsinnode=c(10))

system.time(
gbm.DBT <- train(make.names(DBT_Manual)~.,
                 data=select(DBT_train, -ID_PACIENTE, -color, -pcolor),
                 tuneGrid=grid,
                 verbose=T,
                 metric='Accuracy',
                 distribution='multinomial',
                 trControl=ctrl,
                 #tuneLength=9,
                 method='gbm')
)

plot(gbm.DBT)                
plot(varImp(gbm.DBT, scale=T))

system.time(
    gbm.DBT2 <- train(make.names(DBT_Manual)~CANT_PROB_DBT_REL + CANT_CONSUMOS + CANT_GLU_AMB_FR + CANT_GLU_AMB_R + CANT_HGLI_R + EDAD20050101,
                     data=DBT_train,
                     tuneGrid=grid,
                     verbose=T,
                     metric='Accuracy',
                     distribution='multinomial',
                     trControl=ctrl,
                     method='gbm')
)

varImp(gbm.DBT2)
test_results <- predict(gbm.DBT2, DBT_test)
confusionMatrix(test_results, make.names(DBT_test$DBT_Manual))

#AUC CI
0.9883011+c(-1,1)*qnorm(.975)*0.004514493

0.9579703+c(-1,1)*qnorm(.975)*0.01681903


test_results$obs <- DBT_test$DBT_Alg_Missing
test_results$pred <- predict(gbm.DBT3, DBT_test)
mnLogLoss(test_results, lev = levels(test_results$obs))
multiClassSummary(test_results, lev = levels(test_results$obs))
multiclass.roc(test_results$obs, test_results$pred)


gbm.DBT_full <- train(make.names(DBT_Manual)~CANT_PROB_DBT_REL + CANT_CONSUMOS + CANT_GLU_AMB_FR + CANT_GLU_AMB_R + CANT_HGLI_R + EDAD20050101,
                 data=DBT,
                 tuneGrid=grid,
                 verbose=T,
                 metric='Accuracy',
                 distribution='multinomial',
                 trControl=ctrl,
                 #tuneLength=3,
                 method='gbm')


a <- as.data.frame(gbm.DBT_full$results)

plot(gbm.DBT_full)
0.9923+c(-1,1)*qnorm(.975)*(0.005821818/sqrt(10))
0.969+c(-1,1)*qnorm(.975)*(0.01861743/sqrt(10))


library(pROC)
predictions <- as.numeric(predict(gbm.DBT2, DBT_test, type = 'raw'))
ROC <-multiclass.roc(DBT_test$DBT_Manual, predictions)
ci(ROC)

library(ROCR)
prediction(predictions, DBT_test$DBT_Manual)


plot(1-ROC$rocs[[1]]$specificities)



DBT_test_roc <- DBT_test
DBT_test_roc$


test_results <- predict(gbm.DBT2, DBT_test)
test_results2 <- NULL
test_results2$obs <- make.names(DBT_test$DBT_Manual)
test_results2$pred <- predict(gbm.DBT2, DBT_test)
confusionMatrix(test_results, test_results$obs)
multiClassSummary(test_results, lev = test_results$obs)


library(pROC)
roc(as.vector(test_results$obs), as.vector(as.numeric(test_results))
?roc

# Pendientes
# Escribir el algoritmo en reg ex para extraer las glucemias del texto libre
# Wranglear la base para validacion manual
# Validacion manual
# Terminar de entrenar en la bas de derivacion
# Wranglear la base para validacion del algoritmo
# Validar en la base de validación