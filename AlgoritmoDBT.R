library(dplyr)
library(caret)
library(tidyr)
library(readxl)
library(gbm)

#Read in data
DBT <- read_excel('PMA0719336_ScoreRiesgoCV_Corr_20150715 - FINAL CON OUTCOMES -Missing 2.xlsx')
DBT <- select(DBT, -Comentarios, -Total_con_guardia, -Total_sin_guardia, -CANT_GLU_GUARDIA_FR, -CANT_GLU_GUARDIA_R)
DBT$DBT_Alg_Missing <- make.names(DBT$DBT_Alg_Missing)
DBT <- DBT[-1664,] %>% select(-DBT_Alg_Missing, -Result_Alg_Missing, -Missing_DBT_Manual)

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
DBT.scaled <- scale(select(DBT, -ID_PACIENTE, -DBT_Manual, -EDAD20050101))
hc.DBT <- hclust(dist(DBT.scaled), method ="complete")
plot(hc.DBT,main='Hierarchical Clustering - Average Method', xlab='', sub='', cex=.1)
heatmap(as.matrix(DBT.scaled), Rowv=NA)

library(plot3D)
fit <- cmdscale(dist(DBT),eig=TRUE, k=3) # k is the number of dim
# plot solution 
x1 <- fit$points[,1]
y1 <- fit$points[,2]
z1 <- fit$points[,3]
plot3d(x1, y1, z1, xlab="Coordinate 1", ylab="Coordinate 2", zlab='Coordinate 3', 
       main="MDS Toy Training Set - 3 dimensions", type="p", col=as.integer(DBT$DBT_Manual))



#Split dataset
traintestpartition <- createDataPartition(DBT$DBT_Alg_Missing,
                                 p=.7,
                                 list=F,
                                 times=1)
DBT_train <- DBT[traintestpartition,]
DBT_test <- DBT[-traintestpartition,]

#Describe using boosted trees
cl<-makeCluster(2) #change the 4 to your number of CPU cores
registerDoSNOW(cl)

ctrl <- trainControl(method='repeatedCV', 
                     number=10,
                     repeats=5, 
                     verboseIter = T, 
                     classProbs = T, 
                     allowParallel = F,
                     summaryFunction = multiClassSummary)

grid <- expand.grid(n.trees=c(100,500,1000),
                    interaction.depth=c(1:8),
                    shrinkage=c(0.01),
                    n.minobsinnode=c(10,50))

system.time(
gbm.DBT <- train(DBT_Alg_Missing~CANT_PROB_DBT_REL + CANT_CONSUMOS + CANT_GLU_AMB_FR + CANT_GLU_AMB_R +
                     CANT_HGLI_FR + CANT_HGLI_R + CANT_GLU_120M_FR + CANT_GLU_120M_R + EDAD20050101,
                 data=DBT_train,
                 tuneGrid=grid,
                 verbose=T,
                 metric='Accuracy',
                 distribution='multinomial',
                 trControl=ctrl,
                 method='gbm')
)
                 
grid2 <- expand.grid(n.trees=c(500),
                    interaction.depth=c(1:5),
                    shrinkage=c(0.001, 0.01, 0.1),
                    n.minobsinnode=10)

system.time(
    gbm.DBT2 <- train(DBT_Alg_Missing~CANT_PROB_DBT_REL + CANT_CONSUMOS + CANT_GLU_AMB_FR + CANT_GLU_AMB_R +
                         CANT_HGLI_FR + CANT_HGLI_R + CANT_GLU_120M_FR + CANT_GLU_120M_R + EDAD20050101,
                     data=DBT_train,
                     tuneGrid=grid2,
                     verbose=T,
                     metric='Accuracy',
                     distribution='multinomial',
                     trControl=ctrl,
                     method='gbm')
)

grid3 <- expand.grid(n.trees=c(500),
                     interaction.depth=4,
                     shrinkage=0.01,
                     n.minobsinnode=10)

system.time(
    gbm.DBT3 <- train(DBT_Alg_Missing~CANT_PROB_DBT_REL + CANT_CONSUMOS + CANT_GLU_AMB_FR + CANT_GLU_AMB_R +
                          CANT_HGLI_FR + CANT_HGLI_R + CANT_GLU_120M_FR + CANT_GLU_120M_R + EDAD20050101,
                      data=DBT_train,
                      tuneGrid=grid3,
                      verbose=T,
                      metric='Accuracy',
                      distribution='multinomial',
                      trControl=ctrl,
                      method='gbm')
)

varImp(gbm.DBT3)
plot(varImp(gbm.DBT3))
plot(gbm.DBT3, i.var=1)
plot.gbm(gbm.DBT3, i="CANT_GLU_AMB_R")

gbm.DBT4 <- gbm(DBT_Alg_Missing~CANT_PROB_DBT_REL + CANT_CONSUMOS + CANT_GLU_AMB_FR + CANT_GLU_AMB_R +
                    CANT_HGLI_FR + CANT_HGLI_R + CANT_GLU_120M_FR + CANT_GLU_120M_R + EDAD20050101,
                data=DBT_train,
                n.trees=500,
                interaction.depth=4,
                shrinkage=0.01,
                cv.folds=10,
                distribution='multinomial',
                verbose="CV",
                n.cores = 2)


test_results <- predict(gbm.DBT3, DBT_test, type = "prob")
test_results$obs <- DBT_test$DBT_Alg_Missing
test_results$pred <- predict(gbm.DBT3, DBT_test)
mnLogLoss(test_results, lev = levels(test_results$obs))
multiClassSummary(test_results, lev = levels(test_results$obs))
multiclass.roc(test_results$obs, test_results$pred)



test_results <- predict(gbm.DBT3, DBT_test)
test_results$obs <- DBT_test$DBT_Alg_Missing
test_results$pred <- predict(gbm.DBT3, DBT_test)
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