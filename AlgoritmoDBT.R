library(dplyr)
library(caret)
library(tidyr)
library(readxl)
library(gbm)
library(doSNOW)

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
                     data=select(DBT_train, -ID_PACIENTE, -color, -pcolor),
                     tuneGrid=grid,
                     verbose=T,
                     metric='Accuracy',
                     distribution='multinomial',
                     trControl=ctrl,
                     method='gbm')
)

varImp(gbm.DBT2)
grid3 <- expand.grid(n.trees=c(500),
                     interaction.depth=4,
                     shrinkage=0.01,
                     n.minobsinnode=10)

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
                 #tuneGrid=grid,
                 verbose=T,
                 metric='Accuracy',
                 distribution='multinomial',
                 trControl=ctrl,
                 tuneLength=3,
                 method='gbm')



0.9609+c(-1,1)*qnorm(.975)*0.01656741
0.952+c(-1,1)*qnorm(.975)*0.004514493


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