#prm(list=ls())
dati=dati_train %>% select(-c("round"))
dati=dati[complete.cases(dati),]
test=dati_test %>% select(-c("round"))
n<-nrow(dati)
set.seed(123)
sel<-sample(1:n,size=round(n*0.1,0),replace=F)
train <-dati[-sel,]
validation<-dati[sel,]

set.seed(123)
index<-sample(1:dim(train)[1],0.7*dim(train)[1],replace=F)
new_train<-train[index,]
new_test<-train[-index,]
library(e1071)
library(randomForest)
model <- randomForest(risultato ~ . ,data=new_train,type="classification"
)

predictedY <- predict(model, new_test %>%select(-c("risultato"))
)
#points(new_test$risultato, predictedY, col = "red", pch=4)
table()
cbind(new_test$risultato,predictedY)
sum(diag(table(new_test$risultato,predictedY)
))/nrow(new_test)
#[1] 0.4971751
summary(model)

predictedY2<- predict(model, test)


set.seed(123)
model <- randomForest(risultato ~ . ,data=dati,type="classification"
)

predictedY <- predict(model, new_test %>%select(-c("risultato")))



predictedYbis<- predict(model, test %>%select(-c("risultato")))
table(predictedYbis,test$risultato)
# predictedYbis  1  2  X
#             1 57 19 33
#             2 20 28 13
#             X  5  9  6
prevfin2=predictedYbis


model2 <- randomForest(risultato ~ . ,data=dati,type="classification",ntree=1200
)

predicted_forest <- predict(model2, test %>%select(-c("risultato")))
table(predicted_forest,test$risultato)
# predicted_forest  1  2  X
#                1 55 19 31
#                2 21 29 13
#                X  6  8  8

write.csv2(dati,"datitrain.csv")
write.csv2(test,"datitest.csv")
model3 <- randomForest(risultato ~ . ,data=dati,type="classification",ntree=2500, type="prob"
)
varImpPlot(model3,type=2)
predicted_forest2500 <- predict(model3, test %>%select(-c("risultato")))
predicted_forest2500_prob <- predict(model3, test %>%select(-c("risultato")),type="prob")
table(predicted_forest2500,test$risultato)
# predicted_forest2500  1  2  X
#                    1 56 20 32
#                    2 20 30 13
#                    X  6  6  7
# boosting ----------------------------------------------------------------
library(caret)
set.seed(123)
xgb_grid = expand.grid(
  nrounds = 500,
  eta = c(0.1,  0.01),
  max_depth = c(2, 3, 4),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(3 ,5),
  subsample=1
)
my_control <-trainControl(method="cv", number=3)


# PRE PROCESSING ----------------------------------------------------------
all=rbind(dati %>%select(-c("risultato")),test %>%select(-c("risultato")))
numericVars <- which(sapply(all, is.numeric))
numericVarNames <- names(numericVars) 
DFnumeric <- all[, names(all) %in% numericVarNames]
DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) ]
cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')
for(i in 1:ncol(DFnumeric)){
  if (abs(skewness(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)
combined <- DFnorm
str(combined)
index<-sample(1:dim(combined)[1],0.7*dim(combined)[1],replace=F)
train1 <- combined[index,]
test1 <- combined[-index,]
str(test)
str(dati)
# label_train <-dati$risultato
label_train <-as.numeric(dati$risultato)-1
str(combined)
str(dati)
str(test)
################

dtrain <- xgb.DMatrix(data = as.matrix(combined[c(1:656),]), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(combined[c(657:846),]))
num_class = length(levels(dati$risultato))
default_param = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# PREVISIONI XGB--------------------------------------------------------------
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 2500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 2500)
XGBpred <- predict(xgb_mod, dtest,reshape=T)
XGBpred = as.data.frame(XGBpred)
colnames(XGBpred) = levels(dati$risultato) #need to reverse the log to the real values
xgb.pred=XGBpred
#xgb.pred$prediction=ifelse(xgb.pred$X>0.30,"X",ifelse(xgb.pred$`1`>xgb.pred$`2`,"1","2"))
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label =as.character( test$risultato)
table(xgb.pred$prediction,xgb.pred$label)
#    1  2  X
# 1 59 23 35
# 2 23 31 14
# X  0  2  3
table(xgb.pred$label)
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))





# CON TUNING --------------------------------------------------------------
set.seed(27042018)
my_control <-trainControl(method="cv", number=3)
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)
# xgb_caret <- train(x=train1, y=dati$risultato[index], method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
xgb_caret <- train(x=dtrain, y=dati$risultato, method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 

xgb_caret$bestTune
# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 4    1000         2 0.01     0                1                4         1
label_train <-as.numeric(dati$risultato)-1

# put our testing & training data into two seperates Dmatrixs objects
# dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
# dtest <- xgb.DMatrix(data = as.matrix(test1))
num_class = length(levels(dati$risultato))
default_param = list(
  booster="gbtree",
  eta=0.01,
  max_depth=2,
  gamma=0,
  subsample=1,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 2500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
# [340]	train-mlogloss:0.857654+0.012976	test-mlogloss:1.009247+0.041152
xgb_mod2 <- xgb.train(data = dtrain, params=default_param, nrounds = 340)
XGBpred2 <- predict(xgb_mod2, dtest,reshape=T)
XGBpred2 = as.data.frame(XGBpred2)
colnames(XGBpred2) = levels(dati$risultato) #need to reverse the log to the real values
xgb2.pred=XGBpred2
# xgb.pred$prediction=ifelse(xgb.pred$X>0.30,"X",ifelse(xgb.pred$`1`>xgb.pred$`2`,"1","2"))
xgb2.pred$prediction = apply(xgb2.pred,1,function(x) colnames(xgb2.pred)[which.max(x)])
xgb2.pred$label =as.character( test$risultato)
table(xgb2.pred$prediction,xgb2.pred$label)
write.csv2(xgb2.pred,"xgbprevisioni.csv")

#    1  2  X
# 1 65 27 40
# 2 17 27 10
# X  0  2  2
table(xgb.pred$label)
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))



###proviamo SVM
grid <- expand.grid(C = c(0.001,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(3233)
svm_Linear_Grid <- train(x=dati %>% select(-c("risultato")), y=dati$risultato, method = "svmLinear",
                         trControl=my_control,
                         tuneGrid = grid,
                         tuneLength = 10)#, 
                         classProbs=T)

svm_Linear_Grid$bestTune
#C=0.01

#   C     RMSE       Rsquared   MAE       
# 0.00        NaN        NaN         NaN
# 0.01  0.1299583  0.8971963  0.08474574
# 0.05  0.1304852  0.8958769  0.08447978
# 0.10  0.1306073  0.8956993  0.08477223
# 0.25  0.1308595  0.8953571  0.08495476
# 0.50  0.1308635  0.8953413  0.08493712
# 0.75  0.1307871  0.8954637  0.08494924
# 1.00  0.1307713  0.8954820  0.08488513
# 1.25  0.1308028  0.8954352  0.08491083
# 1.50  0.1307469  0.8955460  0.08491518
# 1.75  0.1308088  0.8954248  0.08490545
# 2.00  0.1308118  0.8954155  0.08494978
# 5.00  0.1307613  0.8955063  0.08492209
plot(svm_Linear_Grid)

SVMpred <- predict(svm_Linear_Grid, test %>%select(-c("risultato")),type="probabilities")

SVMpred <- predict(svm_Linear_Grid, test %>%select(-c("risultato")),reshape=T)
table(SVMpred,test$risultato)
sum(diag(table(SVMpred,test$risultato)))/sum(table(SVMpred,test$risultato))
sum(diag(table(SVMpred,test$risultato)))/sum(table(SVMpred,test$risultato))
#[1] 0.5052632
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))


# ARRIVATI FIN QUI  -------------------------------------------------------



##SVM RADIALE
grid <- expand.grid(C = c(100,200,50,1000,10,1,5,0.1,0.005,150), 
                    sigma = c( 0.0005, .00075, .001,0.02,0.1,0.04))
set.seed(3233)

svm_Linear_Grid_rad <- train(x=dati %>% select(-c("risultato")), y=dati$risultato, method = 'svmRadial',
                         trControl=my_control,
                         tuneGrid = grid)

svm_Linear_Grid_rad$bestTune
#   sigma   C
# 49 5e-04 200

SVMpred_rad <- predict(svm_Linear_Grid_rad, test %>%select(-c("risultato")),reshape=T)
table(SVMpred_rad,test$risultato)
sum(diag(table(SVMpred_rad,test$risultato)))/sum(table(SVMpred,test$risultato))
#[1] 0.5105263

model <- svm (risultato ~ ., dati, probability=TRUE, cost = 200, gamma = 0.1)

predsvm_prob= predict(model, test %>%select(-c("risultato")), probability=TRUE)
str(predsvm_prob)
head(attr(predsvm_prob,"probabilities"))
# previsioni cumulate -----------------------------------------------------

SVMpred_rad
SVMpred
xgb2.pred$prediction
xgb.pred$prediction
predictedYbis
predicted_forest
predicted_forest2500
str(predicted_forest2500)
str(predicted_forest)
str(predictedYbis)

risu=data.frame(predictedYbis,SVMpred,xgb2.pred$prediction,xgb.pred$prediction,
      predicted_forest2500,predicted_forest)
prev=vector()
for (i in 1:nrow(risu)){
  prev[i]=Mode(risu[i,])[[1]]
  }

Mode(risu[3,])
risu[4,]
