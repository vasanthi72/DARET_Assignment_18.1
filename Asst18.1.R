load("E:/kamagyana/Computing/DARET/R-code-submissions/Asst17.1.Rdata.RData")
colnames(compbicepsdata)
ncol(compbicepsdata)
nrow(compbicepsdata)
sum(is.na(compbicepsdata))
table(compbicepsdata$classe)
compbicepsdata$movetype <- ifelse(compbicepsdata$classe == "B","NC",ifelse(compbicepsdata$classe =="C","NC",ifelse(compbicepsdata$classe == "D","NC",ifelse(compbicepsdata$classe =="E","NC","C"))))
table(compbicepsdata$movetype)
compbicepsdata$classe <- as.factor(compbicepsdata$classe)
compbicepsdata$movetype <- as.factor(compbicepsdata$movetype)
smp_size <- floor(0.75 * nrow(compbicepsdata)); set.seed(123);
train_ind <- sample(seq_len(nrow(compbicepsdata)), size = smp_size);
bitrain <- compbicepsdata[train_ind, ];bitest <- compbicepsdata[-train_ind, ]
colnames(bitrain)
colnames(bitest)
nrow(bitrain)
nrow(bitest)
ncol(bitrain)
ncol(bitest)
head(bitrain)
head(bitest)
library(tree)
library(C50)
fit1 <- tree(classe~., data =  bitrain[,-52])
summary(fit1)
plot(fit)
plot(fit1)
test(fit1)
text(fit1)
pred1 <- predict(fit1,bitest[,-52],type="class")
library(C50)
library(caret)
library(mltools)
confusionMatrix(pred1,bitest$classe)
CMpred1 <- confusionMatrix(pred1, bitest$classe)
CMpred1$byClass[,7]
mcc(preds = pred1, actuals = bitest$classe)
library(rpart)
fit2 <- rpart(classe~.,data=bitrain[,-52])
fit2
pred2 <- predict(fit2,bitest[,-52],type="class")
confusionMatrix(pred2,bitest$classe)
CMpred2 <- confusionMatrix(pred2, bitest$classe)
CMpred2$byClass[,7]
mcc(preds = pred2, actuals = bitest$classe)
rpart.plot(fit2)
library(rpart.plot)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fit2)C
plotcp(fit2)
library(caret)
library(rpart)
train_control <-trainControl(method="cv", number=10)
model <-  train(classe~.,data=bitrain[,-52],trControl = train_control,method = "rpart")
model
pred3 <- predict(model,bitest[,-52])
pred3 <- cbind(bitest,pred3)
colnames(pred3)
confusionMatrix(pred3$classe, pred3$pred3)
CMpred3 <- confusionMatrix(pred3$classe, pred3$pred3)
CMpred3$byClass[,7]
mcc(preds = pred3$pred3, actuals = pred3$classe)
model1 <- train(classe~.,data=bitrain[,-52],trControl = train_control, method = "C5.0")
model1
pred4 <- predict(model1,bitest[,-52]); pred4 <- cbind(bitest,pred4);confusionMatrix(pred4$classe, pred4$pred4)
CMpred4 <- confusionMatrix(pred4$classe, pred4$pred4)
CMpred4$byClass[,7]
mcc(preds = pred4$pred4, actuals = pred4$classe)
library(gbm)
library(doParallel)
model2 <- train(classe~.,data=bitrain[,-52],trControl = train_control, method = "bstTree"); model2
pred5 <- predict(model2,bitest[,-52]); pred5 <- cbind(bitest,pred5);confusionMatrix(pred5$classe, pred5$pred5)
CMpred5 <- confusionMatrix(pred5$classe, pred5$pred5)
CMpred5$byClass[,7]
mcc(preds = pred5$pred5, actuals = pred5$classe)
model3 <- train(classe~.,data=bitrain[,-52],trControl = train_control, method = "C5.0Cost"); model3
model3 <- train(classe~.,data=bitrain[,-52],trControl = train_control, method = "C5.0Rules"); model3
pred6 <- predict(model3,bitest[,-52]); pred6 <- cbind(bitest,pred6);confusionMatrix(pred6$classe, pred6$pred6)
CMpred6 <- confusionMatrix(pred6$classe, pred6$pred6)
CMpred6$byClass[,7]
mcc(preds = pred6$pred6, actuals = pred6$classe)
model4 <- train(classe~.,data=bitrain[,-52],trControl = train_control, method = "C5.0Tree"); model4
pred7 <- predict(model4,bitest[,-52]); pred7 <- cbind(bitest,pred7);confusionMatrix(pred7$classe, pred7$pred7)
CMpred7 <- confusionMatrix(pred7$classe, pred7$pred7)
CMpred7$byClass[,7]
mcc(preds = pred7$pred7, actuals = pred7$classe)
library(modeltools)
library(strucchange)
library(coin)
library(party)
model5 <- train(classe~.,data=bitrain[,-52],trControl = train_control, method = "ctree"); model5
pred8 <- predict(model5,bitest[,-52]); pred8 <- cbind(bitest,pred8);confusionMatrix(pred8$classe, pred8$pred8)
CMpred8 <- confusionMatrix(pred8$classe, pred8$pred8)
CMpred8$byClass[,7]
mcc(preds = pred8$pred8, actuals = pred8$classe)
model6 <- train(classe~.,data=bitrain[,-52],trControl = train_control, method = "ctree2"); model6
pred9 <- predict(model6,bitest[,-52]); pred9 <- cbind(bitest,pred9);confusionMatrix(pred9$classe, pred9$pred9)
CMpred9 <- confusionMatrix(pred9$classe, pred9$pred9)
CMpred9$byClass[,7]
mcc(preds = pred9$pred9, actuals = pred9$classe)




