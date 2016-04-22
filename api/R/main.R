### Pre-processing: start parallel computing
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
###

### Training: training parameter setting
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, savePredictions = TRUE, summaryFunction = twoClassSummary)
###

### Training: build a binary relevance training set
training_set <- as.data.frame(cbind(without_antidepressant_chem, LABEL = without_antidepressant_bio[, 1]))
###

### Resampling: resampling training set prior to training
# computational cost and performanerce dilemma: perc.over = 1000, perc.under = 250 (current optimal)
smote_training_set <- SMOTE(LABEL ~ ., training_set, perc.over = 1000, perc.under = 250)
###

### Training: start training model
model <- train(Class ~ ., data = down_training_set, trControl = train_control, method = "knn", tuneLength = 10, metric = "ROC")
###

### Prediction: predict result
predict_result <- list()
for(i in 1:ncol(without_antidepressant_bio)) {
	predict_result[[i]] <- predict(chem_bio_model[[i]]$finalModel, antidepressant_chem)
}

testing_set_predict_result <- data.frame(row.names = row.names(antidepressant_bio))
for(i in 1:ncol(antidepressant_bio)) {
	testing_set_predict_result <- cbind(testing_set_predict_result, predict_result[[i]][, 2])
}
colnames(testing_set_predict_result) <- colnames(antidepressant_chem)
###

### Performance evaluation: build up confusion matrix
predict_result <- predict(model, antidepressant_chem)
confusionMatrix(predict_result, antidepressant_bio[, 1])
###

### Performance evaluation: combine all different threshold (here: 0.0 ~ 1.0) confusion matrix into one list
pred <- melt(testing_predict_result, measure.vars = colnames(testing_predict_result))[, 2]
obs <- melt(antidepressant_bio, measure.vars = colnames(antidepressant_bio))[, 2]
###

### Performance evaluation: plot roc curve
# plot roc curve in terms of sensitivity and specificity
library(pROC)
roc_pred <- roc(obs, pred)
plot(roc_pred)
# finding threshold by ploting roc curve with best cutoff points by maximizing sensitivity and specificity
plot(roc_result, print.thres = "best", print.thres.best.method = "youden")
# finding threshold by ploting roc curve with best cutoff points by finding the closest point to (0, 1)
plot(roc_result, print.thres = "best", print.thres.best.method = "closest.topleft")

# plot roc curve in terms of sensitivity and 1-specificity with the best cutoff points and auc score (no threshold info. list on)
library(Epi)
ROC(form = obs ~ pred, plot = "ROC")

### Pre-processing: stop parallel computing
stopCluster(cl)
# unregister (cancel parallel computing)
registerDoSEQ()
###
