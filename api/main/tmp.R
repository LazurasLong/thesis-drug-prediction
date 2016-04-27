### Pre-processing: remove features with all 0s
removed_data_index <- which(apply(without_antidepressant_pharm, 2, function(x) { all(x == 0) }))
without_antidepressant_pharm_cleaned <- without_antidepressant_pharm[, -removed_data_index]
# clean testing set for data consistency
antidepressant_pharm_cleaned <- antidepressant_pharm[, -removed_data_index]
###

### Training
knn_pharm_bio_model <- vector("list", ncol(without_antidepressant_pharm_cleaned))
for(i in 1:ncol(without_antidepressant_bio)) {
	# training parameter setting (5 fold cross validation)
	train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, savePredictions = TRUE, summaryFunction = twoClassSummary)
	# build a binary relevance training set
	training_set <- as.data.frame(cbind(without_antidepressant_pharm_cleaned, LABEL = without_antidepressant_bio_nominal[, i]))
	# resampling training set prior to training
	# computational cost and performanerce dilemma: perc.over = 1000, perc.under = 250 (current optimal)
	smote_training_set <- SMOTE(LABEL ~ ., training_set, perc.over = 1000, perc.under = 250)
	# start training model
	model <- train(LABEL ~ ., data = smote_training_set, trControl = train_control, method = "knn", tuneLength = 5, metric = "ROC")
	knn_pharm_bio_model[[i]] <- model
}
###

### Prediction: predict result
## [CAUTIOUS] execute this line before merging data (one time process only)
predict_result <- vector("list", ncol(antidepressant_pharm_cleaned))
testing_predict_result <- data.frame(row.names = row.names(antidepressant_bio))
##
## repeat several times untill all the data are merged
# change i according to the boundary of new data set (use j in case the data boundary start at 1)
# j <- 1
for(i in 1:300) {
	predict_result[[i]] <- predict(knn_pharm_bio_model[[i]]$finalModel, antidepressant_pharm_cleaned)
#	predict_result[[i]] <- predict(knn_pharm_bio_model[[j]]$finalModel, antidepressant_pharm_cleaned)
#	j <- j + 1
}
# change i according to the boundary of new data set
for(i in 1:300) {
	testing_predict_result <- cbind(testing_predict_result, predict_result[[i]][, 2])
}
# execute this line after all the data are merged (one time process)
colnames(testing_predict_result) <- colnames(antidepressant_bio)
###

### Performance evaluation: combine all different threshold (here: 0.0 ~ 1.0) confusion matrix into one list
pred <- melt(testing_predict_result, measure.vars = colnames(testing_predict_result))[, 2]
#是否as vector就可以解決= =
obs <- melt(antidepressant_bio, measure.vars = colnames(antidepressant_bio))[, 2]
#是否as vector就可以解決= =
###

### Performance evaluation: plot roc curve
# plot roc curve in terms of sensitivity and specificity
roc_result <- roc(obs, pred)
# plot(roc_result)
# finding threshold by ploting roc curve with best cutoff points by maximizing sensitivity and specificity
# plot(roc_result, print.thres = "best", print.thres.best.method = "youden")
# finding threshold by ploting roc curve with best cutoff points by finding the closest point to (0, 1)
# plot(roc_result, print.thres = "best", print.thres.best.method = "closest.topleft")

### Pre-processing: stop parallel computing
stopCluster(cl)
# unregister (cancel parallel computing)
registerDoSEQ()
###
