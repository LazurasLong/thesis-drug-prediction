### Manually combine all different threshold (here: 0.0 ~ 1.0) confusion matrix into one list
confusion_matrix_list <- NULL
l <- 1
for(i in seq(from = 0, to = 1, by = 0.1)) {
	sum_confusion_matrix <- 0
	for(j in 1:ncol(antidepressant_bio)) {
		sum_confusion_matrix <- sum_confusion_matrix + confusion.matrix(antidepressant_bio[, j], testing_predict_result[, j], i)
	}
	confusion_matrix_list[[l]] <- sum_confusion_matrix
	l <- l + 1
}
###

### Combine all different threshold (here: 0.0 ~ 1.0) confusion matrix into one list
pred <- melt(testing_predict_result, measure.vars = colnames(testing_predict_result))[, 2]
obs <- melt(antidepressant_bio, measure.vars = colnames(antidepressant_bio))[, 2]

knn_chem_pred_bio_confusion_matrix_list <- NULL
for(i in seq(from = 0, to = 1, 0.1)) {
	tmp <- pred
	tmp[tmp >= i] <- 1
	tmp[tmp < i] <- 0
	knn_chem_pred_bio_confusion_matrix_list <- list.append(knn_chem_pred_bio_confusion_matrix, list(confusionMatrix(tmp, obs, positive = "1"), paste("Cutoff: ", i)))
}
###

### Plot roc curve in terms of true positive rate (tpr) and false positive rate (fpr) (also know as sensitivity and 1-specificity)
library(ROCR)
rocr_pred <- prediction(pred, obs)
plot(performance(rocr_pred, "tpr", "fpr"))
###

### Evaluate algorithms by plotting roc curve (TO BE MODIFIED)
plot(nb_roc)
plot(knn_roc, col = "blue", add = TRUE)
plot(rpart_roc, col = "red", add = TRUE)
legend("topright", legend = c("NB", "KNN", "RPART"), col = c("black", "blue", "red"), lwd = 2)
###
