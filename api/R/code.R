library(caret)
library(DMwR)
library(doParallel)

# Preprocessing: parallel computing
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Action: start training knn
for(i in 1:ncol(without_antidepressant_bio)) {
	training_set <- as.data.frame(cbind(without_antidepressant_chem, LABEL = without_antidepressant_bio[, i]))
  smote_training_set <- SMOTE(LABEL ~ ., training_set, perc.over = 1000, perc.under = 250)
  model <- train(LABEL ~ ., data = smote_training_set, trControl = train_control, method = "knn", tuneLength = 5, metric = "ROC")
  chem_bio_model[[i]] <- model
}


stopCluster(cl)
# unregister (cancel parallel computing)
registerDoSEQ()
