### Command: check antidepressant info.
# ex. PubChem Id: 3386
which(rownames(biomat) == 3386)
# [1] 204
colnames(biomat[, which(biomat[204, ] == 1)])
# [1] "AOFB_HUMAN"  "SC6A4_HUMAN"
###

### Command: funtional as label powerset (LP) muti-labels transforming
pharm_glue <- as.data.frame(apply(pharm, 1, paste, collapse=""))
colnames(pharm_glue) <- "LABEL"
###

### Command: find the particular drug's max score index of prediction producing by CCA
test <- as.data.frame(bio.newpred)
which.max(test[!is.na(match(rownames(test), 2337)), ])
###

### Command: mldr package
chem_pharm <- data.frame(chem, pharm)

library(mldr)
mymldr <- mldr_from_dataframe(chem_pharm, labelIndices = (ncol(chem)+1):ncol(chem_pharm), name = "chem_pharm")

summary(mymldr)

mymldr_br <- mldr_transform(mymldr, type = "BR")
mymldr_lp <- mldr_transform(mymldr, type = "LP", mymldr$labels$index)

library(RWeka)
classifier <- IBk(classLabel ~ ., data = mymldr_lp, control = Weka_control(K = 10))
# evaluate_Weka_classifier(classifier, numFolds = 5)

pred_matrix <- as.matrix(classifier$predictions)
pred_matrix <- matrix(unlist(apply(pred_matrix, 1, strsplit, "")), nrow = nrow(pred_matrix))
pred_matrix <- apply(pred_matrix, 2, as.numeric)
result <- mldr_evaluate(mymldr, pred_matrix)

which(pharm[1, ] == 1) %in% which(pred_matrix[1, ] == 1)
which((which(pharm[5, ] == 1) %in% which(pred_matrix[5, ] == 1)) == TRUE)
# [1] 49
which(pharm[5, ] == 1)[49]
# [1] 1279
pharm[5, 1279]
# [1] 1
pred_matrix[5, 1279]
# [1] 1
###

### Preprocessing: separate antidepressant from data set (One time preprocessing)
# Known antidepressant PubChem CID
antidepressant_list <- c(6434118, 76968116, 76962653, 76960151, 76956911, 73416972, 73416962, 73416955, 73416810, 73416655, 23663953, 15893898, 9884029, 6603149, 6434754, 6433351, 6420022, 6419921, 5353833, 5284550, 5282426, 5282425, 5282318, 5281088, 3045275, 667477, 667468, 443945, 441358, 439280, 198375, 171003, 146571, 146570, 121249, 101726, 71587, 71478, 71424, 68870, 68551, 68539, 65700, 65327, 62884, 62857, 34870, 34869, 33611, 25382, 25381, 22576, 21722, 21087, 21086, 11065, 9419, 9417, 8228, 6305, 5666, 5584, 5355, 5092, 4976, 4543, 4205, 4184, 4011, 3947, 3696, 3386, 3158, 2995, 2895, 2801, 2771, 2160, 444, 144)

antidepressant_idx <- which(rownames(chemmat) %in% antidepressant_list)
antidepressant <- chemmat[antidepressant_idx, ]
write.csv(antidepressant_data, "antidepressant_chem.csv")
without_antidepressant <- chemmat[-antidepressant_idx,]
write.csv(without_antidepressant, "without_antidepressant_chem.csv")
###

### Preprocessing: change label factor from numeric to character (One time preprocessing)
without_antidepressant_bio <- as.data.frame(apply(without_antidepressant_bio, 2, factor, levels = c(0, 1), label = c("no", "yes")))
###

### Preprocessing: parallel computing
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
# - machine learning code goes in here - #
stopCluster(cl)
# unregister (cancel parallel computing)
registerDoSEQ()
###

### Action: training parameter setting
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, savePredictions = TRUE, summaryFunction = twoClassSummary)
###

### Action: build a binary relevance training set
training_set <- as.data.frame(cbind(without_antidepressant_chem, LABEL = without_antidepressant_bio[, 1]))
###

### Action: train the model with resampling training set prior to training
down_training_set <- downSample(training_set[, -ncol(training_set)], training_set$LABEL, yname = "LABEL")
# High performance with very long computational time (not yet try to tune the parameter)
up_training_set <- upSample(training_set[, -ncol(training_set)], training_set$LABEL, yname = "LABEL")
# [Current choice] computational cost and performanerce dilemma: perc.over = 1000, perc.under = 250 (current optimal)
smote_training_set <- SMOTE(LABEL ~ ., training_set, perc.over = 1000, perc.under = 250)
# Good performanerce with quite expensive computational cost (not yet try to tune the parameter )
rose_training_set <- ROSE(LABEL ~ ., training_set)$data
###

### Action: start training model
model <- train(Class ~ ., data = down_training_set, trControl = train_control, method = "knn", tuneLength = 10, metric = "ROC")
###

predict_result <- list()
for(i in 1:ncol(without_antidepressant_bio)) {
	predict_result[[i]] <- predict(chem_bio_model[[i]]$finalModel, antidepressant_chem)
}

testing_set_predict_result <- data.frame(row.names = row.names(antidepressant_bio))
for(i in 1:ncol(antidepressant_bio)) {
	testing_set_predict_result <- cbind(testing_set_predict_result, predict_result[[i]][, 2])
}
colnames(testing_set_predict_result) <- colnames(antidepressant_chem)

### Action: performance evaluation
predict_result <- predict(model, antidepressant_chem)
confusionMatrix(predict_result, antidepressant_bio[, 1])

# plot roc curve (TO BE MODIFIED)
plot(nb_roc)
plot(knn_roc, col = "blue", add = TRUE)
plot(rpart_roc, col = "red", add = TRUE)
legend("topright", legend = c("NB", "KNN", "RPART"), col = c("black", "blue", "red"), lwd = 2)
###
