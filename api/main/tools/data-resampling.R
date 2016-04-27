### Addressing data imbalanced situation by resampling training set prior to training
down_training_set <- downSample(training_set[, -ncol(training_set)], training_set$LABEL, yname = "LABEL")
# high performance with very long computational time (not yet try to tune the parameter)
up_training_set <- upSample(training_set[, -ncol(training_set)], training_set$LABEL, yname = "LABEL")
# computational cost and performanerce dilemma: perc.over = 1000, perc.under = 250 (current optimal)
smote_training_set <- SMOTE(LABEL ~ ., training_set, perc.over = 1000, perc.under = 250)
# good performanerce with quite expensive computational cost (not yet try to tune the parameter )
rose_training_set <- ROSE(LABEL ~ ., training_set)$data
###
