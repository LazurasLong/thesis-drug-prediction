### plot multiple lines
png("scca.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_chem_pred_pheno$pred_score, pheno_cleaned, colors[8], T, F, F)
eval_auroc(scca_bio_pred_pheno$pred_score, pheno_cleaned, colors[7], T, F, T)
eval_auroc(scca_antidp_chem_pred_pheno, antidp_pheno, colors[6], T, F, T)
eval_auroc(scca_antidp_bio_pred_pheno, antidp_pheno, colors[5], T, F, T)
legend("bottomright", c("scca chem predict pheno", "scca bio predict pheno", "scca chem predict pheno (antidepressants)", "scca bio predict pheno (antidepressants)"), cex = 0.7, lwd = c(1, 1), col = colors[8:5], inset = 0.05)
mtext("SCCA", line = 0.5)
abline(a = 0, b = 1, lty = "dashed")
dev.off()
###

### testing ggplot inside eval_auroc (too slow)
fpr_tpr <- as.data.frame(cbind(fpr = rocr_result@x.values[[1]], tpr = rocr_result@y.values[[1]]))
print(ggplot(fpr_tpr, aes(x = fpr, y = tpr, col = "scca"), col = color) +
  geom_line() +
  geom_point(aes(x = cutoff[1], y = cutoff[2])) +
  xlab("False positive rate (1-Specificity)") +
  ylab("True positive rate (Sensitivity)") +
  labs(col = "Algorithms"))
###

### Plot high resolution (300 dpi) png
png("tmp1.png", width = 4, height = 4, units = 'in', res = 300)
# remember to turn off the device
dev.off()
png("tmp2.png", units = "px", width = 1600, height = 1600, res = 300)
# remember to turn off the device
dev.off()
###

### draw abline
abline(a = 0, b = 1, lty = "dashed")

### Save model
save(knn_pharm_bio_model, file = "knn-pharm-bio-model698-1368.RData")
###

### Check antidepressant info.
# ex. PubChem Id: 3386
which(rownames(biomat) == 3386)
# [1] 204
colnames(biomat[, which(biomat[204, ] == 1)])
# [1] "AOFB_HUMAN"  "SC6A4_HUMAN"
###

### Funtional as label powerset (LP) muti-labels transforming
pharm_glue <- as.data.frame(apply(pharm, 1, paste, collapse=""))
colnames(pharm_glue) <- "LABEL"
###

### Find the particular drug's max score index of prediction producing by CCA
test <- as.data.frame(bio.newpred)
which.max(test[!is.na(match(rownames(test), 2337)), ])
###

### mldr package - dealing with multi-label situation
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
