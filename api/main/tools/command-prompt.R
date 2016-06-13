<<<<<<< HEAD
### plot multiple lines
png("scca.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_chem_pred_pheno$pred_score, pheno_cleaned, colors[8], T, F, F)
eval_auroc(scca_bio_pred_pheno$pred_score, pheno_cleaned, colors[7], T, F, T)
eval_auroc(scca_antidp_chem_pred_pheno, antidp_pheno, colors[6], T, F, T)
eval_auroc(scca_antidp_bio_pred_pheno, antidp_pheno, colors[5], T, F, T)
legend("bottomright", c("scca chem predict pheno", "scca bio predict pheno", "scca chem predict pheno (antidepressants)", "scca bio predict pheno (antidepressants)"), cex = 0.7, lwd = c(1, 1), col = colors[8:5], inset = 0.05)
mtext("SCCA", line = 0.5)
=======
### Draw barplot
test <- seq(200, 2614, 200)
test[13] <- 2614
j <- 1
k <- 1
for(i in test) {
  png(paste0("antidp-adr-freq (barplot visualization)-", k, ".png"), units = "px", width = 3200, height = 1600, res = 300)
  print(ggplot(rf_antidp_chem_plus_bio_pred_pheno_fcutoff_and_obs[j:i, ], aes(x = ADR, y = Count, fill = State)) + geom_bar(position = "identity", stat = "identity", alpha = .5) + labs(fill = "") + theme(text = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) + theme(panel.background = element_blank(), plot.background = element_rect(fill = "#f8f2e4"), legend.background = element_blank()) + scale_fill_stata())
  dev.off()
  j <- i
  k <- k + 1
}

png("antidp-target-freq (barplot visualization).png", units = "px", width = 3200, height = 1600, res = 300)
print(ggplot(rf_antidp_chem_plus_pheno_pred_bio_fcutoff_and_obs, aes(x = Target, y = Count, fill = State)) + geom_bar(position = "identity", stat = "identity", alpha = .5) + labs(fill = "") + theme(text = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) + theme(panel.background = element_blank(), plot.background = element_rect(fill = "#f8f2e4"), legend.background = element_blank()) + scale_fill_wsj("colors6", ""))
dev.off()
###

### Draw network graph
test <- seq(50, 522, 52)
test[10] <- 522
j <- 1
k <- 1
for(i in test) {
    png(paste0("rf_antidp_chem_plus_bio_pred_pheno (network visualization)-", k, ".png"), units = "px", width = 3200, height = 1600, res = 300)
    print(ggnet2(network(rf_antidp_chem_plus_bio_pred_pheno_fcutoff_cleaned[, i:j], directed = T), color = "mode", palette = "Set1", label = T, label.alpha = 0.8, label.size = 2, alpha = 0.7, edge.size = 0.25, edge.alpha = 1, edge.color = "grey", size = "degree", size.legend = "Degree", size.cut = 5, size.min = 1, mode = "circle") + theme_wsj() + scale_color_stata(labels = c("actor" = "Drug", "event" = "ADR"), name = "") + guides(size = guide_legend(order = 1), colour = guide_legend(order = 2)))
    dev.off()
    j <- i
    k <- k + 1
}

png("rf_antidp_chem_plus_pheno_pred_bio (network visualization).png", units = "px", width = 3200, height = 1600, res = 300)
ggnet2(network(rf_antidp_chem_plus_pheno_pred_bio_fcutoff_cleaned, directed = T), color = "mode", palette = "Set1", label = T, label.alpha = 0.8, label.size = 2, alpha = 0.7, edge.size = 0.25, edge.alpha = 1, edge.color = "grey", size = "degree", size.legend = "Degree", size.cut = 5, mode = "kamadakawai") + theme_wsj() + scale_colour_wsj("colors6", "", labels = c("actor" = "Drug", "event" = "Target"))
dev.off()
###

### Draw ROC/PR curve
png("chem-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_chem_pred_pheno, pheno_cleaned, color = colors[7], plot = T)
eval_auroc(svm_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(rf_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[5], plot = T, add = T)
eval_auroc(knn_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("bio-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_bio_pred_pheno, pheno_cleaned, color = colors[7], plot = T)
eval_auroc(svm_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(rf_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[5], plot = T, add = T)
eval_auroc(knn_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("chem-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_chem_pred_bio, bio_cleaned, color = colors[7], plot = T)
eval_auroc(svm_chem_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(rf_chem_pred_bio$pred_score, bio_cleaned, color = colors[5], plot = T, add = T)
eval_auroc(knn_chem_pred_bio$pred_score, bio_cleaned, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("pheno-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_pheno_pred_bio, bio_cleaned, color = colors[7], plot = T)
eval_auroc(svm_pheno_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(rf_pheno_pred_bio$pred_score, bio_cleaned, color = colors[5], plot = T, add = T)
eval_auroc(knn_pheno_pred_bio$pred_score, bio_cleaned, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("chem-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_chem_pred_pheno, pheno_cleaned, color = colors[7], plot = T)
eval_aupr(svm_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(rf_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[5], plot = T, add = T)
eval_aupr(knn_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("bio-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_bio_pred_pheno, pheno_cleaned, color = colors[7], plot = T)
eval_aupr(svm_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(rf_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[5], plot = T, add = T)
eval_aupr(knn_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("chem-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_chem_pred_bio, bio_cleaned, color = colors[7], plot = T)
eval_aupr(svm_chem_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(rf_chem_pred_bio$pred_score, bio_cleaned, color = colors[5], plot = T, add = T)
eval_aupr(knn_chem_pred_bio$pred_score, bio_cleaned, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("pheno-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_pheno_pred_bio, bio_cleaned, color = colors[7], plot = T)
eval_aupr(svm_pheno_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(rf_pheno_pred_bio$pred_score, bio_cleaned, color = colors[5], plot = T, add = T)
eval_aupr(knn_pheno_pred_bio$pred_score, bio_cleaned, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

###
png("antidp-chem-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_auroc(svm_antidp_chem_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_auroc(rf_antidp_chem_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
eval_auroc(knn_antidp_chem_pred_pheno, antidp_pheno, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("antidp-bio-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_antidp_bio_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_auroc(svm_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_auroc(rf_antidp_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
eval_auroc(knn_antidp_bio_pred_pheno, antidp_pheno, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("antidp-chem-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_auroc(svm_antidp_chem_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_auroc(rf_antidp_chem_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
eval_auroc(knn_antidp_chem_pred_bio, antidp_bio, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("antidp-pheno-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_antidp_pheno_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_auroc(svm_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_auroc(rf_antidp_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
eval_auroc(knn_antidp_pheno_pred_bio, antidp_bio, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("antidp-chem-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_aupr(svm_antidp_chem_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_aupr(rf_antidp_chem_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
eval_aupr(knn_antidp_chem_pred_pheno, antidp_pheno, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("antidp-bio-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_antidp_bio_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_aupr(svm_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_aupr(rf_antidp_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
eval_aupr(knn_antidp_bio_pred_pheno, antidp_pheno, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("antidp-chem-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_aupr(svm_antidp_chem_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_aupr(rf_antidp_chem_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
eval_aupr(knn_antidp_chem_pred_bio, antidp_bio, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("antidp-pheno-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_antidp_pheno_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_aupr(svm_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_aupr(rf_antidp_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
eval_aupr(knn_antidp_pheno_pred_bio, antidp_bio, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

###
png("plus-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[7], plot = T)
eval_auroc(svm_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(rf_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
eval_auroc(knn_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("plus-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[7], plot = T)
eval_aupr(svm_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(rf_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
eval_aupr(knn_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("plus-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[7], plot = T)
eval_auroc(svm_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(rf_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
eval_auroc(knn_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("plus-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[7], plot = T)
eval_aupr(svm_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(rf_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
eval_aupr(knn_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("antidp-plus-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_auroc(svm_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_auroc(rf_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
eval_auroc(knn_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("antidp-plus-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_aupr(svm_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_aupr(rf_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
eval_aupr(knn_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("antidp-plus-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_auroc(svm_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_auroc(rf_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
eval_auroc(knn_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[4], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("antidp-plus-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_aupr(svm_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_aupr(rf_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
eval_aupr(knn_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[4], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("scca", "svm", "rf", "knn"), cex = 0.7, lwd = c(1, 1), col = colors[7:4], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

###
png("scca-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_chem_pred_pheno, pheno_cleaned, color = colors[7], plot = T)
eval_auroc(scca_bio_pred_pheno, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(scca_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("scca-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_chem_pred_bio, bio_cleaned, color = colors[7], plot = T)
eval_auroc(scca_pheno_pred_bio, bio_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(scca_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("svm-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(svm_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[7], plot = T)
eval_auroc(svm_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(svm_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("svm-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(svm_chem_pred_bio$pred_score, bio_cleaned, color = colors[7], plot = T)
eval_auroc(svm_pheno_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(svm_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("rf-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(rf_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[7], plot = T)
eval_auroc(rf_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(rf_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("rf-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(rf_chem_pred_bio$pred_score, bio_cleaned, color = colors[7], plot = T)
eval_auroc(rf_pheno_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(rf_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("knn-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(knn_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[7], plot = T)
eval_auroc(knn_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(knn_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("knn-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(knn_chem_pred_bio$pred_score, bio_cleaned, color = colors[7], plot = T)
eval_auroc(knn_pheno_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_auroc(knn_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
>>>>>>> a109cfd... Using random forest as new algorithm for the experiment, nearly done with data visualization
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
