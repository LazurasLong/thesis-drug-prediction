### Draw barplot
library(ggnet)
library(ggplot2)
library(ggthemes)

tmp <- seq(200, 2614, 200)
tmp[13] <- 2614
j <- 1
k <- 1
for(i in tmp) {
  png(paste0("antidp-se-freq (barplot visualization)-", k, ".png"), units = "px", width = 3200, height = 1600, res = 300)
  print(ggplot(rf_antidp_chem_plus_bio_pred_pheno_fcutoff_and_obs[j:i, ], aes(x = SE, y = Count, fill = State)) + geom_bar(position = "identity", stat = "identity", alpha = .5) + labs(fill = "") + xlab("Side Effect") + theme(text = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) + theme(panel.background = element_blank(), plot.background = element_rect(fill = "#f8f2e4"), legend.background = element_blank()) + scale_fill_stata())
  dev.off()
  j <- i
  k <- k + 1
}

png("antidp-target-freq (barplot visualization).png", units = "px", width = 3200, height = 1600, res = 300)
print(ggplot(rf_antidp_chem_plus_pheno_pred_bio_fcutoff_and_obs, aes(x = Target, y = Count, fill = State)) + geom_bar(position = "identity", stat = "identity", alpha = .5) + labs(fill = "") + theme(text = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) + theme(panel.background = element_blank(), plot.background = element_rect(fill = "#f8f2e4"), legend.background = element_blank()) + scale_fill_wsj("colors6", ""))
dev.off()
###

### Draw network graph
library(ggnet)
library(ggplot2)
library(ggthemes)

tmp <- seq(50, 522, 52)
tmp[10] <- 522
j <- 1
k <- 1
for(i in tmp) {
    png(paste0("rf-antidp-chem-plus-bio-pred-pheno (network visualization)-", k, ".png"), units = "px", width = 3200, height = 1600, res = 300)
    print(ggnet2(network(rf_antidp_chem_plus_bio_pred_pheno_fcutoff_cleaned[, i:j], directed = T), color = "mode", palette = "Set1", label = T, label.alpha = 0.8, label.size = 2, alpha = 0.7, edge.size = 0.25, edge.alpha = 1, edge.color = "grey", size = "degree", size.legend = "Degree", size.cut = 5, size.min = 1, mode = "circle") + theme_wsj() + scale_color_stata(labels = c("actor" = "Drug", "event" = "Side Effect"), name = "") + guides(size = guide_legend(order = 1), colour = guide_legend(order = 2)))
    dev.off()
    j <- i
    k <- k + 1
}

png("rf-antidp-chem-plus-pheno-pred-bio (network visualization).png", units = "px", width = 3200, height = 1600, res = 300)
ggnet2(network(rf_antidp_chem_plus_pheno_pred_bio_fcutoff_cleaned, directed = T), color = "mode", palette = "Set1", label = T, label.alpha = 0.8, label.size = 2, alpha = 0.7, edge.size = 0.25, edge.alpha = 1, edge.color = "grey", size = "degree", size.legend = "Degree", size.cut = 5, mode = "kamadakawai") + theme_wsj() + scale_colour_wsj("colors6", "", labels = c("actor" = "Drug", "event" = "Target"))
dev.off()

# Input should be a weighted matrix: 1: solid (known interaction in grey), 3: dotted (predict interaction in grey), 5: longdash (validated interaction in #D07A77)
tmp <- network(rf_antidp_chem_plus_pheno_pred_bio_fcutoff_cleaned_weighted, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weights")
set.edge.attribute(tmp, "color", ifelse(tmp %e% "weights" == 5, "#D07A77", "grey"))
png("rf_antidp_chem_plus_pheno_pred_bio (network visualization).png", units = "px", width = 3200, height = 1600, res = 300)
ggnet2(tmp, color = "mode", palette = "Set1", label = T, label.alpha = 0.8, label.size = 2, alpha = 0.7, edge.lty  = "weights", edge.color = "color", edge.alpha = 1, size = "degree", size.legend = "Degree", size.cut = 5, mode = "kamadakawai") + theme_wsj() + scale_colour_wsj("colors6", "", labels = c("actor" = "Drug", "event" = "Target")) + guides(size = guide_legend(order = 1), colour = guide_legend(order = 2))
dev.off()

# Subgroup three drugs in one graph
tmp <- network(rf_antidp_chem_plus_pheno_pred_bio_fcutoff_cleaned_weighted[1:3, apply(rf_antidp_chem_plus_pheno_pred_bio_fcutoff_cleaned_weighted[1:3, ], 2, sum) != 0], matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weights")
set.edge.attribute(tmp, "color", ifelse(tmp %e% "weights" == 5, "#D07A77", "grey"))
png("rf_antidp_chem_plus_pheno_pred_bio (network visualization)-1.png", units = "px", width = 3200, height = 1600, res = 300)
ggnet2(tmp, color = "mode", palette = "Set1", label = T, label.alpha = 0.8, label.size = 2, alpha = 0.7, edge.lty  = "weights", edge.color = "color", edge.alpha = 1, size = "degree", size.legend = "Degree", size.cut = 5, mode = "kamadakawai") + theme_wsj() + scale_colour_wsj("colors6", "", labels = c("actor" = "Drug", "event" = "Target")) + guides(size = guide_legend(order = 1), colour = guide_legend(order = 2))
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
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("scca-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_chem_pred_pheno, pheno_cleaned, color = colors[7], plot = T)
eval_aupr(scca_bio_pred_pheno, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(scca_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("scca-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_chem_pred_bio, bio_cleaned, color = colors[7], plot = T)
eval_aupr(scca_pheno_pred_bio, bio_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(scca_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("svm-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(svm_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[7], plot = T)
eval_aupr(svm_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(svm_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("svm-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(svm_chem_pred_bio$pred_score, bio_cleaned, color = colors[7], plot = T)
eval_aupr(svm_pheno_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(svm_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("rf-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(rf_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[7], plot = T)
eval_aupr(rf_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(rf_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("rf-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(rf_chem_pred_bio$pred_score, bio_cleaned, color = colors[7], plot = T)
eval_aupr(rf_pheno_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(rf_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("knn-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(knn_chem_pred_pheno$pred_score, pheno_cleaned, color = colors[7], plot = T)
eval_aupr(knn_bio_pred_pheno$pred_score, pheno_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(knn_chem_plus_bio_pred_pheno, pheno_cleaned, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("knn-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(knn_chem_pred_bio$pred_score, bio_cleaned, color = colors[7], plot = T)
eval_aupr(knn_pheno_pred_bio$pred_score, bio_cleaned, color = colors[6], plot = T, add = T)
eval_aupr(knn_chem_plus_pheno_pred_bio, bio_cleaned, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

###
png("scca-antidp-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_auroc(scca_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_auroc(scca_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("scca-antidp-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(scca_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_auroc(scca_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_auroc(scca_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("svm-antidp-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(svm_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_auroc(svm_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_auroc(svm_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("svm-antidp-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(svm_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_auroc(svm_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_auroc(svm_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("rf-antidp-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(rf_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_auroc(rf_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_auroc(rf_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("rf-antidp-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(rf_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_auroc(rf_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_auroc(rf_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("knn-antidp-predict-pheno.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(knn_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_auroc(knn_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_auroc(knn_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("knn-antidp-predict-bio.png", units = "px", width = 1600, height = 1600, res = 300)
eval_auroc(knn_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_auroc(knn_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_auroc(knn_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
abline(a = 0, b = 1, lty = "dashed")
legend("bottomright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("ROC", line = 0.5)
dev.off()

png("scca-antidp-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_aupr(scca_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_aupr(scca_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("scca-antidp-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(scca_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_aupr(scca_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_aupr(scca_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("svm-antidp-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(svm_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_aupr(svm_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_aupr(svm_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("svm-antidp-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(svm_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_aupr(svm_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_aupr(svm_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("rf-antidp-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(rf_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_aupr(rf_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_aupr(rf_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("rf-antidp-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(rf_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_aupr(rf_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_aupr(rf_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("knn-antidp-predict-pheno (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(knn_antidp_chem_pred_pheno, antidp_pheno, color = colors[7], plot = T)
eval_aupr(knn_antidp_bio_pred_pheno, antidp_pheno, color = colors[6], plot = T, add = T)
eval_aupr(knn_antidp_chem_plus_bio_pred_pheno, antidp_pheno, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict pheno", "bio predict pheno", "chem unite bio predict pheno"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()

png("knn-antidp-predict-bio (pr).png", units = "px", width = 1600, height = 1600, res = 300)
eval_aupr(knn_antidp_chem_pred_bio, antidp_bio, color = colors[7], plot = T)
eval_aupr(knn_antidp_pheno_pred_bio, antidp_bio, color = colors[6], plot = T, add = T)
eval_aupr(knn_antidp_chem_plus_pheno_pred_bio, antidp_bio, color = colors[5], plot = T, add = T)
abline(a = 1, b = -1, lty = "dashed")
legend("topright", c("chem predict bio", "pheno predict bio", "chem unite pheno predict bio"), cex = 0.7, lwd = c(1, 1), col = colors[7:5], inset = 0.05)
mtext("PR", line = 0.5)
dev.off()
###

### Draw freq. chart and csv
tmp <- read.csv("scca-antidp-chem-intersect-pheno-predict-bio.csv", row.names = 1, check.names = F)
tmp <- tmp[, colSums(tmp) != 0]
tmp <- as.data.frame(tmp)
tmp <- data.frame(Feature = colnames(tmp), Count = apply(tmp, 2, sum))
png("tmp.png", units = "px", width = 1600, height = 1600, res = 300)
ggplot(tmp, aes(x = Count)) + geom_histogram(binwidth = 1) + xlab("Freq. of predicted antidepressant biological features")
dev.off()
write.csv(tmp[order(tmp$Count, decreasing = T), ], "freq. of predicted antidepressant biological features.csv")
###

### Testing ggplot inside eval_auroc (too slow)
fpr_tpr <- as.data.frame(cbind(fpr = rocr_result@x.values[[1]], tpr = rocr_result@y.values[[1]]))
print(ggplot(fpr_tpr, aes(x = fpr, y = tpr, col = "scca"), col = color) +
  geom_line() +
  geom_point(aes(x = cutoff[1], y = cutoff[2])) +
  xlab("False positive rate (1-Specificity)") +
  ylab("True positive rate (Sensitivity)") +
  labs(col = "Algorithms"))
###

### Change label factor from numeric to character (one time process)
without_antidepressant_bio <- as.data.frame(apply(without_antidepressant_bio, 2, factor, levels = c(0, 1), label = c("no", "yes")))
###

### Check antidepressant info.
# ex. PubChem Id: 3386
which(rownames(biomat) == 3386)
# [1] 204
colnames(biomat[, which(biomat[204, ] == 1)])
# [1] "AOFB_HUMAN"  "SC6A4_HUMAN"
###

### Find the particular drug's max score index of prediction producing by CCA
test <- as.data.frame(bio.newpred)
which.max(test[!is.na(match(rownames(test), 2337)), ])
###

### Query the names of drug targets that are in the data matrix
sum(apply(rf_antidp_chem_plus_pheno_pred_bio_fcutoff_cleaned_weighted, 2, function(x) {any(x == 5)}))
###

### Save & Load model
save(knn_pharm_bio_model, file = "knn-pharm-bio-model698-1368.RData")
load(file = "knn-pharm-bio-model698-1368.RData")
###

### [Data resampling] Addressing data imbalanced situation by resampling training set prior to training
down_training_set <- downSample(training_set[, -ncol(training_set)], training_set$LABEL, yname = "LABEL")
# high performance with very long computational time (not yet try to tune the parameter)
up_training_set <- upSample(training_set[, -ncol(training_set)], training_set$LABEL, yname = "LABEL")
# computational cost and performanerce dilemma: perc.over = 1000, perc.under = 250 (current optimal)
smote_training_set <- SMOTE(LABEL ~ ., training_set, perc.over = 1000, perc.under = 250)
# good performanerce with quite expensive computational cost (not yet try to tune the parameter )
rose_training_set <- ROSE(LABEL ~ ., training_set)$data
###

### Funtional as label powerset (LP) muti-labels transforming
pharm_glue <- as.data.frame(apply(pharm, 1, paste, collapse=""))
colnames(pharm_glue) <- "LABEL"
###

### mldr & RWeka package - dealing with multi-label situation
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

### Run caret
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
#try replace with as vector
obs <- melt(antidepressant_bio, measure.vars = colnames(antidepressant_bio))[, 2]
#try replace with as vector
###

### Performance evaluation: plot roc curve
# plot roc curve in terms of sensitivity and specificity
roc_result <- roc(obs, pred)
# plot(roc_result)
# finding threshold by ploting roc curve with best cutoff points by maximizing sensitivity and specificity
# plot(roc_result, print.thres = "best", print.thres.best.method = "youden")
# finding threshold by ploting roc curve with best cutoff points by finding the closest point to (0, 1)
# plot(roc_result, print.thres = "best", print.thres.best.method = "closest.topleft")
###

### Confusion matrix
confusionMatrix(rf_chem_intersect_bio_pred_pheno_cutoff, pheno_cleaned, positive = "1")
###
