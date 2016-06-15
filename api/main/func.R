### Current parameter setting: c1 = 0.1, c2 = 0.1, ncomp = 80
scca <- function(x, y, c1 = 0.1, c2 = 0.1, ncomp = 80) {
  result <- CCA(x = x, z = y, typex = "standard", typez = "standard", penaltyx = c1, penaltyz = c2, K = ncomp, trace = FALSE, standardize = FALSE)
  ### NOT SURE IF IT'S ALRIGHT
  # adjustment of positive and negatieve
#  for(i in 1:ncomp) {
#    if(result$u[order(abs(result$u[, i]), decreasing = T)[1], i] < 0) {
#      result$u[, i] <- -result$u[, i]
#      result$v[, i] <- -result$v[, i]
#    }
#  }
  ###
  rownames(result$u) <- colnames(x)
  rownames(result$v) <- colnames(y)
  scorex <- x %*% result$u
  scorey <- y %*% result$v
  rownames(scorex) <- rownames(x)
  rownames(scorey) <- rownames(y)
  corvec <- rep(0, ncomp)
  for(i in 1:ncomp)
    corvec[i] <- cor(scorex[, i], scorey[, i])
  list(u = result$u, v = result$v, d = result$d, scorex = scorex, scorey = scorey, rho = corvec)
}

scca_pred <- function(x, y, pred) {
  # standardize x, y, pred
  xmean <- apply(x, 2, mean)
  ymean <- apply(y, 2, mean)
  xsd <- apply(x, 2, sd)
  ysd <- apply(y, 2, sd)
  xsd[xsd == 0] <- 1
  ysd[ysd == 0] <- 1
  x <- x - matrix(xmean, nrow(x), ncol(x), byrow = TRUE)
  pred <- pred - matrix(xmean, nrow(pred), ncol(pred), byrow = TRUE)
  y <- y - matrix(ymean, nrow(y), ncol(y), byrow = TRUE)
  x <- x * matrix(1/xsd, nrow(x), ncol(x), byrow = TRUE)
  pred <- pred * matrix(1/xsd, nrow(pred), ncol(pred), byrow = TRUE)
  y <- y * matrix(1/ysd, nrow(y), ncol(y), byrow = TRUE)
  # training
  result <- scca(x, y)
  # testing
  ### predict score (DON'T KNOW WHY)
  #> result <- scca(wo_antidp_chem, wo_antidp_bio)
  #> dim(antidp_chem)
  #[1]  15 653
  #> dim(result$u)
  #[1] 653  80
  #> dim(diag(result$rho))
  #[1] 80 80
  #> dim(t(result$v))
  #[1]  80 984
  pred_score <- (pred %*% result$u) %*% diag(result$rho) %*% t(result$v)
  ### DON'T KNOW WHY
  pred_score  <- pred_score * matrix(ysd, nrow = nrow(pred), ncol = ncol(y), byrow = TRUE)
  pred_score  <- pred_score + matrix(ymean, nrow = nrow(pred), ncol = ncol(y), byrow = TRUE)
  ###
  rownames(pred_score) <- rownames(pred)
  colnames(pred_score) <- colnames(y)
  pred_score
}

### try sigma = 0.008, C = 2
svm_pred <- function(x, y, pred) {
  pred_score <- matrix(0, nrow(pred), ncol(y))
  for(i in 1:ncol(y)) {
    ### DON'T KNOW WHY
    if(length(unique(y[, i])) == 1) {
      # no training
      # testing prediction score
      pred_score[, i] <- rep(-1, nrow(pred))
    ###
    } else {
      # training
      model <- ksvm(x = x, y = y[, i], type = "C-svc", scaled = TRUE, kernel = "rbfdot", kpar = list(sigma = 0.2), C = 1)
      # testing
      pred_score[, i] <- predict(model, pred, type = "decision")
    }
  }
  rownames(pred_score) <- rownames(pred)
  colnames(pred_score) <- colnames(y)
  pred_score
}

rf_pred <- function(x, y, pred) {
  pred_score <- matrix(0, nrow(pred), ncol(y))
  for(i in 1:ncol(y)) {
    # training
    model <- randomForest(x, y[, i], ntree = 100, nodesize = 5, mtry = 10)
    # testing
    pred_score[, i] <- predict(model, pred, type = "response")
  }
  rownames(pred_score) <- rownames(pred)
  colnames(pred_score) <- colnames(y)
  pred_score
}

### try k = 55
knn_pred <- function(x, y, pred, k = 50) {
  pred_score <- matrix(0, nrow(pred), ncol(y))
  score <- pred %*% t(x)
  for(i in 1:nrow(pred)){
    testing_idx <- 1:nrow(x)
    maxidx <- testing_idx[order(score[i, testing_idx], decreasing = T)[1:k]]
    if(k == 1)
      avg <- y[maxidx, ]
    else
      avg <- apply(y[maxidx, ], 2, mean)
    pred_score[i, ] <- avg
  }
  rownames(pred_score) <- rownames(pred)
  colnames(pred_score) <- colnames(y)
  pred_score
}

### Do cross-validation
cv <- function(x, y, fold = 5, method = "rf") {
  # start timer
  start_time <- proc.time()
  size <- trunc(nrow(x) / fold)
  idx <- 1:nrow(x)
  # prediction score matrix
  pred_score <- matrix(0, nrow(x), ncol(y))
  rownames(pred_score) <- rownames(y)
  colnames(pred_score) <- colnames(y)
  for(i in 1:fold) {
    if(length(idx) >= size) {
      testing_idx <- sample(idx, size = size)
      idx <- setdiff(idx, testing_idx)
    } else {
      testing_idx <- idx
      idx <- setdiff(idx, testing_idx)
    }
    x_obs <- x[-testing_idx, ]
    y_obs <- y[-testing_idx, ]
    x_pred <- x[testing_idx, ]
    if(method == "scca")
      pred_score[testing_idx, ] <- scca_pred(x_obs, y_obs, x_pred)
    else if(method == "svm")
      pred_score[testing_idx, ] <- svm_pred(x_obs, y_obs, x_pred)
    else if(method == "rf")
      pred_score[testing_idx, ] <- rf_pred(x_obs, y_obs, x_pred)
    else if(method == "knn")
      pred_score[testing_idx, ] <- knn_pred(x_obs, y_obs, x_pred)
  }
#  list(pred_score = pred_score, elapsed_time = proc.time() - start_time)
  list(micro_avg = perf_eval(pred_score, y), pred_score = pred_score, elapsed_time = proc.time() - start_time)
}

perf_eval <- function(pred, obs) {
  # calculate micro-average (local metrics)
  auroc <- eval_auroc(pred, obs)
  aupr <- eval_aupr(pred, obs)
  # calculate macro-average (global metrics)
### TROUBLE
#  global_auroc <- rep(0, ncol(obs))
#  global_aupr <- rep(0, ncol(obs))
#  for(i in 1:ncol(obs)) {
#    global_auroc[i] <- eval_auroc(pred[, i], obs[, i])
#    global_aupr[i] <- eval_aupr(pred[, i], obs[, i])
#  }
#  list(macro_avg = list(auroc = mean(global_auroc), aupr = mean(global_aupr)), micro_avg = list(auroc = auroc, aupr = aupr))
###
  list(auroc = auroc, aupr = aupr)
}

### Evaluate area under receiver operating characteristic (ROC) curve by ROCR
eval_aupr <- function(pred, obs, color = "black", plot = FALSE, add = FALSE) {
  rocr_pred <- prediction(as.vector(pred), as.vector(obs))
  rocr_result <- performance(rocr_pred, "prec", "rec")
  if(plot == TRUE) {
    plot(rocr_result, col = color, add = add, xlim = c(0, 1), ylim = c(0, 1))
    ### Execute this block in the console after finishing plotting all the curves to add legend, title and a random line to the plot
    # legend("bottomright", "scca", cex = 0.7, lwd = c(1, 1), col = color, inset = 0.05)
    # mtext("SCCA", line = 0.5)
    # abline(a = 0, b = 1, lty = "dashed")
    ###
  }
  f <- approxfun(sapply(rocr_result@x.values, function(x) {ifelse(is.na(x), 0, x)}),
                 sapply(rocr_result@y.values, function(x) {ifelse(is.na(x), 0, x)}))
  integrate(f, 0, 1, subdivisions = 2000)$value
}

### Evaluate area under receiver operating characteristic (ROC) curve by ROCR
eval_auroc <- function(pred, obs, color = "black", plot = FALSE, show_threshold = FALSE, add = FALSE) {
  rocr_pred <- prediction(as.vector(pred), as.vector(obs))
  rocr_result <- performance(rocr_pred, "auc")
  auc <- rocr_result@y.values[[1]]
  if(plot == TRUE) {
    rocr_result <- performance(rocr_pred, "tpr", "fpr")
    cutoff <- best_cutoff(rocr_result, rocr_pred)
    plot(rocr_result, col = color, add = add, xlim = c(0, 1), ylim = c(0, 1))
    if(show_threshold) {
      text <- paste0("Threshold: ", round(cutoff[3], 5), "\nAUC: ", round(auc, 5), "\n(", round(cutoff[1], 2), ", ", round(cutoff[2], 2), ")")
      text(cutoff[1], cutoff[2], labels = text, cex = 0.7, pos = 3, col = color)
      points(cutoff[1], cutoff[2], pch = 20, col = color)
    }
    ### Execute this block in the console after finishing plotting all the curves to add legend, title and a random line to the plot
    # legend("bottomright", "scca", cex = 0.7, lwd = c(1, 1), col = color, inset = 0.05)
    # mtext("SCCA", line = 0.5)
    # abline(a = 0, b = 1, lty = "dashed")
    ###
    list(threshold = cutoff[3], auc = auc)
  } else
    auc
}

### Determine the best cutoff by finding point closest to P(0, 1)
best_cutoff <- function(rocr_result, rocr_pred) {
    result = mapply(FUN <- function(x, y, p) {
        d <- (x - 0)^2 + (y-1)^2
        ind <- which(d == min(d))
        c(FPR = x[[ind]], TPR = y[[ind]], cutoff = p[[ind]])
    }, rocr_result@x.values, rocr_result@y.values, rocr_pred@cutoffs)
}

### Determine the best cutoff by finding the highest F measure
best_fcutoff <- function(pred, obs) {
  rocr_pred <- prediction(as.vector(pred), as.vector(obs))
  rocr_result <- performance(rocr_pred, measure = "f", x.measure = "cutoff")
  best_f <- which.max(rocr_result@"y.values"[[1]])
  x <- rocr_result@"x.values"[[1]][best_f]
  y <- rocr_result@"y.values"[[1]][best_f]
  plot(rocr_result, xlim = c(0, 1), ylim = c(0, 1))
  text <- paste0("(", round(x, 5), ", ", round(y, 5), ")")
  text(x, y, labels = text, cex = 0.7, pos = 3)
  points(x, y, pch = 20)
  list(threshold = x, f = y)
}

### Calculate Z-Score
cal_zscore <- function(x) {
  mean <- mean(x)
  sd <- sd(x)
  if(sd == 0)
    sd <- 1
  x <- x - matrix(mean, nrow(x), ncol(x), byrow = TRUE)
  x <- x * matrix(1/sd, nrow(x), ncol(x), byrow = TRUE)
  x
}

### Convert original matrix into score matrix with format: Drug | Protein/ADR | Score, sort by score in descending order
make_score_matrix <- function(m) {
  sm <- matrix(0, nrow = nrow(m) * ncol(m), ncol = 3)
  k <- 1
  for(i in 1:nrow(m)) {
    for(j in 1:ncol(m)) {
      sm[k, 1] <- rownames(m)[i]
      sm[k, 2] <- colnames(m)[j]
      sm[k, 3] <- m[i, j]
      k <- k + 1
    }
  }
  sm <- sm[order(sm[, 3], decreasing = T), ]
}

### Convert predict and observe matrices into frequency matrix with format: Subject(ADR or Protein) | State(Predict or Observe) | Count, sort by ADR/Protein in alphabetical order
make_freq_matrix <- function(predm, obsm) {
  tmp <- apply(predm, 2, sum)
  tmp <- tmp[tmp != 0]
  tmp <- tmp[order(tmp, decreasing = T)]
  tmp <- melt(tmp)
  tmp <- tmp %>% mutate(Subject = rownames(tmp))
  colnames(tmp) <- c("Predict", "Subject")
  tmp2 <- apply(obsm, 2, sum)
  tmp2 <- tmp2[tmp2 != 0]
  tmp2 <- tmp2[order(tmp2, decreasing = T)]
  tmp2 <- melt(tmp2)
  tmp2 <- tmp2 %>% mutate(Subject = rownames(tmp2))
  colnames(tmp2) <- c("Observe", "Subject")
  colnames(tmp) <- c("Predict", "Subject")
  fm <- merge(tmp, tmp2, all = T)
  fm3[is.na(fm)] <- 0
  colnames(fm) <- c("Subject", "Predict", "Observe")
  fm <- melt(fm)
  colnames(fm) <- c("Subject", "State", "Count")
  fm <- fm[order(fm$Subject), ]
}
