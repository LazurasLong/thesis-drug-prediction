scca <- function(x, y, c1 = 0.1, c2 = 0.1, ncomp = 80) {
  result <- CCA(x = x, z = y, typex = "standard", typez = "standard", penaltyx = c1, penaltyz = c2, K = ncomp, trace = FALSE, standardize = FALSE)
  rownames(result$u) <- colnames(x)
  rownames(result$v) <- colnames(y)
  scorex <- x %*% result$u
  scorey <- y %*% result$v
  rownames(scorex) <- rownames(x)
  rownames(scorey) <- rownames(y)
  corvec <- rep(0, ncomp)
  for(j in 1:ncomp) {
    corvec[j] <- cor(scorex[, j], scorey[, j])
  }
  list(u = result$u, v = result$v, d = result$d, scorex = scorex, scorey = scorey, rho = corvec)
}

scca_pred <- function(x, y, pred, c1 = 0.1, c2 = 0.1, ncomp = 80) {
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
  result <- scca(x = x, y = y, c1 = c1, c2 = c2, ncomp = ncomp)
  # testing
  ### predict score (DON'T KNOW WHY)
  pred_score <- (pred %*% result$u) %*% diag(result$rho) %*% t(result$v)
  ### DON'T KNOW WHY
  pred_score  <- pred_score * matrix(ysd, nrow = nrow(pred), ncol = ncol(y), byrow = TRUE)
  pred_score  <- pred_score + matrix(ymean, nrow = nrow(pred), ncol = ncol(y), byrow = TRUE)
  ###
  rownames(pred_score) <- rownames(pred)
  colnames(pred_score) <- colnames(y)
  pred_score
}

best_cutoff <- function(rocr_result, rocr_pred) {
    result = mapply(FUN <- function(x, y, p) {
        d <- (x - 0)^2 + (y-1)^2
        ind <- which(d == min(d))
        c(FPR = x[[ind]], TPR = y[[ind]], cutoff = p[[ind]])
    }, rocr_result@x.values, rocr_result@y.values, rocr_pred@cutoffs)
}

perf_eval <- function(pred, obs) {
  # calculate micro-average (local metrics)
  auroc <- eval_auroc(as.vector(pred), as.vector(obs))
  # calculate macro-average (global metrics)
  local_auroc <- rep(0, ncol(obs))
  for(i in 1:ncol(obs)) {
    local_auroc[i] <- eval_auroc(pred[, i], obs[, i])
  }
  list(macro_avg = mean(local_auroc), micro_avg = auroc)
}

### Evaluate area under receiver operating characteristic (ROC) curve by ROCR
eval_auroc <- function(pred, obs, color, plot = FALSE, show_threshold = FALSE, add = FALSE) {
  rocr_pred <- prediction(as.vector(pred), as.vector(obs))
  rocr_result <- performance(rocr_pred, "auc")
  auc <- rocr_result@y.values[[1]]
  if(plot == TRUE) {
    rocr_result <- performance(rocr_pred, "tpr", "fpr")
    cutoff <- best_cutoff(rocr_result, rocr_pred)
    plot(rocr_result, col = color, add = add)
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

### Do cross-validation
cv <- function(x, y, fold = 5, method = "scca") {
  # start timer
  start_time <- proc.time()
  size <- trunc(nrow(x) / fold)
  idx <- 1:nrow(x)
  # prediction score matrix
  pred_score <- matrix(0, nrow(x), ncol(y))
  rownames(pred_score) <- rownames(y)
  colnames(pred_score) <- colnames(y)
  for(r in 1:fold) {
    # random seed
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
    y_pred <- y[testing_idx, ]
    if(method == "scca")
      pred_score[testing_idx, ] <- scca_pred(x_obs, y_obs, x_pred, c1 = 0.1, c2 = 0.1, ncomp = 80)
  }
  list(auroc = perf_eval(pred_score, y), pred_score = pred_score, elapsed_time = proc.time() - start_time)
}
